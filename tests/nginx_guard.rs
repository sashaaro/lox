//! Интеграционный тест FFI-клея модуля `ngx_http_lox_module`.
//!
//! Собирает модуль (и vendored nginx), поднимает реальный nginx с тест-конфигом,
//! где директива `lox_guard` стоит в нескольких `location`, и проверяет, что guard-ы
//! на lox корректно пропускают запрос или возвращают 403.
//!
//! Тест помечен `#[ignore]` — он медленный (при первом запуске собирает nginx из
//! исходников) и требует libclang. Запуск:
//!
//! ```bash
//! cargo test -p codecrafters-interpreter --test nginx_guard -- --ignored --nocapture
//! ```

use std::io::Write;
use std::net::TcpStream;
use std::path::{Path, PathBuf};
use std::process::{Child, Command};
use std::time::{Duration, Instant};

/// Корень workspace (он же корень корневого crate).
fn workspace_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
}

/// Каталог сборки cargo (`target/`), с учётом возможного `CARGO_TARGET_DIR`.
fn target_dir() -> PathBuf {
    match std::env::var_os("CARGO_TARGET_DIR") {
        Some(dir) => PathBuf::from(dir),
        None => workspace_root().join("target"),
    }
}

/// Имя файла собранного динамического модуля для текущей ОС.
fn module_filename() -> &'static str {
    if cfg!(target_os = "macos") {
        "libngx_lox.dylib"
    } else {
        "libngx_lox.so"
    }
}

/// Собрать модуль + vendored nginx в профиле debug.
fn build_module() {
    let status = Command::new(env!("CARGO"))
        .current_dir(workspace_root())
        .args(["build", "-p", "ngx-lox", "--features", "vendored"])
        .status()
        .expect("failed to spawn cargo build");
    assert!(status.success(), "cargo build -p ngx-lox failed");
}

/// Путь к собранному модулю.
fn module_path() -> PathBuf {
    let p = target_dir().join("debug").join(module_filename());
    assert!(p.exists(), "module not found at {}", p.display());
    p
}

/// Путь к бинарю nginx, собранному nginx-sys (хеш в пути нестабилен — берём по glob).
fn find_nginx() -> PathBuf {
    let build_dir = target_dir().join("debug").join("build");
    let entries = std::fs::read_dir(&build_dir)
        .unwrap_or_else(|e| panic!("cannot read {}: {e}", build_dir.display()));
    for entry in entries.flatten() {
        let name = entry.file_name();
        if name.to_string_lossy().starts_with("nginx-sys-") {
            let candidate = entry.path().join("out/objs/nginx");
            if candidate.is_file() {
                return candidate;
            }
        }
    }
    panic!("nginx binary not found under {}", build_dir.display());
}

/// Свободный TCP-порт на localhost.
fn free_port() -> u16 {
    std::net::TcpListener::bind("127.0.0.1:0")
        .expect("bind ephemeral port")
        .local_addr()
        .unwrap()
        .port()
}

/// Временный prefix nginx; удаляется при Drop (в т.ч. при панике ассерта).
struct TempPrefix(PathBuf);

impl TempPrefix {
    fn new() -> Self {
        let dir = std::env::temp_dir().join(format!("lox-nginx-test-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(dir.join("logs")).unwrap();
        std::fs::create_dir_all(dir.join("conf")).unwrap();
        TempPrefix(dir)
    }
    fn path(&self) -> &Path {
        &self.0
    }
}

impl Drop for TempPrefix {
    fn drop(&mut self) {
        let _ = std::fs::remove_dir_all(&self.0);
    }
}

/// Запущенный процесс nginx; глушится при Drop.
struct NginxProc(Child);

impl Drop for NginxProc {
    fn drop(&mut self) {
        let _ = self.0.kill();
        let _ = self.0.wait();
    }
}

/// Дождаться, пока nginx начнёт принимать соединения на порту (таймаут 5 c).
fn wait_ready(port: u16) {
    let deadline = Instant::now() + Duration::from_secs(5);
    while Instant::now() < deadline {
        if TcpStream::connect(("127.0.0.1", port)).is_ok() {
            return;
        }
        std::thread::sleep(Duration::from_millis(50));
    }
    panic!("nginx did not start listening on port {port}");
}

/// Извлечь HTTP-статус из результата ureq: не-2xx по умолчанию приходит как
/// `Err(Error::StatusCode)`, его тоже считаем валидным кодом ответа.
fn status(result: Result<ureq::http::Response<ureq::Body>, ureq::Error>) -> u16 {
    match result {
        Ok(resp) => resp.status().as_u16(),
        Err(ureq::Error::StatusCode(code)) => code,
        Err(e) => panic!("request failed: {e}"),
    }
}

#[test]
#[ignore = "запускает реальный nginx; cargo test ... -- --ignored"]
fn lox_guard_allows_and_denies() {
    build_module();
    let nginx = find_nginx();
    let module = module_path();
    let port = free_port();
    let backend_port = free_port();
    let prefix = TempPrefix::new();

    // Guard работает на access-фазе, поэтому контент location'а должен производиться в
    // контент-фазе (после access). `return 200` отрабатывает раньше — в rewrite-фазе — и
    // обошёл бы guard. Поэтому фронт-location'ы проксируют на backend-сервер, чей
    // `return 200` к этому моменту уже за access-фазой фронта.
    let conf_path = prefix.path().join("conf/nginx.conf");
    let conf = format!(
        r#"daemon off;
master_process off;
error_log {logs}/error.log info;
pid {logs}/nginx.pid;
load_module {module};
events {{}}
http {{
    access_log off;

    server {{
        listen 127.0.0.1:{backend_port};
        location / {{ return 200 "ok"; }}
    }}

    server {{
        listen 127.0.0.1:{port};

        location /open {{
            proxy_pass http://127.0.0.1:{backend_port};
        }}
        location /admin {{
            lox_guard 'header("x-token") == "secret";';
            proxy_pass http://127.0.0.1:{backend_port};
        }}
        location /api/legacy {{
            lox_guard '!(method == "POST");';
            proxy_pass http://127.0.0.1:{backend_port};
        }}
    }}
}}
"#,
        logs = prefix.path().join("logs").display(),
        module = module.display(),
        port = port,
        backend_port = backend_port,
    );
    std::fs::File::create(&conf_path)
        .unwrap()
        .write_all(conf.as_bytes())
        .unwrap();

    let child = Command::new(&nginx)
        .args([
            "-p",
            &prefix.path().to_string_lossy(),
            "-c",
            &conf_path.to_string_lossy(),
        ])
        .spawn()
        .expect("failed to spawn nginx");
    let _proc = NginxProc(child);
    wait_ready(port);

    let base = format!("http://127.0.0.1:{port}");

    // Контроль: location без guard работает как обычно.
    assert_eq!(status(ureq::get(format!("{base}/open")).call()), 200);

    // guard по заголовку: без X-Token — 403, с правильным — 200.
    assert_eq!(status(ureq::get(format!("{base}/admin")).call()), 403);
    assert_eq!(
        status(
            ureq::get(format!("{base}/admin"))
                .header("x-token", "secret")
                .call()
        ),
        200
    );

    // guard по методу: POST запрещён, GET разрешён.
    assert_eq!(
        status(ureq::post(format!("{base}/api/legacy")).send_empty()),
        403
    );
    assert_eq!(status(ureq::get(format!("{base}/api/legacy")).call()), 200);
}
