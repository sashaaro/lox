fn main() {
    // Символы nginx (ngx_array_push, ngx_palloc, ...) предоставляются бинарём nginx
    // в момент загрузки модуля, а не при линковке. На macOS линкер по умолчанию падает
    // на неразрешённых символах — разрешаем их отложенный поиск (как делает nginx на Linux).
    if std::env::var("CARGO_CFG_TARGET_OS").as_deref() == Ok("macos") {
        println!("cargo::rustc-link-arg=-undefined");
        println!("cargo::rustc-link-arg=dynamic_lookup");
    }
}
