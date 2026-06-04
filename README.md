# Lox Interpreter in Rust

Это шаг за шагом реализованный интерпретатор языка Lox из книги [Crafting Interpreters](https://craftinginterpreters.com/), написанный на Rust.

## Пример

```bash
var a = [10, 20, 30];
a[1] = 42;
print a[1]; // → 42

fun add(a, b) {
  return a + b;
}
print add(2, 3); // → 5
```

### Запуск

```bash
cargo run
```


### Тесты

```bash
cargo test
```

## nginx-модуль: guard-ы на lox

Воркспейс содержит crate [`nginx-module`](nginx-module) (`ngx_http_lox_module`) на базе
[ngx-rust](https://github.com/nginx/ngx-rust). Он добавляет директиву `lox_guard`, которая
позволяет писать условия допуска запросов прямо на языке lox: на access-фазе выражение
вычисляется, и если оно «истинно» — запрос проходит дальше, иначе nginx возвращает
`403 Forbidden`.

В окружение guard-а инжектируется контекст запроса:

| Имя | Тип | Значение |
|-----|-----|----------|
| `uri`            | переменная | путь запроса, например `/admin` |
| `method`         | переменная | HTTP-метод: `GET`, `POST`, … |
| `header("name")` | функция    | значение заголовка запроса (или `nil`); имя без учёта регистра |

### Сборка модуля

Зависимости: C-компилятор, make, OpenSSL, PCRE2, Zlib, libclang (для bindgen), Rust ≥ 1.85.

> На macOS, если bindgen не находит libclang, укажите путь:
> `export LIBCLANG_PATH=/Library/Developer/CommandLineTools/usr/lib`.
> Линковка cdylib с отложенным поиском символов nginx (`-undefined dynamic_lookup`)
> настраивается автоматически через [`nginx-module/build.rs`](nginx-module/build.rs).

**Вариант A — против установленных исходников nginx** (для прод-загрузки версия nginx
должна совпадать с целевой). Достаточно выполнить только `./configure`:

```bash
curl -O https://nginx.org/download/nginx-1.28.3.tar.gz
tar xf nginx-1.28.3.tar.gz
( cd nginx-1.28.3 && ./configure --with-compat )

NGINX_BUILD_DIR=$PWD/nginx-1.28.3/objs \
NGINX_SOURCE_DIR=$PWD/nginx-1.28.3 \
  cargo build --release -p ngx-lox

# результат: target/release/libngx_lox.{so|dylib}
#   → переименовать/скопировать в ngx_http_lox_module.so
```

**Вариант B — vendored** (nginx скачивается и собирается автоматически; удобно для
локальной проверки и CI):

```bash
cargo build --release -p ngx-lox --features vendored
```

> Сборка модуля изолирована от интерпретатора: `cargo run` / `cargo test` собирают только
> пакет `codecrafters-interpreter` и не требуют исходников nginx.

### Использование в nginx.conf

```nginx
load_module /path/to/ngx_http_lox_module.so;

events {}
http {
    server {
        listen 8080;

        location /admin {
            # пускаем только если заголовок X-Token == "secret"
            lox_guard 'header("x-token") == "secret";';
            return 200 "ok\n";
        }

        location /api {
            # запретить POST на /api/legacy
            lox_guard '!(method == "POST" and uri == "/api/legacy");';
            proxy_pass http://backend;
        }
    }
}
```

Истинное выражение ⇒ запрос идёт дальше по пайплайну nginx; ложное ⇒ `403 Forbidden`.

```bash
curl -H 'X-Token: secret' localhost:8080/admin   # → 200 ok
curl                       localhost:8080/admin   # → 403 Forbidden
```