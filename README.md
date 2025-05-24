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