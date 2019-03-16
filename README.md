# Getters By Type

[![Crates.io](https://img.shields.io/crates/v/getters-by-type.svg)](https://crates.io/crates/getters-by-type)
[![Docs](https://docs.rs/getters-by-type/badge.svg)](https://docs.rs/getters-by-type)

This crate provides `GettersByType` and `GettersMutByType` derive macros for structs, which implements a getter method for each type they contain. The getter methods return an array containing references to all the fields of the same type. The `GettersMutByType` derive also adds a mut version for those methods.

Example using `GettersByType` :

```rust
#[derive(GettersByType)]
struct Test {
    first: i32,
    second: i32,
    third: i32,
}

let test = Test { first: 6, second: 12, third: 24 };

// Let's sum all the i32 fields with a fold expression:
assert_eq!(test.get_fields_i32().iter().fold(0, |acc, x| **x + acc), 42);
```

Example using `GettersMutByType` :


```rust
#[derive(GettersMutByType)]
struct Test {
    first: Updater,
    second: Updater,
    ...
    onehundredth: Updater,
}

impl Updater {
    fn update(&mut self) {...}
}

let mut test = Test::new();

// Let's update all the Updater fields
for updater in test.get_mut_fields_updater().iter_mut() {
    updater.update();
}
```

Check more examples on the test file: [tests/getters_by_type.rs](tests/getters_by_type.rs)

## Usage

With Cargo, you can this line to your Cargo.toml:

```toml
[dependencies]
getters-by-type = "*"
```

## Development

This currently works with primitive types, and many other referencial and genecic types, such as &str or Vec, but there are cases that currently are not covered, like the Fn types. Want to contribute? Pull requests are always welcome. Because this is my first work with procedural macros, I guess thinks can improve a fair lot under the hood, so there should be a few low hanging fruits already. Let's go for them!

## License

MIT