# Getters By Type

[![Crates.io](https://img.shields.io/crates/v/getters-by-type.svg)](https://crates.io/crates/getters-by-type)
[![Docs](https://docs.rs/getters-by-type/badge.svg)](https://docs.rs/getters-by-type)

This crate provides `GettersByType` derive macro for structs, which implements a getter method for each type they contain.

The generated methods start with the prefix `get_fields_` and end with a transcription of the type they refer.

Example using `GettersByType` :

```rust
#[derive(GettersByType)]
struct Foo {
    first: i32,
    second: i32,
    third: i32,
}

let object = Foo { first: 6, second: 12, third: 24 };

// Let's sum all the i32 fields with a fold expression:
assert_eq!(object.get_fields_i32().iter().fold(0, |acc, x| **x + acc), 42);
```

As you notice, the getter methods return an array containing references to all the fields of the same type.
In that example, the return type of the method `get_fields_i32` would be `[&i32; 3]`.

This crate also provides a `mut` version `GettersMutByType` which also adds a mut version for those methods.

In this case, the generated methods start with the prefix `get_mut_fields_` instead.

Example using `GettersMutByType` :


```rust
#[derive(GettersMutByType)]
struct Foo {
    first: Updater,
    second: Updater,
    ...
    onehundredth: Updater,
}

impl Updater {
    fn update(&mut self) {...}
}

let mut object = Foo::new();

// Let's update all the Updater fields
for updater in object.get_mut_fields_updater().iter_mut() {
    updater.update();
}
```

In this example, the return type of the method `get_mut_fields_updater` would be `[&mut Updater; 3]`.
There is no dynamic memory allocation happening within the getter methods, as they just return a fixed array with references.
There isn't also unsafe code being generated.

For more information, check the [documentation page](https://docs.rs/getters-by-type).

## Usage

With Cargo, you can add this line to your Cargo.toml:

```toml
[dependencies]
getters-by-type = "*"
```

## Development

This currently works with primitive types, and many other referencial and genecic types, such as `&str` or `Vec`, but there are cases that are not completely covered, like the trait objects. Want to contribute? Pull requests are always welcome. Because this is my first work with procedural macros, I guess things can improve a fair lot under the hood, so there should be a few low hanging fruits already. Let's go for them!

## License

MIT
