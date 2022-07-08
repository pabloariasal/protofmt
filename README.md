# protofmt - Formatter for Protocol Buffer Files

| 👎 | 👍 |
------------------------------------|-------|
| ![before](screenshots/before.svg)|![after](screenshots/after.svg)|

# Formatting Rules
* Applies google's official protobuf style guide
* Indentation with 2 spaces
* Import groups are sorted alphabetically
* Makes all string literals double quoted: `option opt = 'foo';` => `option opt = "foo";`
* Makes all enum fields uppercase: `enum E { field = 1 }` => `enum E { FIELD = 1 }`
* Makes opening braces on the declaration line: `message M {`
* Strips redundant semicolons: `enum E {};` => `enum E {}`
* Full comment support

# Installation
`protofmt` is written in Haskell, you can use `cabal` to install it.
```sh
git clone https://github.com/pabloariasal/protofmt.git
cd protofmt
cabal install
```

# Usage

Just pass the file to be formatted:

```
protofmt <path_to_unformatted_proto_file>
```

**Example:**
```sh
$ cat unformatted.proto
syntax = 'proto3';
message M { string field = 1; };

$ protofmt unformatted.proto
syntax = "proto3";
message M {
  string field = 1;
}

$ protofmt unformatted.proto | tee formatted.proto
syntax = "proto3";
message M {
  string field = 1;
}

$ cat formatted.proto
syntax = "proto3";
message M {
  string field = 1;
}
```

# Known Limitations (to be fixed soon)

## Language

* Only protobuf 3
* `inf` and `nan` are not supported as floating point literals (PR welcome)
* enum fields must be decimal literals (no hex, octal or binary) (PR welcome)
* No service definitions (PR welcome)
* No `optional` nor `required` keywords supported (PR welcome)

## API
* Only one file can be processed at once
* No `--in-place` flag supported
* No `--recursive` flag supported

# Contributing
