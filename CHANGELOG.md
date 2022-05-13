# CHANGELOG

## 1.0.0 (2022-5-13)

### Breaking Changes

- Change bytecode instructions to use hyphens instead of underscores:
Use `:if-acmpeq` now instead of `:if_acmpeq`.
- Change `decode-modified-utf8` to signal `simple-error` instead of
`class-format-error`.

### Enhancements

- Add `print-object` implementations to make classes, fields, and methods
human readable.
- Increase portability by not relying on SBCL behavior.

### Bug Fixes

- Fix instruction byte length calculation.
- Fix pool-index sometimes returning `nil` instead of the index.

## 0.2.1 (2021-11-13)

### Bug Fixes

- Fix example in README.
- Fix formatting in MANUAL.

- Fix exported symbols for the Code attribute.
- Fix bytecode encoding.
    - Fix conversion of signed operands.
    - Fix calculation of bytecode offsets.
    - Fix operand encoding for `iinc`.

## 0.2.0 (2021-11-01)

### Breaking Changes

- Change class, field, method, and attribute representations from structures
to objects. This greatly simplifies the API.
- Change single and double float constants to directly use the Lisp
`single-float` and `double-float` types since all major implementations use
IEEE 754.

### Enhancements

- Add a change log.
- Increase portability by removing uses of `with-slots` on structures.

### Bug Fixes

- Fix encoding 8 byte constants (`long` and `double`) in the constant pool.
- Fix access flag encoding in the InnerClasses attribute.
- Fix SourceDebugExtension to represent its contents as a modified UTF-8
string.

