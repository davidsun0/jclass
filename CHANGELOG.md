# CHANGELOG

## 0.2.1 (2021-11-13)

### Bug Fixes

- Fix example in README
- Fix formatting in MANUAL

- Fix exported symbols for the Code attribute
- Fix bytecode encoding
    - Fix conversion of signed operands
    - Fix calculation of bytecode offsets
    - Fix operand encoding for `iinc`

## 0.2.0 (2021-11-01)

### Breaking Changes

- Changed class, field, method, and attribute representations from structures
to objects. This greatly simplifies the API.
- Changed single and double float constants to directly use the Lisp
`single-float` and `double-float` types since all major implementations use
IEEE 754.

### Enhancements

- Added a change log.
- Increased portability by removing uses of `with-slots` on structures.

### Bug Fixes

- Fixed encoding 8 byte constants (`long` and `double`) in the constant pool.
- Fixed access flag encoding in the InnerClasses attribute.
- Fixed SourceDebugExtension to represent its contents as a modified UTF-8
string.

