# jclass Manual

This file is intended to serve as a reference and not a starting point.
I recommend reading the [tutorial](TUTORIAL.md) and the
[Java Virtual Machine Specification](https://docs.oracle.com/javase/specs/index.html)
first.

jclass is built from the perspective of the Java Virtual Machine, and not from
that of the Java Programming Language. There are important differences that are
covered not covered in this manual.

## Java Classes

[Structure] **java-class**

[Function] **make-java-class** minor-version major-version flags name
parent interfaces fields methods attributes => java-class

[Accessor] **java-class-minor-version** java-class => minor-version \
[Accessor] **java-class-major-version** java-class => major-version \
[Accessor] **java-class-flags** java-class => flags \
[Accessor] **java-class-name** java-class => name \
[Accessor] **java-class-parent** java-class => parent \
[Accessor] **java-class-interfaces** java-class => interfaces \
[Accessor] **java-class-fields** java-class => fields \
[Accessor] **java-class-methods** java-class => methods \
[Accessor] **java-class-attributes** java-class => attributes

- *minor-version*: an integer representing a Java Class minor version
- *major-version*: an integer representing a Java Class major version
- *flags*: a list of keywords from this set: `:public :final :super :interface
:abstract :synthetic :annotation :enum :module`
- *name*: a string of this class's [binary name](#binary-names)
- *parent*: a string of this class's parent's [binary name](#binary-names)
- *interfaces*: a list of strings of implemented interface names
- *fields*: a list of [field](#fields) structures
- *methods*: a list of [method](#methods) structures
- *attributes*: a list of [attribute](#attributes) structures

[Function] **java-class-p** object => generalized-boolean

Returns true if *object* is of type *java-class*; otherwise, returns false.

---

[Function] **java-class-bytes** java-class &optional constant-pool => bytes

- *java-class*: a `java-class` structure
- *constant-pool*: a `constant-pool` structure
- *bytes*: a list of integers between 0 and 255, inclusive

Assembles the `java-class` structure into a list of bytes representing its class file.

A `constant-pool` structure is used to generate the class file. There is no
need to provide a pool structure unless you need fine-grained control over the
order or number of constants in the pool.

---

[Function] **disassemble-jclass** bytes => java-class, constant-pool

- *bytes*: a vector of integers between 0 and 255, inclusive
- *java-class*: a `java-class` structure
- *constant-pool*: an array of constant values

*Exceptional situations*: signals `class-format-error` when parsing a malformed
class file.

Disassembles a vector of bytes into a java-class structure.

The 0th indexed item in the constant pool will always be `nil`. Constant pool
indices start at 1.

---

[*Condition Type*] **class-format-error**

Direct superclasses: `error`

A `class-format-error` condition represents errors related to malformed Java
class files.

jclass only signals a `class-format-error` for structural issues.
It is possible to creat a class file that is structurally valid, but invalid
according to the semantics of the JVM, in which case no error is signaled.
For example, it is possible to create a class that extends and interface or
implements a class. On the other hand, jclass *will* signal an error for
issues like an invalid annotation value, which prevents further parsing.

Fun trivia: You can annotate method, fields, etc. with classes that don't
implement `java.lang.annotations.Annotation`! In fact, you can annotate things
with pretty much anything - integer literals, strings, you name it. OpenJDK's
HotSpot gladly accepts and runs these classes. Trying to read the annotation
with reflection does throw a `java.lang.annotation.AnnotationFormatError` though.

Of course, jclass will not signal a `class-format-error` when generating such
a malformed class.

## Fields

[Structure] **field-info**

[Function] **make-field-info** flags name descriptor attributes => field-info

[Accessor] **field-info-flags** field-info => flags \
[Accessor] **field-info-name** field-info => name \
[Accessor] **field-info-descriptor** field-info => descriptor \
[Accessor] **field-info-attributes** field-info => attributes

- *flags*: a list of keywords from this set: `:public :private :protected :static
:final :volatile :transient :synthetic :enum`
- *name*: a string denoting this field's name
- *descriptor*: a string of this field's type descriptor
- *attributes*: a list of [attribute](#Attributes) structures

[Function] **field-info-p** object => generalized-boolean

Returns true if *object* is of type *field-info*; otherwise, returns false.

## Methods

[Structure] **method-info**

[Function] **make-method-info** flags name descriptor attributes => method-info

[Accessor] **method-info-flags** method-info => flags \
[Accessor] **method-info-name** method-info => name \
[Accessor] **method-info-descriptor** method-info => descriptor \
[Accessor] **method-info-attributes** method-info => attributes

- *flags*: a list of keywords from this set: `:public :private :protected :static
:final :synchronized :bridge :varargs :native :abstract :strict :synthetic`
- *name*: a string denoting this method's name
- *descriptor*: a string of this methods's type descriptor
- *attributes*: a list of [attribute](#Attributes) structures

[Function] **method-info-p** object => generalized-boolean

Returns true if *object* is of type *method-info*; otherwise, returns false.

## Constants

These structures represent the values of the class file constant pool.

When the type of a constant is unambiguous, jclass will automatically build a
constant from its basic components. For example, what interface a class implements
must be a list of `Class_info`s, which are uniquely identified by the name of
the class or interface they represent. Therefore the interfaces list of a
`java-class` structure might be `(list "java/lang/Comparable"
"java/lang/Iterable")`.

The astute may realize that a class info constant does not actually contain a
string, but instead references a `UTF8_info` that does. This is because the
reference is also unambiguous in its type. As a result,
constants in jclass are created with the minimal amount of information needed.

### Ambiguous Constant Types

When the constant type *is* ambiguous, the user must pass a constant explicitly.

Places where constant type is ambiguous:
- `ldc` and `ldc_w` takes any loadable constant
- `ldc2_w` takes a `Long_info` or a `Double_info`
- `invokespecial` takes a `Methodref_info` or a `InterfaceMethodref_info`
constant
- The ConstantValue attribute may take one of several constants.
    - See the JVM Spec, Table 4.7.2-A for details
- Several attributes can take multiple constant types

### Optional Constants

Sometimes a constant is optional and a constant pool index may be zero
(remember that constant pool indices start at 1).
jclass will output a zero if the supplied value is `nil`.

### Constant Pool Functions

[Function] **make-utf8-info** string => utf8-info \
[Accessor] **utf8-info-text** utf8-info => string \
[Function] **utf8-info-p** object => boolean

- *string*: a string

Unicode characters may be used if each element of *string* represents a code point.

[Function] **make-integer-info** integer => integer-info \
[Accessor] **integer-info-value** integer-info => integer \
[Function] **integer-info-p** object => boolean

- *integer*: an 32-bit signed integer

[Function] **make-float-info** ieee-bits => float-info \
[Accessor] **float-info-ieee-bits** float-info => ieee-bits \
[Function] **float-info-p** object => boolean

- *ieee-bits*: a 32-bit signed integer whose two's compliment bits represent a 32-bit IEEE float

[Function] **make-long-info** integer => long-info \
[Accessor] **long-info-value** long-info => integer \
[Function] **long-info-p** object => boolean

- *integer*: a 64-bit signed integer

[Function] **make-double-info** ieee-bits => double-info \
[Accessor] **double-info-ieee-bits** double-info ieee-bits \
[Function] **double-info-p** object => boolean

- *ieee-bits*: a 64-bit signed integer whose two's compliment bits represent a 64-bit IEEE float

[Function] **make-class-info** name => class-info \
[Accessor] **class-info-name** class-info => name \
[Function] **class-info-p** object => boolean

- *name*: a string representing a Java class name

See the JVM Specification 4.2 for restrictions on class names.

[Function] **make-string-info** string => string-info \
[Accessor] **string-info-text** string-info => string \
[Function] **string-info-p** object => boolean

- *string*: a string

Unicode characters may be used if each element of *string* represents a code point.

---

[Function] **make-field-ref-info** class-name name type => field-ref-info \
[Accessor] **field-ref-info-class-name** field-ref-info => class-name \
[Accessor] **field-ref-info-name** field-ref-info => name \
[Accessor] **field-ref-info-type** field-ref-info => type \
[Function] **field-ref-info-p** object => boolean

[Function] **make-method-ref-info** class-name name type => method-ref-info \
[Accessor] **method-ref-info-class-name** method-ref-info => class-name \
[Accessor] **method-ref-info-name** method-ref-info => name \
[Accessor] **method-ref-info-type** method-ref-info => type \
[Function] **method-ref-info-p** object => boolean

[Function] **make-interface-method-ref-info** class-name name type => interface-method-ref \
[Accessor] **interface-method-ref-info-class-name** interface-method-ref => class-name \
[Accessor] **interface-method-ref-info-name** interface-method-ref => name \
[Accessor] **interface-method-ref-info-type** interface-method-ref => type \
[Function] **interface-method-ref-info-p** object => boolean

[Function] **make-name-and-type-info** name type => name-and-type-info \
[Accessor] **name-and-type-info-name** name-and-type-info => name \
[Accessor] **name-and-type-info-type** name-and-type-info => type \
[Function] **name-and-type-info-p** object => boolean

- *class-name*: a string representing a Java class name
- *name*: a string representing the name of the field, method, or name and type info
- *type*: a string representing a type descriptor

See the JVM Specification 4.2 for restrictions on field and method names. \
See the JVM Specification 4.3 for restrictions on descriptors.

---

[Function] **make-method-handle-info** kind reference => method-handle-info \
[Accessor] **method-handle-info-kind** method-handle-info => kind \
[Accessor] **method-handle-info-reference** method-handle-info => reference \
[Function] **method-handle-info-p** object => boolean

- *kind*: an 8-bit signed integer
- *reference*: a field-ref-info, method-ref-info, or interface-method-ref-info

See the JVM Specification 4.4.8 for the semantics of the method handle info.

[Function] **make-method-type-info** descriptor => method-type-info \
[Accessor] **method-type-info-descriptor** method-type-info => descriptor \
[Function] **method-type-info-p** object => boolean

- *descriptor*: a string representing a type descriptor

See the JVM Specification 4.3 for restrictions on descriptors.

---

[Function] **make-dynamic-info** bootstrap-index name type => dynamic-info \
[Accessor] **dynamic-info-bootstrap-index** dynamic-info => bootstrap-index \
[Accessor] **dynamic-info-name** dynamic-info => name \
[Accessor] **dynamic-info-type** dynamic-info => type \
[Function] **dynamic-info-p** object => boolean

[Function] **make-invoke-dynamic-info** bootstrap-index name type => dynamic-info \
[Accessor] **invoke-dynamic-info-bootstrap-index** dynamic-info => bootstrap-index \
[Accessor] **invoke-dynamic-info-name** dynamic-info => name \
[Accessor] **invoke-dynamic-info-type** dynamic-info => type \
[Function] **invoke-dynamic-info-p** object => boolean

- *bootstrap-index*: an 8-bit integer representing an index in the class's
bootstrap-methods attribute
- *name*: a string representing the name of a field or method
- *type*: a string representing a type descriptor

See the JVM Specification 4.4.10 for the semantics of the dynamic info and the
invoke dynamic info. \
See the JVM Specification 4.2 for restrictions on field and method names. \
See the JVM Specification 4.3 for restrictions on descriptors.

---

[Function] **make-module-info** name => module-info \
[Accessor] **module-info-name** module-info => name \
[Function] **module-info-p** object => boolean

[Function] **make-package-info** name => package-info \
[Accessor] **package-info-name** package-info => name \
[Function] **package-info-p** object => boolean

- *name*: a string representing the name of a module or package

## Attributes

All accessors are `setf`-able.

Anonymous inner structures are represented by a list containing the values in
order.

### Constant Value Attribute

[Structure] *constant-value*

[Function] **make-constant-value** value => constant-value \
[Accessor] **constant-value-value** constant-value => value \
[Function] **constant-value-p** object => boolean

- *value*: an integer-info, float-info, long-info, double-info, or string-info

### Code Attribute

[Structure] *code*

[Function] **make-code** max-stack max-locals bytecode exceptions attributes
=> code \
[Accessor] **code-max-stack** code => max-stack \
[Accessor] **code-max-locals** code => max-locals \
[Accessor] **code-bytecode** code => bytecode \
[Accessor] **code-exceptions** code => exceptions \
[Function] **code-p** object => boolean

- *max-stack*: a 16-bit integer - the max stack size used
- *max-locals*: a 16 bit integer - the max number of locals used
- *bytecodes*: a list of bytecode instructions or an array of raw bytes
- *exceptions*: a list of exception table entries
    - each entry is a list with the form `(start-pc end-pc handler-pc catch-type)`
    - `start-pc`, `end-pc`, `handler-pc` are 16 bit integers
    - `catch-type` is a string that names a class or nil to catch all exceptions
- *attributes*: a list of attributes

Examples of bytecode instructions:
- no operands: `:iinc` or `(:iinc)`
- one operand: `(:ifeq 10)` or `(:ldc "Hello, world!")`
- `tableswitch`: `(:tableswitch 44 0 2 28 30 38)`
    - The order of the operands is default, low, high, and then the offsets
    - Padding is automatically calculated
- `lookupswitch`: `(:lookupswitch 75 (98030 64) (2571410 36) (99162322 50))`
    - The order of the operands is default and then the match-offset pairs
    - Padding is automatically calculated
- `wide`: `(:wide :iinc 300 500)` `(:wide :fstore 300)`

### Stack Map Table Attribute

[Structure] *stack-map-table*

[Function] **make-stack-map-table** entries => stack-map-table \
[Accessor] **stack-map-table-entries** stack-map-table => entries \
[Function] **stack-map-table-p** object => boolean

- *entries*: a list where each element is a stack map frame

Stack Map Frame Formats

| type | format |
|------|--------|
| same | `(frame-type)` |
| same locals 1 stack item | `(frame-type verification)` |
| same locals 1 stack item extended | `(247 offset verification)` |
| chop |`(frame-type offset)` |
| same extended | `(251 offset)` |
| append frame | `(frame-type offset &rest verifications)` |
| full frame | `(255 offset locals stack-items)` |
    
- `locals` and `stack-items` are lists of verification type infos.

Verification Type Info Formats

| type | format |
| top | `(0)` |
| integer | `(1)` |
| float | `(2)` |
| null | `(5)` |
| uninitialized this | `(6)` |
| object | `(7 class-name)` |
| long | `(4)` |
| double | `(3)` |

- `class-name` is a string denoting a class name

### Exceptions Attribute

[Structure] *exceptions*

[Function] **make-exceptions** exception-list => exceptions \
[Accessor] **exceptions-exceptions** exceptions => exception-list \
[Function] **exceptions-p** object => boolean

- *exception-list*: a list of strings, each denoting a class name

### Inner Classes Attribute

[Structure] *inner-classes*

[Function] **make-inner-classes** classes => inner-classes \
[Accessor] **inner-classes-classes** inner-classes => classes \
[Function] **inner-classes-p** object => boolean

- *classes*: a list where each element is a list with the form
`(inner outer name access)`
    - *inner*: a string denoting an inner class name
    - *outer*: a string denoting an outer class name
    - *name*: a string denoting the original simple name, or `nil` if anonymous
    - *access*: a list of keywords from this set: `:public :private :protected
:static :final :interface :abstract :synthetic :annotation :enum`

### Enclosing Method Attribute

[Structure] *enclosing-method*

[Function] **make-enclosing-method** class name type => enclosing-method \
[Accessor] **enclosing-method-class** enclosing-method => class \
[Accessor] **enclosing-method-name** enclosing-method => name \
[Accessor] **enclosing-method-type** enclosing-method => type \
[Function] **enclosing-method-p** object => boolean

- *class*: a string denoting a class name
- *name*: a string denoting a method name
- *type*: a string denoting a type descriptor

### Synthetic Attribute

[Structure] *synthetic*

[Function] **make-synthetic** => synthetic-attribute \
[Function] **synthetic-p** object => boolean

The Synthetic attribute has no slots.

### Signature Attribute

[Structure] *signature*

[Function] **make-signature** signature-string => signature \
[Accessor] **signature-signature** signature => signature-string \
[Function] **signature-p** object => boolean

- *signature*: a string denoting a class signature

See the JVM Spec 4.7.9.1 for signature formats.

### Source File Attribute

[Structure] *source-file*

[Function] **make-source-file** name => source-file \
[Accessor] **source-file-name** source-file => name \
[Function] **source-file-p** object => boolean

- *name*: a string denoting a file name

### Source Debug Extension Attribute

[Structure] *source-debug-extension*

[Function] **make-source-debug-extension** debug => source-debug-extension \
[Accessor] **source-debug-extension-debug** source-debug-extension => debug \
[Function] **source-debug-extension-p** object => boolean

- *debug*: an `(array (unsigned-byte 8) (*))` containing binary data

The binary format of *debug* is not specified by the Java Virtual Machine.

### Line Number Table Attribute

[Structure] *line-number-table*

[Function] **make-line-number-table** line-numbers => line-number-table \
[Accessor] **line-number-table-line-numbers** line-number-table => line-numbers \
[Function] **line-number-table-p** object => boolean

- *line-numbers*: a list where each element has the form `(start-pc line-number)`
    - *start-pc*: an integer denoting the bytecode index
    - *line-number*: the line number corresponding to start-pc in the source file

### Local Variable Table Attribute

[Structure] *local-variable-table*

[Function] **make-local-variable-table** local-variables => local-variable-table \
[Accessor] **local-variable-table-local-variables** local-variable-table => local-variables \
[Function] **local-variable-table-p** object => boolean

- *local-variables*: a list where each element has the form
`(start-pc length name descriptor index)`
    - *start-pc*: an integer denoting the bytecode index
    - *length*: an integer denoting the length of bytes
    - *name*: a string denoting the local variable name
    - *descriptor*: a field descriptor denoting the type of the local variable
    - *index*: an integer denoting the index in the local variable array of the frame

## Local Variable Type Table Attribute

[Structure] *local-variable-type-table*

[Function] **make-local-variable-type-table** local-variables => local-variable-type-table \
[Accessor] **local-variable-type-table-local-variables** local-variable-type-table =>
local-variables \
[Function] **local-variable-type-table-p** object => boolean

- *local-variables*: a list where each element has the form
`(start-pc length name descriptor index)`
    - *start-pc*: an integer denoting the bytecode index
    - *length*: an integer denoting the length of bytes
    - *name*: a string denoting the local variable name
    - *signature*: a field signature denoting the type of the local variable
    - *index*: an integer denoting the index in the local variable array of the frame

### Deprecated Attribute

[Structure] *deprecated*

[Function] **make-deprecated** => deprecated-attribute
[Function] **deprecated-p** object => boolean

The Deprecated attribute has no slots.

### Annotations

[Structure] *annotation*

[Function] **make-annotation** type element-value-pairs => annotation \
[Accessor] **annotation-type** annotation => type \
[Accessor] **annotation-element-value-pairs** annotation => element-value-pairs \
[Function] **annotation-p** object => boolean

- *type*: a string denoting a field descriptor
- *element-value-pairs*: a list where each element has the form `(tag value)`

#### Element Tags And Values

|tag|value|
|---|-----|
|#\\B| an 8 bit integer |
|#\\Z| 0 or 1 |
|#\\I| a 32 bit integer |
|#\\S| an 16 bit integer |
|#\\J| a 64 bit integer |
|#\\C| a character with a code point of U+FFFF or below |
|#\\F| a 32 bit integer representing the bits of a 32 bit float |
|#\\D| a 64 bit integer representing the bits of a 64 bit float |
|#\\s| a string |
|#\\e| a list of an enum type and value |
|#\\@| an annotation |
|#\\[| a list of element-value-pairs |

[Structure] *runtime-visible-annotations* \
[Structure] *runtime-invisible-annotations* \
[Structure] *runtime-visible-parameter-annotations* \
[Structure] *runtime-invsible-parameter-annotations*

[Function] **make-runtime-visible-annotations**
annotations => runtime-visible-annotations \
[Function] **make-runtime-invisible-annotations**
annotations => runtime-visible-annotations \
[Function] **make-runtime-visible-parameter-annotations**
annotations => runtime-visible-parameter-annotations \
[Function] **make-runtime-visible-parameter-annotations**
annotations => runtime-visible-parameter-annotations

[Accessor] **runtime-visible-annotations-annotations**
runtime-visible-annotations => annotations \
[Accessor] **runtime-invisible-annotations-annotations**
runtime-invisible-annotations => annotations \
[Accessor] **runtime-visible-parameter-annotations-annotations**
runtime-visible-parameter-parameter-annotations => annotations \
[Accessor] **runtime-invisible-parameter-annotations-annotations**
runtime-invisible-parameter-annotations => annotations

[Function] **runtime-visible-annotations-p** object => boolean \
[Function] **runtime-invisible-annotations-p** object => boolean \
[Function] **runtime-visible-parameter-annotations-p** object => boolean \
[Function] **runtime-invisible-parameter-annotations-p** object => boolean

- *annotations*: a list of annotations

### Type Annotations

[Structure] *type-path*

[Function] **make-type-path** paths => type-path \
[Accessor] **type-path-paths** type-path => paths \
[Function] **type-path-p** object => boolean

- *paths*: a list where each element has the form `(kind argument-index)`
    - *kind*: an integer
    - *argument-index*: an integer

See the JVM Spec 4.7.20.2 for the semantics of kind and argument index

[Structure] *type-annotation*

[Function] **make-type-annotation** target-type target-info target-path
type element-value-pairs => type-annotation \
[Accessor] **type-annotation-target-type** type-annotation => target-type \
[Accessor] **type-annotation-target-info** type-annotation => target-info \
[Accessor] **type-annotation-target-path** type-annotation => target-path\
[Accessor] **type-annotation-type** type-annotation => type \
[Accessor] **type-annotation-element-value-pairs** type-annotation => element-value-pairs \
[Function] **type-annotation-p** object => boolean

- *target-type*: an integer - See the JVM Spec tables 4.7.20-A, B, and C
- *target-info*: a target info
- *target-path*: a type path
- *type*: a string denoting a field descriptor
- *element-value-pairs*: a list where each element has the form `(tag value)`
    - See [Element Tags and Values](#element-tags-and-values)

| target type | target info |
|-------------|-------------|
| type parameter | an 8 bit integer |
| supertype | a 16 bit integer |
| type parameter bound | a list of two 8 bit integers |
| empty | ignored |
| formal parameter | an 8 bit integer |
| throws | a 16 bit integer |
| localvar | a table - see below |
| catch | a 16 bit integer |
| offset | a 16 bit integer |
| type argument | a list of a 16 bit integer and an 8 bit integer |

- *localvar table* is a list where each element has the form `(start-pc length index)`
    - *start-pc*: an integer offset in the bytecode array
    - *length*: an integer length of bytecode
    - *index*: an integer index inthe local variable array

See the JVM Spec 4.7.20.1 for target info semantics

[Structure] *runtime-visible-type-annotaitons* \
[Structure] *runtime-invisible-type-annotations*

[Function] **make-runtime-visible-type-annotations** type-annotations =>
runtime-visible-type-annotations \
[Function] **make-runtime-invisible-type-annotations** type-annotations =>
runtime-invisible-type-annotations

[Accessor] **runtime-visible-type-annotations-annotations**
runtime-visible-type-annotations => type-annotations \
[Accessor] **runtime-invisible-type-annotations-annotations**
runtime-invisible-type-annotations => type-annotations

[Function] **runtime-visible-type-annotations-p** object => boolean \
[Function] **runtime-invisible-type-annotations-p** object => boolean

- *type-annotations*: a list of type annotations

### Annotation Default Attribute

[Structure] *annotation-default*

[Function] **make-annotation-default** tag value => annotation default \
[Accessor] **annotation-default-tag** annotation-default => tag \
[Accessor] **annotation-default-value** annotation-default => value \
[Function] **annotation-default-p** object => boolean

- *tag*, *value*: an [element tag and value](#element-tags-and-values)

### Bootstrap Methods Attribute

[Structure] *bootstrap-methods*

[Function] **make-bootstrap-methods** methods => bootstrap-methods \
[Accessor] **bootstrap-methods-methods** bootstrap-methods => methods \
[Function] **bootstrap-methods-p** object => boolean

- *methods*: a list where each entry has the form `(kind reference arguments)`
    - *kind*: an integer between 1 and 9, inclusive
    - *reference*: a loadable constant
    - *arguments*: a list of loadable constants

See the JVM Spec 4.4.8 for details on bootstrap kinds and references

### Method Parameters Attribute

[Structure] *method-parameters*

[Function] **make-method-parameters** parameters => method-parameters \
[Accessor] **method-parameters-parameters** method-parameters => paramters \
[Function] **method-parameters-p** object => boolean

- *parameters*: a list where each entry has the form `(name access-flags)`
    - *name*: a string denoting a parameter's unqualified name
    - *access-flags*: a list of keywords from the set `:final :synthetic :mandated`

### Module Attribute

[Structure] *module*

[Function] **make-module** name flags version requires exports opens uses provides => module \
[Accessor] **module-name** module => name \
[Accessor] **module-flags** module => flags \
[Accessor] **module-version** module => version \
[Accessor] **module-requires** module => requires \
[Accessor] **module-exports** module => exports \
[Accessor] **module-opens** module => opens \
[Accessor] **module-uses** module => uses \
[Accessor] **module-provides** module => provides \
[Function] **module-p** object => boolean

- *name*: a string denoting the module name
- *flags*: a list of keywords from the set `:open :synthetic :mandated`
- *version*: a string denoting the module version
- *requires*: a list where each element has the form `(module flags version)`
    - *module*: a string denoting the required module name
    - *flags*: a list of keywords from the set `:transitive :static-phase :synthetic :mandated`
    - *version*: a string denoting the required module version
- *exports*: a list where each element has the form `(package flags exports-to)`
    - *package*: a package name
    - *flags*: a list of keywords from the set `:synthetic :mandated`
    - *exports-to*: a list of strings denoting module names
- *opens*:  a list where each element has the form `(package flags opens-to)`
    - *package*: a package name
    - *flags*: a list of keywords from the set `:synthetic :mandated`
    - *opens-to*: a list of strings denoting module names
- *uses*: a list of strings denoting class names
- *provides*: a list where each element has the form `(name provides-with)`
    - *name*: a string denoting a class name
    - *provides-with*: a list of strings denoting class names

### Module Packages Attribute

[Structure] *module-pacakges*

[Function] **make-module-packages** packages => module-packages \
[Accessor] **module-packages-packages** module-packages => packages \
[Function] **module-packages-p** object => boolean

- *packages*: a list of strings denoting package names

### Module Main Class Attribute

[Structure] *module-main-class* main-class

[Function] **make-module-main-class** main-class => module-main-class \
[Accessor] **module-main-class-main-class** module-main-class => main-class \
[Function] **module-main-class-p** object => boolean

- *main-class*: a string denoting a class name

### Nest Host Attribute

[Structure] *nest-host*

[Function] **make-nest-host** classes => nest-host \
[Accessor] **nest-host-classes** nest-host => classes \
[Function] **nest-host-p** object => boolean

- *classes*: a list of strings denoting class names

### Record Attribute

[Structure] *record*

[Function] **make-record** components => record \
[Accessor] **record-components** record => components \
[Function] **record-p** object => boolean

- *components*: a list where each entry has the form `(name descriptor attributes)`
    - *name*: a string denoting the component name
    - *descriptor*: a string denoting a field descriptor
    - *attributes*: a list of attributes

### Permitted Subclasses Attribute

[Structure] *permitted-subclasses*

[Function] **make-permitted-subclasses** classes => permitted-subclasses \
[Accessor] **permitted-subclasses-classes** permitted-subclasses => classes
[Function] **permitted-subclasses-p** object => boolean

- *classes*: a list of strings denoting class names

### Creating Custom Attributes

The Java Virtual Machine Specification says that the class file format can be
extended with custom attributes.

See the implementations for the standard attributes. Each attribute is defined
with through a DSL that automatically generates and installs serialization and
deserialization functions - i.e. if you use `def-attribute`, the class generation
and parsing functions will be able to handle your attribute with no further
code necessary.

Attribute definitions implicitly add the six byte header which contains the
index of the attribute name and the length of the attribute in bytes.

The structure DSL can be expanded with the macro `def-serialization`.

