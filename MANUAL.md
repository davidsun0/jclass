# jclass Manual

This file is intended to serve as a reference and not a starting point.
I recommend reading the
[Java Virtual Machine Specification](https://docs.oracle.com/javase/specs/index.html)
first.

jclass is built from the perspective of the Java Virtual Machine, and not from
that of the Java Programming Language. There are important differences that are
covered not covered in this manual.

## Java Classes

[Class] **java-class**

[Accessor] **minor-version** java-class => minor-version \
[Accessor] **major-version** java-class => major-version \
[Accessor] **flags** java-class => flags \
[Accessor] **name** java-class => name \
[Accessor] **parent** java-class => parent \
[Accessor] **interfaces** java-class => interfaces \
[Accessor] **fields** java-class => fields \
[Accessor] **methods** java-class => methods \
[Accessor] **attributes** java-class => attributes

- *minor-version*: a 16-bit Java Class minor version
- *major-version*: a 16-bit Java Class major version
- *flags*: a list of keywords from this set: `:public :final :super :interface
:abstract :synthetic :annotation :enum :module`
- *name*: this class's [name](#binary-names) string
- *parent*: this class's parent's name string
- *interfaces*: a list of implemented interface name strings
- *fields*: a list of [field](#fields) structures
- *methods*: a list of [method](#methods) structures
- *attributes*: a list of [attribute](#attributes) structures

---

[Function] **java-class-bytes** java-class &optional constant-pool => bytes

- *java-class*: a `java-class` structure
- *constant-pool*: a `constant-pool` structure
- *bytes*: a list of integers between 0 and 255, inclusive

Assembles the `java-class` structure into a list of bytes representing its class file.

A `constant-pool` structure is used to generate the class file. There is no
need to provide a pool structure unless you need fine-grained control over the
order or number of constants in the pool. The `constant-pool` structure and its
associated functions are not exported.

---

[Function] **disassemble-jclass** bytes => java-class, constant-pool

- *bytes*: a vector of integers between 0 and 255, inclusive
- *java-class*: a `java-class` structure
- *constant-pool*: an array of constant values

*Exceptional situations*: signals `class-format-error` when parsing a malformed
class file.

Disassembles a vector of bytes into a java-class structure.

The 0th indexed item in the constant pool will always be `nil`, which represents
"no constant". Constant pool indices start at 1.
See [Optional Constants](#optional-constants) for details.

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

> Fun trivia: You can annotate method, fields, etc. with classes that don't
> implement `java.lang.annotations.Annotation`! In fact, you can annotate things
> with pretty much anything - integer literals, strings, you name it. OpenJDK's
> HotSpot gladly accepts and runs these classes. Trying to read the annotation
> with reflection does throw a `java.lang.annotation.AnnotationFormatError` though.
>
> Of course, jclass will not signal a `class-format-error` when generating such
> a malformed class.

## Fields

[Class] **field-info**

[Accessor] **flags** field-info => flags \
[Accessor] **name** field-info => name \
[Accessor] **descriptor** field-info => descriptor \
[Accessor] **attributes** field-info => attributes

- *flags*: a list of keywords from this set: `:public :private :protected :static
:final :volatile :transient :synthetic :enum`
- *name*: this field's name string
- *descriptor*: this field's type descriptor string
- *attributes*: a list of [attribute](#Attributes) structures

## Methods

[Class] **method-info**

[Accessor] **flags** method-info => flags \
[Accessor] **name** method-info => name \
[Accessor] **descriptor** method-info => descriptor \
[Accessor] **attributes** method-info => attributes

- *flags*: a list of keywords from this set: `:public :private :protected :static
:final :synchronized :bridge :varargs :native :abstract :strict :synthetic`
- *name*: this method's name string
- *descriptor*: this methods's type descriptor string
- *attributes*: a list of [attribute](#Attributes) structures

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
- Several attributes can take multiple constant types:
    - The ConstantValue attribute may take one of several constants.
        - See the JVM Spec, Table 4.7.2-A for details
    - The BoostrapMethods attribute may take one of several constants.

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

- *bootstrap-index*: an 8-bit integer index in the class's bootstrap-methods attribute
- *name*: a field or method name string
- *type*: a type descriptor string

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

[Reader] **attribute-name** attribute => attribute-name

- *attribute-name*: a string denoting the JVM specified name.
    - For example, the boostrap-methods attribute name is "BootstrapMethods"

Anonymous inner structures are represented by a list containing the values in
order.

### Constant Value Attribute

[Class] *constant-value*

[Accessor] **value** constant-value => value

- *value*: an integer-info, float-info, long-info, double-info, or string-info

### Code Attribute

[Class] *code*

[Accessor] **max-stack** code => max-stack \
[Accessor] **max-locals** code => max-locals \
[Accessor] **bytecode** code => bytecode \
[Accessor] **exceptions** code => exceptions

- *max-stack*: a 16-bit integer for the max stack size used
- *max-locals*: a 16-bit integer for the max number of locals used
- *bytecodes*: a list of bytecode instructions or an array of raw bytes
- *exceptions*: a list of exception table entries
    - each entry is a list with the form `(start-pc end-pc handler-pc catch-type)`
    - `start-pc`, `end-pc`, `handler-pc` are 16-bit bytecode offsets
    - `catch-type` is a class name string or `nil` to catch all exceptions
- *attributes*: a list of attributes

Examples of bytecode instructions:
- no operands: `:iinc` or `(:iinc)`
- one operand: `(:ifeq 10)` or `(:ldc "Hello, world!")`
- `tableswitch`: `(:tableswitch 44 0 2 28 30 38)`
    - The order of the operands is default, low, high, and then the offsets
    - Padding is automatically calculated
- `lookupswitch`: `(:lookupswitch 75 (98030 64) (2571410 36) (99162322 50))`
    - The order of the operands is `default &rest match-offset-pairs`
    - Each match-offset pair is `(match offset)`
    - Match and offset values are 32-bit signed integers
    - Padding is automatically calculated
- `wide`: `(:wide :iinc 300 500)` `(:wide :fstore 300)`
- `label` pseudo instruction: `(:label "loop")`

### Stack Map Table Attribute

[Class] *stack-map-table*

[Accessor] **table-entries** stack-map-table => entries

- *entries*: a list where each item is a stack map frame

#### Stack Map Frame Formats

| type                              | format                                    |
|-----------------------------------|-------------------------------------------|
| same                              | `(frame-type)`                            |
| same locals 1 stack item          | `(frame-type verification)`               |
| same locals 1 stack item extended | `(247 offset verification)`               |
| chop                              | `(frame-type offset)`                     |
| same extended                     | `(251 offset)`                            |
| append frame                      | `(frame-type offset &rest verifications)` |
| full frame                        | `(255 offset locals stack-items)`         |
    
- `locals` and `stack-items` are lists of verification type infos

#### Verification Type Info

| type               | format                |
|--------------------|-----------------------|
| top                | `:top`                |
| integer            | `:integer`            |
| float              | `:float`              |
| null               | `:null`               |
| uninitialized this | `:uninitialized-this` |
| object             | `class-name`          |
| uninitialized      | `offset`              |
| long               | `:long`               |
| double             | `:double`             |

- `class-name`: a class name string
- `offset`: a 16-bit bytecode offset

### Exceptions Attribute

[Class] *exceptions*

[Accessor] **exceptions** exceptions => exception-list

- *exception-list*: a list of class name strings

### Inner Classes Attribute

[Class] *inner-classes*

[Accessor] **classes** inner-classes => classes

- *classes*: a list where each item has the form `(inner outer name access)`
    - *inner*: a string denoting an inner class name
    - *outer*: a string denoting an outer class name
    - *name*: a string denoting the original simple name, or `nil` if anonymous
    - *access*: a list of keywords from this set: `:public :private :protected
:static :final :interface :abstract :synthetic :annotation :enum`

### Enclosing Method Attribute

[Class] *enclosing-method*

[Accessor] **class** enclosing-method => class \
[Accessor] **name** enclosing-method => name \
[Accessor] **type** enclosing-method => type

- *class*: a class name string
- *name*: method name string
- *type*: a type descriptor string

### Synthetic Attribute

[Class] *synthetic*

The Synthetic attribute has no slots.

### Signature Attribute

[Class] *signature*

[Accessor] **signature** signature => signature-string

- *signature-string*: a class signature string

The Signature Attribute is used for preserving type information for Java
generics. See the JVM Spec 4.7.9.1 for signature formats.

### Source File Attribute

[Class] *source-file*

[Accessor] **name** source-file => name

- *name*: a file name string

### Source Debug Extension Attribute

[Class] *source-debug-extension*

[Accessor] **debug** source-debug-extension => debug

- *debug*: an array of unsigned bytes

The binary format of *debug* is not specified by the Java Virtual Machine.

### Line Number Table Attribute

[Class] *line-number-table*

[Accessor] **line-numbers** line-number-table => line-numbers

- *line-numbers*: a list where each item has the form `(start-pc line-number)`
    - *start-pc*: an integer denoting the bytecode index
    - *line-number*: the line number corresponding to start-pc in the source file

### Local Variable Table Attribute

[Class] *local-variable-table*

[Accessor] **local-variables** local-variable-table => local-variables

- *local-variables*: a list where each item has the form
`(start-pc length name descriptor index)`
    - *start-pc*: a 16-bit bytecode index
    - *length*: a 16-bit byte length
    - *name*: a local variable name string
    - *descriptor*: the local variable's field descriptor string
    - *index*: a 16-bit local variable index

### Local Variable Type Table Attribute

[Class] *local-variable-type-table*

[Accessor] **local-variables** local-variable-type-table => local-variables

- *local-variables*: a list where each item has the form
`(start-pc length name descriptor index)`
    - *start-pc*: a 16-bit bytecode index
    - *length*: a 16-bit byte length
    - *name*: a local variable name string
    - *descriptor*: the local variable's field signature string
    - *index*: a 16-bit local variable index

### Deprecated Attribute

[Class] *deprecated*

The Deprecated attribute has no slots.

### Annotations

[Class] *annotation*

[Accessor] **type** annotation => type \
[Accessor] **element-value-pairs** annotation => element-value-pairs

- *type*: a field descriptor string
- *element-value-pairs*: a list where each item has the form `(tag value)`

#### Element Tags And Values

| type   | tag  | value                                                    |
|--------|------|----------------------------------------------------------|
| byte   | #\\B | an 8-bit integer                                         |
| char   | #\\C | a character with a code point of U+FFFF or below         |
| double | #\\D | a 64-bit integer representing the bits of a 64-bit float |
| float  | #\\F | a 32-bit integer representing the bits of a 32-bit float |
| int    | #\\I | a 32-bit integer                                         |
| long   | #\\J | a 64-bit integer                                         |
| short  | #\\S | a 16-bit integer                                         |
| boolean| #\\Z | 0 or 1                                                   |
| String | #\\s | a string                                                 |
| Enum   | #\\e | a list of an enum type and value                         |
| Class  | #\\@ | an annotation                                            |
| Array  | #\\[ | a list of element-value-pairs                            |

[Class] *runtime-visible-annotations* \
[Class] *runtime-invisible-annotations* \
[Class] *runtime-visible-parameter-annotations* \
[Class] *runtime-invsible-parameter-annotations*

[Accessor] **annotations** runtime-visible-annotations => annotations \
[Accessor] **annotations** runtime-invisible-annotations => annotations \
[Accessor] **annotations**
runtime-visible-parameter-parameter-annotations => annotations \
[Accessor] **annotations**
runtime-invisible-parameter-annotations => annotations

- *annotations*: a list of annotations

### Type Annotations

[Class] *type-path*

[Accessor] **paths** type-path => paths

- *paths*: a list where each item has the form `(kind argument-index)`
    - *kind*: an 8-bit kind
    - *argument-index*: an 8-bit index

See the JVM Spec 4.7.20.2 for the semantics of kind and argument index

[Class] *type-annotation*

[Accessor] **target-type** type-annotation => target-type \
[Accessor] **target-info** type-annotation => target-info \
[Accessor] **target-path** type-annotation => target-path\
[Accessor] **type** type-annotation => type \
[Accessor] **element-value-pairs** type-annotation => element-value-pairs

- *target-type*: an 8-bit integer - See the JVM Spec tables 4.7.20-A, B, and C
- *target-info*: a target info
- *target-path*: a type path
- *type*: a string denoting a field descriptor
- *element-value-pairs*: a list where each item has the form `(tag value)`
    - See [Element Tags and Values](#element-tags-and-values)

| target type          | target info                                     |
|----------------------|-------------------------------------------------|
| type parameter       | an 8-bit integer                                |
| supertype            | a 16-bit integer                                |
| type parameter bound | a list of two 8-bit integers                    |
| empty                | ignored                                         |
| formal parameter     | an 8-bit integer                                |
| throws               | a 16-bit integer                                |
| localvar             | a table - see below                             |
| catch                | a 16-bit integer                                |
| offset               | a 16-bit integer                                |
| type argument        | a list of a 16-bit integer and an 8-bit integer |

- *localvar table* is a list where each item has the form `(start-pc length index)`
    - *start-pc*: a 16-bit bytecode offset
    - *length*: a 16-bit byte length
    - *index*: a 16-bit local variable array index

See the JVM Spec 4.7.20.1 for target info semantics

[Class] *runtime-visible-type-annotaitons* \
[Class] *runtime-invisible-type-annotations*

[Accessor] **annotations** runtime-visible-type-annotations => type-annotations \
[Accessor] **annotations** runtime-invisible-type-annotations => type-annotations

- *type-annotations*: a list of type annotations

### Annotation Default Attribute

[Class] *annotation-default*

[Accessor] **tag** annotation-default => tag \
[Accessor] **value** annotation-default => value \

- *tag*, *value*: an [element tag and value](#element-tags-and-values)

### Bootstrap Methods Attribute

[Class] *bootstrap-methods*

[Accessor] **methods** bootstrap-methods => methods

- *methods*: a list where each item has the form `(kind reference arguments)`
    - *kind*: a 8-bit kind
    - *reference*: a loadable constant
    - *arguments*: a list of loadable constants

See the JVM Spec 4.4.8 for details on bootstrap kinds and references

### Method Parameters Attribute

[Class] *method-parameters*

[Accessor] **parameters** method-parameters => paramters

- *parameters*: a list where each item has the form `(name access-flags)`
    - *name*: parameter's unqualified name string
    - *access-flags*: a list of keywords from the set `:final :synthetic :mandated`

### Module Attribute

[Class] *module*

[Accessor] **name** module => name \
[Accessor] **flags** module => flags \
[Accessor] **version** module => version \
[Accessor] **requires** module => requires \
[Accessor] **exports** module => exports \
[Accessor] **opens** module => opens \
[Accessor] **uses** module => uses \
[Accessor] **provides** module => provides

- *name*: a module name string
- *flags*: a list of keywords from the set `:open :synthetic :mandated`
- *version*: a module version string
- *requires*: a list where each item has the form `(module flags version)`
    - *module*: a module name string
    - *flags*: a list of keywords from the set `:transitive :static-phase
:synthetic :mandated`
    - *version*: a module version string
- *exports*: a list where each item has the form `(package flags exports-to)`
    - *package*: a package name string
    - *flags*: a list of keywords from the set `:synthetic :mandated`
    - *exports-to*: a list of module name strings
- *opens*:  a list where each item has the form `(package flags opens-to)`
    - *package*: a package name string
    - *flags*: a list of keywords from the set `:synthetic :mandated`
    - *opens-to*: a list of module name strings
- *uses*: a list of class name strings
- *provides*: a list where each item has the form `(name provides-with)`
    - *name*: a class name string
    - *provides-with*: a list of class name strings

### Module Packages Attribute

[Class] *module-pacakges*

[Accessor] **packages** module-packages => packages

- *packages*: a list of package name strings

### Module Main Class Attribute

[Class] *module-main-class* main-class

[Accessor] **main-class** module-main-class => main-class

- *main-class*: a class name string

### Nest Host Attribute

[Class] *nest-host*

[Accessor] **classes** nest-host => classes

- *classes*: a list of class name strings

### Record Attribute

[Class] *record*

[Accessor] **components** record => components

- *components*: a list where each item has the form `(name descriptor attributes)`
    - *name*: a component name string
    - *descriptor*: a field descriptor string
    - *attributes*: a list of attributes

### Permitted Subclasses Attribute

[Class] *permitted-subclasses*

[Accessor] **classes** permitted-subclasses => classes

- *classes*: a list of class name strings

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

