# jclass Manual

This file is intended to serve as a reference and not a starting point.
I recommend reading the [tutorial](TUTORIAL.md) and the
[Java Virtual Machine Specification](https://docs.oracle.com/javase/specs/index.html)
first.

jclass is built from the perspective of the Java Virtual Machine, and not from
that of the Java Programming Language. There are important differences that are
covered not covered in this manual.

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
reference is also unambiguous in its tye. As a result,
constants in jclass are created with the minimal amount of information needed.

### Ambiguous Constant Types

When the constant type *is* ambiguous, the user must pass a constant explicitly.

Places where constant type is ambiguous:
- `ldc` and `ldc_w` takes any loadable constant
- `ldc2_w` takes a `Long_info` or a `Double_info`
- `invokespecial` takes a `Methodref_info` or a `InterfaceMethodref_info`
constant

### Optional Constants

Sometimes a constant is optional and a constant pool index may be zero
(remember that constant pool indices start at 1).
jclass will output a zero if the supplied value is NIL.

### Constant Functions

[*Function*] **make-utf8-info** string => utf8-info \
[*Accessor*] **utf8-info-text** utf8-info => string \
[*Function*] **utf8-info-p** object => boolean

- *string*: a string

Unicode characters may be used if each element of *string* represents a code point.

[*Function*] **make-integer-info** integer => integer-info \
[*Accessor*] **integer-info-value** integer-info => integer \
[*Function*] **integer-info-p** object => boolean

- *integer*: an 32-bit signed integer

[*Function*] **make-float-info** ieee-bits => float-info \
[*Accessor*] **float-info-ieee-bits** float-info => ieee-bits \
[*Function*] **float-info-p** object => boolean

- *ieee-bits*: a 32-bit signed integer whose two's compliment bits represent a 32-bit IEEE float

[*Function*] **make-long-info** integer => long-info \
[*Accessor*] **long-info-value** long-info => integer \
[*Function*] **long-info-p** object => boolean

- *integer*: a 64-bit signed integer

[*Function*] **make-double-info** ieee-bits => double-info \
[*Accessor*] **double-info-ieee-bits** double-info ieee-bits \
[*Function*] **double-info-p** object => boolean

- *ieee-bits*: a 64-bit signed integer whose two's compliment bits represent a 64-bit IEEE float

[*Function*] **make-class-info** name => class-info \
[*Accessor*] **class-info-name** class-info => name \
[*Function*] **class-info-p** object => boolean

- *name*: a string representing a Java class name

See the JVM Specification 4.2 for restrictions on class names.

[*Function*] **make-string-info** string => string-info \
[*Accessor*] **string-info-text** string-info => string \
[*Function*] **string-info-p** object => boolean

- *string*: a string

Unicode characters may be used if each element of *string* represents a code point.

---

[*Function*] **make-field-ref-info** class-name name type => field-ref-info \
[*Accessor*] **field-ref-info-class-name** field-ref-info => class-name \
[*Accessor*] **field-ref-info-name** field-ref-info => name \
[*Accessor*] **field-ref-info-type** field-ref-info => type \
[*Function*] **field-ref-info-p** object => boolean

[*Function*] **make-method-ref-info** class-name name type => method-ref-info \
[*Accessor*] **method-ref-info-class-name** method-ref-info => class-name \
[*Accessor*] **method-ref-info-name** method-ref-info => name \
[*Accessor*] **method-ref-info-type** method-ref-info => type \
[*Function*] **method-ref-info-p** object => boolean

[*Function*] **make-interface-method-ref-info** class-name name type => interface-method-ref \
[*Accessor*] **interface-method-ref-info-class-name** interface-method-ref => class-name \
[*Accessor*] **interface-method-ref-info-name** interface-method-ref => name \
[*Accessor*] **interface-method-ref-info-type** interface-method-ref => type \
[*Function*] **interface-method-ref-info-p** object => boolean

[*Function*] **make-name-and-type-info** name type => name-and-type-info \
[*Accessor*] **name-and-type-info-name** name-and-type-info => name \
[*Accessor*] **name-and-type-info-type** name-and-type-info => type \
[*Function*] **name-and-type-info-p** object => boolean

- *class-name*: a string representing a Java class name
- *name*: a string representing the name of the field, method, or name and type info
- *type*: a string representing a type descriptor

See the JVM Specification 4.2 for restrictions on field and method names. \
See the JVM Specification 4.3 for restrictions on descriptors.

---

[*Function*] **make-method-handle-info** kind reference => method-handle-info \
[*Accessor*] **method-handle-info-kind** method-handle-info => kind \
[*Accessor*] **method-handle-info-reference** method-handle-info => reference \
[*Function*] **method-handle-info-p** object => boolean

- *kind*: an 8-bit signed integer
- *reference*: a field-ref-info, method-ref-info, or interface-method-ref-info

See the JVM Specification 4.4.8 for the semantics of the method handle info.

[*Function*] **make-method-type-info** descriptor => method-type-info \
[*Accessor*] **method-type-info-descriptor** method-type-info => descriptor \
[*Function*] **method-type-info-p** object => boolean

- *descriptor*: a string representing a type descriptor

See the JVM Specification 4.3 for restrictions on descriptors.

---

[*Function*] **make-dynamic-info** bootstrap-index name type => dynamic-info \
[*Accessor*] **dynamic-info-bootstrap-index** dynamic-info => bootstrap-index \
[*Accessor*] **dynamic-info-name** dynamic-info => name \
[*Accessor*] **dynamic-info-type** dynamic-info => type \
[*Function*] **dynamic-info-p** object => boolean

[*Function*] **make-dynamic-info** bootstrap-index name type => dynamic-info \
[*Accessor*] **dynamic-info-bootstrap-index** dynamic-info => bootstrap-index \
[*Accessor*] **dynamic-info-name** dynamic-info => name \
[*Accessor*] **dynamic-info-type** dynamic-info => type \
[*Function*] **dynamic-info-p** object => boolean

- *bootstrap-index*: an 8-bit integer representing an index in the class's
bootstrap-methods attribute
- *name*: a string representing the name of a field or method
- *type*: a string representing a type descriptor

See the JVM Specification 4.4.10 for the semantics of the dynamic info and the
invoke dynamic info. \
See the JVM Specification 4.2 for restrictions on field and method names. \
See the JVM Specification 4.3 for restrictions on descriptors.

---

[*Function*] **make-module-info** name => module-info \
[*Accessor*] **module-info-name** module-info => name \
[*Function*] **module-info-p** object => boolean

[*Function*] **make-package-info** name => package-info \
[*Accessor*] **package-info-name** package-info => name \
[*Function*] **package-info-p** object => boolean

- *name*: a string representing the name of a module or package

## Java Classes

[*Structure*] **java-class**

[*Function*] **make-java-class** minor-version major-version flags name
parent interfaces fields methods attributes => java-class

[*Accessor*] **java-class-minor-version** java-class => minor-version \
[*Accessor*] **java-class-major-version** java-class => major-version \
[*Accessor*] **java-class-flags** java-class => flags \
[*Accessor*] **java-class-name** java-class => name \
[*Accessor*] **java-class-parent** java-class => parent \
[*Accessor*] **java-class-interfaces** java-class => interfaces \
[*Accessor*] **java-class-fields** java-class => fields \
[*Accessor*] **java-class-methods** java-class => methods \
[*Accessor*] **java-class-attributes** java-class => attributes

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

[*Function*] **java-class-p** object => generalized-boolean

Returns true if *object* is of type *java-class*; otherwise, returns false.

---

[*Function*] **java-class-bytes** java-class &optional constant-pool => bytes

- *java-class*: a `java-class` structure
- *constant-pool*: a `constant-pool` structure
- *bytes*: a list of integers between 0 and 255, inclusive

Assembles the `java-class` structure into a list of bytes representing its class file.

A `constant-pool` structure is used to generate the class file. There is no
need to provide a pool structure unless you need fine-grained control over the
order or number of constants in the pool.

---

[*Function*] **disassemble-jclass** bytes => java-class, constant-pool

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

Note that `class-format-error` is only used for **structural** issues.
For example, jclass will not signal an error if you create a class that
implements a non-existant interface or something that is not even an interface.
jclass will signal `class-format-error` for problems like an invalid annotation
value, which prevents further parsing of the annotation.

Fun trivia: You can annotate method, fields, etc. with classes that don't
implement `java.lang.annotations.Annotation`! In fact, you can annotate things
with pretty much anything - integer literals, strings, you name it. OpenJDK's
HotSpot gladly accepts and runs these classes. Trying to read the annotation
with reflection does throw a `java.lang.annotation.AnnotationFormatError` though.

Of course, jclass will not signal a `class-format-error` when generating such
a malformed class.

## Fields

[*Structure*] **field-info**

[*Function*] **make-field-info** flags name descriptor attributes => field-info

[*Accessor*] **field-info-flags** field-info => flags \
[*Accessor*] **field-info-name** field-info => name \
[*Accessor*] **field-info-descriptor** field-info => descriptor \
[*Accessor*] **field-info-attributes** field-info => attributes

- *flags*: a list of keywords from this set: `:public :private :protected :static
:final :volatile :transient :synthetic :enum`
- *name*: a string of this field's name
- *descriptor*: a string of this field's type descriptor
- *attributes*: a list of [attribute](#Attributes) structures

[*Function*] **field-info-p** object => generalized-boolean

Returns true if *object* is of type *field-info*; otherwise, returns false.

## Methods

[*Structure*] **method-info**

[*Function*] **make-method-info** flags name descriptor attributes => method-info

[*Accessor*] **method-info-flags** method-info => flags \
[*Accessor*] **method-info-name** method-info => name \
[*Accessor*] **method-info-descriptor** method-info => descriptor \
[*Accessor*] **method-info-attributes** method-info => attributes

- *flags*: a list of keywords from this set: `:public :private :protected :static
:final :synchronized :bridge :varargs :native :abstract :strict :synthetic`
- *name*: a string of this methods's name
- *descriptor*: a string of this methods's type descriptor
- *attributes*: a list of [attribute](#Attributes) structures

[*Function*] **method-info-p** object => generalized-boolean

Returns true if *object* is of type *method-info*; otherwise, returns false.

## Attributes

Anonymous inner structures are represented by a list containing the values in
order.

[*Structure*] *constant-value*

[*Structure*] *code*

[*Structure*] *stack-map-table*

[*Structure*] *exceptions*

[*Structure*] *inner-classes*

[*Structure*] *enclosing-method*

---

[*Structure*] *synthetic*

[*Function*] make-synthetic => synthetic-attribute

The Synthetic attribute has no slots.

---

[*Structure*] *signature*

[*Structure*] *source-file*

[*Structure*] *source-debug-extension*

[*Structure*] *line-number-table*

[*Structure*] *local-variable-table*

[*Structure*] *local-variable-type-table*

---

[*Structure*] *deprecated*

[*Function*] make-deprecated => deprecated-attribute

The Deprecated attribute has no slots.

--- 

### Annotations

[*Structure*] *runtime-visible-annotations*

[*Structure*] *runtime-invisible-annotations*

[*Structure*] *runtime-visible-parameter-annotations*

[*Structure*] *runtime-invsible-parameter-annotations*

[*Structure*] *annotation*

---

### Type Annotations

[*Structure*] *type-annotation*

[*Structure*] *runtime-visible-type-annotaitons*

[*Structure*] *runtime-invisible-type-annotaitons*

[*Function*] **make-runtime-visible-type-annotations type-annotations** =>
runtime-visible-type-annotations

[*Function*] **make-runtime-invisible-type-annotations type-annotations** =>
runtime-invisible-type-annotations

- *type-annotations*: a list of type annotation structures
- *runtime-visible-type-annotations*: a runtime-visible-type-annotations structure
- *runtime-invisible-type-annotations*: a runtime-invisible-type-annotations structure

---

[*Structure*] *annotation-default*

[*Structure*] *bootstrap-methods*

[*Structure*] *method-parameters*

[*Structure*] *module*

[*Structure*] *module-pacakges*

[*Structure*] *module-main-class*

[*Structure*] *nest-host*

[*Structure*] *record*

[*Structure*] *permitted-subclasses*

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

