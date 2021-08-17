# jclass Manual

This file is intended to serve as a reference and not a starting point.
I recommend reading the [tutorial](TUTORIAL.md) and the
[Java Virtual Machine Specification](https://docs.oracle.com/javase/specs/index.html)
first.

## Constant Representation

jclass automatically builds constants from their most basic components. When
a constant class info is required, supply a string representing a constant utf8
info. For example, the interfaces list of a java-class structure might be:
`(list "java/lang/Comparable" "java/lang/Interable")`.

### Binary Names

Types and class names and should be specified as the Java binary form.
For example, `java.lang.Object` should be input as `java/lang/Object` when
specifying the class and `Ljava/lang/Object;` when specifying the type.
For more information, see the Java Virtual Machine Specification 4.3: Descriptors.

### Booleans, Bytes, and Shorts

Internally, Java represents booleans, bytes, and shorts as integers. Values are
sign extended or cut when reading, writing, or passing values.

Just like the JVM, jclass represents boolean true and false with 1 and 0,
respectively.

### Optional Constants

Sometimes a constant is optional and a constant pool index may be zero.
jclass will output a zero if the supplied value is NIL.

## Java Classes

**Structure** *java-class*

**Constructor** *make-java-class* minor-version major-version flags name
parent interfaces fields methods attributes => java-class

**Accessor** *java-class-minor-version* java-class => minor-version \
**Accessor** *java-class-major-version* java-class => major-version \
**Accessor** *java-class-flags* java-class => flags \
**Accessor** *java-class-name* java-class => name \
**Accessor** *java-class-parent* java-class => parent \
**Accessor** *java-class-interfaces* java-class => interfaces \
**Accessor** *java-class-fields* java-class => fields \
**Accessor** *java-class-methods* java-class => methods \
**Accessor** *java-class-attributes* java-class => attributes

- *minor-version*: an integer representing a Java Class minor version
- *major-version*: an integer representing a Java Class major version
- *flags*: a list of keywords from this set: `:public :final :super :interface
:abstract :synthetic :annotation :enum :module`
- *name*: a string of this class's [binary name](#binary-names)
- *parent*: a string of this class's parent's [binary name](#binary-names)
- *interfaces*: a list of strings of implemented interface names
- *fields*: a list of [field](#fields) structures
- *methods*: a list of [method](#methods) structures
- *attributes*: a list of [attributes](#attributes) structures

**Function** *java-class-p* object => generalized-boolean

Returns true if *object* is of type *java-class*; otherwise, returns false.

---

**Function** *java-class-bytes* java-class &optional constant-pool => bytes

- *java-class*: a `java-class` structure
- *constant-pool*: a `constant-pool` structure
- *bytes*: a list of integers between 0 and 255, inclusive

Assembles the `java-class` structure into a list of bytes representing its class file.

A `constant-pool` structure is used to generate the class file. There is no
need to provide a pool structure unless you need fine-grained control over the
structure of the constant pool in the generated class file.

---

**Function** *disassemble-jclass* bytes => java-class

- *bytes*: a vector of integers between 0 and 255, inclusive
- *java-class*: a `java-class` structure

*Exceptional situations*: signals `class-format-error` when parsing a malformed
class file.

Disassembles a vector of bytes into a java-class structure.

---

**Condition Type** *class-format-error*

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

Of course, generating such a malformed class will not cause jclass to signal
a `class-format-error`.

## Fields

**Structure** *field-info*

**Constructor** *make-field-info* flags name descriptor attributes => field-info

**Accessor** *field-info-flags* field-info => flags \
**Accessor** *field-info-name* field-info => name \
**Accessor** *field-info-descriptor* field-info => descriptor \
**Accessor** *field-info-attributes* field-info => attributes

- *flags*: a list of keywords from this set: `:public :private :protected :static
:final :volatile :transient :synthetic :enum`
- *name*: a string of this field's name
- *descriptor*: a string of this field's type descriptor
- *attributes*: a list of [attributes](#Attributes) structures

**Function** *field-info-p* object => generalized-boolean

Returns true if *object* is of type *field-info*; otherwise, returns false.

## Methods

**Structure** *method-info*

**Constructor** *make-method-info* flags name descriptor attributes => method-info

**Accessor** *method-info-flags* method-info => flags \
**Accessor** *method-info-name* method-info => name \
**Accessor** *method-info-descriptor* method-info => descriptor \
**Accessor** *method-info-attributes* method-info => attributes

- *flags*: a list of keywords from this set: `:public :private :protected :static
:final :synchronized :bridge :varargs :native :abstract :strict :synthetic`
- *name*: a string of this methods's name
- *descriptor*: a string of this methods's type descriptor
- *attributes*: a list of [attributes](#Attributes) structures

**Function** *method-info-p* object => generalized-boolean

Returns true if *object* is of type *method-info*; otherwise, returns false.

## Attributes

Anonymous inner structures are represented by a list containing the values in
order.

**Structure** *constant-value*

**Structure** *code*

**Structure** *stack-map-table*

**Structure** *exceptions*

**Structure** *inner-classes*

**Structure** *enclosing-method*

---

**Structure** *synthetic*

**Constructor** make-synthetic => synthetic-attribute

The Synthetic attribute has no slots.

---

**Structure** *signature*

**Structure** *source-file*

**Structure** *source-debug-extension*

**Structure** *line-number-table*

**Structure** *local-variable-table*

**Structure** *local-variable-type-table*

---

**Structure** *deprecated*

**Constructor** make-deprecated => deprecated-attribute

The Deprecated attribute has no slots.

--- 

### Annotations

**Structure** *runtime-visible-annotations*

**Structure** *runtime-invisible-annotations*

**Structure** *runtime-visible-parameter-annotations*

**Structure** *runtime-invsible-parameter-annotations*

**Structure** *annotation*

---

### Type Annotations

**Structure** *type-annotation*

**Structure** *runtime-visible-type-annotaitons*

**Structure** *runtime-invisible-type-annotaitons*

**Constructor** make-runtime-visible-type-annotations type-annotations => runtime-visible-type-annotations

**Constructor** make-runtime-invisible-type-annotations type-annotations => runtime-invisible-type-annotations

- *type-annotations*: a list of type annotation structures
- *runtime-visible-type-annotations*: a runtime-visible-type-annotations structure
- *runtime-invisible-type-annotations*: a runtime-invisible-type-annotations structure

---

**Structure** *annotation-default*

**Structure** *bootstrap-methods*

**Structure** *method-parameters*

**Structure** *module*

**Structure** *module-pacakges*

**Structure** *module-main-class*

**Structure** *nest-host*

**Structure** *record*

**Structure** *permitted-subclasses*

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

