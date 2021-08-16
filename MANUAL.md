# jclass Manual

This file is intended to serve as a reference and not a starting point.
I recommend reading the [TUTORIAL.md] and the
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
byte extended or cut when reading, writing, or passing values.

Just like the JVM, jclass represents boolean true and false with 1 and 0,
respectively.

### Optional Constants

Sometimes a constant is optional and a constant pool index may be zero.
jclass will output a zero if the supplied value is NIL.

## Classes

Structure *java-class*

Slots:
- *minor-version*: An integer representing a Java Class minor version
- *major-version*: An integer representing a Java Class major version
- *flags*: A list of keywords from this set: `:public :final :super :interface
:abstract :synthetic :annotation :enum :module`
- *name*: A string of this class's [binary name](#binary-names)
- *parent*: A string of this class's parent's [binary name](#binary-names)
- *interfaces*: A list of strings of implemented interface names
- *fields*: A list of [field](#fields) structures
- *methods*: A list of [method](#methods) structures
- *attributes*: A list of [attributes](#attributes) structures

---

Function *java-class-bytes* java-class &optional constant-pool

Assembles the java-class structure into a list of bytes representing its class file.

Function *disassemble-jclass* bytes

Disassembles a vector of bytes into a java-class structure.
Signals `class-format-error` when parsing a malformed class file.

## Fields

Structure *field-info*

Slots:
- *flags*: A list of keywords from this set: `:public :private :protected :static
:final :volatile :transient :synthetic :enum`
- *name*: A string of this field's name
- *descriptor*: A string of this field's type descriptor
- *attributes*: A list of [attributes](#Attributes) structures

## Methods

Structure *method-info*

Slots:
- *flags*: A list of keywords from this set: `:public :private :protected :static
:final :synchronized :bridge :varargs :native :abstract :strict :synthetic`
- *name*: A string of this methods's name
- *descriptor*: A string of this methods's type descriptor
- *attributes*: A list of [attributes](#Attributes) structures

## Attributes

Anonymous inner structures are represented by a list containing the values in
order.

### List of Attribute Structures

Structure *constant-value*

Structure *code*

Structure *stack-map-table*

Structure *exceptions*

Structure *inner-classes*

Structure *enclosing-method*

Structure *synthetic*

Structure *signature*

Structure *source-file*

Structure *source-debug-extension*

Structure *line-number-table*

Structure *local-variable-table*

Structure *local-variable-type-table*

Structure *deprecated*

Structure *runtime-visible-annotations*

Structure *runtime-invisible-annotations*

Structure *runtime-visible-parameter-annotations*

Structure *runtime-invsible-parameter-annotations*

Structure *runtime-visible-type-annotaitons*

Structure *runtime-invisible-type-annotaitons*

Structure *annotation-default*

Structure *bootstrap-methods*

Structure *method-parameters*

Structure *module*

Structure *module-pacakges*

Structure *module-main-class*

Structure *nest-host*

Structure *record*

Structure *permitted-subclasses*

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

