# jclass: Java Class File Builder

jclass builds, assembles, and disassembles  Java class files in Common Lisp,
making it easy to develop low level code for the JVM.

## Portability / Compatibility

jclass should work conforming Common Lisps with the following features:

- Unicode support requires that `char-code` return the Unicode code point.
    - If not working with Unicode, `char-code` should return the ASCII value.

Incompatibility with an implementation that has these features should be
considered a bug. Please report any issues.

## Current Status

jclass is in its still in development. The API is not yet stable.

I have not yet settled on how to represent float and double literals.

### Implemented Structures

- [X] 4.1 The ClassFile Structure
- [X] 4.4 The Constant Pool
- [X] 4.5 Fields
- [X] 4.6 Methods
- [ ] 4.7 Attributes
    - [X] 4.7.2 ConstantValue
    - [X] 4.7.3 Code
    - [X] 4.7.4 StackMapTable
    - [X] 4.7.5 Exceptions
    - [X] 4.7.6 InnerClasses
    - [X] 4.7.7 EnclosingMethod
    - [X] 4.7.8 Synthetic
    - [X] 4.7.9 Signature
    - [X] 4.7.10 SourceFile
    - [X] 4.7.11 SourceDebugExtension
    - [X] 4.7.12 LineNumberTable
    - [X] 4.7.13 LocalVariableTable
    - [X] 4.7.14 LocalVariableTypeTable
    - [X] 4.7.15 Deprecated
    - [X] 4.7.16 RuntimeVisibleAnnotations
    - [X] 4.7.17 RuntimeInvisibleAnnotations
    - [X] 4.7.18 RuntimeVisibleParameterAnnotations
    - [X] 4.7.19 RuntimeInvisibleParameterAnnotations
    - [ ] 4.7.20 RuntimeVisibleTypeAnnotaions
    - [ ] 4.7.21 RuntimeInvisibleTypeAnnotations
    - [X] 4.7.22 AnnotationDefault
    - [X] 4.7.23 BootstrapMethods
    - [X] 4.7.24 MethodParameters
    - [X] 4.7.25 Module
    - [X] 4.7.26 ModulePackages
    - [X] 4.7.27 ModuleMainClass
    - [X] 4.7.28 NestHost
    - [X] 4.7.29 NestMembers
    - [X] 4.7.30 Record
    - [X] PermittedSubclasses

### Bytecode Layer

This layer of abstracts over bytecode offsets with labels.

- [ ] Bytecode instructions (0 / 205)
- [ ] Code attribute
- [ ] StackMapTable attribute
- [ ] LineNumberTable
- [ ] LocalVariableTable
- [ ] LocalVariableTypeTable
- [ ] RuntimeVisibleTypeAnnotations
- [ ] RuntimeInvisibleTypeAnnotations

## Hello World Example

Generating an emtpy class:

```
(with-open-file (stream "./MyClass.class"
				:direction :output
				:element-type '(unsigned-byte 8))
	  (write-sequence
	   (java-class-bytes
        (make-java-class
          0 60 ; Java 16
          '(:public :abstract)
          "MyClass"
          "java/lang/Object"
          '("java/lang/Iterable")
          '() ; no fields
          '() ; no methods
          '() ; no attributes
        ))
	   stream))
```

Output of official Java disassembler:

```
$ javap -v MyClass
Classfile MyClass.class
  Last modified Aug 14, 2021; size 85 bytes
  MD5 checksum 4640da844960e90c35d3c58d0a7d57db
public abstract class MyClass implements java.lang.Iterable
  minor version: 0
  major version: 57
  flags: (0x0401) ACC_PUBLIC, ACC_ABSTRACT
  this_class: #1                          // MyClass
  super_class: #2                         // java/lang/Object
  interfaces: 1, fields: 0, methods: 0, attributes: 0
Constant pool:
  #1 = Class              #4              // MyClass
  #2 = Class              #5              // java/lang/Object
  #3 = Class              #6              // java/lang/Iterable
  #4 = Utf8               MyClass
  #5 = Utf8               java/lang/Object
  #6 = Utf8               java/lang/Iterable
{
}
```

## Library Design

For more information, see [TUTORIAL.md] and [MANUAL.md].

jclass is based off of the Java Virtual Machine Specification, and not the
Java Programming Language.

Aside from classes, fields and methods, all other information is encoded in 
attributes.

## Verification

Since jclass works at the binary level, it only performs structural
validation, not semantic validation. For example, certain attributes and
flags are incompatible. Some features may only work with certain class file
versions. jclass does not ensure that a JVM will accept a generated class.

It is up to the user to correctly match class file features.

