# jclass: Java Class File Generation

jclass makes it easy to build Java class files in Common Lisp, making it easy
to develop low level code for the JVM.

## Portability

jclass should work conforming Common Lisps with the following features:

- Unicode support requires that CHAR-CODE return the Unicode code point

Incompatibility with an implementation that has these features should be
considered a bug. Please report any issues.

## Current Status

jclass is in its early stages of development. The API is unlikely to change
in major ways, but I'm actively developing new features.

## Hello World Example

## Classes

## Fields

Fields are defined by a name, type, and access modifier.

## Methods

### Constructors

Constructors are methods with a special name and signature.

## Attributes

jclass is designed to abstract over the Java class file format, not the Java
programming language. Aside from classes, fields and methods, all other
information is encoded in attributes.

## Verification

jclass does not perform validity checking for JVM compatibility.

For example, bytecode instructions have been added and obsoleted.
New Java features are implemented with new flags or attribute structures,
which may be rejected with an older JVM or JVM class version.

It is up to the user to match features used to JVM class file version.
