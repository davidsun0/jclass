# jclass: Java Class File Builder

jclass builds, assembles, and disassembles  Java class files in Common Lisp,
making it easy to develop low level code for the JVM.

## Hello World Example

For in-depth information, see the [tutorial](#) and [manual](MANUAL.md).

```
;; generate the main method
(defparameter *main*
  (make-method-info
    '(:public :static)
    "main"
    "([Ljava/lang/String;)V" ; void (String[])
    ;; attributes
    (list (make-code
            2 ; max stack size of 2
            1 ; max local count of 1
            `((:getstatic "java/lang/System" "out" "Ljava/io/PrintStream;")
              (:ldc ,(make-string-info "Hello, world!"))
              (:invokevirtual "java/io/PrintStream" "println" "(Ljava/lang/String;)V")
              :return)
            '() ; no exceptions
            '() ; no code attributes
    ))))

;; generate the file
(with-open-file (stream "./Hello.class"
                        :direction :output
                        :element-type '(unsigned-byte 8))
  (write-sequence
    (java-class-bytes
      (make-java-class
        0 55 ; v55.0 = Java 11
        '(:public)
        "Hello"	; class Hello
        "java/lang/Object" ; extends Object
        '() ; no implemented interfaces
        '() ; no fields
        (list *main*) ; one method
        '() ; no class attributes
        ))
    stream))
```

The class file executes as expected:

```
$ java Hello
Hello, world!
```

Output from javap:

```
$ javap -v Hello.class
Classfile Hello.class
  Last modified Aug 22, 2021; size 281 bytes
  MD5 checksum b1d5aa6684ec1b281718b3cb249f21a0
public class Hello
  minor version: 0
  major version: 55
  flags: (0x0001) ACC_PUBLIC
  this_class: #1                          // Hello
  super_class: #2                         // java/lang/Object
  interfaces: 0, fields: 0, methods: 1, attributes: 0
Constant pool:
   #1 = Class              #9             // Hello
   #2 = Class              #10            // java/lang/Object
   #3 = Utf8               main
   #4 = Utf8               ([Ljava/lang/String;)V
   #5 = Fieldref           #11.#13        // java/lang/System.out:Ljava/io/PrintStream;
   #6 = String             #16            // Hello, world!
   #7 = Methodref          #17.#19        // java/io/PrintStream.println:(Ljava/lang/String;)V
   #8 = Utf8               Code
   #9 = Utf8               Hello
  #10 = Utf8               java/lang/Object
  #11 = Class              #12            // java/lang/System
  #12 = Utf8               java/lang/System
  #13 = NameAndType        #14:#15        // out:Ljava/io/PrintStream;
  #14 = Utf8               out
  #15 = Utf8               Ljava/io/PrintStream;
  #16 = Utf8               Hello, world!
  #17 = Class              #18            // java/io/PrintStream
  #18 = Utf8               java/io/PrintStream
  #19 = NameAndType        #20:#21        // println:(Ljava/lang/String;)V
  #20 = Utf8               println
  #21 = Utf8               (Ljava/lang/String;)V
{
  public static void main(java.lang.String[]);
    descriptor: ([Ljava/lang/String;)V
    flags: (0x0009) ACC_PUBLIC, ACC_STATIC
    Code:
      stack=3, locals=1, args_size=1
         0: getstatic     #5                  // Field java/lang/System.out:Ljava/io/PrintStream;
         3: ldc           #6                  // String Hello, world!
         5: invokevirtual #7                  // Method java/io/PrintStream.println:(Ljava/lang/String;)V
         8: return
}
```

## Portability / Compatibility

jclass should work on conforming Common Lisps with the following features:
- `char-code` and `code-char` must work with Unicode code points.
    - If not working with Unicode, the functions must use ASCII values.

Incompatibility with an implementation that meets these requirements should be
considered a bug. Please report any issues.

## Verification

Since jclass works at the binary level, it only performs structural
validation, not semantic validation. For example, certain attributes and
flags are incompatible. Some features may only work with certain class file
versions. jclass does not ensure that a JVM will accept a generated class.

It is up to the user to correctly match class file features.

## Current Status

jclass is in its still in development. The API is not yet stable.

### Implemented Structures

- [X] 4.1 The ClassFile Structure
- [X] 4.4 The Constant Pool
- [X] 4.5 Fields
- [X] 4.6 Methods
- [X] 4.7 Attributes
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
    - [X] 4.7.20 RuntimeVisibleTypeAnnotaions
    - [X] 4.7.21 RuntimeInvisibleTypeAnnotations
    - [X] 4.7.22 AnnotationDefault
    - [X] 4.7.23 BootstrapMethods
    - [X] 4.7.24 MethodParameters
    - [X] 4.7.25 Module
    - [X] 4.7.26 ModulePackages
    - [X] 4.7.27 ModuleMainClass
    - [X] 4.7.28 NestHost
    - [X] 4.7.29 NestMembers
    - [X] 4.7.30 Record
    - [X] PermittedSubclasses (Java 17 Preview Feature)

### Bytecode Instructions

- [ ] Bytecode instructions (202 / 205)
    - [X] Constants (21 / 21)
    - [X] Loads (33 / 33)
    - [X] Stores (33 / 33)
    - [X] Stack (9 / 9)
    - [X] Math (37 / 37)
    - [X] Conversions (15 / 15)
    - [X] Comparisons (19 / 19)
    - [X] References (18 / 18)
    - [ ] Control (9 / 11)
    - [ ] Extended (5 / 6)
    - [X] Reserved (3 / 3)

### Verification Layer

In their raw form, these attributes work with bytecode offsets.
The bytecode layer makes calculating offsets easy with the `label`
pseudoinstruction.

- [ ] Code attribute
- [ ] StackMapTable attribute
- [ ] LineNumberTable attribute
- [ ] LocalVariableTable attribute
- [ ] LocalVariableTypeTable attribute
- [ ] RuntimeVisibleTypeAnnotations attribute
- [ ] RuntimeInvisibleTypeAnnotations attribute

## Library Design

jclass is based off of the Java Virtual Machine Specification, and not the
Java Programming Language. As such, it can generate code that uses the
`invokedynamic` instruction or use special characters in names.

The class file is represented as a tree of structures. Every named structure
in the JVM Specification can be created and manipulated like any other lisp
structure.

There are functions to assemble and disassemble a `java-class` structure to
and from a `.class` file. Note that fields, methods, and attributes cannot be
serialized or deserialized outside of a class.

jclass is built up in these layers:

- `constant-pool.lisp`: handles constant resolution and byte I/O
- `structures.lisp`: defines class, field, method, and attribute structures and
how they are (de)serialized
- `bytecode.lisp`: (dis)assembles bytecode
- `verification.lisp`: abstracts over bytecode offsets with labels

## License

jclass is released under the MIT License.

