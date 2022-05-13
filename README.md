# jclass: Java Class File Builder

jclass makes it easy to generate and analyze Java class files in Common Lisp.
Build a compiler backend or a JVM bytecode analyzer with jclass!

Java classes, fields, methods, annotations, and more are represented as Lisp
objects for easy analysis and manipulation. jclass also automatically builds
the constant pool for you (but you can still make the pool manually if you
want!)

It's also possible to create custom class attributes with jclass.
An attribute description DSL makes it easy to add new attributes. Great for
targeting custom JVMs or experimental features.

jclass is feature complete and the API is mostly stable. jclass will be updated
with new JVM features whenever they are released, so do not `(:use #:jclass)` in
your packages.

## Hello World Example

For in-depth information, see the [manual](MANUAL.md).

```lisp
;; Build the main method
(defparameter *main*
  (make-instance 'jclass:method-info
    :flags '(:public :static)
    :name "main"
    :descriptor "([Ljava/lang/String;)V" ; void (String[])
    :attributes
      (list
        (make-instance 'jclass:code
          :max-stack  2
          :max-locals 1
          :bytecode
            `((:getstatic "java/lang/System" "out" "Ljava/io/PrintStream;")
              (:ldc ,(jclass:make-string-info "Hello, world!"))
              (:invokevirtual "java/io/PrintStream" "println" "(Ljava/lang/String;)V")
              :return)
          :exceptions '()
          :attributes '()))))

;; Generate the class file
(with-open-file (stream "./Hello.class"
                        :direction :output
                        :element-type '(unsigned-byte 8))
  (write-sequence
    (jclass:java-class-bytes
      (make-instance 'jclass:java-class
        ;; version 55.0 = Java 11
        :major-version 55
        :minor-version 0
        :flags '(:public)
        :name "Hello"
        :parent "java/lang/Object"
        :interfaces '()
        :fields '()
        :methods (list *main*)
        :attributes '()))
        stream))
```

The class file executes as expected:

```bash
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

## Installation

### Manual Install

jclass is currently not in Quicklisp or Ultralisp.

Install by cloning to your `~/common-lisp/` or `quicklisp/local-projects/`
directories.

Then run the following:
```lisp
(ql:register-local-projects)
(asdf:load-system "jclass")
```

### Roswell Install

If you have [Roswell](https://github.com/roswell/roswell), you can install from
Github.

```sh
$ ros install davidsun0/jclass
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

### Implemented Structures by JVM Spec Section

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
    - [X] 4.7.31 PermittedSublcasses

### Bytecode Instructions

- [X] Bytecode instructions (205 / 205)
    - [X] Constants (21 / 21)
    - [X] Loads (33 / 33)
    - [X] Stores (33 / 33)
    - [X] Stack (9 / 9)
    - [X] Math (37 / 37)
    - [X] Conversions (15 / 15)
    - [X] Comparisons (19 / 19)
    - [X] References (18 / 18)
    - [X] Control (11 / 11)
    - [X] Extended (6 / 6)
    - [X] Reserved (3 / 3)

## Library Design

jclass is based off of the Java Virtual Machine Specification, and not the
Java Programming Language. As such, it can generate code that uses the
`invokedynamic` instruction or use special characters in names.

The class file is represented as a tree of objects. Every named structure in
the JVM Specification has an equivalent Lisp object.

There are functions to assemble and disassemble a `java-class` object to
and from a `.class` file. As fields, methods, and attributes are part of a
class, they cannot be serialized independently.

## License

MIT

