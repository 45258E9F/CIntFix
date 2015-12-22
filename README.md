# CIntFix
A tool to fix C integer errors by automatic precision improvement

Introduction
-------------
CIntFix is a tool to automatically fix C integer errors (including overflow, underflow, sign conversion and lossy truncation) by elevating precision of bounded program integers. Since the nature of integer errors is the inconsistency of fixed-length bit-vector logic and mathematical integer logic, our tool can solve most of common errors, with acceptable loss of runtime efficiency. CIntFix depends CDT to parse C source code and GMP(https://gmplib.org) to support multi-precision integer arithmetic.

CIntFix is also the prototype tool of the paper "Automatic Fix for C Integer Errors by Precision Improvement".

Requirements
--------------
OS: Linux distributions
libgmp (You can install it from source code or software repositories, GMP 6.0.0 and later is recommanded)
gcc (gcc 5.2.1 is tested) or clang (clang 3.6.2 is tested), cpp (C preprocessor, version 5.2.1 is tested)

Build
------
**1.** compile java files into `.class` files
**2.** compile `intfixtoolkit.c` into `libintfixtoolkit.so` for linking

    $ gcc -shared -o libintfixtoolkit.so -fPIC hello.c -lgmp
    

Run Guide
---------
**1.** Generate translation units (`.i` files) for all `.c` files. You can generate `.i` files using `cpp`. For example:

    $ cpp $(INCLUDES) source.i -o source.c 

Notice that you should specify correct include files or folders for correct analysis.

**2.** Run Java program to fix C source files in the specified folder

    $ java IntErrorFix [your specified folder]
  
  For a C file `code.c`, the name of fixed file is `code.hp.c` while the original one keeps. You can choose to replace the new file with the original one.
  
**3.** Compile the new program with linking to `libintfixtoolkit.so` or adding `intfixtoolkit.h` into existing project.

**4.** Enjoy the fixed program now!

