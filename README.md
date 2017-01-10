# CIntFix
A tool to fix C integer errors by automatic precision improvement

Introduction
-------------
CIntFix is a tool to automatically fix C integer errors (including overflow, underflow, sign conversion and lossy truncation) by elevating precision of bounded program integers. Since the nature of integer errors is the inconsistency of fixed-length bit-vector logic and mathematical integer logic, our tool can solve most of common errors, with acceptable loss of runtime efficiency. CIntFix depends Eclipse CDT (https://eclipse.org/cdt/) to parse C source code and GMP library (https://gmplib.org) to support multi-precision integer arithmetic.

CIntFix is also the prototype tool of the paper "Automatic Fix for C Integer Errors by Precision Improvement". (http://ieeexplore.ieee.org/document/7551988/)

Requirements
--------------
OS: Linux distributions

Eclipse CDT (version 8.8 is tested)

libgmp (You can install it from source code or software repositories, GMP 6.0.0 o later is recommanded)

gcc (gcc 6.2.1 is tested) or clang (clang 3.6.2 is tested)

cpp (C preprocessor, version 6.2.1 is tested)

Build
------
**1.** compile java files into `.class` files

It is highly recommended to import our program as an Eclipse or IntelliJ project.

Run Guide
---------
**1.** Generate translation units (`.i` files) for all `.c` files. You can generate `.i` files using `cpp`. For example:

    $ cpp $(INCLUDES) source.c -o source.i 

Notice that you should specify correct include files or folders for correct analysis.

**2.** Run Java program to fix C source files in the specified folder

    $ java IntErrorFix [your specified folder] [taint level]
  
`[your specified folder]` is the folder containing C files to be fixed. `[taint level]` is the level of taint analysis. `IntErrorFix` allows at most 2 parameters. If only one parameter is given, it is parsed as the folder containing test programs. For a C file `code.c`, the name of fixed file is `code.hp.c` while the original one keeps. You can choose to replace the new file with the original one.
  
**3.** Compile the new program with `intfixtoolkit.c` and `intfixtoolkit.h` in the project.

**4.** Enjoy the fixed program now!

DataSet
-------

Juliet Test Suite v1.2 can be found at https://samate.nist.gov/SRD/testsuite.php.
SPEC 2000 benchmark is a commerical benchmark.
The reference result (updated on Aug. 2016) is here.
