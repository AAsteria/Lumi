# Lumi
The Lumi Programming Language <br/>

A functional programming language based on λ-calculus for scientific research.
We hope "Lumi" to be an efficient and friendly language to light the way for researchers.

## Feature (Updating...)
* Minimal core with packages and macro supplement
* Global variables or static variables within functions
* Y combinator implementation of recursion
* Type tagging (local variables in functions not required, optional)
* Memory management system or register allocation algorithm
* Eager evaluation, considering providing lazy evaluation type
* Pointer (necessary or not?)
* Call-CC and use of Call-CC for loop control and exception handling (wise or not? better alternative?)
* REPL & unit testing
* Friendly indentation system <br/>

Welcome to use Issues as a forum to discuss and request features!


## Lumi Kits
Lumi interpreter, compiler, and REPL (Read–Eval–Print Loop).

#### Running Lumi Program
```shell
lumi <source_file.lumi> [command line arguments]
```

#### Compiling Lumi Program
```shell
lumi -c <source_file.lumi> <output_file.lumo>
```

#### Interactive REPL
```shell
lumi
```