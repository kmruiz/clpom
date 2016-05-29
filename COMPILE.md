how to compile
=============

with clpom
-----------

Just run:

```sh
$> clpom dist
```

without clpom
-------------

### Requirements

* SBCL: http://www.sbcl.org/
* Quicklisp: https://www.quicklisp.org/beta/#installation

### Steps:

* Start SBCL on the project directory:
```sh
$> sbcl
```

* Download all CLPOM dependencies
```sh
$> (ql:quickload '("quicklisp"
   "lisp-unit"
   "cl-ansi-text"
   "hunchentoot"
   "xmls"
   "drakma"
   "split-sequence"
   "zip"
   "cl-json"))
```

* Load the system file into SBCL
```lisp
$> (load "clpom.asd")
```

* Load CLPOM into SBCL
```lisp
$> (asdf:load-system :clpom)
```

* Scope in clpom package
```lisp
$> (in-package :clpom)
```

* Run the compiler (and wait)
```lisp
$> (do-tasks-at-file "project.clpom.lisp" (list "dist"))
```

* Exit SBCL
```lisp
$> (sb-ext:exit)
```

You'll have the binary file (if all is ok) into build/$VERSION/clpom. Add it to your PATH:
