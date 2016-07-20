clpom dsl
=========

The clpom buildfile is called `project.clpom.lisp` and the DSL is lisp-based. It has been designed
to be simple even for non-lispers.

However, because Lisp can be annoying to write, you can use an already existing generator to create
a template project and, from that template, change the things you don't like. Refer to the Quick Start
section in the README.md file.

#### defproject

Creates a new project for this buildscript.

```lisp
(defproject (name) ... buildscript)
```

#### define

Adds a new extra for the project. For example, name, description or dependencies.

```lisp
(define :version "2.0.0")
```
#### task

Creates a new task that runs some arbitrary lisp code.

```lisp
(define :name "say-hi" :does (lambda ($) (log-info "Hi!")) :dependencies (list "clean"))
```

#### requires

Adds a dependency to an existing task

```lisp
(requires :task "dist" :the-dependency "add-version")
```

#### include

Loads an external dependency using quickload.

```lisp
(requires :alexandria)
```

#### plugin

Applies a plugin to the current project.

```lisp
(plugin #'lisp-project)
```