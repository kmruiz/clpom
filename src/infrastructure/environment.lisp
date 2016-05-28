#|
This file is part of clpom project object model
(c) 2016 Kevin Mas <masruizkevin@gmail.com>
See LICENSE for more information
|#

(in-package :clpom)

(defun env/raw (name)
  (sb-ext:posix-getenv name))

(defun env/string (name)
  (env/raw name))

(defun env/number (name)
  (parse-integer name))

(defun env/list (name)
  (split-sequence:split-sequence "," name))

