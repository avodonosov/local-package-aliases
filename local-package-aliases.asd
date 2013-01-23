;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8;  -*-
;;; Copyright (C) 2013 Anton Vodonosov (avodonosov@yandex.ru)
;;; See LICENSE for details.

(asdf:defsystem #:local-package-aliases
  :description "Allows to define inside of a package aliases to refer other packages.
Provides a reader macro $ to use the aliases (active only in packages having
alias mapping, and does not affect other code).
Portable."
  :license "MIT"
  :author "Anton Vodonosov <avodonosov@yandex.ru>"
  :version "0.0.1"
  :serial t
  :components ((:file "local-package-aliases")))
