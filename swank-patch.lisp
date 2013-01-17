;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8;  -*-
;;;; Copyright (C) 2012 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.

;;;; This patch introduces into swank an around hook
;;;; invoked every time when swank evaluates lisp
;;;; code sent from emacs. This requires a tiny
;;;; change in the swank::eval-for-emacs function,
;;;; so this patch redefines this functoin.

(in-package :swank)


;;;; Around Hooks
;;;
;;; Around hook is a function with at least one argument NEXT-FN
;;; and possible other arguments.
;;; If the hook calls (FUNCALL NEXT-FN OTHER ARGUMENTS) it invokes
;;; either the next hook in the hook chain,
;;; or the original body executed within the around hooks
;;; in case the current hook is the last one.

(defun call-within-hooks (around-hooks fn &rest arguments)
  (if around-hooks
      (apply (car around-hooks)
             (lambda ()
               (call-within-hooks (cdr around-hooks) fn))
             arguments)
      (apply fn arguments)))

(defmacro within-hooks ((around-hooks) &body body)
  `(call-within-hooks ,around-hooks (lambda () ,@body)))

#| Example of how around hooks work

(flet ((angle-wrap (next-fn)
         (princ "<") (funcall next-fn) (princ ">"))
       (square-wrap (next-fn)
         (princ "[") (funcall next-fn) (princ "]")))

  (within-hooks ((list #'angle-wrap))
    (princ "hello"))
  ;; printed <hello>

  (within-hooks ((list #'angle-wrap #'square-wrap))
    (princ "hello"))
  ;; printed <[hello]>

  (within-hooks ((list #'square-wrap #'angle-wrap))
    (princ "hello"))
  ;; printed [<hello>]
  )

|#

(defvar *around-eval-for-emacs-hook* '()
  "Hook run (with empty argument list) by EVAL-FOR-EMACS around the expression received with an RPC.")


;;;; Patched eval-for-emacs
;;;
;;; The only change we did comparing to the original eval-for-emacs
;;; is wrapping of (EVAL FORM) into (WITHIN-HOOKS (*AROUND-EVAL-FOR-EMACS-HOOK*) ...)
;;;
;;; The code is based on the eval-for-emacs version from slime CVS of 2012-01-16.
;;; If slime changes this function, we could just copy/paste it again,
;;; and again wrap (EVAL FORM) into our hook.

(defun eval-for-emacs (form buffer-package id)
  "Bind *BUFFER-PACKAGE* to BUFFER-PACKAGE and evaluate FORM.
Return the result to the continuation ID.
Errors are trapped and invoke our debugger."
  (let (ok result condition)
    (unwind-protect
         (let ((*buffer-package* (guess-buffer-package buffer-package))
               (*buffer-readtable* (guess-buffer-readtable buffer-package))
               (*pending-continuations* (cons id *pending-continuations*)))
           (check-type *buffer-package* package)
           (check-type *buffer-readtable* readtable)
           ;; APPLY would be cleaner than EVAL. 
           ;; (setq result (apply (car form) (cdr form)))
           (handler-bind ((t (lambda (c) (setf condition c))))
             (setq result (with-slime-interrupts (within-hooks (*around-eval-for-emacs-hook*)
                                                   (eval form)))))
           (run-hook *pre-reply-hook*)
           (setq ok t))
      (send-to-emacs `(:return ,(current-thread)
                               ,(if ok
                                    `(:ok ,result)
                                    `(:abort ,(prin1-to-string condition)))
                               ,id)))))
