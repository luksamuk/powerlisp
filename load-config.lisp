;;;; load-config.lisp
;;;; Tools for loading Powerlisp configuration
;;;; Copyright © 2018-2020 Lucas Vieira <lucasvieira@protonmail.com>
;;;;
;;;; Licensed under the MIT License.
;;;; See LICENSE for details.

(in-package :powerlisp)

;;; The following are magic for evaluating the user configuration.
(defmacro pl-probe-and-load (filename)
  `(when (probe-file ,filename)
     (load ,filename)))

(pl-probe-and-load "~/.powerlisp")
(pl-probe-and-load "~/.config/powerlisp.lisp")

;;; New config loading
(defun read-config-file (filename)
  (with-open-file (stream filename)
    (let ((*readtable* (copy-readtable))
          (eof-sym (gensym)))
      (set-dispatch-macro-character
       #\# #\.
       (lambda (a b c)
         (declare (ignore a b c))
         (error "Sharp-dot notation is forbidden in config files.")))
      (loop for form = (handler-case (read stream)
                         (error () eof-sym))
         until (eq form eof-sym)
         collect form))))
