;;;; macros-procs.lisp
;;;; External command macros and procedures
;;;; Copyright © 2018-2020 Lucas Vieira <lucasvieira@protonmail.com>
;;;;
;;;; Licensed under the MIT License.
;;;; See LICENSE for details.

(in-package :powerlisp)

(defun pl-build-command (command-parts &optional (query nil))
  (concatenate 'string
	       (car command-parts)
	       query
	       (apply #'concatenate 'string (cdr command-parts))))

(defun pl-send-notification (&rest text)
  #+SBCL
  (sb-ext:run-program *notify-command* text))

(defmacro pl-spawn-process (command input-params yields-input)
  #+SBCL
  `(sb-ext:run-program ,command ,input-params :wait nil
		       ,@(when yields-input
			  '(:input :stream :output :stream))))

(defmacro pl-with-spawned-process ((command input-params yields-input) &body body)
  #+SBCL
  `(let* ((process (pl-spawn-process ,command ,input-params ,yields-input))
	  (process-input (sb-ext:process-input process))
	  (process-output (sb-ext:process-output process)))
     ,@body))

(defun pl-request-input (prompt options)
  (pl-with-spawned-process
   (*launcher-command* (append (list "-p" prompt) *launcher-params*) t)
   ;; Apply input options
   (let ((input-options (mapcar (lambda (x) (string-downcase (format nil "~a~%" x)))
				options)))
     (format process-input "~a" (apply #'concatenate 'string input-options))
     (finish-output process-input)
     (close process-input))
   ;; Await process; dump what we read
   #+SBCL
   (sb-ext:process-wait process)
   (when (listen process-output)
     (read-line process-output))))

(defun pl-call-external (program-path &rest arguments)
  (when (and (stringp program-path))
    (pl-spawn-process program-path arguments nil)))

