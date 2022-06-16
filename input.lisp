;;;; input.lisp
;;;; Input processing
;;;; Copyright © 2018-2020 Lucas Vieira <lucasvieira@protonmail.com>
;;;;
;;;; Licensed under the MIT License.
;;;; See LICENSE for details.

(in-package :powerlisp)

(defun pl-atom-convert (output-string)
  (intern (string-trim " " (string-upcase output-string))))

(defun pl-match-output (output-string options)
  (let ((input-atom (pl-atom-convert output-string)))
    (values input-atom
	    (cdr (assoc input-atom options))
	    output-string)))

(defmacro pl-with-search-query-format (query-format &body body)
  `(let ((query-begin (caadr ,query-format))
	 (query-rest  (cdadr ,query-format)))
     ,@body))

(defmacro pl-build-query-string (query-begin query query-rest)
  `(concatenate 'string ,query-begin ,query
		(apply #'concatenate 'string ,query-rest)))

(defun pl-build-search-query (query engine)
  (let ((engine-query-format (assoc engine *search-engines*)))
    (if (null engine-query-format)
	(pl-send-notification "Powerlisp: Error"
			      (format nil "No search engine \"~a\"" engine))
        (pl-with-search-query-format engine-query-format
				     (pl-build-query-string query-begin
							    query
							    query-rest)))))

(defun pl-options-to-list (options)
  (loop for opt in options collect (car opt)))

(defun pl-request-command (command-atom)
  (let ((function (cdr (assoc command-atom *subcommands*))))
    (when (functionp function) (funcall function))))
