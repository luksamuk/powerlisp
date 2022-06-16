;;;; entry-point.lisp
;;;; Entry point for Powerlisp
;;;; Copyright © 2018-2020 Lucas Vieira <lucasvieira@protonmail.com>
;;;;
;;;; Licensed under the MIT License.
;;;; See LICENSE for details.

(in-package :powerlisp)

(defun run-powerlisp ()
  (loop
     do (setf *rerun-main-menu* nil)
       (with-powerlisp-menu ((format nil "~a~a >> "
				     *launcher-prompt*
				     (if *incognito-mode* " [incognito]" ""))
			     *subcommands*)
	 (if (functionp assoc-value)
	     ;; Check for listing under commands
	     (pl-request-command option)
	     ;; On the other hand, it may be that it is not a command.
	     (progn (powerlisp-notify (format nil "Searching for ~a..." option))
		    (powerlisp-call-browser
		     (pl-build-search-query raw-input
					    *default-search-engine*)))))
     while *rerun-main-menu*))

;; (run-powerlisp)
