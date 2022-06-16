;;;; tools.lisp
;;;; Common tools for a default Powerlisp config
;;;; Copyright © 2018-2020 Lucas Vieira <lucasvieira@protonmail.com>
;;;;
;;;; Licensed under the MIT License.
;;;; See LICENSE for details.

(in-package :powerlisp)

;;; ------------------------------------------------------------------------ ;;;
;;;                               Default bindings                           ;;;
;;; ------------------------------------------------------------------------ ;;;


(powerlisp-add-multi-commands
 (list (cons 'favorites ;; List of favorite websites
	     (lambda ()
	       (with-powerlisp-options-menu ("Website?" *favorite-websites*)
		 (powerlisp-notify (format nil "Accessing ~a..." option))
		 (powerlisp-call-browser assoc-value))))
       (cons 'search-engine ;; Changes default search engine
	     (lambda ()
	       (with-powerlisp-options-menu ("Search engine?" *search-engines*)
		 (setf *default-search-engine* option
		       *launcher-prompt* (format nil "~a+~a"
						 *launcher-prompt* option)))
	       (setf *rerun-main-menu* t)))
       (cons 'goto
	     (lambda ()
	       (with-powerlisp-user-input ((format nil "url~a?"
						   (if *incognito-mode*
						       " [incognito]"
						       "")))
		 (powerlisp-notify (format nil "Accessing \"~a\"..." input))
		 (powerlisp-call-browser input))))
       (cons 'incognito ;; Toggles on/off incognito browsing, if supported
	     (lambda ()
	       (setf *rerun-main-menu* t
		     *incognito-mode* (not *incognito-mode*))))))

