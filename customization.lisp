;;;; customization.lisp
;;;; Powerlisp customization API
;;;; Copyright © 2018-2020 Lucas Vieira <lucasvieira@protonmail.com>
;;;;
;;;; Licensed under the MIT License.
;;;; See LICENSE for details.

(in-package :powerlisp)

;;; The following are underlying procedures used when building your
;;; own configuration files.

(defun powerlisp-add-favorite (atom url)
  "Add a single favorite website to favorites list."
  (setf *favorite-websites*
	(append *favorite-websites* (list (cons atom url)))))

(defun powerlisp-add-multi-favorites (favorites-list)
  "Adds many favorites to the favorites list. Format of the list must follow the
format for the favorites list. Using this function instead of
powerlisp-add-favorite is recommended when you have many websites."
  (setf *favorite-websites*
	(append *favorite-websites* favorites-list)))

(defun powerlisp-add-search-engine (atom query-parts)
    "Add a single search engine to search engines list.
The query-parts parameter must be a list of query components, with the first one
coming before the query value, and the rest coming after the query value. These
strings are concatenated in this order."
    (setf *search-engines*
	  (append *search-engines* (list (list atom query-parts)))))

(defun powerlisp-add-multi-search-engines (engines-list)
    "Adds many search engines  to the search engines list.
Format of the list must follow the format for the search engines list. Using
this function instead of powerlisp-add-search-engine is recommended when you
have many engines."
    (setf *search-engines*
	  (append *search-engines* engines-list)))

(defun powerlisp-add-command (command callback)
  "Adds a command to Powerlisp.
command is the command atom, callback must be a zero-arguments function."
  (and (functionp callback)
       (setf *subcommands*
	     (append (list (cons command callback))
		     *subcommands*))))

(defun powerlisp-add-multi-commands (commands-list)
  "Adds many commands to Powerlisp at once.
The list of commands must be a list comprised of consed atoms + procedures. It
is important to maintain this structure in order for this to work."
  (and (every (lambda (entry) (functionp (cdr entry)))
	      commands-list)
       (setf *subcommands*
	     (append commands-list *subcommands*))))


;;; The following procedures and macros may have unresolved atoms.
;;; This is due to the fact that some customization needs to be made
;;; before we define those missing atoms, since they depend on later
;;; procedures.

(defun powerlisp-spawn-menu (prompt alist)
  "Spawns an input menu with the given prompt, and offers an alist of values.
This function yields two values: an atom equivalent to the user input and, if
the option selected is valid, yields the associated value as well; if not,
yields nil instead."
  (multiple-value-bind (atom assoc-value raw-input)
      (pl-match-output (pl-request-input prompt
				   (pl-options-to-list alist))
		    alist)
    (values atom assoc-value raw-input)))

(defmacro powerlisp-request-user-input (&optional (prompt "input?"))
  "Spawns an input menu with no options. The value returned is a plain string
containing what the user typed. One can customize the prompt by feeding it to
this function."
  `(pl-request-input ,prompt nil))

(defmacro with-powerlisp-user-input ((&optional (prompt "input?")) &body body)
  "Calls the user input, optionally accepting an input prompt. If there is valid
input, the BODY is evaluated, and said input string is bound to `input`."
  `(let ((input (powerlisp-request-user-input ,prompt)))
     (when input ,@body)))

(defmacro with-powerlisp-options-menu ((prompt alist) &body body)
  "Calls an options menu using an alist. If the input matches any of the values
on the alist, the input is bound as an atom to `option`, and the associated
value is bound to `assoc-value`. The body is then executed."
  `(multiple-value-bind (option assoc-value)
       (powerlisp-spawn-menu ,prompt ,alist)
     (when assoc-value ,@body)))

(defmacro with-powerlisp-menu ((prompt alist) &body body)
  "Calls an options menu using an alist, but evaluates the BODY regardless of a
matched input. This macro provides bindings to an `option` atom, and to the
associated value, if existing, as `assoc-value`. It also provides access to the
input string from the user, assigned to `raw-input`."
  `(multiple-value-bind (option assoc-value raw-input)
       (powerlisp-spawn-menu ,prompt ,alist)
     (unless (or (null raw-input)
		 (string= (string-trim '(#\Space #\Tab) raw-input) ""))
       ,@body)))

(defun powerlisp-notify (text &optional (title "Powerlisp"))
  "Sends a notification to the desktop. One can optionally setup the
notification title."
  (pl-send-notification title text))

(defun powerlisp-call-browser (website)
  "Effectively calls the browser with the given website as argument. If the
incognito mode flag is active, the incognito flag is appended to the call."
  (if *incognito-mode*
      (pl-call-external *browser-command* *browser-incognito-flag* website)
      (pl-call-external *browser-command* website)))

(defun powerlisp-call-external (program-path &rest arguments)
  "Calls an external command and does not wait for the process to
finish. `program-path` needs to be an absolute path to the binary.
`arguments` is a list of strings, where each string is an argument.
The arguments need to be isolated, with no whitespace inbetween."
  (apply #'pl-call-external (cons program-path arguments)))

