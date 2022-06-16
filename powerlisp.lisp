;;;; Powerlisp v2.0
;;;; powerlisp.lisp
;;;; A useful utility for Unix powerusers.
;;;; Copyright © 2018-2020 Lucas Vieira <lucasvieira@protonmail.com>
;;;;
;;;; Licensed under the MIT License.
;;;; Permission is hereby granted, free of charge, to any person obtaining a
;;;; copy of this software and associated documentation files (the "Software"),
;;;; to deal in the Software without restriction, including without limitation
;;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;;; and/or sell copies of the Software, and to permit persons to whom the
;;;; Software is furnished to do so, subject to the following conditions:
;;;; 
;;;; The above copyright notice and this permission notice shall be included in
;;;; all copies or substantial portions of the Software.
;;;; 
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;;; DEALINGS IN THE SOFTWARE.

;;; ======================================================================== ;;;
;;;                                TODO-LIST                                 ;;;
;;; ======================================================================== ;;;
;;
;; - Divide project into many proper Lisp files
;; - Create .asd file which defines the current system
;; - Replace SBCL-only functions with UIOP (see run-program and launch-program)
;; - Remove interning of symbols completely, make string-only comparisons
;; - Create a roswell/powerlisp.ros script, adding Roswell support
;; - Add a better customization API


;;; ======================================================================== ;;;
;;;                     Default software and settings                        ;;;
;;; ======================================================================== ;;;

;;; Below are contained global variables which determine the default location of
;;; software on your computer, as well as theme settings for your desired launch
;;; or menu application.
;;; You may want to customize these, or redefine them by hand on your
;;; configuration file.

(defparameter *browser-command*        "/usr/bin/firefox")
(defparameter *browser-incognito-flag* "--private-window")
(defparameter *notify-command*         "/usr/bin/notify-send")
(defparameter *launcher-prompt*        "Powerlisp")
(defparameter *launcher-command*       "/usr/bin/dmenu")
(defparameter *launcher-on-bottom*     t)
(defparameter *launcher-rofi-emulate-dmenu* nil)

;; Default theme is black-and-white.
(defparameter *launcher-font*    "xft:Terminus:size=8")
(defparameter *launcher-bg*      "#000000")
(defparameter *launcher-fg*      "#ffffff")
(defparameter *launcher-sel-bg*  "#ffffff")
(defparameter *launcher-sel-fg*  "#000000")

(defparameter *default-search-engine* 'duckduckgo)

;; Below are contained some default search engines.
;; You should not need to change this by hand; instead, you may use the provided
;; API for changing Powerlisp's commands (see in the sections below).

(defparameter *search-engines*
  '((duckduckgo    ("https://duckduckgo.com/?q="))
    (startpage     ("https://startpage.com/do/search?language=english&cat=web&query="))
    (hooktube      ("https://hooktube.com/results?search_query="))))


;; The following are favorite websites.
;; It should be a list of dotted pairs but, for now, they'll be empty.
(defparameter *favorite-websites* nil)

;; Those subcommands take precedence in order, over the favorite websites,
;; and are always displayed first
(defparameter *subcommands* nil)

;; This variable is kind of a hack to allow a loopback effect on the main menu.
;; Set it to t anytime you want the main menu to be shown after a selection.
(defparameter *rerun-main-menu* nil)

;; This variable controls whether you want to use incognito mode or not.
(defparameter *incognito-mode* nil)

;;; ======================================================================== ;;;
;;;                            Customization API                             ;;;
;;; ======================================================================== ;;;

;;; The following procedures should be used when building your own configuration
;;; files.

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


;;; ======================================================================== ;;;
;;;                         Loading User Configuration                       ;;;
;;; ======================================================================== ;;;

;;; The following are magic for evaluating the user configuration.
(defmacro pl-probe-and-load (filename)
  `(when (probe-file ,filename)
     (load ,filename)))

(pl-probe-and-load "~/.powerlisp")
(pl-probe-and-load "~/.config/powerlisp.lisp")


;;; ======================================================================== ;;;
;;;                           Extra Configuration                            ;;;
;;; ======================================================================== ;;;

;;; With user bindings loaded, we can now define vital procedures, macros and
;;; variables.

(defparameter *launcher-params*
  (list "-fn" *launcher-font*
	"-nb" *launcher-bg*
	"-nf" *launcher-fg*
	"-sb" *launcher-sel-bg*
	"-sf" *launcher-sel-fg*
	"-l" "0"))

(when *launcher-on-bottom*
  (setf *launcher-params*
	(cons "-b" *launcher-params*)))

;; Use rofi flag for emulating dmenu, and also
;; remove the one-line flags
(when *launcher-rofi-emulate-dmenu*
  (setf *launcher-params*
	(cons "-dmenu"
	      (butlast (butlast *launcher-params*)))))


;;; ------------------------------------------------------------------------ ;;;
;;;                    External command macros and procedures                ;;;
;;; ------------------------------------------------------------------------ ;;;

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


;;; ------------------------------------------------------------------------ ;;;
;;;                               Input processing                           ;;;
;;; ------------------------------------------------------------------------ ;;;

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
				     

;;; ======================================================================== ;;;
;;;                               Common Tools                               ;;;
;;; ======================================================================== ;;;

;;; Below are defined common tools, such as main menus and whatnot, for a
;;; default Powerlisp configuration.

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



;;; ------------------------------------------------------------------------ ;;;
;;;                               Entry point                                ;;;
;;; ------------------------------------------------------------------------ ;;;

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
