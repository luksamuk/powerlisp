;;;; powerlisp.lisp
;;;; A useful utility for Linux powerusers.
;;;; Copyright © 2018 Lucas Vieira <lucasvieira@lisp.com.br>
;;;;
;;;; Licensed under the MIT License.
;;;; Permission is hereby granted, free of charge, to any person obtaining a copy of
;;;; this software and associated documentation files (the "Software"), to deal in
;;;; the Software without restriction, including without limitation the rights to
;;;; use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
;;;; the Software, and to permit persons to whom the Software is furnished to do so,
;;;; subject to the following conditions:
;;;; 
;;;; The above copyright notice and this permission notice shall be included in all
;;;; copies or substantial portions of the Software.
;;;; 
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
;;;; FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
;;;; COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
;;;; IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;;;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.    


;;; Important stuff:
;;; This script assumes that you are using certain default programs and settings,
;;; which are the ones I am using:
;;; - SBCL as Common Lisp implementation
;;; - Font: Fixed 9
;;; - Black-and-white theme
;;; - DuckDuckGo as default search engine
;;; - dmenu as launcher
;;; - Firefox as browser
;;; Do not forget to change those to your liking. Hack this file as much as you can.

;;; Run this file using:
;;; $ sbcl --script powerlisp.lisp
;;; Notice that I try not to rely on Quicklisp for anything. This is intentional
;;; so that the shortcut launches faster.


;; ============================================
;; Look and programs customization

(defparameter *input-font*   "xft:Fixed:size=9")
(defparameter *input-bg*     "#FFFFFF")
(defparameter *input-fg*     "#000000")
(defparameter *input-sel-bg* "#000000")
(defparameter *input-sel-fg* "#FFFFFF")
(defparameter *default-search-engine* 'duckduckgo)
(defparameter *browser-command* "/usr/bin/firefox")
(defparameter *notify-command* "/usr/bin/notify-send")
(defparameter *input-command*
  (list "/usr/bin/dmenu"
	"-b"
	"-fn" *input-font*
	"-nb" *input-bg*
	"-nf" *input-fg*
	"-sb" *input-sel-bg*
	"-sf" *input-sel-fg*
	"-l" "0"))
     
				
;; ============================================
;; Websites and search engines

(defparameter *favorite-websites*
  '((reddit . "https://reddit.com")
    (twitter . "https://twitter.com")
    (mastodon . "https://mastodon.gamedev.place")
    (netflix . "https://netflix.com")
    (hooktube . "https://hooktube.com")
    (protonmail . "https://mail.protonmail.com/login")
    (github . "https://github.com")
    (linkedin . "https://linkedin.com")
    (hackernews . "https://news.ycombinator.com")
    (slashdot . "https://slashdot.org")
    (instagram . "https://instagram.com")
    (whatsapp . "https://web.whatsapp.com")
    (cplusplus . "http://cplusplus.com")))

(defparameter *search-engines*
  '((duckduckgo ("https://duckduckgo.com/?q="))
    (hooktube ("https://hooktube.com/results?search_query="))
    (twitter ("https://twitter.com/search?q="))
    (wikipedia ("https://en.wikipedia.org/w/index.php?search="
		"&title=Special%3ASearch"))
    (github ("https://github.com/search?utf8=%E2%9C%93&q="
	     "&type="))
    (wolfram ("https://www.wolframalpha.com/input/?i="))
    (cplusplus ("http://cplusplus.com/search.do?q="))))


;; ============================================
;; Command calling helpers

(defun *build-command* (command-parts &optional (query nil))
  "Build a command using its first part, its next part
and a query, if it is a search engine. If said query exists,
it is sandwiched between the first and last parts."
  (concatenate 'string
	       (car command-parts)
	       query
	       (apply #'concatenate 'string (cdr command-parts))))

(defun send-notification (&rest text)
  "Sends a notification to the desktop."
  #+SBCL (sb-ext:run-program *notify-command* text))

(defun request-input (prompt options)
  "Requests input using your input method. You may provide
selection options. Yields the user input as a string."
  #+SBCL
  (let* ((process (sb-ext:run-program (car *input-command*)
				      (append (list "-p" prompt)
					      (cdr *input-command*))
				      :input :stream
				      :output :stream
				      :wait nil))
	 (process-input  (sb-ext:process-input process))
	 (process-output (sb-ext:process-output process))
	 (input-options (mapcar (lambda (x)
				  (string-downcase (format nil "~a~%" x)))
				options)))
    ;; Submit list of elements to process input
    (format process-input "~a" (apply #'concatenate 'string input-options))
    (finish-output process-input)
    (close process-input)
    ;; Await process end and dump what we read
    (sb-ext:process-wait process)
    (when (listen process-output)
      (read-line process-output))))

(defun call-browser (&rest website)
  "Effectively calls the browser with the given website as argument."
  #+SBCL (sb-ext:run-program *browser-command* website :wait nil))


;; ============================================
;; User input processing

(defun atom-convert (output-string)
  "Convert a user-received string to an atom.
This might need security tweaks..."
  (intern
   (string-trim " " (string-upcase output-string))))
  
(defun match-output (output-string options)
  "Match a string given from the output of a process to a list
of options. Yields both the atom and the value associated with the
string on the referred list of options."
  (let* ((input-atom (atom-convert output-string))
	 (associated-value (cdr (assoc input-atom options))))
    (values input-atom associated-value)))

(defun build-search-query (query engine)
  "Builds a search query for the given search engine. Yields the
prepared URL as a string."
  (let ((engine-query-format (assoc engine *search-engines*)))
    (if (null engine-query-format)
	(send-notification "POWERLISP: ERROR"
			   (format nil "Cannot find search engine \"~a\"" engine))
        (let ((query-begin (caadr engine-query-format))
	      (query-rest  (cdadr engine-query-format)))
	  (concatenate 'string
		       query-begin
		       query
		       (apply #'concatenate 'string query-rest))))))

(defun options-to-list (options)
  "Converts the atoms which are associated with certain URLs in an alist
to a single list of those atoms."
  (loop for opt in options collect (car opt)))

;; ============================================
;; User interface management

(defun request-search ()
  "Prompts the search menu. Asks for the search engine to
be used, and for the query to be searched."
  (let ((command-result
	 (request-input "Search engine?"
			(options-to-list *search-engines*))))
    (multiple-value-bind (engine-atom engine-query-info)
	(match-output command-result *search-engines*)
      (if (null engine-query-info)
	  (when (not (null engine-atom))
	    (send-notification
	     "POWERLISP: ERROR"
	     (format nil "Unknown search engine \"~a\"" engine-atom)))
	  (let ((search-input (request-input "Search target?" nil)))
	    (when (not (null search-input))
	      (send-notification
	       "POWERLISP SEARCH"
	       (format nil "Searching for \"~a\" in ~a..."
		       search-input engine-atom))
	      (call-browser (build-search-query search-input engine-atom))))))))
    

(defun request-from-favorites ()
  "Prompts the favorites menu. Asks for the user to type one of the
favorite websites prompted, or a command (such as search), or even
for text which will be converted to a search query."
  (let ((command-result
	 (request-input "Website, command, plain search?"
			(append '(search)
				(options-to-list *favorite-websites*)))))
    (multiple-value-bind (command-atom command-url)
	(match-output command-result *favorite-websites*)
      (cond ((null command-url)
	     (cond ((eq command-atom 'search)
		    (request-search))
		   (t (when (not (null command-result))
			(send-notification
			 "POWERLISP PLAIN SEARCH"
			 (format nil "Searching for \"~a\"..."
				 command-result))
                        ;; TODO: I want to be able to just type
                        ;; websites on the future.
			(call-browser (build-search-query
				       command-result
				       *default-search-engine*))))))
	    (t (send-notification
		"POWERLISP"
		(format nil "Opening ~a~%(~a)..."
			command-atom
			command-url))
	       (call-browser command-url))))))

(defun run-powerlisp ()
  "Alias for running powerlisp."
  (request-from-favorites))

(run-powerlisp) ;; Magic happens here.
