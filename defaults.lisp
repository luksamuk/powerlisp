;;;; defaults.lisp
;;;; Powerlisp default software and settings definitions.
;;;; Copyright © 2018-2020 Lucas Vieira <lucasvieira@protonmail.com>
;;;;
;;;; Licensed under the MIT License.
;;;; See LICENSE for details.

(in-package :powerlisp)

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

