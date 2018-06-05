# Powerlisp
Common Lisp tool for automating the access to websites for Linux power-users.

## What is this?

Powerlisp is a simple tool to automate your work with dmenu/rofi, and also allowing you
to navigate through multiple menus.

Using Powerlisp, you can:
- Navigate directly to your favorite websites, in your favorite browser;
- Perform a direct plain search on your favorite search engine;
- Search through programming language documentations in Zeal;
- Customize it and make it yours.

All of this is done by taking advantage of `dmenu`. Needless to say, you will also
want to have `zeal` installed in order to search for documentation.

Notice that I am not documenting it too much because it is basically a hack on my own
system. Given that, I made this tool so it would be just extremely easy to take a peek
at the code and hack it to your own needs. Just because we need some more hacker culture
in the world.

Also included, in the "old" directory, is the first version of this utility, which was a
very ugly solution built using Bash. Just so you can compare it with how good Lisp looks.

## How do I use it?

All you need to do is invoke it using a command line:

```bash
$ sbcl --script /path/to/powerlisp.lisp --no-linedit
```

You might want, however, to precompile the file and generate a .fasl so you can easily access it.

For that, open SBCL with minimal overhead:

```bash
$ cd path/to/powerlisp
$ sbcl --noinform --disable-ldb --lose-on-corruption --end-runtime-options --no-linedit
```

Then compile it using the REPL:

```lisp
* (compile-file "powerlisp.lisp")
```

This will generate a `powerlisp.fasl` file, which you can then load as script using the following command:

```bash
$ sbcl --script /path/to/powerlisp.fasl --no-linedit
```

I built it making heavy use of SBCL's extensions so, unless you send me a PR enabling it
to run on other implementations, SBCL is a must.

It is also supposed to never require anything from Quicklisp, relying only on implementation
extensions and the core lib of the language at most. This is because I use this program
several times per day, and so I need it to open as fast as it can.

Powerlisp also assumes, by default, that you like its black-and-white theme, and that you have
dmenu, Firefox and Zeal installed. It also assumes that you like my favorite websites, search
engines, programming languages, and that you use DuckDuckGo as your default search engine. If
you don't like any or all of these, don't worry; as I just said, go ahead and change this
script, hack it as much as you want. Just remember to not hurt the MIT License included, which
I believe is permissive enough.

## How do I make it mine?

As stated above, Powerlisp is customizable.
You can create a file `~/.powerlisp` or `~/.config/powerlisp.lisp` and call whatever
commands you want to customize the Powerlisp global variables (I'll leave to you to read the
file and find out which are).

You can also use the specific functions for adding new features, as stated in the examples below.

### Quick user API reference

Those are the functions you'll mostly be using for your scripts and for customization.

#### (powerlisp-add-favorite atom url)
Add a single favorite website to favorites list.

#### (powerlisp-add-multi-favorites favorites-list)
Adds many favorites to the favorites list. Format of the list must follow the format for the favorites list.
Using this function instead of `powerlisp-add-favorite` is recommended when you have many websites.<br/>
List format example: `'((site1 . "url1") (site2 . "url2") ...)`

#### (powerlisp-add-search-engine atom query-parts)
Add a single search engine to search engines list.
The query-parts parameter must be a list of query components, with the first one coming before the query value,
and the rest coming after the query value. These strings are concatenated in this order.

#### (powerlisp-add-multi-search-engines engines-list)
Adds many search engines  to the search engines list.
Format of the list must follow the format for the search engines list. Using this function instead of
`powerlisp-add-search-engine` is recommended when you have many engines.<br/>
List format example: `'((engine1 ("url-pre-query-part" "url-post-query1" ...)) (engine2 ...))`

#### (powerlisp-add-command command callback)
Adds a command to Powerlisp. `command` is the command atom, `callback` must be a zero-arguments function.

#### (powerlisp-add-multi-commands commands-list)
Adds many commands to Powerlisp at once. The list of commands must be a list comprised of consed
atoms + procedures.<br/>
List format example: `(list (cons 'command1 #'procedure1) (cons 'command2 #procedure2) ...)`

#### (powerlisp-spawn-menu prompt alist)
Spawns an input menu with the given prompt, and offers an alist of values. This function yields two values: an
atom equivalent to the user input and, if the option selected is valid, yields the associated value as well;
if not, yields nil instead.<br/>
Alist format example: `'((atom1 . associated-value1) (atom2 . associated-value2) ...)`

#### (powerlisp-request-user-input &optional prompt)
Spawns an input menu with no options. The value returned is a plain string containing what the user typed. One can
customize the prompt by feeding it to this function.

#### (with-powerlisp-options-menu (prompt alist) &body ...)
Calls an options menu using an alist. If the input matches any of the values on the alist, the input is bound as an
atom to `option`, and the associated value is bound to `assoc-value`. The body is then executed.

#### (powerlisp-notify text &optional title)
Sends a notification to the desktop. One can optionally setup the notification title.

#### (powerlisp-call-external program-path &rest arguments)
Calls an external command and does not wait for the process to finish. `program-path` needs to be an absolute
path to the binary. `arguments` is a list of strings, where each string is an argument.
The (optional) arguments need to be isolated, with no whitespace inbetween.

#### (powerlisp-call-browser &rest website)
Calls the default browser with one (or more) websites to open. This command is basically a specialized version of
`powerlisp-call-external`, however there is no need to encapsulate the websites into lists.

### Examples

Below you can see what you may want to add as commands for Powerlisp.
Keep in mind that you can also make extensive use of everything I defined on the script file as well, so
the possibilities are endless.

#### Adding Google Search Engine and making it the default
```lisp
(powerlisp-add-multi-search-engines
 '((google ("https://google.com/search?q="))))
 
(setf *default-search-engine* 'google)
```

or

```lisp
(powerlisp-add-search-engine 'google
  '("https://google.com/search?q="))
  
(setf *default-search-engine* 'google)
```

#### Adding Common Lisp Brasil to the favorites
```lisp
(powerlisp-add-multi-favorites
 '((clbr . "https://lisp.com.br")))
```

or

```lisp
(powerlisp-add-favorite 'clbr "https://lisp.com.br")
```

#### Adding an external command to the actual commands
External commands require absolute paths.
```lisp
(powerlisp-add-command 'terminal
  (lambda ()
    (powerlisp-call-external "/usr/bin/xterm")))
```

or


```lisp
(defun call-terminal ()
  (powerlisp-call-external "/usr/bin/xterm"))

(powerlisp-add-command 'terminal #'call-terminal)
```

#### Creating a submenu for some games

```lisp
;;; Create an alist of games
(defparameter *games*
  '((steam . "/usr/bin/steam")
    (doom  . "/usr/bin/gzdoom")))

;;; Create a function which yields the game menu
(defun request-games ()
  (with-powerlisp-options-menu ("Game?" *games*)
    ;; On valid selection, we get "option" and "assoc-value"
    (powerlisp-notify (format nil "Opening game ~a..."
	                          option))
    (powerlisp-call-external assoc-value)))

;;; Add a Games menu to the favorites menu
(powerlisp-add-command 'games #'request-games)
```

## Why would you use Common Lisp?

There are a few reasons why. First, I wanted to get away from the Bash implementation as fast
as possible, because it was ugly and Bash was just not made for more than a few lines to
automate some operations.

So I had to pick a language which would fill my scripting needs. And that turns out to be...
yes, Common Lisp. Just because it is so underrated, and because people would probably choose
stuff like Python, Ruby, or even JS. It still bothers me that Common Lisp is so forgotten.
It does very, very good stuff, and does it well.

Lastly, I did it because I could. And you should too. Also, learn Common Lisp, you just won't
regret it.

## Features

- Customizable favorite websites
- Customizable search engines
- Customizable commands
- API for building submenus

### Planned
- Customizable overall programs (browser, launcher, etc)
