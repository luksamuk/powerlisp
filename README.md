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
$ sbcl --script /path/to/powerlisp.lisp
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

### Planned
- Customizable overall programs (browser, launcher, etc)
- Smarter tool for building submenus inside commands
