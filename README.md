# ![Icon](sucker.png) cl-sucker
[![CI](https://github.com/VitoVan/cl-sucker/actions/workflows/main.yml/badge.svg)](https://github.com/VitoVan/cl-sucker/actions/workflows/main.yml) [![platform support](https://img.shields.io/badge/platform-Linux%20%7C%20macOS%20%7C%20Windows-blue.svg)](https://github.com/VitoVan/cl-sucker/releases/latest)

Sucker's single file binary creator.

## How to use?

```bash
sucker <include-directory> <executable-file>

Usages:

    lnx-sucker ./supergame/ ./supergame/bin/run

    mac-sucker ./supergame/ ./supergame/bin/run

    win-sucker.exe ./somevirus/ ./somevirus/diddle.exe
```

You will get a file named `puker` in the current directory, send that file to your users, then they will be happy.

## Where to download?

https://github.com/VitoVan/cl-sucker/releases/latest

The higher number suffix, the higher suckabilities and much more suckfulness.

## How it works?

1. Suck

   Loads everything in the `include-directory` recursively into memory, and dump itself as an executable lisp image.

2. Puke

   Writes everything loaded to the local disk, and execute the `executable-file` set when sucking.

## Is it free?

You know, this cl-sucker mechanism sucks, so you just DO WHAT THE FUCK YOU WANT TO.

This program is distributed WITHOUT ANY WARRANTY, you really SHOULD NOT use it unless you can't find any other better alternatives.

## Will it fuck up my users' computer?

It depends on you.

The only shits puked out by cl-sucker is located at:

```lisp
(defparameter *cl-sucker-dir*
  #+darwin "~/.local/rekcus/cache/"
  #+linux "~/.local/share/rekcus/cache/"
  #+win32 "~/AppData/Local/rekcus/cache/")
```

## The generated executable is sooooooo big!

That should be called 'Super Cool': https://youtu.be/M7vkau14LDI?t=361

## Heap exhausted?

That means you are sucking to many files, please try the 1024 or higher version, they got higher suckabilities, and much more suckfulness.

## Advanced usage?

You can write your own hooks:

1. after-suck
2. after-puke

Check [ordinary-sucker](https://github.com/VitoVan/cl-sucker/blob/main/ordinary-sucker.lisp) for more details.

Put your code in a file called `holy-sucker.lisp` in the same directory with `sucker`, and it will be loaded automatically.

There are also two ENVs you can use:

```bash
# VERBOSE MODE, default: True, set it to FALSE to disable verbose mode
CL_SUCKER_GIBBERISH=NIL
# HOLY FILE location, default: holy-sucker.lisp, set it to any file you want
CL_SUCKER_HOLYFILE=NIL
```

## Supreme usage?

You do not actually need to download the binary sucker, write your own `holy-sucker.lisp`, and in the same directory:

```bash
sbcl --eval "(ql:quickload 'your-awesome-shit)" \
     --eval "(ql:quickload 'cl-sucker)" \
     --eval "(cl-sucker:entry)"
```

## How to build?

```bash
git clone https://github.com/VitoVan/cl-sucker.git ~/quicklisp/local-projects/cl-sucker

sbcl --disable-debugger \
     --eval "(ql:quickload 'cl-sucker)" \
     --eval "(asdf:make :cl-sucker)"

file ~/quicklisp/local-projects/cl-sucker/sucker
```

[![Lisp Caution](http://www.lisperati.com/lisplogo_warning2_256.png)](http://www.lisperati.com/logo.html)
