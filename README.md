# cl-sucker

[![CI](https://github.com/VitoVan/cl-sucker/actions/workflows/main.yml/badge.svg)](https://github.com/VitoVan/cl-sucker/actions/workflows/main.yml)

Sucker's single file binary creator.

## How to use?

```bash
sucker <input-directory> <executable-file>

Usages:

    lnx-sucker ./supergame/ ./supergame/bin/run

    mac-sucker ./supergame/ ./supergame/bin/run

    win-sucker.exe ./somevirus/ ./somevirus/diddle.exe
```

You will get a file named `puker` in the current directory, send that file to your users, then they will be happy.

## Where to download?

https://github.com/VitoVan/cl-sucker/releases/latest

The higher number subfix, the higher suckabilities and much more suckfulness.

## How it works?

1. Suck

   Loads everything in the `input-directory` recursively into memory, and dump itself as an executable lisp image.

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

## How do I say to my customers, in case they asked about the internal machanism

If your customers are the civilized ones who cannot accept the word *sucker*, then you have at least 3 solutions:

1. tell them to take care of their own shit and fuck off, or
2. tell them you are using the Common Lisp - SUpreme Compress Kit Enhanced Revision
3. tell them you are using the REKCUS packaging system. If they asked about what REKCUS stands for - which they won't since they are civilized person - then go back to the first solution

## The generated executable `puker` is so big!

Oh yeah? So what?

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

## How to build?

```bash
git clone https://github.com/VitoVan/cl-sucker.git ~/quicklisp/local-projects/cl-sucker

sbcl --disable-debugger \
     --eval "(ql:quickload 'cl-sucker)" \
     --eval "(asdf:make :cl-sucker)"

file ~/quicklisp/local-projects/cl-sucker/sucker
```
