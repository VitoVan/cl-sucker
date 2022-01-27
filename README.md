# cl-sucker

Self-contained single binary creator, for suckers.

# How to use?

```bash
sucker <input-directory> <executable-file>

Usages:

    lnx-sucker ./supergame/ ./supergame/bin/run

    mac-sucker ./supergame/ ./supergame/bin/run

    win-sucker.exe ./somevirus/ ./somevirus/diddle.exe
```

You will get a file named `puker` in the current directory, send that file to your users, then they will be happy.

# Where to download?

# How it works?

# Is it free?

# How to build?

```bash
sbcl --disable-debugger \
     --eval "(ql:quickload 'cl-sucker)" \
     --eval "(asdf:make :cl-sucker)"
```
