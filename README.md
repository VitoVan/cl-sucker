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

[![v41.9.1 CI Build](https://github.com/VitoVan/cl-sucker/actions/workflows/main.yml/badge.svg?branch=v41.9.1)](<https://github.com/VitoVan/cl-sucker/releases/tag/v41.9.0>)

# How it works?

# Is it free?

# How to build?

```bash
sbcl --disable-debugger \
     --eval "(ql:quickload 'cl-sucker)" \
     --eval "(asdf:make :cl-sucker)"
```
