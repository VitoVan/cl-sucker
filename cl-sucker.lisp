(in-package #:cl-sucker)

(defparameter *cl-sucker-files* nil)

;; use `rekcus` instead of `sucker`
;; let's take care of those civilized ones
(defparameter *cl-sucker-dir*
  #+darwin "~/.local/rekcus/cache/"
  #+linux "~/.local/share/rekcus/cache/"
  #+win32 "~/AppData/Local/rekcus/cache/")

(defparameter *cl-sucker-gibberish* t)
(defparameter *cl-sucker-holyfile* (or (uiop:getenv "CL_SUCKER_HOLYFILE") #p"./holy-sucker.lisp"))
(defparameter *cl-sucker-ascii* "
  ██████  █    ██  ▄████▄   ██ ▄█▀▓█████  ██▀███  
▒██    ▒  ██  ▓██▒▒██▀ ▀█   ██▄█▒ ▓█   ▀ ▓██ ▒ ██▒
░ ▓██▄   ▓██  ▒██░▒▓█    ▄ ▓███▄░ ▒███   ▓██ ░▄█ ▒
  ▒   ██▒▓▓█  ░██░▒▓▓▄ ▄██▒▓██ █▄ ▒▓█  ▄ ▒██▀▀█▄  
▒██████▒▒▒▒█████▓ ▒ ▓███▀ ░▒██▒ █▄░▒████▒░██▓ ▒██▒
▒ ▒▓▒ ▒ ░░▒▓▒ ▒ ▒ ░ ░▒ ▒  ░▒ ▒▒ ▓▒░░ ▒░ ░░ ▒▓ ░▒▓░
░ ░▒  ░ ░░░▒░ ░ ░   ░  ▒   ░ ░▒ ▒░ ░ ░  ░  ░▒ ░ ▒░
░  ░  ░   ░░░ ░ ░ ░        ░ ░░ ░    ░     ░░   ░ 
      ░     ░     ░ ░      ░  ░      ░  ░   ░     
                  ░                               
")

(defun file-to-byte (path)
  (with-open-file (stream path :element-type '(unsigned-byte 8))
    (let ((data nil))
      (loop
        for x = (read-byte stream nil nil)
        while (not (equal nil x))
        do (push x data))
      data)))

(defun byte-to-file (bytes path)
  (with-open-file (stream path :direction :output :element-type '(unsigned-byte 8))
    (loop for x in (reverse bytes)
          do (write-byte x stream))))

(defun init-system ()
  (format t "~A~%cl-sucker version: ~A~%~%"
          *cl-sucker-ascii*
          (slot-value (asdf:find-system "cl-sucker") 'asdf:version))
  ;; handle ENV
  (let ((gibberish (uiop:getenv "CL_SUCKER_GIBBERISH"))
        (holyfile (uiop:getenv "CL_SUCKER_HOLYFILE"))
        (args (uiop:command-line-arguments)))
    (format t "INITIALISING CL-SUCKER SYSTEM ...~%")
    (format t "ARGs: ~A~%" args)
    (format t "ENVs: CL_SUCKER_GIBBERISH=~A, CL_SUCKER_HOLYFILE=~A~%" gibberish holyfile)
    (when (and gibberish
               (or
                (string= (string-upcase gibberish) "FALSE")
                (string= (string-upcase gibberish) "-1")
                (string= (string-upcase gibberish) "0")))
      (format t "INIT: SWITCHING OFF GIBBERISH (CL_SUCKER_GIBBERISH = ~A)~%" gibberish)
      (setf *cl-sucker-gibberish* nil))
    (when holyfile
      (format t "INIT: SETTING HOLY FILE (CL_SUCKER_HOLYFILE = ~A)~%" holyfile)
      (setf *cl-sucker-holyfile* holyfile)))
  ;; handle command line args
  (let* ((args (uiop:command-line-arguments)))
    (if args
        ;; if there exists args, then it's ordinary sucker
        (progn
          (when (or (member "-h" args :test #'string-equal) (not (= (length args) 2)))
            (show-help-and-quit))
          (let ((input-dir (first args))
                (exec-file (second args)))
            (setf *cl-sucker-input-dir* input-dir)
            (setf *cl-sucker-exec-file* exec-file)))
        ;; if not args provided, then it's holy sucker
        ;; try loading sucker-config.lisp
        (if (probe-file *cl-sucker-holyfile*)
            (progn
              (msg t "LOADING HOLYFILE: ~A" *cl-sucker-holyfile*)
              (load *cl-sucker-holyfile*))
            (progn
              (msg t "HOLYFILE FAILED TO LOAD: ~A" *cl-sucker-holyfile*)
              (show-help-and-quit))))))

(defun msg (destination control-string &rest args)
  (when *cl-sucker-gibberish*
    (apply #'format destination control-string args)))

(defun suck (&optional (dir *cl-sucker-input-dir*))
  (cl-fad:walk-directory
   dir
   (lambda (x)
     (msg t "SUCKING: ~A~%" x)
     (push (cons (enough-namestring x)
                 (unless (cl-fad:directory-pathname-p x)
                   (file-to-byte x)))
           *cl-sucker-files*))
   :directories t)
  (after-suck))

(defun puke ()
  (ensure-directories-exist *cl-sucker-dir*)
  (msg t "PUKE INTO: ~A~%" *cl-sucker-dir*)
  (loop for x in *cl-sucker-files*
        do (let ((filepath (merge-pathnames (car x) *cl-sucker-dir*)))
             (if (probe-file filepath)
                 (msg t "SKIP: ~A~%" filepath)
                 (progn
                   (msg t "PUKING: ~A~%" filepath)
                   (if (cl-fad:directory-pathname-p filepath)
                     (ensure-directories-exist filepath)
                     (byte-to-file (cdr x) filepath))))))
  (after-puke))

(defun exec (&optional (cmd *cl-sucker-exec-file*))
  (let ((cmd-path (uiop:native-namestring (merge-pathnames cmd *cl-sucker-dir*))))
    (msg t "EXECUTING (CHMOD +X): ~A~%" cmd-path)
    #-win32 (osicat-posix:chmod cmd-path (logior nix:s-iread nix:s-iwrite nix:s-iexec))
    (msg t "EXECUTING (RUN-PROGRAM): ~A~%" cmd-path)
    (uiop:run-program cmd-path :output t)))

(defun show-help-and-quit ()
  (format t
          "This program is free software. It comes without any warranty, to
the extent permitted by applicable law. You can redistribute it
and/or modify it under the terms of the Do What The Fuck You Want
To Public License, Version 2, as published by Sam Hocevar. See
http://www.wtfpl.net/ for more details.
~A
cl-sucker version: ~A

sucker <input-directory> <executable-file>

Usages:

    sucker ./supergame/ ./supergame/bin/run

    sucker.exe ./somevirus/ ./somevirus/diddle.exe

You will get a file named `puker` in the current directory.

For more advanced usages (e.g. holy-sucker.lisp),
check: https://github.com/VitoVan/cl-sucker

" *cl-sucker-ascii* (slot-value (asdf:find-system "cl-sucker") 'asdf:version))
  (uiop:quit))

(defun entry ()
  (init-system)
  (suck)
  (uiop:quit))
