(in-package #:cl-sucker)

;; this is not needed in oridnary sucker,
;; but if you're using holy-sucker.lisp,
;; these two must be set
(defparameter *cl-sucker-input-dir* nil)
(defparameter *cl-sucker-exec-file* nil)

(defun after-suck ()
  (msg t "APPLYING DEFAULT AFTER-SUCK HOOK: DUMP~%")
  (setf uiop:*image-entry-point* #'puke)
  (uiop:dump-image
   (merge-pathnames (uiop:getcwd)
                    (format nil "puker~A"
                            #+win32 ".exe"
                            #-win32 ""))
   :executable t
   :compression 9
   #+(and sbcl win32) :application-type
   #+(and sbcl win32) :gui))

(defun after-puke ()
  (msg t "APPLYING DEFAULT AFTER-PUKE HOOK: EXECUTING~%")
  (exec))
