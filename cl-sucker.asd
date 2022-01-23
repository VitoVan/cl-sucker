#+sbcl (sb-ext:unlock-package :sb-ext)
#+sbcl
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c)
                   :executable t
                   #+sb-core-compression :compression
                   #+sb-core-compression 9))

(asdf:defsystem #:cl-sucker
  :description "Self-contained single binary creator, for suckers."
  :author "Vito Van"
  :version 41.9.0
  :license "WTFPL"
  :serial t
  :depends-on (#:unix-opts
               #:cl-fad
               #-win32 #:osicat)
  :components ((:file "package")
               (:file "ordinary-sucker")
               (:file "cl-sucker"))
  :build-operation "program-op"
  :build-pathname "sucker"
  :entry-point "cl-sucker:entry")
