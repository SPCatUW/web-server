;; Printing cooperative web server system definition

(defpackage :pcws-system-definition
  (:nicknames :pcws-sysdef)
  (:use :common-lisp :asdf))

(in-package :pcws-sysdef)

;; Manually load dependencies
(ql:quickload :hunchentoot)
(ql:quickload :cl-who)

(defsystem :cooperative-web-server
  :description "Web server for Student Printing Cooperative at the University of Washington"
  :version "0.1"
  :author "SPC@UW"
  :license "BSD 3-Clause"  ;; check!
  :components ((:file "package")
               (:file "base" :depends-on ("package"))
               (:file "user-accounts" :depends-on ("base" "package"))
               (:file "upload-download" :depends-on ("package" "base" "user-accounts"))))
