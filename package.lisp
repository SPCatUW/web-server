;; Package definition --- which symbols to use, etc.

(defpackage :printing-coop-web-server
  (:nicknames :pcws)
  (:use :common-lisp :hunchentoot :cl-who)
  (:import-from "COMMON-LISP-USER" "WITH-GENSYM"))
