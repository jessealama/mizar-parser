(in-package :cl-user)

(defpackage :mizar-parser
  (:use :cl :cl-ppcre :com.gigamonkeys.pathnames :alexandria :hunchentoot :cl-who :hunchentoot-utils))

;; early cl-who configuration of attribute delimit character
(setf cl-who:*attribute-quote-char* #\")
