
(in-package :cl-user)

(declaim (optimize (compilation-speed 0) (speed 0) (debug 3)))

(defpackage :mizar-asd
  (:use :cl :asdf))

(in-package :mizar-asd)

(defsystem :mizar-parser
  :description "Tools for parsing Mizar texts."
  :long-description ""
  :author "Jesse Alama <jesse.alama@gmail.com>"
  :maintainer "Jesse Alama <jesse.alama@gmail.com>"
  :serial t
  :depends-on ("com.gigamonkeys.pathnames" "trivial-garbage" "hunchentoot" "cl-who")
  :components ((:file "packages")
	       (:file "utils")
	       (:file "mizar")
	       (:file "parse")))
