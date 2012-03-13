(in-package :cl-user)

(defpackage :mizar-parser
  (:use :cl :cl-ppcre :alexandria :hunchentoot :cl-who :hunchentoot-utils)
  (:import-from :cl-fad :delete-directory-and-files
		        :pathname-as-directory
			:file-exists-p
			:directory-exists-p))

;; early cl-who configuration of attribute delimit character
(setf cl-who:*attribute-quote-char* #\")
