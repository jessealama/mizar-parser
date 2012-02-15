
(in-package :mizar-parser)

(define-constant +parser-port+
    4387
  :test #'=
  :documentation "The port on which we listen for parsing requests.")

(defclass parser-acceptor (acceptor)
  ())

(defvar *parser-acceptor*
  (make-instance 'parser-acceptor
		 :port +parser-port+)
  "The Hunchentoot acceptor that we use for the parser service.")

(defmethod acceptor-status-message ((acceptor parser-acceptor) http-status-code &key &allow-other-keys)
  (when (>= http-status-code 400)
    ""))

(defgeneric handle (method format strictness)
  (:documentation "Handle the current request, assuming that it was made using the HTTP method METHOD (e.g., GET, HEAD, PUT, OPTIONS, etc)."))

;; Don't emit the Server response header
(defmethod handle :before (method format strictness)
  (declare (ignore method format strictness))
  (setf (header-out "Server") nil))

;; By default, all requests are bad
(defmethod handle (method format strictness)
  (declare (ignore method format strictness))
  (setf (return-code*) +http-bad-request+)
  (setf (content-type*) nil))

;; We don't handle anything requests except those for "/"
(defmethod handle :around ((method symbol) (format string) (strictness string))
  (let ((uri (script-name*)))
    (if (string= uri "/")
	(let ((format (cond ((string= format "text") :text)
			    ((string= format "xml") :xml)
			    ((stringp format) nil)
			    ((null format) :text)))
	      (strictness (cond ((string= strictness "none") :none)
				((string= strictness "weak") :weak)
				((string= strictness "more") :more)
				((stringp strictness) nil)
				((null format) :none))))
	  (if (and format strictness)
	      (handle method format strictness)
	      (progn
		(setf (return-code*) +http-bad-request+)
		(setf (content-type*) nil))))
	(progn
	  (setf (return-code*) +http-not-found+)
	  (setf (content-type*) nil)))))

(defmethod handle ((method (eql :options)) format strictness)
  (declare (ignore format strictness))
  (setf (return-code*) +http-no-content+)
  (setf (content-type*) nil)
  (setf (header-out "Accept") "OPTIONS, HEAD, GET")
  "")

(defmethod handle ((method (eql :head)) format strictness)
  (declare (ignore method))
  (setf (content-type*) nil)
  (setf (return-code*) +http-no-content+)
  (handle :get format strictness))

(defmethod handle :around ((method (eql :get)) format strictness)
  (declare (ignore format strictness))
  (let ((message (raw-post-data :force-text t)))
    (if message
	(call-next-method)
	(setf (return-code*) +http-bad-request+))))

(defmethod handle ((method (eql :get))
		   (format (eql :xml))
		   (strictness (eql :none)))
  (let ((message (raw-post-data :force-text t)))
    (let* ((tempdir (temporary-directory))
	   (article-path (merge-pathnames "article.miz" (format nil "~a/" tempdir))))
      (write-string-into-file message article-path
			      :if-exists :error
			      :if-does-not-exist :create
			      :external-format :ascii)
      (if (accom article-path)
	  (if (wsmparser article-path)
	      (let ((wsx-path (merge-pathnames "article.wsx" (format nil "~a/" tempdir))))
		(setf (return-code*) +http-ok+)
		(setf (content-type*) "application/xml")
		(file-as-string wsx-path))
	      (setf (return-code*) +http-bad-request+
		    (content-type*) nil))
	  (setf (return-code*) +http-bad-request+
		(content-type*) nil)))))

(defmethod acceptor-dispatch-request ((acceptor parser-acceptor) request)
  (let ((method (request-method request))
	(format (get-parameter "format" request))
	(strictness (get-parameter "strictness" request)))
    (handle method format strictness)))

(defun launch-parsing-service ()
  "Launch the Mizar parsing service."
  (start *parser-acceptor*))
