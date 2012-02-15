
(in-package :mizar-parser)

(define-constant +parser-port+
    4387
  :test #'=
  :documentation "The port on which we listen for parsing requests.")

(define-constant +biggest-article-length+
    1000000
  :test #'=
  :documentation "The length of the largest article that we accept.")

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
  (let ((message-length-header (header-in* "Content-Length")))
    (if message-length-header
	(let ((message-length (handler-case (parse-integer message-length-header :junk-allowed nil)
				(error () nil))))
	  (if (integerp message-length)
	      (cond ((null message-length)
		     (setf (return-code*) +http-length-required+
			   (content-type*) nil))
		    ((< message-length 0)
		     (setf (return-code*) +http-length-required+
			   (content-type*) nil))
		    ((> message-length +biggest-article-length+)
		     (setf (return-code*) +http-request-entity-too-large+
			   (content-type*) nil))
		    (t
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
			     (setf (content-type*) nil))))))))
	(progn
	  (setf (return-code*) +http-length-required+
		(content-type*) nil)))))

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
	(setf (return-code*) +http-bad-request+
	      (content-type*) nil
	      (header-out "Server") nil))))

(defmethod handle ((method (eql :get))
		   (format (eql :xml))
		   (strictness (eql :none)))
  (let ((message (raw-post-data :force-text t)))
    (let* ((tempdir (temporary-directory))
	   (article (make-instance 'article
				   :directory (pathname-as-directory (pathname tempdir))
				   :name "article"
				   :text message)))
      (multiple-value-bind (accom-ok? accom-crashed?)
	  (accom article)
	(if accom-ok?
	    (multiple-value-bind (wsmparser-ok? wsmparser-crashed?)
		(wsmparser article)
	      (if wsmparser-ok?
		  (let ((wsx-path (file-with-extension article "wsx")))
		    (setf (return-code*) +http-ok+)
		    (setf (content-type*) "application/xml")
		    (file-as-string wsx-path))
		  (if wsmparser-crashed?
		      (setf (return-code*) +http-internal-server-error+
			    (content-type*) nil)
		      (setf (return-code*) +http-bad-request+
			    (content-type*) nil))))
	    (if accom-crashed?
		(setf (return-code*) +http-internal-server-error+
		      (content-type*) nil)
		(setf (return-code*) +http-bad-request+
		      (content-type*) nil)))))))

(defmethod acceptor-dispatch-request ((acceptor parser-acceptor) request)
  (let ((method (request-method request))
	(format (get-parameter "format" request))
	(strictness (get-parameter "strictness" request)))
    (handle method format strictness)))

(defun launch-parsing-service ()
  "Launch the Mizar parsing service."
  (start *parser-acceptor*))
