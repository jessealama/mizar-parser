
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

(defgeneric handle (method)
  (:documentation "Handle the current request, assuming that it was made using the HTTP method METHOD (e.g., GET, HEAD, PUT, OPTIONS, etc)."))

;; Don't emit the Server response header
(defmethod handle :before (method)
  (declare (ignore method))
  (setf (header-out "Server") nil))

;; We don't handle anything requests except those for "/"
(defmethod handle :around (method)
  (declare (ignore method))
  (let ((uri (request-uri*)))
    (if (string= uri "/")
	(call-next-method)
	(progn
	  (setf (return-code*) +http-not-found+)
	  (setf (content-type*) nil)))))

;; By default, all requests are bad
(defmethod handle (method)
  (declare (ignore method))
  (setf (return-code*) +http-bad-request+)
  (setf (content-type*) nil))

(defmethod handle ((method (eql :options)))
  (setf (return-code*) +http-no-content+)
  (setf (content-type*) nil)
  (setf (header-out "Accept") "OPTIONS, HEAD, GET")
  "")

(defmethod handle ((method (eql :head)))
  (declare (ignore method))
  (setf (content-type*) nil)
  (setf (return-code*) +http-no-content+)
  (handle :get))

(defmethod handle ((method (eql :get)))
  "Method: GET")

(defun not-found ()
  (setf (return-code*) +http-not-found+)
  "")

(defmethod acceptor-dispatch-request ((acceptor parser-acceptor) request)
  (let ((method (request-method request)))
    (handle method)))

(defun launch-parsing-service ()
  "Launch the Mizar parsing service."
  (start *parser-acceptor*))
