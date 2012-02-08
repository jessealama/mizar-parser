
(in-package :mizar-parser)

(defun run-mizar-tool (tool flags path)
  #+ccl
  (let ((proc (ccl:run-program tool
		   (append flags (list (namestring path)))
		   :wait t
		   :input nil
		   :output nil
		   :error nil)))
    (multiple-value-bind (status exit-code)
	(ccl:external-process-status proc)
      (declare (ignore status))
      (and (numberp exit-code)
	   (zerop exit-code)))))

(defmacro run-mizar-tool-with-standard-flags (tool article)
  `(run-mizar-tool ,tool '("-q" "-s" "-l") ,article))

(defun accom (article)
  (run-mizar-tool-with-standard-flags "accom" article))

(defun wsmparser (article)
  (run-mizar-tool-with-standard-flags "wsmparser" article))

(defun msmprocessor (article)
  (run-mizar-tool-with-standard-flags "msmprocessor" article))
