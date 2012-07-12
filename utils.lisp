
(in-package :mizar-parser)

(defun temporary-directory ()
  #+ccl
  (let ((mktemp-process (ccl:run-program "mktemp"
					 '("-d")
					 :wait t
					 :input nil
					 :error nil
					 :output :stream)))
    (with-open-stream (mktemp-output
		       (ccl:external-process-output-stream mktemp-process))
      (values (read-line mktemp-output))))
  #+sbcl
  (let ((mktemp-process (sb-ext:run-program "mktemp"
					    '("-d")
					    :wait t
					    :search t
					    :input nil
					    :error nil
					    :output :stream)))
    (with-open-stream (mktemp-output
		       (sb-ext:process-output mktemp-process))
      (values (read-line mktemp-output))))
  #- (or sbcl ccl)
  (error "We don't yet implement TEMPORARY-DIRECTORY for your Common Lisp."))

(defun lines-of-file (path)
  (let (lines)
    (with-open-file (file path :direction :input
			       :if-does-not-exist :error)
      (symbol-macrolet
	  (($line (read-line file nil nil)))
	(do ((line $line $line))
	    ((null line))
	  (push line lines))))
    (reverse lines)))

(defun file-as-string (path)
  (with-output-to-string (s)
    (with-open-file (file path
			  :direction :input
			  :if-does-not-exist :error)
      (loop
	 for line = (read-line file nil nil)
	 do
	   (if line
	       (format s "~a~%" line)
	       (return))))))
