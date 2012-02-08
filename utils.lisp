
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
  #-ccl
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
  (let ((newline (make-string 1 :initial-element #\Newline))
	(lines (lines-of-file path)))
    (if lines
	(reduce #'(lambda (s1 s2)
		    (concatenate 'string s1 newline s2))
		(lines-of-file path))
	"")))
