
(in-package :mizar-parser)

(defclass article ()
  ((name
    :initarg :name
    :type string
    :reader name
    :documentation "The name of the article (a string of alphanumeric characters of length between 1 and 8.")
   (directory
    :initarg :directory
    :type pathname
    :accessor article-directory
    :documentation "The directory where the .miz for this article can be found.")
   (text
    :initarg :text
    :type (or nil string)
    :reader article-text
    :documentation "The (full) text of the article.")))

(defmethod initialize-instance :after ((article article) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (when (not (slot-boundp article 'directory))
    (error "One must specify a directory for an article."))
  (when (not (slot-boundp article 'name))
    (error "One must specify a name for an article."))
  (let ((path (miz-file article)))
    (when (slot-boundp article 'text)
      (write-string-into-file (slot-value article 'text)
			      path
			      :if-exists :supersede
			      :if-does-not-exist :create
			      :external-format :ascii))))

(defgeneric file-with-extension (article extension))

(defmethod file-with-extension ((article article) extension)
  (let ((name (name article))
	(dir (pathname-as-directory (article-directory article))))
    (merge-pathnames (format nil "~a.~a" name extension) dir)))

(defgeneric err-file (article))

(defmethod err-file ((article article))
  (file-with-extension article "err"))

(defgeneric miz-file (article))

(defmethod miz-file ((article article))
  (file-with-extension article "miz"))

(defgeneric empty-err-file? (article))

(defmethod empty-err-file? ((article article))
  (zerop
   (with-open-file (file (err-file article))
     (file-length file))))

(defun message-file ()
  (merge-pathnames "mizar.msg" (mizfiles)))

(defgeneric explain-error (err-code)
  (:documentation "Consult the mizar.msg file to explain the error code ERR-CODE."))

(defmethod explain-error ((err-code-str string))
  (let ((err-code (parse-integer err-code-str :junk-allowed nil)))
    (explain-error err-code)))

(let ((error-table (make-hash-table :test #'eql)))
  (defmethod explain-error ((err-code integer))
    (multiple-value-bind (known-value found?)
	(gethash err-code error-table)
      (if found?
	  known-value
	  (setf (gethash err-code error-table)
		(let ((message-file (message-file)))
		  (if (file-exists-p message-file)
		      (let (explanation)
			(with-open-file (messages message-file
						  :direction :input
						  :if-does-not-exist :error)
			  (loop
			     with pattern = (format nil "^# ~d$" err-code)
			     for line = (read-line messages nil :eof)
			     do
			       (cond ((eq line :eof) (return))
				     ((null line) (return))
				     ((scan pattern line)
				      (let ((explanation-line (read-line messages nil :eof)))
					(cond ((eq explanation-line :eof) (return))
					      ((null explanation-line) (return))
					      (t
					       (setf explanation explanation-line)
					       (return))))))))
			explanation)
		      (error "The mizar error message file does not exist at the expected location~%~%  ~a~%" (namestring message-file))))))))
  (defun error-table ()
    error-table))

(defgeneric explain-errors (article)
  (:documentation "Explain the error file for ARTICLE."))

(defmethod explain-errors :around ((article article))
  (let ((err-file (file-with-extension article "err")))
    (if (file-exists-p err-file)
	(call-next-method)
	(error "The article ~a has no .err file." article))))

(defmethod explain-errors ((article article))
  (let ((err-file (file-with-extension article "err"))
	(explanation ""))
    (with-open-file (errs err-file
			  :if-does-not-exist :error
			  :direction :input)
      (loop
	 with err-line-pattern = "^([0-9]+) ([0-9]+) ([0-9]+)$"
	 for line = (read-line errs nil :eof)
	 do
	   (cond ((eq line :eof) (return))
		 ((null line) (return))
		 ((register-groups-bind (line-num col-num err-code)
		      (err-line-pattern line)
		    (let ((explanation-for-line (explain-error err-code)))
		      (setf explanation
			    (concatenate 'string
					 explanation
					 (format nil "~a ~a ~a ~a~%" line-num col-num err-code explanation-for-line))))))
		 (t
		  (setf explanation
			(concatenate 'string
				     explanation
				     (format nil "[Error: unable to parse the error line \"~a\"~%" line)))
		  (return)))))
    explanation))

(define-constant +mizar-system-version+
    "7.13.01"
  :test #'string=
  :documentation "The system version of the Mizar that we currently support.")

(define-constant +mml-version+
    "4.181.1147"
  :test #'string=
  :documentation "The MML version that we support.")

(define-constant +mizar-version+
    (format nil "~a-~a" +mizar-system-version+ +mml-version+)
  :test #'string=
  :documentation "The system number and MML version that we use, in that order, separated by a dash.")

(define-constant +mizar-release-root-dir+
    (pathname "/home/mizar-items/mizar/release/")
  :test #'equal
  :documentation "The directory under which we look for Mizar installations.")

(defun path-for-tool (tool &optional (mizar-version +mizar-version+))
  (let ((directory-for-version (merge-pathnames (format nil "~a/" mizar-version)
						+mizar-release-root-dir+)))
    (if (directory-p directory-for-version)
	(let ((bin-subdir (merge-pathnames "bin/" directory-for-version)))
	  (if (directory-p bin-subdir)
	      (merge-pathnames tool bin-subdir)
	      (error "The bin subdirectory~%~%  ~a~%~%of~%~%  ~a~%~%does not exist." bin-subdir directory-for-version)))
	(error "The release directory ~a doest not exist." directory-for-version))))

(defun mizfiles (&optional (mizar-system-version +mizar-system-version+)
		           (mml-version +mml-version+))
  (let ((full-system-name (format nil "~a-~a" mizar-system-version mml-version)))
    (pathname (format nil "~a~a/"
		      (namestring +mizar-release-root-dir+)
		      full-system-name))))

(defgeneric run-mizar-tool (tool flags article))

(defmethod run-mizar-tool :around (tool flags article)
  (let ((tool-path (path-for-tool tool)))
    (if (file-exists-p tool-path)
	(call-next-method)
	(error "We cannot find ~a at its expected location~%~%  ~a~%" tool tool-path))))

(defmethod run-mizar-tool (tool flags (article article))
  #+ccl
  (let* ((path (miz-file article))
	 (tool-path (path-for-tool tool))
	 (proc (ccl:run-program tool-path
				(append flags (list (namestring path)))
				:wait t
				:input nil
				:output nil
				:error nil)))
    (multiple-value-bind (status exit-code)
	(ccl:external-process-status proc)
      (declare (ignore status))
      (values (and (numberp exit-code)
		   (zerop exit-code))
	      (when (and (numberp exit-code)
			 (not (zerop exit-code)))
		(or (not (file-exists-p (err-file article)))
		    (empty-err-file? article))))))
  #+sbcl
  (let* ((path (miz-file article))
	 (err-path (file-with-extension article "err"))
	 (tool-path (path-for-tool tool))
	 (proc (sb-ext:run-program tool-path
				   (append flags (list (namestring path)))
				   :environment (list (format nil "MIZFILES=~a" (namestring (mizfiles))))
				   :search nil
				   :wait t
				   :input nil
				   :output nil
				   :error nil)))
    (let ((exit-code (sb-ext:process-exit-code proc)))
      (values (and (numberp exit-code)
		   (zerop exit-code))
	      (and (file-exists-p err-path)
		   (empty-err-file? article)))))
  #-(or ccl sbcl)
  (error "We don't handle your Common Lisp.  Sorry."))

(defmacro run-mizar-tool-with-standard-flags (tool article)
  `(run-mizar-tool ,tool '("-q" "-s" "-l") ,article))

(defgeneric accom (article))
(defgeneric wsmparser (article))
(defgeneric msmprocessor (article))
(defgeneric msplit (article))
(defgeneric mglue (article))

(defmethod accom ((article article))
  (run-mizar-tool-with-standard-flags "accom" article))

(defmethod wsmparser ((article article))
  (run-mizar-tool-with-standard-flags "wsmparser" article))

(defmethod msmprocessor ((article article))
  (run-mizar-tool-with-standard-flags "msmprocessor" article))

(defmethod msplit ((article article))
  (run-mizar-tool-with-standard-flags "msplit" article))

(defmethod mglue ((article article))
  (run-mizar-tool-with-standard-flags "mglue" article))

