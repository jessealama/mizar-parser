
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

(defgeneric run-mizar-tool (tool flags article))

(defmethod run-mizar-tool (tool flags (article article))
  #+ccl
  (let* ((path (miz-file article))
	 (proc (ccl:run-program tool
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
  #-ccl
  (error "We don't handle your Common Lisp.  Sorry."))

(defmacro run-mizar-tool-with-standard-flags (tool article)
  `(run-mizar-tool ,tool '("-q" "-s" "-l") ,article))

(defgeneric accom (article))
(defgeneric wsmparser (article))
(defgeneric msmprocessor (article))

(defmethod accom ((article article))
  (run-mizar-tool-with-standard-flags "accom" article))

(defmethod wsmparser ((article article))
  (run-mizar-tool-with-standard-flags "wsmparser" article))

(defmethod msmprocessor ((article article))
  (run-mizar-tool-with-standard-flags "msmprocessor" article))
