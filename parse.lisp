
(in-package :mizar-parser)

(define-constant +parser-port+
    4387
  :test #'=
  :documentation "The port on which we listen for parsing requests.")

(define-constant +biggest-article-length+
    1000000
  :test #'=
  :documentation "The length of the largest article that we accept.")

(define-constant +information-message+
    (with-html-output-to-string (dummy)
      (:head
       (:title "Parsing Mizar Texts")
       (:link :rel "stylesheet" :href "/parsing/parsing.css" :type "text/css" :media "screen")
       (:link :rel "icon" :href "/parsing/favicon.ico" :type "image/png"))
      (:body
       (:h1 "About this service")
       (:p "The intention behind this site is to facilitate programmatic access to Mizar parsing services.  At the moment no HTML-driven interface to the parser services is provided; you are now looking at the only HTML page that this service emits, which is entirely informational.")
       (:p "This site documents some of the cutting-edge developments in Mizar text transformations.  The site was announced in the paper &lsquo;New developments in parsing Mizar&rsquo;, by Czes&#322;aw Bylinski and Jesse Alama (submitted to CICM 2012 Track E [Systems and Projects]).")
       (:h1 "Example usage")
       (:p "One can use this service using any means that permit one to include a Mizar text in the body of an HTTP message.  As mentioned earlier, the service does not (at present) aim to provide a user-friendly, HTML interface to the Mizar parser tools.  Rather, one uses the service by submitting certain HTTP requests (" (:a :href "#http-resources" "as documented below") ").  One can use standard commandline tools such as " (:a :href "http://curl.haxx.se/" :title "cURL" "curl")  ", " (:a :href "http://www.gnu.org/software/wget/" :title "GNU Wget" "wget") ", and " (:a :href "http://search.cpan.org/~gaas/libwww-perl-6.04/lib/LWP.pm" :title "libwww-perl" "lwp-request") ", as well as through HTTP interfaces such as " (:a :href "http://dvcs.w3.org/hg/xhr/raw-file/tip/Overview.html" :title "XMLHttpRequest" "XMLHttpRequest") " and HTTP libraries for " (:a :href "http://search.cpan.org/~gaas/libwww-perl-6.04/lib/LWP.pm" :title "LWP (Perl)" "Perl") ", " (:a :href "http://docs.python.org/library/httplib.html" :title "httplib (Python library)" "Python") ", " (:a :href "http://hackage.haskell.org/package/HTTP" :title "HTTP (Haskell library)" "Haskell") ", " (:a :href "http://ruby-doc.org/stdlib-1.9.3/libdoc/net/http/rdoc/Net/HTTP.html" :title "HTTP (Ruby library)" "Ruby") ", " (:a :href "http://weitz.de/drakma/" :title "Drakma (Common Lisp)" "Common Lisp") ", " (:a :href "http://www.w3.org/Library/" :title "Libwww (C library)" "C") ", etc.")
       (:p "With curl, for example, one can use the service in this way at the commandline:")
       ((:blockquote :class "session")
         (:ol
	  ((:li :class "comment") "Store the content of article.miz in a shell variable.")
	  ((:li :class "command") "article=`cat article.miz`")
	  ((:li :class "comment") "Note the escaping of the article variable and the URI.")
	  ((:li :class "comment") "Your browser may wrap the next command over multiple lines;")
	  ((:li :class "comment") "keep in mind that the URI should not contain whitespace.")
	  ((:li :class "command") "curl -X GET --data \"$article\" 'http://mizar.cs.ualberta.ca/parsing/?strictness=wsm&amp;format=text'")
	  ((:li :class "pseudo-response") "Weakly Strict Mizar form of article.miz, in plain text format.")
	  ((:li :class "command") "curl -X GET --data \"$article\" 'http://mizar.cs.ualberta.ca/parsing/?strictness=none&amp;format=xml'")
	  ((:li :class "pseudo-response") "Parse tree for article.miz, in XML format.")
	  ((:li :class "command") "curl -X GET --data \"$article\" 'http://mizar.cs.ualberta.ca/parsing/?strictness=msm&amp;format=text'")
	  ((:li :class "pseudo-response") "More Strict Mizar form of article.miz, in plain text format.")
	  ((:li :class "command") "curl -X GET --data \"$article\" 'http://mizar.cs.ualberta.ca/parsing/?strictness=msm&amp;format=xml'")
	  ((:li :class "pseudo-response") "XML parse tree of More Strict Mizar form of article.miz.")))
       (:p "Within a Javascript application, one could prepare a suitable HTTP request using the aforementioned XmlHttpRequest approach.  Java developers can use (for example) " (:a :href "http://hc.apache.org/httpcomponents-client-ga/" :title "HttpClient" "the Apache Foundation&apos;s HttpClient") " interface.")
       ((:h1 :id "http-resources") "HTTP resources")
       (:p "At present, the Mizar parsing service handles requests for only one resource, " (:code "/parsing") ".  Accessing any other resource, with any HTTP method, will result in a " (:code "404 (Not Found)") " error.")
       (:ul
	(:li (:p "Supported HTTP Methods: " (:strong "GET") ", " (:strong "HEAD") ", " (:strong "OPTIONS") ".")
	     (:p (:strong "POST") " is not supported."))
	(:li (:p "Protocol: In the message body, include the Mizar text."))
	(:li (:p "Query Parameters: We support the parameters " (:code "strictness") " and " (:code "format") ".")
	     (:ul
	      (:li (:p (:code "format") ": Two supported values:")
		   (:ul
		    (:li (:code "text"))
		    (:li (:code "xml")))
		   (:p "The default is " (:code "xml") "."))
	      (:li (:p (:code "strictness") ": Three supported values:")
		   (:ul
		    (:li (:p (:code "none"))
			 (:p "No transformation to the text is done."))
		    (:li (:p (:code "wsm"))
			 (:p "The Weakly Strict Mizar transformation."))
		    (:li (:p (:code "msm"))
			 (:p "The More Strict Mizar transformation.")))
		   (:p "The default is " (:code "none") "."))))
	(:li (:p "Response (" (:strong "GET") "): An XML representation of the parse tree for the given Mizar text, if the text is parseable; the MIME type will be " (:code "application/xml") ". and the return code will be " (:code "200 (OK)") ".  If the text is not parsable, and if it can be determined that the error lies in the supplied text rather than with the Mizar tools themselves, then the return code will be " (:code "400 (Bad Request)") " and the response body will be a plain text listing (served with the MIME type " (:code "text/plain") ") of the errors in the supplied text.  More precisely, the response will be a list of lines each of which adheres to the format")
	     (:p (:code "line-number column-number error-number explanation"))
	     (:p "where " (:code "line-number") ", " (:code "column-number") ", and " (:code "error-number") " are positive natural numbers, and " (:code "explanation") " is an natural language of the error.")
	     (:p "If it can be determined that the supplied text is problematic because the Mizar tools themselves crash when operating on the text, then the return code will be " (:code "500 (Internal Server Error)") ", and there will be an empty response body."))
	(:li (:p "Response (" (:strong "HEAD") ") The response will be computed described in the " (:strong "GET") " case, but ther will be an empty response body serived without any MIME type."))
	(:li (:p "Response (" (:strong "OPTIONS") ") The return code will be " (:code "200 (OK)") " and the mesasge body will be the string " (:code "GET, HEAD, OPTIONS") ".")))
       (:h1 "Mizar version")
       (:p "At the moment, this site works with Mizar texts according to system version " (:strong (str +mizar-system-version+)) " and MML version " (:strong (str +mml-version+)) ".")
       (:h1 "Related resources")
       (:ul
	(:li (:p "For more information about Mizar, see " (:a :href "http://mizar.org" "the Mizar homepage") ".")))
       (:h1 "Contact")
       (:p "If you have any questions, comments, or bug reports, feel free to email " (:a :href "mailto:jesse.alama@gmail.com" "the site maintainer") ".")))
  :test #'string=
  :documentation "The informational message that we output when we detect that information is what is sought (as opposed to detecting that a parser operation should be carried out).")

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

(defmethod handle ((method symbol) (format null) (strictness string))
  (handle method "xml" strictness))

(defmethod handle ((method symbol) (format string) (strictness null))
  (handle method format "none"))

;; We don't handle anything requests except those for "/"
(defmethod handle ((method symbol) (format string) (strictness string))
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
					       ((string= format "xml") :xml)))
				 (strictness (cond ((string= strictness "none") :none)
						   ((string= strictness "wsm") :wsm)
						   ((string= strictness "msm") :msm))))
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

(defmethod handle (method (format null) strictness)
  (handle method :xml strictness))

(defmethod handle (method format (strictness null))
  (handle method format :none))

(defmethod handle :around ((method (eql :get)) (format null) (strictness null))
  (declare (ignore format strictness))
  (let ((message (raw-post-data :force-text t)))
    (if message 
	(call-next-method)
	(progn
	  (setf (content-type*) "application/xhtml+xml")
	  (with-html (:doctype "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">")
	    (str +information-message+))))))

(defmethod handle :around ((method (eql :get)) format strictness)
  (declare (ignore format strictness))
  (let ((message (raw-post-data :force-text t)))
    (cond (message
	   (call-next-method))
	  (t
	   (setf (return-code*) +http-bad-request+
		  (content-type*) nil)))))

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

(defmethod handle ((method (eql :get))
		   (format (eql :text))
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
		  (let ((miz-path (file-with-extension article "miz")))
		    (setf (return-code*) +http-ok+)
		    (setf (content-type*) "text/plain")
		    (file-as-string miz-path))
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

(defmethod handle ((method (eql :get))
		   (format (eql :text))
		   (strictness (eql :wsm)))
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
		  (let ((wsm-path (file-with-extension article "wsm")))
		    (setf (return-code*) +http-ok+)
		    (setf (content-type*) "text/plain")
		    (file-as-string wsm-path))
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

(defmethod handle ((method (eql :get))
		   (format (eql :xml))
		   (strictness (eql :wsm)))
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
		  (let ((wsm-path (file-with-extension article "wsm"))
			(tpr-path (file-with-extension article "tpr")))
		    (multiple-value-bind (msplit-ok? msplit-crashed?) 
			(msplit article)
		      (if msplit-ok?
			(progn
			  (rename-file wsm-path tpr-path)
			  (mglue article)
			  (multiple-value-bind (wsmparser-ok-again? wsmparser-crashed-again?)
			      (wsmparser article)
			    (if wsmparser-ok-again?
				(let ((wsx-path (file-with-extension article "wsx")))
				  (setf (return-code*) +http-ok+)
				  (setf (content-type*) "application/xml")
				  (file-as-string wsx-path))
				(if wsmparser-crashed-again?
				    (setf (return-code*) +http-internal-server-error+
					  (content-type*) nil)
				    (setf (return-code*) +http-bad-request+
					  (content-type*) nil)))))
			(if msplit-crashed?
			    (setf (return-code*) +http-internal-server-error+
				  (content-type*) nil)
			    (setf (return-code*) +http-bad-request+
				  (content-type*) nil)))))
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

(defmethod handle ((method (eql :get))
		   (format (eql :text))
		   (strictness (eql :msm)))
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
		  (multiple-value-bind (msmprocessor-ok? msmprocessor-crashed?)
		      (msmprocessor article)
		    (if msmprocessor-ok?
			(let ((msm-path (file-with-extension article "msm")))
			  (setf (return-code*) +http-ok+)
			  (setf (content-type*) "text/plain")
			  (file-as-string msm-path))
			(if msmprocessor-crashed?
			    (setf (return-code*) +http-internal-server-error+
				  (content-type*) nil)
			    (setf (return-code*) +http-bad-request+
				  (content-type*) nil))))
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

(defmethod handle ((method (eql :get))
		   (format (eql :xml))
		   (strictness (eql :msm)))
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
		  (multiple-value-bind (msmprocessor-ok? msmprocessor-crashed?)
		      (msmprocessor article)
		    (if msmprocessor-ok?
			(let ((msm-path (file-with-extension article "msm"))
			      (tpr-path (file-with-extension article "tpr")))
			  (msplit article)
			  (rename-file msm-path tpr-path)
			  (mglue article)
			  (multiple-value-bind (wsmparser-ok-again? wsmparser-crashed-again?)
			      (wsmparser article)
			    (if wsmparser-ok-again?
				(let ((wsx-path (file-with-extension article "wsx")))
				  (setf (return-code*) +http-ok+)
				  (setf (content-type*) "application/xml")
				  (file-as-string wsx-path))
				(if wsmparser-crashed-again?
				    (setf (return-code*) +http-internal-server-error+
					  (content-type*) nil)
				    (setf (return-code*) +http-bad-request+
					  (content-type*) nil)))))
			(if msmprocessor-crashed?
			    (setf (return-code*) +http-internal-server-error+
				  (content-type*) nil)
			    (setf (return-code*) +http-bad-request+
				  (content-type*) nil))))
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
	(strictness (get-parameter "strictness" request))
	(uri (request-uri request)))
    (cond ((string= uri "/parsing.css")
	   (handle-static-file #p"/home/mizar-items/mizar-parser/parsing.css" "text/css"))
	  ((string= uri "/favicon.ico")
	   (handle-static-file #p"/home/mizar-items/mizar-parser/favicon.ico" "image/png"))
	  (t
	   (handle method format strictness)))))

(defun launch-parsing-service ()
  "Launch the Mizar parsing service."
  (start *parser-acceptor*))
