
(in-package :mizar-parser)

(define-constant +project-root-directory+
    "/home/mizar-items/mizar-parser/"
  :test #'string=
  :documentation "The directory under which any static data is stored.")

(defun file-in-project-directory (path)
  (merge-pathnames path (pathname +project-root-directory+)))

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
       (:p "This site documents some of the cutting-edge developments in Mizar text transformations.  The site was announced in the paper &ldquo;" (:a :href "http://arxiv.org/abs/1205.0170" "New developments in parsing Mizar") "&rdquo;, by Czes&#322;aw Bylinski and Jesse Alama, which was accepted at " (:a :href "http://www.informatik.uni-bremen.de/cicm2012/cicm.php?event=sysproj&amp;menu=general" :title "Conference on Intelligent Computer Mathematics (CICM 2012) Track E: Systems and Projects" "CICM 2012 Track E (Systems and Projects)") ".")
       ((:h1 :id "examples") "Examples")
       (:p "Here are several examples illustrating the transformations  currently supported and the XML representation of the texts.")
       (:ul
	(:li
	 (:p (:a :href "http://mizar.org/version/current/html/xboole_0.html" (:tt "XBOOLE_0") ": Boolean Properties of Sets - Definitions (by Library Committee)"))
	 (:ul
	  (:li (:a :href "xboole_0.miz" "Plain text"))
	  (:li (:a :href "xboole_0.wsx" "XML parse tree (of the untransformed article)"))
	  (:li (:a :href "xboole_0.wsm" "Weakly Strict Mizar form (plain text)"))
	  (:li (:a :href "xboole_0.msm" "More Strict Mizar form (plain text)"))))
	(:li
	 (:p (:a :href "http://mizar.org/version/current/html/card_lar.html" (:tt "CARD_LAR") ": Mahlo and inaccessible cardinals (by Josef Urban)"))
	 (:ul
	  (:li (:a :href "card_lar.miz" "Plain text"))
	  (:li (:a :href "card_lar.wsx" "XML parse tree (of the untransformed article)"))
	  (:li (:a :href "card_lar.wsm" "Weakly Strict Mizar form (plain text)"))
	  (:li (:a :href "card_lar.msm" "More Strict Mizar form (plain text)"))))
	(:li
	 (:p (:a :href "http://mizar.org/version/current/html/group_10.html" (:tt "GROUP_10") ": The Sylow theorems (by Marco Riccardi)"))
	 (:ul
	  (:li (:a :href "group_10.miz" "Plain text"))
	  (:li (:a :href "group_10.wsx" "XML parse tree (of the untransformed article)"))
	  (:li (:a :href "group_10.wsm" "Weakly Strict Mizar form (plain text)"))
	  (:li (:a :href "group_10.msm" "More Strict Mizar form (plain text)")))))
       ((:h1 :id "download") "Download")
       (:p "A " (:a :href "/parsing/mizparse.pl" "simple Perl script") " is available to facilitate access to this service.  To get started, just do")
       ((:blockquote :class "session")
	(:ol
	 ((:li :class "comment") "Get an XML parse tree for article.miz")
	 ((:li :class "command") "mizparse.pl article.miz")
	 ((:li :class "pseudo-response") "XML document apparing on standard output, if there were no errors.")
	 ((:li :class "comment") "The plain text representation of the Weakly Strict Mizar form of article.miz")
	 ((:li :class "command") "mizparse.pl --transform=wsm --format=text article.miz")
	 ((:li :class "pseudo-response") "Plain text document on standard output")
	 ((:li :class "comment") "Use mizparse in a text transformation toolchain")
	 ((:li :class "command") "cool-rewriter article.miz | mizparse.pl --format=xml --transform=msm - | xsltproc hot-stylesheet.xsl - | ...")))
       (:p "For more information, just do " (:code "mizparse.pl --man") ".")
       (:h1 "Do things your way")
       (:p "If you'd like to parse Mizar texts on your own machine, first make sure you have Mizar version " (:strong "7.13.01") " (or newer).  Go to " (:a :href "http://mizar.org" "the Mizar homepage") " to download the latest version. The tools that produce parse trees and do the Weakly Strict Mizar and More Strict Mizar text normalizations are " (:code "wsmparser") " and " (:code "msmprocessor") ".  One needs to first run the " (:code "accom") " tool before running " (:code "wsmparser") ", and one needs to run " (:code "wsmparser") " before running " (:code "msmprocessor") ".")
       (:p (:code "wsmparser") " produces two files.  If your Mizar article is called &lsquo;article&rsquo;, doing")
       ((:blockquote :class "session")
	(:ol
	 ((:li :class "command") "wsmparser article")))
       (:p "will produce " (:code "article.wsm") " and " (:code "article.wsx") ".  The " (:code "wsm") " file is a plain text representation of the Weakly Strict Mizar transform of " (:code "article.miz") ". The " (:code "wsx") " file is an XML parse tree for " (:code "article.miz") ". (" (:code "article.wsx") " is not an XML represesentation of the WSM-ified " (:code "article.miz") ".  To get that, one must change the " (:code ".miz") " file and run " (:code "wsmparser") " again.)" "  Similarly, " (:code "msmprocessor") " will produce two files, " (:code "article.msm") " and " (:code "article.msx") ".")
       (:h1 "For the hackers")
       (:p "One can use this service using any tool that permits one to include a Mizar text in the body of an HTTP message, ship it off to this server, and look at the response.  As mentioned earlier, the service does not (at present) aim to provide a user-friendly, HTML interface to the Mizar parser tools.  The " (:a :href "#download" :title "mizparse" "mizparse script") " can be consulted if you want to see how to access the service in your own scripts and tools.  The whole of the service can be accessed by submitting certain HTTP requests (" (:a :href "#http-resources" "as documented below") ").  One can use standard commandline tools such as " (:a :href "http://curl.haxx.se/" :title "cURL" "curl")  ", " (:a :href "http://www.gnu.org/software/wget/" :title "GNU Wget" "wget") ", and " (:a :href "http://search.cpan.org/~gaas/libwww-perl-6.04/lib/LWP.pm" :title "libwww-perl" "lwp-request") ", as well as through HTTP interfaces such as " (:a :href "http://dvcs.w3.org/hg/xhr/raw-file/tip/Overview.html" :title "XMLHttpRequest" "XMLHttpRequest") " and HTTP libraries for " (:a :href "http://search.cpan.org/~gaas/libwww-perl-6.04/lib/LWP.pm" :title "LWP (Perl)" "Perl") ", " (:a :href "http://docs.python.org/library/httplib.html" :title "httplib (Python library)" "Python") ", " (:a :href "http://hackage.haskell.org/package/HTTP" :title "HTTP (Haskell library)" "Haskell") ", " (:a :href "http://ruby-doc.org/stdlib-1.9.3/libdoc/net/http/rdoc/Net/HTTP.html" :title "HTTP (Ruby library)" "Ruby") ", " (:a :href "http://weitz.de/drakma/" :title "Drakma (Common Lisp)" "Common Lisp") ", " (:a :href "http://www.w3.org/Library/" :title "Libwww (C library)" "C") ", etc.")
       (:p "With curl, for example, one can use the service in this way at the commandline:")
       ((:blockquote :class "session")
         (:ol
	  ((:li :class "comment") "Note the escaping of the article variable and the URI.")
	  ((:li :class "comment") "Your browser may wrap the next command over multiple lines;")
	  ((:li :class "comment") "keep in mind that the URI should not contain whitespace.")
	  ((:li :class "command") "curl --data @article.miz 'http://mizar.cs.ualberta.ca/parsing/?strictness=wsm&amp;format=text'")
	  ((:li :class "pseudo-response") "Weakly Strict Mizar form of article.miz, in plain text format.")
	  ((:li :class "command") "curl --data @article.miz 'http://mizar.cs.ualberta.ca/parsing/?strictness=none&amp;format=xml'")
	  ((:li :class "pseudo-response") "Parse tree for article.miz, in XML format.")
	  ((:li :class "command") "curl --data @article.miz 'http://mizar.cs.ualberta.ca/parsing/?strictness=msm&amp;format=text'")
	  ((:li :class "pseudo-response") "More Strict Mizar form of article.miz, in plain text format.")
	  ((:li :class "command") "curl --data @article.miz 'http://mizar.cs.ualberta.ca/parsing/?strictness=msm&amp;format=xml'")
	  ((:li :class "pseudo-response") "XML parse tree of More Strict Mizar form of article.miz.")))
       (:p "Within a Javascript application, one could prepare a suitable HTTP request using the aforementioned XmlHttpRequest approach.  Java developers can use (for example) " (:a :href "http://hc.apache.org/httpcomponents-client-ga/" :title "HttpClient" "the Apache Foundation&apos;s HttpClient") " interface.")
       ((:h1 :id "http-resources") "HTTP resource(s)")
       (:p "At present, the Mizar parsing service handles requests for only one resource, " (:code "/parsing") ".  Accessing any other resource, with any HTTP method, will result in a " (:code "404 (Not Found)") " error.")
       (:ul
	(:li (:p "Supported HTTP Methods: " (:strong "GET") ", " (:strong "POST") ", " (:strong "HEAD") ", " (:strong "OPTIONS") "."))
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
	(:li (:p "Response (" (:strong "POST") "): An XML representation of the parse tree for the given Mizar text, if the text is parseable; the MIME type will be " (:code "application/xml") " and the return code will be " (:code "200 (OK)") ".  If the text is not parsable, and if it can be determined that the error lies in the supplied text rather than with the Mizar tools themselves, then the return code will be " (:code "400 (Bad Request)") " and the response body will be a plain text listing (served with the MIME type " (:code "text/plain") ") of the errors in the supplied text.  More precisely, the response will be a list of lines each of which adheres to the format")
	     (:p (:code "line-number column-number error-number explanation"))
	     (:p "where " (:code "line-number") ", " (:code "column-number") ", and " (:code "error-number") " are positive natural numbers, and " (:code "explanation") " is an natural language of the error.")
	     (:p "If it can be determined that the supplied text is problematic because the Mizar tools themselves crash when operating on the text, then the return code will be " (:code "500 (Internal Server Error)") ", and there will be an empty response body."))
	(:li (:p "Response (" (:strong "HEAD") ") The response will be computed described in the " (:strong "POST") " case, but ther will be an empty response body serived without any MIME type."))
	(:li (:p "Response (" (:strong "OPTIONS") ") The return code will be " (:code "200 (OK)") " and the mesasge body will be the string " (:code "GET, POST, HEAD, OPTIONS") ".")))
       (:h1 "Mizar version")
       (:p "At the moment, this site works with Mizar texts according to system version " (:strong (str +mizar-system-version+)) " and MML version " (:strong (str +mml-version+)) ".")
       (:h1 "Related resources")
       (:ul
	(:li (:p "For more information about Mizar, see " (:a :href "http://mizar.org" "the Mizar homepage") "."))
	(:li (:p "To learn more about how Mizar works, see &ldquo;" (:a :href "http://jfr.cib.unibo.it/article/view/1980" :title "Mizar in a nutshell" "Mizar in a nutshell") "&rdquo;, by Adam Grabowski, Artur Korni&#322;owicz, and Adam Naumowicz, " (:i "Journal of Formalized Reasoning") " " (:strong "3") "(2), 2010, pp. 153&ndash;245."))
	(:li (:p "For a historical overview of Mizar, see &ldquo;" (:a :href "http://markun.cs.shinshu-u.ac.jp/mizar/mma.dir/2005/mma2005(2).pdf" :title "Special Issue on 30 Years of Mizar" "Mizar: The first 30 years") "&rdquo;, by Roman Matuszewski and Piotr Rudnicki, " (:i "Mechanized Mathematics and Its Applications") " " (:strong "4") "(1), March 2005, pp. 3&ndash;24.")))
       (:h1 "The guts")
       (:p "If you're curious about the code that underlies this site, see " (:a :href "https://github.com/jessealama/mizar-parser" :title "mizar-parser on GitHub" "the git repository for it") ".  The site is written in Common Lisp using the excellent " (:a :href "http://weitz.de/hunchentoot/" :title "Hunchentoot" "hunchentoot") " web server.")
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

(defgeneric handle (method format strictness article)
  (:documentation "Handle the current request on ARTICLE, assuming that it was made using the HTTP method METHOD (e.g., GET, POST, HEAD, PUT, OPTIONS, etc)."))

;; Don't emit the Server response header
(defmethod handle :before (method format strictness article)
  (declare (ignore method format strictness article))
  (setf (header-out "Server") nil))

(defmacro return-message (http-return-code &key (message "")
					        (mime-type nil))
  `(progn
     (setf (return-code*) ,http-return-code
	   (content-type*) ,mime-type)
     (setf (header-out "Server") nil)
     (setf (header-out "Vary") nil)
     ,message))

;; By default, all requests are bad
(defmethod handle (method format strictness article)
  (declare (ignore method format strictness article))
  (let ((uri (request-uri*))
	(method-symbol (request-method*))
	(message (raw-post-data :force-text t)))
    (if message
	(return-message +http-bad-request+
			:mime-type "text/plain"
			:message (format nil "Your request for~%~%  ~a~%~%with the HTTP method~%~%  ~a~%~%with the message of length ~d could not be understood." uri (symbol-name method-symbol) (length message)))
	(return-message +http-bad-request+
			:mime-type "text/plain"
			:message (format nil "Your request for~%~%  ~a~%~%with the HTTP method~%~%  ~a~%~%with an empty message could not be understood." uri (symbol-name method-symbol))))))

(defmethod handle :around ((method symbol) format strictness article)
  (declare (ignore format strictness article))
  (let* ((method-name (symbol-name method))
	(method-name-lc (format nil "~(~a~)" method-name)))
    (cond ((string= method-name-lc "post")
	   (call-next-method))
	  ((string= method-name-lc "head")
	   (call-next-method))
	  ((string= method-name-lc "options")
	   (call-next-method))
	  (t
	   (setf (header-out "Allow") "POST, HEAD, OPTIONS")
	   (return-message +http-method-not-allowed+
			   :mime-type "text/plain"
			   :message "We support only the POST, HEAD, and OPTIONS HTTP methods.")))))

(defmethod handle (method format strictness (article null))
  (declare (ignore method format strictness))
  (error "Unable to handle a request for a null article."))

(defmethod handle ((method symbol) (format null) (strictness string) article)
  (handle method "xml" strictness article))

(defmethod handle ((method symbol) (format string) (strictness null) article)
  (handle method format "none" article))

(defmacro empty-message-with-code (http-return-code)
  `(setf (return-code*) ,http-return-code
	 (content-type*) nil))

(defun parse-integer-if-possible (integer-string)
  (handler-case (parse-integer integer-string :junk-allowed nil)
    (error () nil)))

(defmethod handle ((method symbol) (format string) (strictness string) article)
  (let ((format (cond ((string= format "text") :text)
		      ((string= format "xml") :xml)))
	(strictness (cond ((string= strictness "none") :none)
			  ((string= strictness "wsm") :wsm)
			  ((string= strictness "msm") :msm))))
    (if (and format strictness)
	(handle method format strictness article)
	(return-message +http-bad-request+))))

(defmethod handle ((method (eql :options)) format strictness article)
  (declare (ignore format strictness article))
  (setf (header-out "Accept") "OPTIONS, HEAD, POST")
  (return-message +http-no-content+))

(defmethod handle ((method (eql :head)) format strictness article)
  (declare (ignore method))
  (setf (content-type*) nil)
  (setf (return-code*) +http-no-content+)
  (handle :post format strictness article))

(defmethod handle (method (format null) strictness article)
  (handle method :xml strictness article))

(defmethod handle (method format (strictness null) article)
  (handle method format :none article))

(defmethod handle :around ((method (eql :post)) format strictness (article article))
  (declare (ignore format strictness))
  (multiple-value-bind (accom-ok? accom-crashed?)
      (accom article)
    (if accom-ok?
	(call-next-method)
	(if accom-crashed?
	    (return-message +http-internal-server-error+)
	    (let ((error-explanation (explain-errors article)))
	      (setf (return-code*) +http-bad-request+
		    (content-type*) "text/plain"
		    (header-out "Server") error-explanation)
	      error-explanation)))))

(defmethod handle ((method (eql :post))
		   (format (eql :xml))
		   (strictness (eql :none))
		   (article article))
  (multiple-value-bind (wsmparser-ok? wsmparser-crashed?)
      (wsmparser article)
    (if wsmparser-ok?
	(let ((wsx-path (file-with-extension article "wsx")))
	  (setf (return-code*) +http-ok+)
	  (setf (content-type*) "application/xml")
	  (file-as-string wsx-path))
	(if wsmparser-crashed?
	    (return-message +http-internal-server-error+)
	    (return-message +http-bad-request+)))))

(defmethod handle ((method (eql :post))
		   (format (eql :text))
		   (strictness (eql :none))
		   (article article))
  (let ((miz-path (file-with-extension article "miz")))
    (return-message +http-ok+
		    :message (file-as-string miz-path)
		    :mime-type "text/plain")))

(defmethod handle ((method (eql :post))
		   (format (eql :text))
		   (strictness (eql :wsm))
		   (article article))
  (multiple-value-bind (wsmparser-ok? wsmparser-crashed?)
      (wsmparser article)
    (if wsmparser-ok?
	(multiple-value-bind (msplit-ok? msplit-crashed?)
	    (msplit article)
	  (if msplit-ok?
	      (let ((wsm-path (file-with-extension article "wsm"))
		    (tpr-path (file-with-extension article "tpr"))
		    (miz-path (file-with-extension article "miz")))
		(rename-file wsm-path tpr-path)
		(multiple-value-bind (mglue-ok? mglue-crashed?)
		    (mglue article)
		  (if mglue-ok?
		      (return-message +http-ok+
				      :message (file-as-string miz-path)
				      :mime-type "text/plain")
		      (if mglue-crashed?
			  (return-message +http-internal-server-error+)
			  (return-message +http-bad-request+)))))
	      (if msplit-crashed?
		  (return-message +http-internal-server-error+)
		  (return-message +http-bad-request+))))
	(if wsmparser-crashed?
	    (return-message +http-internal-server-error+)
	    (return-message +http-bad-request+)))))

(defmethod handle ((method (eql :post))
		   (format (eql :xml))
		   (strictness (eql :wsm))
		   (article article))
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
			  (return-message +http-ok+
					  :message (file-as-string wsx-path)
					  :mime-type "application/xml"))
			(if wsmparser-crashed-again?
			    (setf (return-code*) +http-internal-server-error+
				  (content-type*) nil)
			    (setf (return-code*) +http-bad-request+
				  (content-type*) nil)))))
		(if msplit-crashed?
		    (return-message +http-internal-server-error+)
		    (return-message +http-bad-request+)))))
	(if wsmparser-crashed?
	    (return-message +http-internal-server-error+)
	    (return-message +http-bad-request+)))))

(defmethod handle ((method (eql :post))
		   (format (eql :text))
		   (strictness (eql :msm))
		   (article article))
  (multiple-value-bind (wsmparser-ok? wsmparser-crashed?)
      (wsmparser article)
    (if wsmparser-ok?
	(multiple-value-bind (msmprocessor-ok? msmprocessor-crashed?)
	    (msmprocessor article)
	  (if msmprocessor-ok?
	      (let ((msm-path (file-with-extension article "msm"))
		    (tpr-path (file-with-extension article "tpr"))
		    (miz-path (file-with-extension article "miz")))
		(multiple-value-bind (msplit-ok? msplit-crashed?)
		    (msplit article)
		  (if msplit-ok?
		      (progn
			(rename-file msm-path tpr-path)
			(multiple-value-bind (mglue-ok? mglue-crashed?)
			    (mglue article)
			  (if mglue-ok?
			      (return-message +http-ok+
					      :message (file-as-string miz-path)
					      :mime-type "text/plain")
			      (if mglue-crashed?
				  (return-message +http-internal-server-error+)
				  (return-message +http-bad-request+)))))
		      (if msplit-crashed?
			  (return-message +http-internal-server-error+)
			  (return-message +http-bad-request+)))))
	      (if msmprocessor-crashed?
		  (return-message +http-internal-server-error+)
		  (return-message +http-bad-request+))))
	(if wsmparser-crashed?
	    (return-message +http-internal-server-error+)
	    (return-message +http-bad-request+)))))

(defmethod handle ((method (eql :post))
		   (format (eql :xml))
		   (strictness (eql :msm))
		   (article article))
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
			(return-message +http-ok+
					:message (file-as-string wsx-path)
					:mime-type "application/xml"))
		      (if wsmparser-crashed-again?
			  (return-message +http-internal-server-error+)
			  (return-message +http-bad-request+)))))
	      (if msmprocessor-crashed?
		  (return-message +http-internal-server-error+)
		  (return-message +http-bad-request+))))
	(if wsmparser-crashed?
	    (return-message +http-internal-server-error+)
	    (return-message +http-bad-request+)))))

(defmacro emit-canned-message ()
  `(progn
     (setf (content-type*) "application/xhtml+xml")
     (with-html (:doctype "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">")
       (str +information-message+))))

(defmethod handle (method format strictness (article-text string))
  (let* ((tempdir (temporary-directory))
	 (article (make-instance 'article
				 :directory (pathname-as-directory (pathname tempdir))
				 :name "article"
				 :text article-text)))
    (prog1
	(handle method format strictness article)
      (delete-directory-and-files tempdir
				  :if-does-not-exist :ignore))))

(defmethod acceptor-dispatch-request ((acceptor parser-acceptor) request)
  (let ((uri (script-name request))
	(method (request-method request)))
    (cond ((string= uri "/parsing.css")
	   (if (eq method :get)
	       (handle-static-file (file-in-project-directory "parsing.css")
				   "text/css")
	       (return-message +http-method-not-allowed+)))
	  ((string= uri "/favicon.ico")
	   (if (eq method :get)
	       (handle-static-file (file-in-project-directory "favicon.ico")
				   "image/png")
	       (return-message +http-method-not-allowed+)))
	  ((string= uri "/mizparse.pl")
	   (if (eq method :get)
	       (handle-static-file (file-in-project-directory "mizparse.pl")
				   "text/plain")
	       (return-message +http-method-not-allowed+)))

	  ;; examples
	  ((string= uri "/xboole_0.miz")
	   (if (eq method :get)
	       (handle-static-file (file-in-project-directory "xboole_0.miz")
				   "text/plain")
	       (return-message +http-method-not-allowed+)))
	  ((string= uri "/xboole_0.wsx")
	   (if (eq method :get)
	       (handle-static-file (file-in-project-directory "xboole_0.wsx")
				   "application/xml")
	       (return-message +http-method-not-allowed+)))
	  ((string= uri "/xboole_0.wsm")
	   (if (eq method :get)
	       (handle-static-file (file-in-project-directory "xboole_0.wsm")
				   "text/plain")
	       (return-message +http-method-not-allowed+)))
	  ((string= uri "/xboole_0.msm")
	   (if (eq method :get)
	       (handle-static-file (file-in-project-directory "xboole_0.msm")
				   "text/plain")
	       (return-message +http-method-not-allowed+)))

	  ((string= uri "/card_lar.miz")
	   (if (eq method :get)
	       (handle-static-file (file-in-project-directory "card_lar.miz")
				   "text/plain")
	       (return-message +http-method-not-allowed+)))
	  ((string= uri "/card_lar.wsx")
	   (if (eq method :get)
	       (handle-static-file (file-in-project-directory "card_lar.wsx")
				   "application/xml")
	       (return-message +http-method-not-allowed+)))
	  ((string= uri "/card_lar.wsm")
	   (if (eq method :get)
	       (handle-static-file (file-in-project-directory "card_lar.wsm")
				   "text/plain")
	       (return-message +http-method-not-allowed+)))
	  ((string= uri "/card_lar.msm")
	   (if (eq method :get)
	       (handle-static-file (file-in-project-directory "card_lar.msm")
				   "text/plain")
	       (return-message +http-method-not-allowed+)))

	  ((string= uri "/group_10.miz")
	   (if (eq method :get)
	       (handle-static-file (file-in-project-directory "group_10.miz")
				   "text/plain")
	       (return-message +http-method-not-allowed+)))
	  ((string= uri "/group_10.wsx")
	   (if (eq method :get)
	       (handle-static-file (file-in-project-directory "group_10.wsx")
				   "application/xml")
	       (return-message +http-method-not-allowed+)))
	  ((string= uri "/group_10.wsm")
	   (if (eq method :get)
	       (handle-static-file (file-in-project-directory "group_10.wsm")
				   "text/plain")
	       (return-message +http-method-not-allowed+)))
	  ((string= uri "/group_10.msm")
	   (if (eq method :get)
	       (handle-static-file (file-in-project-directory "group_10.msm")
				   "text/plain")
	       (return-message +http-method-not-allowed+)))

	  ;; the guts
	  ((string= uri "/")
	   (let ((message-length-header (header-in* "Content-Length" request))
		 (message (raw-post-data :force-text t
					 :request request)))
	     (if message
		 (if message-length-header
		     (let ((message-length (parse-integer-if-possible message-length-header)))
		       (if (integerp message-length)
			   (cond ((< message-length 0)
				  (return-message +http-length-required+))
				 ((> message-length +biggest-article-length+)
				  (return-message +http-request-entity-too-large+))
				 ((string= uri "/")
				  (let ((format (get-parameter "format" request))
					(strictness (get-parameter "strictness" request)))
				    (handle method format strictness message)))
				 ((string= uri "/pretty-print")
				  (return-message +http-accepted+))
				 (t
				  (return-message +http-not-found+
						  :mime-type "text/plain"
						  :message (format nil "Unknown resource ~a." uri))))
			   (return-message +http-length-required+)))
		     (return-message +http-length-required+))
		 (emit-canned-message))))
	  (t
	   (return-message +http-not-found+
			   :mime-type "text/plain"
			   :message (format nil "Unknown resource ~a." uri))))))

(defmethod acceptor-status-message ((acceptor parser-acceptor) http-return-code &key &allow-other-keys)
  "This method enures that some method on the ACCEPTOR-STATUS-MESSAGE
generic function is called on PARSER-ACCEPTOR object.  The value NIL
ensures that whatever messages we prepare earlier are indeed returned.
Without this method, it seems that hunchentoot tries to be clever and
hijack from us, when the HTTP return code is >= 400, our carefully
prepared message bodies."
  (declare (ignore acceptor http-return-code))
  nil)

(defun launch-parsing-service ()
  "Launch the Mizar parsing service."
  (start *parser-acceptor*))
