;; Web server base.  Macros for defining handler functions, HTMl-generation, and etc.

(in-package :printing-coop-web-server)

(defparameter *url->handler* (make-hash-table :test 'equal)
  "Hash table that stores handler functions by URL")

(defmacro defpage (function-name url &body body)
  "Define a handler function with the given name and bind it to the supplied URL"
  `(progn (defun ,function-name ()
            ,@body)
          (setf (gethash ,url *url->handler*)
                (symbol-function ',function-name))))

(defun find-handler-for-request (request)
  "Given a Hunchentoot request, find the handler associated with the requested URL"
  (gethash (script-name* request) *url->handler*))

;; put #'find-handler-for-request into list of handlers...
(setf *dispatch-table*
      `(,@*dispatch-table* ,(symbol-function 'find-handler-for-request)))

;; Stolen from :WEB-HELPERS

(defmacro html (&body body)
  "Has CL-WHO convert the BODY into HTML generation code.  Should be used
inside HTML-PAGE (particularly for separate functions that are called
inside HTML-PAGE) in order for HTML to be properly output."
  `(with-html-output (*standard-output*) 
     ,@body)) 

(defmacro html-page ((&key title more-header-content) &body body)
  "Generates HTML page and headers.  This should be wrapped around the
highest level of HTML generation, such is in the dispatch handler.
All other HTML generation for the same page should be conducted inside this
wrapper."
  `(with-html-output-to-string (*standard-output* nil :prologue t)
     (:html 
      (:head
       (:meta :http-equiv "Content Type"
              :content "text/html;charset=utf-8")
       ,@more-header-content
       (:title ,title " - Student Printing Cooperative"))
      (:body
       ,@body))))

(defmacro return-404-from (name)
  "Set response headers to indicate HTTP 404 --- Not found; returns from
supplied name"
  `(progn (setf (return-code*) +http-not-found+)
          (return-from ,name)))


(defparameter *authorization-db* nil
  "An alist containing (username . password) cons cells; used to authenticate users")

(defun verify-against-db (database username password)
  "Searches the database for the username, then checks to see if the supplied
password matches with that in the database (currently an alist)."
  (and username password
       (string= password (cdr (assoc username database :test #'equal)))))

;; (defun store-credentials-in-db (username password database)
;;   (push (cons username password) database))

;; Reads in and stores initial credentials from outside file.
;; FIXME: Allow for multiple sets of credentials.
(with-open-file (in "key" :direction :input)
  (push (cons (read in) (read in)) *authorization-db*))

(defmacro with-basic-authentication ((&key (database '*authorization-db*)
                                           username password) &body body)
  "Executes the BODY only if the authorization matches a username-password pair
in the database or the supplied username and password.  Preference is given
to the username and password; if those are not supplied, then the
authentication is performed using the database."
  (with-gensym (uname psswd)
    `(multiple-value-bind (,uname, psswd)
         (authorization)
       (if ,(if (and username password)
                `(and (equal ,uname, username)
                      (equal ,psswd ,password))
                `(verify-against-db ,database ,uname ,psswd))
           (progn ,@body)
           (require-authorization)))))
