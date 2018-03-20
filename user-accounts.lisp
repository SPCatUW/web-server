;; Management of user accounts.

(in-package :pcws)

(defclass user ()
  ((username :accessor username :initarg :username :type :string)
   (password :accessor user-password :initarg :password :type :string)
   (first-name :accessor user-first-name :initarg :first-name :type :string)
   (last-name :accessor user-last-name :initarg :last-name :type :string)
   (email :accessor user-email :initarg :email :type :string)
   (jobs :accessor user-jobs :initform nil :type :list
         :documentation "List of job numbers that the user has queued")))

(defparameter *users* (make-hash-table :test 'equal)
  "Hash table of users objects stored by usernames")

(defun username->user (username)
  (gethash username *users*))

(defun store-user (user)
  "Store user by username"
  (setf (gethash (username user) *users*) user))

(defun make-user (username first last password email)
  (make-instance 'user :username username :password password :email email
                 :first-name first :last-name last))

(defpage register-user "/user/register"
  (if (eql (request-method*) :post)
      ;; Bind the values of various post parameters to local variables
      (destructuring-bind (first last username password retype email)
          (mapcar #'post-parameter '("first-name" "last-name" "username" "password" "password-retype" "email"))
        (cond ((not (string= password retype)) ;; passwords must match
               (redirect "/user/register?password-mismatch=t"))
              ((not (null (username->user username)))
               (redirect "/user/register?occupied-username=t"))
              (t (store-user (make-user username first last password email))
                 (setf (session-value 'username) username) ;; log in the user
                 (html-page (:title "Registration complete!")
                   (:h2 "Your registration is complete.")
                   (:p "Your username is "
                       (:strong (fmt "~A" username))
                       ".  The email which you would like use to contact you at is "
                       (:strong (fmt "~A" email))
                       ".")
                   (:p "Thank you!"))))))
  (html-page (:title "Register")
    (:h1 "Registration")
    (:form :method :post
           (:p "Real Name: "
               (:input :name "first-name" :type :text :placeholder "First name")
               (:input :name "last-name" :type :text :placeholder "Last name"))
           (:p "Username: " (:input :name "username" :type :text))
           (when (get-parameter "occupied-username")
             (html (:p (:em "Username taken.  Please choose a different username."))))
           (:p "Password: " (:input :name "password" :type :password))
           (:p "Retype Password: " (:input :name "password-retype" :type :password))
           (when (get-parameter "password-mismatch")
             (html (:p (:em "Passwords do not match.  Please retype."))))
           (:p "Email: " (:input :name "email" :type :email))
           (:p (:input :type :submit :value "Submit")))))

(defun generate-login-widget (redirect-uri &key incorrect-password)
  "Creates a login form with an optional hidden value for the page to redirect
to upon sucessful login."
  (html
    (:h1 "Login")
    (:form :method :post
           (:p "Username: " (:input :name "username" :type :text))
           (:p "Password: " (:input :name "password" :type :password))
           (when incorrect-password
             (html (:p (:em "Incorrect password."))))
           (:p (:input :type :submit :value "Submit"))
           (when redirect-uri
             (html (:input :name "redirect" :type :hidden :value redirect-uri))))))

(defpage user-login-page "/user/login"
  (cond (*session*  ;; if user is already logged in, redirect or tell them so
         (let ((redirect-uri (or (get-parameter "redirect")
                                 (post-parameter "redirect"))))
           (if redirect-uri
               (redirect redirect-uri)
               (html-page (:title "You are already logged in")
                 (:h2 "You are already logged in")
                 (:p "Username: " (princ (session-value 'username)))
                 (:p (fmt "Time logged in: ~A seconds since midnight, 1900 :)"
                          (session-start *session*)))))))
        ((eql (request-method*) :post)
         (let ((user (username->user (post-parameter "username")))
               (password (post-parameter "password"))
               (redirect-uri (post-parameter "redirect")))
           (if (and (not (null user)) ;; check user exists and password is correct
                    (string= password (user-password user)))
               ;; start user session; store username to session
               (progn (setf (session-value 'username (start-session))
                            (username user))
                      (if redirect-uri  ;; redirect to page if necessary.
                          (redirect redirect-uri)
                          (html-page (:title "Login sucessful")
                            (:h2 "You have logged in")
                            (:p "Username: " (:strong (princ (username user)))))))
               ;; Login failed, try again.
               (html-page (:title "Login")
                 (generate-login-widget redirect-uri :incorrect-password t)))))
        (t (html-page (:title "Login")
             (generate-login-widget (get-parameter "redirect"))))))

(defmacro with-valid-user-session ((&optional user-var redirect-uri) &body body)
  "Checks to see if user's session is valid.  If valid, retrieves and binds the
user-object to USER-VAR if latter is supplied, then executes BODY in
a(n implicit) progn.  Otherwise prompts login and redirect to supplied page."
  `(if *session* ;; Valid session?  (Value of NIL means not valid)
       ,(if user-var
            `(let ((,user-var (username->user (session-value 'username))))
               ,@body)
            `(progn ,@body))
       ;; No valid session?  Redirect to login, which may redirect to another
       ;;  page if desired upon sucessful login.
       (redirect ,(if redirect-uri
                      (format nil "/user/login?redirect=~A" redirect-uri)
                      "/user/login"))))

(defun verify-user* (&key (parameter-function #'post-parameter))
  "Verifies the user via the supplied username and password; returns the user object if credentials are valid.  Returns NIL otherwise."
  (let* ((username (funcall parameter-function "username"))
         (password (funcall parameter-function "password"))
         (user (username->user username)))
    (when (and user
               (equal (user-password user) password))
      user)))


