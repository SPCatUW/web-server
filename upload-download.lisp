(in-package :pcws)

;; FIXME: Use logical pathnames
(defvar *queue-directory* (merge-pathnames "queue/" (truename "./"))
  "Directory where queued files to be printed are stored.")
(ensure-directories-exist *queue-directory*)

;;--------------------------------------------------------------------
;; Job classes, creation, storage, etc.
;;--------------------------------------------------------------------

(defclass job ()
  ((number :accessor job-number :type :integer :initarg :number)
   (filepath :accessor job-path :initarg :path
             :documentation "Path that represents file where job is stored.")
   (filename :accessor job-filename :initarg :filename
             :documentation "Original name of file as transmitted by web browser.  Files are NOT stored on disk by this name; they are stored at the filepath.")
   (mime-type :accessor job-mime-type :initarg :mime-type)))

(defclass user-job (job)
  ((user :accessor user-job-user :initarg :user
         :documentation "The user that owns or submitted the job")))

(defmethod initialize-instance :after ((job user-job) &key)
  "Add the job's number to the user's job queue."
  (push (job-number job) (user-jobs (user-job-user job))))

(defmacro make-job (number path filename mime-type &optional user)
  `(make-instance ',(cond (user 'user-job)
                          (t 'job))
                  :number ,number :path ,path :filename ,filename
                  :mime-type ,mime-type
                  ,@(when user `(:user ,user))))

(defparameter *number->job* (make-hash-table)
  "Job information stored by job number")

(defun get-job-by-number (number)
  (gethash number *number->job*))

(defun store-job (job)
  (setf (gethash (job-number job) *number->job*) job))

(defgeneric authenticate-then-transmit-job (job)
  (:documentation "Authenticates the request for the job and transmits it IFF the request is properly authorized."))

(defmethod authenticate-then-transmit-job ((job user-job))
  "Checks to see if the user credentials supplied in the request are valid and transmits job file if so.  If credentials are invalid, the return code is set to HTTP 403 (Forbidden)."
  (let ((user (verify-user* :parameter-function #'get-parameter)))
    (if (and user
             (member (job-number job)
                     (user-jobs user))
             (eql user (user-job-user job)))
        (handle-static-file (job-path job) (job-mime-type job))
        (progn (setf (return-code*) +http-forbidden+)
               (return-from authenticate-then-transmit-job)))))

;;--------------------------------------------------------------------
;; Webpages to upload and store job files
;;--------------------------------------------------------------------

(defpage upload-acceptor "/print"
  (with-valid-user-session (nil "/print")
    (html-page (:title "Submit Print Job - Student Printing Cooperative")
      (:h1 "Submit print job")
      (:form :method :post
             :action "/print/upload-confirmation"
             :enctype "multipart/form-data"
             (:p "Select file to print: "
                 (:input :type :file
                         :name "submitted-file"))
             (:p (:input :type :submit))))))

(let ((counter 0))
  (defun process-file (file-details &optional user)
    "Moves file to queue directory, renames, stores job information, and returns the job number"
    (let ((new-path (make-pathname :name (format nil "q-~A" counter)
                                   :defaults *queue-directory*)))
      (rename-file (first file-details) new-path)
      (store-job (make-job counter
                           new-path
                           (second file-details)
                           (third file-details)
                           user)))
    (prog1 counter      ;; returns the job number
      (incf counter))))

(defpage store-file-and-confirm "/print/upload-confirmation"
  (with-valid-user-session (user "/print/upload-confirmation")
    (let ((submitted-file (post-parameter "submitted-file")))
      (if submitted-file
          (let ((job-number (process-file submitted-file user)))
            (html-page (:title "Print Job Submitted - Student Printing Cooperative")
              (:h2 "Your print job has been submitted")
              (:p "File: " (:strong (fmt (second submitted-file))))   ;; second element of SUBMITTED-FILE is the filename supplied by the browser
              (:p "Job number: " (:strong (fmt "~A" job-number)))))
          ;; Redirect users to queue listing if there is no file submitted
          (redirect "/user/queue")))))


;;--------------------------------------------------------------------
;; Job backend --- for communicating with print station software.
;;--------------------------------------------------------------------

(defpage download-job-file "/backend/file"
  (with-basic-authentication ()
    (let ((job-number-string (get-parameter "job-number")))
      (if (not job-number-string)  ;; Is job number supplied?
          (return-404-from download-job-file)
          (let ((job (get-job-by-number (read-from-string job-number-string))))
            (if (null job)  ;; Job does not exist? --> Return 404
                (return-404-from download-job-file)
                (authenticate-then-transmit-job job)))))))

(defun user-queue->json (user)
  "Prints relevant user information and their queue to JSON dictionary; jobs are
printed as an array of arrays: [[number1, filename1], [number2, filename2],...]."
  (format nil "{\"username\" : ~S, \"jobs\" : [~{[~{~S~^, ~}]~^, ~}]}"
          (username user)
          (mapcar (lambda (jnumber)
                    (list jnumber (job-filename (get-job-by-number jnumber))))
                  (user-jobs user))))

(defpage backend-user-queue "/backend/user/queue"
  (with-basic-authentication ()
    (setf (content-type*) "text/plain")
    (let ((username (get-parameter "username"))
          (password (get-parameter "password")))
      (if (and username password
               (equal password (user-password (username->user username))))
          (princ (user-queue->json (username->user username)))
          (princ "Invalid user credentials")))))
