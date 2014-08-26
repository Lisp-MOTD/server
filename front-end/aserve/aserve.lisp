(in-package :motd-server)

(defun extract-args-from-uri (prefix uri)
  (let* ((uri (puri:parse-uri uri))
         (path (puri:uri-path uri))
         (start (search prefix path))
         (suffix (when (zerop start)
                   (subseq path (length prefix)))))
    (when (and (plusp (length suffix))
               (char= (elt suffix 0) #\/))
      (flet ((read-item (item)
               (let ((*read-eval* nil))
                 (with-input-from-string (in item)
                   (read in)))))
        (mapcar #'read-item
                (rest (split-sequence:split-sequence #\/ suffix)))))))

(defun make-prefix-handler (prefix function)
  (lambda (req ent)
    (net.aserve:with-http-response (req ent
                                    :content-type "text/plain")
      (net.aserve:with-http-body (req ent)
        (let ((*standard-output* net.html.generator:*html-stream*))
          (handler-case
              (let ((args (extract-args-from-uri prefix
                                                 (net.aserve:request-uri req))))
                (print (apply function args)))
            (error (err)
              (pprint err))))))))


(defmacro def-prefix-handler (name (&rest lambda-list) &body body)
  (let ((prefix (gensym "PREFIX-")))
    `(let ((,prefix ,(concatenate 'string
                                  "/"
                                  (string-downcase (symbol-name name)))))
       (progn
         (defun ,name (,@lambda-list)
           ,@body)
         (net.aserve:publish-prefix :prefix ,prefix
                                    :function (make-prefix-handler
                                               ,prefix
                                               #',name))
         ',name))))

(def-prefix-handler motds/after (message-id)
  (motd-server:get-all-motds-after message-id))

(def-prefix-handler motds/most-recent (how-many-to-retrieve)
  (motd-server:get-most-recent-motds how-many-to-retrieve))

(def-prefix-handler motds/proposed ()
  (motd-server:get-all-proposed-messages))

(def-prefix-handler tags/all ()
  (motd-server:get-all-tags))

(defun start-server (&key (port 80))
  (net.aserve:start :port port))

(defun shutdown-server (handle)
  (net.aserve:shutdown :server handle))

#|
(defgeneric propose-message (message))

(defgeneric publish-message (message-id))

(defgeneric add-translation (message-id language text))
(defgeneric delete-translation (message-id language))

(defgeneric add-tag (message-id tag))
(defgeneric delete-tag (message-id tag))

(defgeneric delete-message (message-id))
|#
