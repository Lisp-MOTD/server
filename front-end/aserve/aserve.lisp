(in-package :motd-server-aserve)

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

(defun get-posted-message (raw-msg)
  (let ((*read-eval* nil))
    (motd-server:extract-authenticated-message
     (with-input-from-string (*standard-input* raw-msg)
       (motd-commands:eval-command (read))))))

(defun make-post-handler (function)
  (lambda (req ent)
    (net.aserve:with-http-response (req ent
                                    :content-type "text/plain;charset=utf-8")
      (net.aserve:with-http-body (req ent)
        (write-sequence
         (trivial-utf-8:string-to-utf-8-bytes
          (with-output-to-string (*standard-output*)
            (handler-case
                (let ((msg (get-posted-message
                            (net.aserve:request-query-value "command" req))))
                  (pprint (funcall function msg)))
              (error (err)
                (pprint err)))))
         net.html.generator:*html-stream*)))))

(defmacro def-post-handler (name msg-type (&rest args) &body body)
  (let ((path (gensym "PATH-")))
    `(let ((,path ,(concatenate 'string
                                "/"
                                (string-downcase (symbol-name name)))))
       (progn
         (defun ,name (,@args)
           ,@body)
         (flet ((destructurer (msg)
                  (cond
                    ((typep msg ',msg-type)
                     (adt:with-data (,msg-type ,@args) msg
                       (funcall #',name ,@args)))

                    ((motd-commands:motd-general-error-p msg)
                     msg)

                    (t
                     (motd-commands:decoding-error msg)))))
           (net.aserve:publish
            :path ,path
            :function (make-post-handler #'destructurer)))))))

(defun make-prefix-handler (prefix function)
  (lambda (req ent)
    (net.aserve:with-http-response (req ent
                                    :content-type "text/plain;charset=utf-8")
      (net.aserve:with-http-body (req ent)
        (write-sequence
         (trivial-utf-8:string-to-utf-8-bytes
          (with-output-to-string (*standard-output*)
            (handler-case
                (let ((args (extract-args-from-uri
                             prefix
                             (net.aserve:request-uri req))))
                  (pprint (apply function args)))
              (error (err)
                (pprint err)))))
         net.html.generator:*html-stream*)))))


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
  (motd-server:get-all-proposed-motds))

(def-prefix-handler tags/all ()
  (motd-server:get-all-tags))

(def-post-handler motds/add-translation motd-commands:add-translation
    (message-id language text)
  (motd-server:add-translation message-id language text))

(def-post-handler motds/delete-translation motd-commands:delete-translation
    (message-id language)
  (motd-server:delete-translation message-id language))

(defun start-server (&key (port 80))
  (net.aserve:start :port port))

(defun shutdown-server (handle)
  (net.aserve:shutdown :server handle))

#|
(defgeneric propose-message (message))

(defgeneric publish-message (message-id))

(defgeneric delete-translation (message-id language))

(defgeneric add-tag (message-id tag))
(defgeneric delete-tag (message-id tag))

(defgeneric delete-message (message-id))
|#
