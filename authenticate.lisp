(in-package :motd-server)

(defun generate-signature (payload author timestamp private-key)
  (declare (ignore payload author timestamp private-key))
  ;; TODO: really do this
  0)

(defun valid-signature-p (payload author timestamp signature)
  (declare (ignore payload author timestamp signature))
  ;; TODO: really do this
  t)

(defmethod create-authenticated-message ((payload string)
                                         (author string)
                                         private-key)
  (with-output-to-string (*standard-output*)
    (let* ((timestamp (get-universal-time))
           (signature (generate-signature payload author
                                          timestamp private-key)))
      (pprint (list :authenticated-message
                    :payload payload
                    :author author
                    :timestamp timestamp
                    :signature signature)))))

(defconstant +timestamp-tolerance+ 600)

(defmethod extract-authenticated-message ((authenticated-message string))
  (handler-case
      (with-input-from-string (*standard-input* authenticated-message)
        (destructuring-bind (tag &rest p-list) (read)
          (assert (eq tag :authenticated-message))
          (let ((payload (getf p-list :payload))
                (author (getf p-list :author))
                (timestamp (getf p-list :timestamp))
                (signature (getf p-list :signature)))
            (check-type payload string)
            (check-type author string)
            (check-type timestamp integer)
            (assert (< (- (get-universal-time) timestamp)
                       +timestamp-tolerance+))
            (unless (valid-signature-p payload author timestamp signature)
              (error "Invalid message signature for author: ~A" author))
            payload)))
    (error ()
      authenticated-message)))
