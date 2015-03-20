(in-package :motd-server)

(defgeneric create-signing-public-key (public-key)
  (:method ((public-key motd-commands:dsa-public-key))
    (adt:with-data (motd-commands:dsa-public-key p q g y) public-key
      (ironclad:make-public-key :dsa :p p :q q :g g :y y))))

(defun valid-signature-p (user-name bytes signature)
  (let ((public-key (retrieve-public-key =motd-db= *motd-db* user-name)))
    (when public-key
      (adt:with-data (motd-commands:dsa-signature r s) signature
        (ironclad:verify-signature (create-signing-public-key public-key)
                                   (ironclad:digest-sequence :sha1 bytes)
                                   (ironclad:make-dsa-signature r s))))))

(defun authorized-p (user-name command)
  (declare (ignore user-name command))  ; TODO: something more nuanced
  t)

(defmethod extract-authenticated-message ((authenticated-message
                                           motd-commands:authenticated-message)
                                          &optional
                                            (*command-time-threshold*
                                             +default-command-time-threshold+))
  (handler-case
      (adt:with-data (motd-commands:authenticated-message
                      user-name base64-bytes signature)
          authenticated-message

        (let ((utf-8-bytes (cl-base64:base64-string-to-usb8-array
                            base64-bytes)))
          (cond
            ((not (valid-signature-p user-name utf-8-bytes signature))
             (motd-commands:authentication-failed user-name))

            (t
             (adt:with-data (motd-commands:signed-message _ timestamp cmd)
                 (motd-commands:eval-command
                  (let ((*read-eval* nil)
                        (utf-8-string (trivial-utf-8:utf-8-bytes-to-string
                                       utf-8-bytes)))
                    (with-input-from-string (*standard-input* utf-8-string)
                      (read))))

               (let ((delta-time (- (get-universal-time) timestamp)))
                 (cond
                   ((<= *command-time-threshold* (abs delta-time))
                    (motd-commands:time-difference-too-great delta-time))

                   ((not (authorized-p user-name cmd))
                    (motd-commands:not-authorized user-name))

                   (t
                    cmd))))))))
    (error ()
      (motd-commands:decoding-error authenticated-message))))
