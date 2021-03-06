(in-package #:motd-server-sqlite3)

(defmacro plist-bind ((&rest vars) plist &body body)
  (let ((plist-var (gensym "PLIST-")))
    `(let ((,plist-var ,plist))
       (let ,(mapcar #'(lambda (var)
                         `(,var (getf ,plist-var ,(%column-name var))))
                     vars)
         ,@body))))

(defun results-to-messages (results)
  (let ((ids nil)
        (expirations (make-hash-table :test #'eql))
        (timestamps (make-hash-table :test #'eql))
        (translations (make-hash-table :test #'eql))
        (tags (make-hash-table :test #'eql)))
    (dolist (result results)
      (plist-bind (id expiration timestamp language text tag) result
        (pushnew id ids)
        (setf (gethash id expirations) expiration
              (gethash id timestamps) timestamp)
        (when (and language text)
          (push (cons (intern language :keyword) text)
                (gethash id translations)))
        (when tag
          (push (intern tag :keyword)
                (gethash id tags)))))

    (remove nil
            (mapcar (lambda (id)
                      (let ((translations (gethash id translations)))
                        (when translations
                          (motd-server:create-message
                           (nreverse translations)
                           (nreverse (gethash id tags))
                           :id id
                           :timestamp (gethash id timestamps)
                           :expiration (gethash id expirations)))))
                    (nreverse ids)))))

(defun results-to-tags (results)
  (mapcar (lambda (entry)
            (plist-bind (tag) entry
              (intern tag :keyword)))
          results))

(defun results-to-public-keys (results)
  (mapcar (lambda (entry)
            (plist-bind (public-key) entry
              (when public-key
                (let ((*read-eval* nil))
                  (with-input-from-string (*standard-input* public-key)
                    (motd-commands:eval-command (read)))))))
          results))

(defun retrieve-all-motds-after (db message-id)
  (let ((results (dbi:execute (retrieve-after db) message-id)))
    (results-to-messages (dbi:fetch-all results))))

(defun retrieve-most-recent-motds (db how-many-to-retrieve)
  (let ((results (dbi:execute (retrieve-recent db) how-many-to-retrieve)))
    (results-to-messages (dbi:fetch-all results))))

(defun retrieve-all-proposed-motds (db)
  (let ((results (dbi:execute (retrieve-proposed db))))
    (results-to-messages (dbi:fetch-all results))))

(defun retrieve-all-tags (db)
  (let ((results (dbi:execute (retrieve-tags db))))
    (results-to-tags (dbi:fetch-all results))))

(defun retrieve-public-key (db user-name)
  (let ((results (dbi:execute (retrieve-public-key-stmt db) user-name)))
    (first (results-to-public-keys (dbi:fetch-all results)))))
