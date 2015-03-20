(in-package #:motd-server-aserve+sqlite3)

(defclass aserve+sqlite3 ()
  ((aserve-handle :initarg :aserve-handle
                  :reader aserve-handle
                  :writer %aserve-handle
                  :documentation "Handle to the front-end server.")))

(defmacro error-protect ((&optional (var (gensym "ERR-"))) form &body on-error)
  "Execute the given FORM.  If any error occurs during that execution,
invoke the ON-ERROR code with the error bound to VAR."
  `(handler-case
       ,form
     (error (,var)
       ,@on-error
       (error ,var))))

(defun start-server (&key (port 80) (db-name #P"./motd.sqlite3"))
  "Start an AllegroServe server on the given PORT using the sqlite3
database DB-NAME on the back end.  Return a handle on which one can
later call #'SHUTDOWN-SERVER."
  (setf motd-server:=motd-db= motd-server-sqlite3:=sqlite3-motd-db=
        motd-server:*motd-db-connection-info* db-name)

  (motd-server:motd-open-database)

  (error-protect ()
      (let ((http-handle (motd-server-aserve:start-server :port port)))
        (error-protect ()
            (make-instance 'aserve+sqlite3
                           :aserve-handle http-handle)
          (motd-server-aserve:shutdown-server http-handle)))
    (motd-server:motd-close-database)))

(defun shutdown-server (handle)
  "Given a HANDLE returned by #'START-SERVER, shutdown the
AllegroServe front-end and the sqlite3 back-end."
  (check-type handle aserve+sqlite3)
  (ignore-errors
    (motd-server-aserve:shutdown-server (aserve-handle handle)))
  (ignore-errors
    (motd-server:motd-close-database))
  (%aserve-handle nil handle))
