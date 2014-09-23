(in-package #:motd-server-sqlite3)


(defun replace-all (string old new)
  (map 'string
       (lambda (ch)
         (if (char= ch old)
             new
             ch))
       string))

(defun %column-name (keyword)
  (intern (replace-all (string-downcase
                        (symbol-name keyword))
                       #\-
                       #\_)
          :keyword))
