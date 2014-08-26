(in-package :motd-server)

(defparameter *all-motds*
  '((message 5 (:common-lisp))
    (message 4 (:common-lisp))
    (message 3 (:emacs-lisp))
    (message 2 (:common-lisp))
    (message 1 (:ilc))))

(defparameter *proposed-motds*
  '((message 7)
    (message 6)))

(defconstant +how-many-to-retrieve-limit+ 4)

(defmethod get-all-motds-after (message-id)
  (let ((pos (position message-id *all-motds* :key #'second)))
    (get-most-recent-motds (min (or pos +how-many-to-retrieve-limit+)
                                +how-many-to-retrieve-limit+))))

(defmethod get-most-recent-motds ((how-many-to-retrieve integer))
  (let ((how-many-to-retrieve (min how-many-to-retrieve
                                   +how-many-to-retrieve-limit+
                                   (length *all-motds*))))
    (subseq *all-motds* 0 how-many-to-retrieve)))

(defmethod get-all-proposed-messages ()
  *proposed-motds*)

(defmethod get-all-tags ()
  (sort (remove-duplicates (loop :for motd :in *all-motds*
                              :appending (third motd)))
        #'string<
        :key #'symbol-name))
