(in-package :motd-server)

(deftype message-id () '(integer 1 *))
(deftype timestamp () '(integer 0 *))
(deftype language () 'keyword)
(deftype text () 'string)
(deftype tag () 'keyword)

(defclass translation ()
  ((language :initarg :language
             :type language
             :reader language)
   (text :initarg :text
         :type text
         :reader text)))

(defclass message ()
  ((id :initarg :id
       :type message-id
       :reader id)
   (expiration :initarg :expiration
               :type (or null timestamp)
               :reader expiration)
   (translations :initarg :translations
                 ; :type list-of-translations
                 :reader translations)
   (tags :initarg :tags
         ; :type list-of-tags
         :reader tags))
  (:default-initargs :expiration nil)
  (:documentation "This is the base class for a message of the day."))

(defclass published-message (message)
  ((timestamp :initarg :timestamp
              :type timestamp
              :reader timestamp))
  (:documentation "This class represents a published message."))

(defun %translation-to-plist (obj)
  (cons (language obj) (text obj)))

(defun %message-to-plist (obj)
  (append (list 'message)
          (when (slot-boundp obj 'id)
            (list :id (id obj)))
          (when (expiration obj)
            (list :expiration (expiration obj)))
          (list :translations (mapcar #'%translation-to-plist
                                      (translations obj))
                :tags (tags obj))))

(defun %published-message-to-plist (obj)
  (append (list 'published-message)
          (rest (%message-to-plist obj))
          (list :timestamp (timestamp obj))))

(defmethod print-object ((obj translation) stream)
  (if *print-pretty*
      (let ((*package* #.*package*))
        (pprint (%translation-to-plist obj) stream))
      (call-next-method)))

(defmethod print-object ((obj message) stream)
  (if *print-pretty*
      (let ((*package* #.*package*))
        (pprint (%message-to-plist obj) stream))
      (call-next-method)))

(defmethod print-object ((obj published-message) stream)
  (if *print-pretty*
      (let ((*package* #.*package*))
        (pprint (%published-message-to-plist obj) stream))
      (call-next-method)))

(defmethod create-message (translations tags &key id expiration timestamp)
  (flet ((make-translation (entry)
           (destructuring-bind (language . text) entry
             (check-type language language)
             (check-type text text)
             (make-instance 'translation
                            :language language
                            :text text)))
         (make-tag (tag)
           (check-type tag tag)
           tag))
    (check-type id (or null message-id))
    (check-type expiration (or null timestamp))
    (check-type timestamp (or null timestamp))

    (apply #'make-instance
           (if timestamp
               'published-message
               'message)
           :translations (mapcar #'make-translation translations)
           :tags (mapcar #'make-tag tags)
           (append (when expiration
                     (list :expiration expiration))
                   (when timestamp
                        (list :timestamp timestamp))
                   (when id
                     (list :id id))))))
