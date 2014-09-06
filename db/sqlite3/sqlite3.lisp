(in-package #:motd-server-sqlite3)

(defclass sqlite3-motd-db (motd-server:motd-db)
  ((db-handle :reader db-handle
              :writer %db-handle
              :documentation "Handle to the sqlite3 database connection.")
   (retrieve-after :reader retrieve-after
                   :writer %retrieve-after
                   :documentation "Prepared statement for retrieving
                   all messages newer than a given message.")
   (retrieve-recent :reader retrieve-recent
                   :writer %retrieve-recent
                   :documentation "Prepared statement for retrieving
                   the most recent messages.")
   (retrieve-proposed :reader retrieve-proposed
                   :writer %retrieve-proposed
                   :documentation "Prepared statement for retrieving
                   the proposed messages.")
   (retrieve-tags :reader retrieve-tags
                  :writer %retrieve-tags
                  :documentation "Prepared statement for retrieving
                  all of the tags currently in use.")
   (retrieve-public-key :reader retrieve-public-key
                  :writer %retrieve-public-key
                  :documentation "Prepared statement for retrieving
                  a user's public key."))
  (:documentation "Sqlite3 subclass of MOTD-DB."))

(defmethod initialize-instance :after ((db sqlite3-motd-db)
                                       &key
                                         (db-name #P"./motd.sqlite3")
                                         &allow-other-keys)
  (let ((handle (dbi:connect :sqlite3
                             :database-name db-name)))
    ;; TODO: figure out how to make sure this is serialized
    ;; sqlite3 support two types of concurrency... serialized is the one
    ;; where multiple threads can write to the same database handle.
    (%db-handle handle db)
    (ensure-schema-loaded db)
    (%retrieve-after (dbi:prepare handle +retrieve-after-statement+) db)
    (%retrieve-recent (dbi:prepare handle +retrieve-recent-statement+) db)
    (%retrieve-proposed (dbi:prepare handle +retrieve-proposed-statement+) db)
    (%retrieve-tags (dbi:prepare handle +retrieve-tags-statement+) db)
    (%retrieve-public-key (dbi:prepare handle +retrieve-public-key-statement+)
                          db)))

(defun open-sqlite3-motd-database (db-name)
  "Create a handle to the sqlite3 database DB-NAME."
  (make-instance 'sqlite3-motd-db :db-name db-name))

(defun close-sqlite3-motd-database (db)
  "Disconnect from the sqlite3 database with the handle DB."
  (check-type db sqlite3-motd-db)
  (dbi:disconnect (db-handle db))
  (slot-makunbound db 'db-handle))

(defparameter *schema*
  (list
   "PRAGMA encoding = \"UTF-8\""

   "CREATE TABLE IF NOT EXISTS messages (
      id INTEGER PRIMARY KEY ASC AUTOINCREMENT,
      expiration INTEGER
    )"

   "CREATE TABLE IF NOT EXISTS published_messages (
      message_id INTEGER REFERENCES messages (id) ON DELETE CASCADE,
      timestamp INTEGER
    )"

   "CREATE TABLE IF NOT EXISTS message_translations (
      message_id INTEGER REFERENCES messages (id) ON DELETE CASCADE,
      language TEXT,
      text TEXT,

      CONSTRAINT unique_translation UNIQUE (message_id, language)
        ON CONFLICT REPLACE
    )"

   "CREATE TABLE IF NOT EXISTS message_tags (
      message_id INTEGER REFERENCES messages (id) ON DELETE CASCADE,
      tag TEXT,

      CONSTRAINT unique_tag UNIQUE (message_id, tag)
        ON CONFLICT IGNORE
    )"

   "CREATE TABLE IF NOT EXISTS public_keys (
      user_name TEXT PRIMARY KEY ON CONFLICT REPLACE,
      public_key TEXT
    )"))

(defun ensure-schema-loaded (db)
  (let ((handle (db-handle db)))
    (dolist (cmd *schema*)
      (dbi:execute (dbi:prepare handle cmd)))))

(defparameter *canned-data-inserts*
  (list
   "INSERT INTO messages (id) VALUES
      (1),
      (2),
      (3),
      (4),
      (5),
      (6),
      (7)"

   "INSERT INTO published_messages (message_id,timestamp) VALUES
      (1,1),
      (2,2),
      (3,3),
      (4,5),
      (5,4)"

   "INSERT INTO message_translations (message_id, language, text) VALUES
      (1, \"EN\", \"Message 1\"),
      (2, \"EN\", \"Message 2\"),
      (2, \"FR\", \"Le Message Deux\"),
      (2, \"ES\", \"El Message Dos\"),
      (3, \"EN\", \"  July 2014 Quicklisp dist update now available
    http://tinyurl.com/quicklisp-august-2014\"),
      (3, \"FR\",
\"  Juillet 2014 Quicklisp mise à jour de la distribution maintenant disponible
    http://tinyurl.com/quicklisp-august-2014\"),
      (4, \"EN\", \"  ILC 2014 -- Montreal, Canada -- August 15-17\"),
      (5, \"EN\", \"  August 2014 Quicklisp dist update now available
    http://tinyurl.com/quicklisp-august-2014\"),
      (6, \"EN\", \"Message 6\"),
      (7, \"EN\", \"Message 7\")"

   "INSERT INTO message_tags (message_id, tag) VALUES
      (1, \"COMMON-LISP\"),
      (2, \"EMACS-LISP\"),
      (3, \"COMMON-LISP\"),
      (3, \"QUICKLISP\"),
      (4, \"COMMON-LISP\"),
      (4, \"ILC\"),
      (5, \"COMMON-LISP\"),
      (5, \"QUICKLISP\"),
      (7, \"COMMON-LISP\")"

   "INSERT INTO public_keys (user_name, public_key) VALUES
      (\"pat\",
       \"(MOTD-COMMANDS:DSA-PUBLIC-KEY 4757122097207085360194790135388628344037590339624687611188205740375433211746300182223067451208003176777099298437752863557940066182226601493689129574019805097929522688435386028990253012956286635613976197584644802760493436125549084105322284102931234006079275991498561901742690235792527742691425854270070791275421061644209814483251439474240172200029626113620051384972548286997895158383277616498607891029330513173065644551048402660554459931045975363740288260820146838784682137002402668265843527291204911402754420034748717479750107254915011815880851605008803971822745134489536312902577050849584248357010225069765331938764069054813991454142641384159333058644632377323742730825234744874632110958039152177773485730618553221403393269986426386412734530137798193380337318336585099563527287139491036654463702611521703164226331424155926636794184161820709824223600865184642318431749638680497131619631728705493444852783797432370230527776929
183845405541294355033399488123604697155848585171770237387725318106434479566687
4415508950929932471733690400981319962016565145590288545210424446085535898838076269798788899392281731752333952941797578054480668249931614680709726117478402550706899966357544435999355165037093047987410067273222042320328889792105954630462417590652889772630284337954739060243133797636948376639204582594874374320262712636274763329167404076546126231555004493292494077912888401910855163032205832296002306712394312181209766862117076511273280013062578446631411752907046335355466979609760371203175869596666026965082775641241066586963266243593427439208031836385060262467975663423514152948058188338456086233701808929283524823083936069324893317483922104100119402749863734336188944606096235317761535712398126528142921798958212569821506725022817144345058380910587324814117422327632312676594978814526573768938229262941159507164922368443163763421395638948240060241984243370111626463646789194928738754645889623309558524062460444278994708645436
1329293958926829950523980732590791541374708565185513561638645722595043957359807207394595396628406523964032097562419545882472615977587265340567817185021980640667607077984967075486965991836303385949784006395769923180850309301578883430850647438576857275394231681848291429666272100879087441966848742522243321639451727716636919861718177597255149961174925113669823791580018165657707646098999428435163940897525056136478361795699546062211889844719582368441169317009817928201978235715853206720634895559127870453659913739681340928930051403337630342177100984762218536513103716225993893822372062655099113909836141936398151262877688410592073100319178471568784401895007296240055345069365383043098828198650531174787088663723867112955666947068608626799671489607353306564512280332748752660121063942859665336184014330970409576370494364670401818284873308761931136750128899866676409633779345241566379261979797351207718654368333598488110977576279)\")"))

(defun ensure-canned-data-loaded (db)
  (let ((handle (db-handle db)))
    (dolist (insert *canned-data-inserts*)
      (dbi:execute (dbi:prepare handle insert)))))
