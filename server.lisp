(setf sb-impl::*default-external-format* :UTF-8)
;;(declaim (optimize (debug 3)))
(ql:quickload '(cl-json hunchentoot cl-mongo html-template alexandria))

(in-package :cl-mongo)
(defmacro sethash(key value hash-table)
  `(setf (gethash ,key ,hash-table) ,value))

(defun document->ht (doc/docs)
  "Convert a document to a hash-table, recursively."
  (cond
    ((equal (type-of doc/docs) 'document)
     (let* ((ht (slot-value doc/docs 'cl-mongo::elements))
            (keys (alexandria:hash-table-keys ht)))
       (dolist (x keys)
         (sethash x (document->ht (gethash x ht)) ht))
       ht))
    ((listp doc/docs)
     (mapcar #'document->ht doc/docs))
    ((equal (type-of doc/docs) 'cl-mongo::bson-time)
     (bson-time-to-ut doc/docs))
    (t doc/docs)))
(export 'document->ht)

(in-package :alexandria)
(defun hash-table-klist (table/table-list &optional parent)
  (cond
    ((hash-table-p table/table-list)
     (let* ((klist nil))
       (dolist (x (hash-table-keys table/table-list))
         (push
          (hash-table-klist (gethash x table/table-list) x) klist)
         (push
          (intern
           (string-upcase (if parent (concatenate 'string (string parent) "." x) x))
           "KEYWORD") klist))
       klist))
    ((listp table/table-list)
     (mapcar #'(lambda (ht) (hash-table-klist ht parent)) table/table-list))
    (t table/table-list)))
(export 'hash-table-klist)

(defpackage uncle-jay
  (:use :cl :json :hunchentoot :cl-mongo :html-template :alexandria))
(in-package :uncle-jay)

;;init db
(db.use "uncle-jay")

;; Start Hunchentoot
(setf *show-lisp-errors-p* t)
(setf *acceptor* (make-instance 'hunchentoot:easy-acceptor
                                :port 5000
                                :access-log-destination "log/access.log"
                                :message-log-destination "log/message.log"
                                :error-template-directory  "www/errors/"
                                :document-root "www/"))
;;no cache
(setf *force-default* t)

(defun start-server ()
  (start *acceptor*)
  (format t "Server started at 5000"))

(defun db-find-customer (&key cid)
  (let* ((result (docs (db.find "customer"
                                (if cid
                                    (kv "cid" cid)
                                    :all)))))
    (if cid
        (car result)
        result)))

(defun db-add-customer (cid &key name)
  (if (db-find-customer :cid cid)
      "Already Exists."
      (progn
        (db.insert "customer"
                   (add-element "cdate" (get-universal-time)
                                (add-element "balance" 0
                                             (add-element "name" name
                                                          (add-element "cid" cid (make-document))))))
        "Insert Ok.")))

(defun db-del-customer (cid)
  (db.delete "customer" (db-find-customer :cid cid)))

(defun controller-hello()
  (with-output-to-string (*default-template-output*)
    (fill-and-print-template #p"tmpl/hello.tmpl" '(:name "Vito"))))

(defun controller-customer()
  (with-output-to-string (*default-template-output*)
    (fill-and-print-template #p"tmpl/customer.tmpl"
                             `(:customer t
                                         :rows ,(hash-table-klist (document->ht (db-find-customer)))))))

(setf *dispatch-table*
      (list
       (create-regex-dispatcher "^/hello$" 'controller-hello)
       (create-regex-dispatcher "^/customer$" 'controller-customer)))

(start-server)
