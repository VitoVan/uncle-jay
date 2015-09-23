(setf sb-impl::*default-external-format* :UTF-8)
;;(declaim (optimize (debug 3)))
(ql:quickload '(cl-json hunchentoot cl-mongo html-template alexandria))

(in-package :cl-mongo)
(defun sethash(key value hash-table)
  (if (hash-table-p value)
      (dolist (x (alexandria:hash-table-keys value))
        (sethash (concatenate 'string (string key) "." x) (gethash x value) hash-table))
      (progn
        (remhash key hash-table)
        (setf (gethash key hash-table) value)))
  hash-table)

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
(export 'sethash)

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
                                (add-element "bill-no" 0
                                             (add-element "balance" 0
                                                          (add-element "name" name
                                                                       (add-element "cid" cid (make-document)))))))
        "Insert Ok.")))

(defun db-del-customer (cid)
  (db.delete "customer" (db-find-customer :cid cid)))

(defun db-find-bill (customer &key bid)
  (let* ((result (docs (db.find "bill"
                                (kv "customer" customer
                                    (if bid
                                        (kv "bid" bid))) :limit 0))))
    (if bid
        (car result)
        result)))

(defun gen-bid (customer)
  (let* ((bill-no (1+ (or (get-element "bill-no" (db-find-customer :cid customer)) 0))))
    (db.update
     "customer"
     ($ "cid" customer)
     (kv ($set "bill-no" bill-no)))
    bill-no))

(defun cal-balance (customer amount)
  (db.update
     "customer"
     ($ "cid" customer)
     (kv ($inc "balance" amount))))

(defun db-add-bill (customer name amount &key note)
  (let* ((customer-doc (db-find-customer :cid customer))
         (customer-balance (get-element "balance" customer-doc))
         (current-balance (+ customer-balance amount)))
    (db.insert "bill"
               (add-element "cdate" (get-universal-time)
                            (add-element "amount" amount
                                         (add-element "name" name
                                                      (add-element "customer" customer
                                                                   (add-element "balance" current-balance
                                                                                (add-element "note" note
                                                                                             (add-element "bid" (gen-bid customer) (make-document)))))))))
    (cal-balance customer amount)
    "Insert Ok."))

(defun db-discard-bill (bid customer)
  (db.update
   "bill"
   (kv ($ "bid" bid) ($ "customer" customer))
   (kv ($set "discard" t))))

(defun ctl-hello()
  (with-output-to-string (*default-template-output*)
    (fill-and-print-template #p"tmpl/hello.tmpl" '(:name "Vito"))))

(defun ctl-customer()
  (let* ((cid (parameter "id"))
         (customer-result (document->ht (db-find-customer :cid cid)))
         (bill-result-klist (hash-table-klist (list (document->ht (db-find-bill cid)))))
         (customer-result-klist (hash-table-klist
                                 (if cid
                                     (sethash 'customer customer-result (make-hash-table))
                                     (list customer-result))))
         (tmpl-path (if cid #p"tmpl/customer-single.tmpl" #p"tmpl/customer-list.tmpl"))
         (values (cons :customer
                       (cons t
                             (if cid
                                 (append customer-result-klist
                                         (cons :rows bill-result-klist))
                                 (cons :rows customer-result-klist))))))
    (log-message* *log-lisp-warnings-p* (format nil "FFFFFFFFFFFFF::  ~A~%" values))
    (with-output-to-string (*default-template-output*)
      (fill-and-print-template tmpl-path
                               values))))

(defun ctl-add-customer()
  (let* ((cid (parameter "cid"))
         (name (parameter "name")))
    (db-add-customer cid :name name)))

(defun ctl-add-bill()
  (let* ((customer (parameter "customer"))
         (name (parameter "name"))
         (amount (parse-integer (parameter "amount") :junk-allowed t))
         (note (parameter "note")))
    (db-add-bill customer name amount :note note)))

(setf *dispatch-table*
      (list
       (create-regex-dispatcher "^/hello$" 'ctl-hello)
       (create-regex-dispatcher "^/customer$" 'ctl-customer)
       (create-regex-dispatcher "^/customer/add$" 'ctl-add-customer)
       (create-regex-dispatcher "^/bill/add$" 'ctl-add-bill)))

(start-server)
