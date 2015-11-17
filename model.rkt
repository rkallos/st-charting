#lang racket/base

(require racket/list
         racket/local
         db)

(require "functions.rkt" "crc32.rkt")

; Create structures to store entries as database rows
(struct entries (db))
(struct entry
  (table id title ideal real created due-date pid ord done?)
  #:transparent
  #:mutable)
(struct user (table id))

; initialize-chart!: directory -> entries
; Initializes the db, and returns the entries structure
(define (initialize-chart! home)
  (define db (sqlite3-connect #:database home #:mode 'create))
  (define the-chart (entries db))
  (unless (table-exists? db "entries")
    (begin
      (query-exec db
                  (string-append
                   "CREATE TABLE entries "
                   "(id INTEGER PRIMARY KEY, title TEXT, ideal TEXT, real TEXT, "
                   "created_date TEXT, due_date TEXT, pid INTEGER, ord INTEGER, done INTEGER)"
                   ))
      (query-exec db
                  (string-append
                   "CREATE TABLE users "
                   "(id INTEGER PRIMARY KEY, name TEXT, email TEXT, password TEXT,"
                   "created_date TEXT, active BOOLEAN, root_id INTEGER, FOREIGN KEY(root_id) REFERENCES entries(id))"
                   )))
    (entries-insert-entry!
     the-chart "Top-level Chart" "Ideal" "Real"
     (current-seconds) "NULL" "NULL" "NULL"))
  the-chart)

(define (entry-tuple->entry a-entry-tuple id t i r c dd p o d)
  (entry (entry-table a-entry-tuple)
         id t i r c dd p o d))

; fetch-entry: entry-tuple -> entry or #f
; Fetches entire row from database
(define (fetch-entry a-table a-id)
  (define result
    (query-maybe-row
     (entries-db a-table)
     "SELECT * FROM entries WHERE id=?"
     a-id))
  (if result
      (apply entry
             (cons a-table (vector->list result)))
      #f))

(define (entry-parent a-entry)
  (fetch-entry (entry-table a-entry) (entry-pid a-entry)))

; entry-insert-entry!: entries string*6 -> void
; Adds entry to entries
(define (entries-insert-entry! a-table t i r cd dd pid ord)
  (query-exec
   (entries-db a-table)
   (string-append
    "INSERT INTO entries "
    "(title, ideal, real, created_date, due_date, pid, ord, done) "
    "VALUES (?, ?, ?, ?, ?, ?, ?, ?)")
   t i r cd dd pid ord "0"))

;; entry-delete-entry! entry -> void
;; Deletes an entry and all its children
(define (entries-delete-entry! a-entry)
  (let ([table (entries-db (entry-table a-entry))]
        [id (entry-id a-entry)])
    (query-exec
     table
     "DELETE FROM entries WHERE pid=?"
     id)
    (query-exec
     table
     "DELETE FROM entries WHERE id=?"
     id)))

; entry-edit-entry!: id -> void
; edits entry at id
(define (entry-edit-entry! a-entry
                           #:title [title ""]
                           #:ideal [ideal ""]
                           #:real [real ""]
                           #:due_date [due_date ""]
                           #:pid [pid ""]
                           #:ord [ord ""]
                           #:done [done 0])
  (local [; cmd: The SQL query to update an entry, with a conditional number of columns to update.
          (define cmd
            (string-append
             "UPDATE entries SET "
             (if (string=? title "")
                 ""
                 (string-append "title=\"" title "\", "))
             (if (string=? ideal "")
                 ""
                 (string-append "ideal=\"" ideal "\", "))
             (if (string=? real "")
                 ""
                 (string-append "real=\"" real "\", "))
             (if (string=? due_date "")
                 ""
                 (string-append "due_date=\"" due_date "\", "))
             (if (string=? pid "")
                 ""
                 (string-append "pid=\"" pid "\", "))
             (if (string=? ord "")
                 ""
                 (string-append "ord=\"" ord "\", "))
             (if (or (= done 1) (= done 0))
                 ""
                 (string-append "done=\"" (string->number done) "\", "))))]
    (query-exec
     (entries-db (entry-table a-entry))
     ; Check for and remove trailing comma from cmd
     (string-append
      (if (string=? ", "
                    (substring cmd (- (string-length cmd) 2)))
          (substring cmd 0 (- (string-length cmd) 2))
          cmd)
      "WHERE id=?")
     (entry-id a-entry))))

(define (entry-set-title! a-entry a-val)
  (entry-edit-entry! a-entry
                     #:title a-val))

(define (entry-set-ideal! a-entry a-val)
  (entry-edit-entry! a-entry
                     #:ideal a-val))

(define (entry-set-real! a-entry a-val)
  (entry-edit-entry! a-entry
                     #:real a-val))

(define (entry-set-due-date! a-entry a-val)
  (entry-edit-entry! a-entry
                     #:due_date a-val))

(define (entry-set-pid! a-entry a-val)
  (entry-edit-entry! a-entry
                     #:pid a-val))

(define (entry-set-ord! a-entry a-val)
  (entry-edit-entry! a-entry
                     #:ord a-val))

(define (entry-set-done! a-entry)
  (entry-edit-entry! a-entry
                     #:done 1))

(define (entry-set-undone! a-entry)
  (entry-edit-entry! a-entry
                     #:done 0))

; entries-entries : entries -> (listof entry)
; returns (listof entry) in the table a-entries
(define (fetch-all-entries a-table)
  (map (lambda (row)
         (apply entry (cons a-table (vector->list row))))
       (query-rows
        (entries-db a-table)
        "SELECT * from entries")))

(define (fetch-all-ids a-table)
  (query-list
   (entries-db a-table)
   "SELECT id from entries"))

; entries-top : entries -> entry
; returns top entry by selecting for MIN(id)
(define (entries-top a-table)
  (define min-id
    (query-value
     (entries-db a-table)
     "SELECT MIN(id) FROM entries"))
  (fetch-entry a-table min-id))

; entries-latest : entries -> entry
; returns latest entry by selecting for MAX(id)
(define (entries-latest a-table)
  (define min-id
    (query-value
     (entries-db a-table)
     "SELECT MAX(id) FROM entries"))
  (fetch-entry a-table min-id))

; entry-title: entry -> string?
; Queries for entry title
(define (entry-title/db a-entry)
  (query-value
   (entries-db (entry-table a-entry))
   "SELECT title FROM entries WHERE id = ?"
   (entry-id a-entry)))

; entry-real: entry -> string?
; Queries for entry real text
(define (entry-real/db a-entry)
  (query-value
   (entries-db (entry-table a-entry))
   "SELECT real FROM entries WHERE id = ?"
   (entry-id a-entry)))

; entry-ideal: entry -> string?
; Queries for entry ideal text
(define (entry-ideal/db a-entry)
  (query-value
   (entries-db (entry-table a-entry))
   "SELECT ideal FROM entries WHERE id = ?"
   (entry-id a-entry)))

; entry-created: entry -> string?
; Queries for entry creation date
(define (entry-created/db a-entry)
  (query-value
   (entries-db (entry-table a-entry))
   "SELECT created_date FROM entries WHERE id = ?"
   (entry-id a-entry)))

; entry-due: entry -> string?
; Queries for entry due date
(define (entry-due-date/db a-entry)
  (query-value
   (entries-db (entry-table a-entry))
   "SELECT due_date FROM entries WHERE id = ?"
   (entry-id a-entry)))

; entry-pid: entry -> number
; Queries for entry parent-id
(define (entry-pid/db a-entry)
  (query-value
   (entries-db (entry-table a-entry))
   "SELECT pid FROM entries WHERE id = ?"
   (entry-id a-entry)))

; entry-ord: entry -> number
; Queries for entry order
(define (entry-ord/db a-entry)
  (query-value
   (entries-db (entry-table a-entry))
   "SELECT ord FROM entries where id = ?"
   (entry-id a-entry)))

(define (entry-done?/db a-entry)
  (= 1
     (query-value
      (entries-db (entry-table a-entry))
      "SELECT done FROM entries where id = ?"
      (entry-id a-entry))))

; entry-children : entry -> (listof step)
; returns child entries (respresented as step)
#;
(define (entry-children-order a-entry a-order)
  (local [(define a-entries (entry-table a-entry))
          (define (id->entry an-id)
            (entry-tuple a-entries an-id))]
    (map id->entry
         (query-list
          (entries-db a-entries)
          (string-append
           "SELECT id FROM entries WHERE pid = ?1 ORDER BY ord "
           a-order)
          (entry-id a-entry)))))
#;
(define (entry-children a-entry)
  (entry-children-order a-entry "DESC"))

(define (entry-children a-entry)
  (map (lambda (row)
         (apply entry (cons (entry-table a-entry) (vector->list row))))
       (query-rows
        (entries-db (entry-table a-entry))
        "SELECT * FROM entries WHERE pid = ? ORDER BY ord"
        (entry-id a-entry))))

; next-in-order : entry -> integer
; Returns max(ord) from table where pid = (entry-id a-entry)
(define (next-in-order a-entry)
  (let ([v (query-value
            (entries-db (entry-table a-entry))
            "SELECT max(ord) FROM entries WHERE pid = ?"
            (entry-id a-entry))])
    (if (integer? v)
        (add1 v)
        1)))

; entry-crc : entry -> string
(define (entry-crc a-entry)
  (string->crc32/hex (number->string (entry-id a-entry))))

(define (id-crc a-id)
  (string->crc32/hex (number->string a-id)))

; create-root-node : entries user -> void
; Create root entry, assign it to user. Done on user creation
(define (create-root-node a-entries a-user)
  (begin (entries-insert-entry!
          a-entries
          "Your first entry"
          "Enter your ideal state here"
          "Enter your current state here"
          (get-date)
          "NULL"
          "NULL"
          1)))


(provide (all-defined-out))
