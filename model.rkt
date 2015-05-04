#lang racket/base

(require racket/list
         racket/local
         db)

(require "crc32.rkt")

; Create structures to store entries as database rows
(struct entries (db))
(struct entry (table id))

; initialize-chart!: directory -> entries
; Initializes the db, and returns the entries structure
(define (initialize-chart! home)
  (define db (sqlite3-connect #:database home #:mode 'create))
  (define the-chart (entries db))
  (unless (table-exists? db "entries")
    (query-exec db
                (string-append
                 "CREATE TABLE entries "
                 "(id INTEGER PRIMARY KEY, title TEXT, ideal TEXT, real TEXT, "
                 "created_date TEXT, due_date TEXT, pid INTEGER, ord INTEGER)"))
    (entries-insert-entry!
     the-chart "Top-level Chart" "Ideal" "Real"
     (current-seconds) "NULL" "NULL" "NULL"))
  the-chart)

; entry-insert-entry!: entries string*6 -> void
; Adds entry to entries
(define (entries-insert-entry! a-entries t i r cd dd pid ord)
  (query-exec
   (entries-db a-entries)
   (string-append
    "INSERT INTO entries "
    "(title, ideal, real, created_date, due_date, pid, ord) "
    "VALUES (?, ?, ?, ?, ?, ?, ?)")
   t i r cd dd pid ord))

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
                           #:pid [pid ""])
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
                 (string-append "pid=\"" pid "\", "))))]
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

; entries-entries : entries -> (listof entry)
; returns (listof entry) in the table a-entries
(define (entries-entries a-entries)
  (local [(define (id->entry an-id)
            (entry a-entries an-id))]
    (map id->entry
         (query-list
          (entries-db a-entries)
          "SELECT id FROM entries"))))

; entries-query : entries id -> entry
; returns entry or #false if not found
(define (entries-query a-entries a-id)
  (local [(define (id->entry an-id)
            (when an-id
              (entry a-entries an-id)))]
    (id->entry
     (query-maybe-value
      (entries-db a-entries)
      "SELECT id FROM entries WHERE id=?"
      a-id))))

; entries-top : entries -> entry
; returns top entry by selecting for MIN(id)
(define (entries-top a-entries)
  (local [(define (id->entry an-id)
            (entry a-entries an-id))]
    (id->entry
     (query-value
      (entries-db a-entries)
      "SELECT MIN(id) FROM entries"))))

; entries-latest : entries -> entry
; returns latest entry by selecting for MAX(id)
(define (entries-latest a-entries)
  (local [(define (id->entry an-id)
            (entry a-entries an-id))]
    (id->entry
     (query-value
      (entries-db a-entries)
      "SELECT MAX(id) FROM entries"))))

; entry-title: entry -> string?
; Queries for entry title
(define (entry-title a-entry)
  (query-value
   (entries-db (entry-table a-entry))
   "SELECT title FROM entries WHERE id = ?"
   (entry-id a-entry)))

; entry-real: entry -> string?
; Queries for entry real text
(define (entry-real a-entry)
  (query-value
   (entries-db (entry-table a-entry))
   "SELECT real FROM entries WHERE id = ?"
   (entry-id a-entry)))

; entry-ideal: entry -> string?
; Queries for entry ideal text
(define (entry-ideal a-entry)
  (query-value
   (entries-db (entry-table a-entry))
   "SELECT ideal FROM entries WHERE id = ?"
   (entry-id a-entry)))

; entry-created: entry -> string?
; Queries for entry creation date
(define (entry-created a-entry)
  (query-value
   (entries-db (entry-table a-entry))
   "SELECT created_date FROM entries WHERE id = ?"
   (entry-id a-entry)))

; entry-due: entry -> string?
; Queries for entry due date
(define (entry-due-date a-entry)
  (query-value
   (entries-db (entry-table a-entry))
   "SELECT due_date FROM entries WHERE id = ?"
   (entry-id a-entry)))

; entry-pid: entry -> number
; Queries for entry parent-id
(define (entry-pid a-entry)
  (query-value
   (entries-db (entry-table a-entry))
   "SELECT pid FROM entries WHERE id = ?"
   (entry-id a-entry)))

; entry-ord: entry -> number
; Queries for entry order
(define (entry-ord a-entry)
  (query-value
   (entries-db (entry-table a-entry))
   "SELECT ord FROM entries where id = ?"
   (entry-id a-entry)))

; entry-parent: entry -> entry
; Queries for entry parent
(define (entry-parent a-entry)
  (local [(define a-entries (entry-table a-entry))]
    (entry a-entries
     (query-value
      (entries-db a-entries)
      "SELECT pid FROM entries WHERE id = ?"
      (entry-id a-entry)))))

; entry-children : entry -> (listof step)
; returns child entries (respresented as step)
(define (entry-children-order a-entry a-order)
  (local [(define a-entries (entry-table a-entry))
          (define (id->entry an-id)
            (entry a-entries an-id))]
    (map id->entry
         (query-list
          (entries-db a-entries)
          (string-append
           "SELECT id FROM entries WHERE pid = ?1 ORDER BY ord "
           a-order)
          (entry-id a-entry)))))
(define (entry-children a-entry)
  (entry-children-order a-entry "DESC"))

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

(provide (all-defined-out))
