; -------------------------------------------------------------------------
; Managing csv files
; -------------------------------------------------------------------------

(define (db-load, f1 str lst)
    (setq lst '())
    (setq f1 (open "file.csv" "read"))

    (while (setq str (read-line f1))
            (setq lst (cons    (parse str ",") lst))
          ;(write-line)

    );while
    (close f1)
    (reverse lst)
)
 


(define (db-fields lst)
    (nth 0 lst)
)



(define (db-tell lst id, i l aux-lst result)
    (setq i 0)
    (setq l (length lst))

    (while (< i l)
        (setq aux-lst (nth i lst))
            (if (= id (first aux-lst))
                (setq result aux-lst)
            )
            (setq i (+ 1 i))
    );end while
    result
)



(define (db-tell2 lst id a-field, i l aux-lst result position)
    (setq i 0)
    (setq l (length lst))

    (while (< i l)
        (setq aux-lst (nth i lst))
            (if (= id (first aux-lst))
                (setq result aux-lst)
            )
            (setq i (+ 1 i))
    );end while
    ;(cons result (find a-field (nth 0 lst)))
    (setq position (find a-field (nth 0 lst)))
    (nth position result) ;index
)



(define (db-new-field lst name-field, i l lst-out)
    (setq lst-out '())
    (setq l (length lst)) ;number of records in lst

    ;in the following line we append the new field's name
    (setq lst-out (cons    (append (nth 0 lst) (list name-field)) lst-out))

    ;then we copy the entire database, making space in every record
    ;for the new field

    (setq i 1)
    (while (< i l)
        (setq lst-out (cons (append (nth i lst) (list "")) lst-out))
        (setq i (+ 1 i))
    );while

    (reverse lst-out)
);end function


(define (db-get-row-number db key, i length1 row-number)
    (setq i 0)
    (setq length1 (length db))

    (while (< i length1)
        (if (= key (first (nth i db)))
            (setq row-number i)
        )
        (setq i (+ 1 i))
    );while end
    row-number
)



;This is an auxiliary function to (db-update).
(define (db-update-record db record field new-data, position     i l record-out)
   (setq record-out '())
   (setq i 0)
   (setq l (length record))
   ;gets the position of the field looking in the first record:
   (setq position (find field (nth 0 db)))
   ;replaces the data:

   (while (< i l)
    (if (= i position)
       ;if it evaluates to true
         (setq record-out (cons new-data record-out))
       ;else copy the element
        (setq record-out (cons (nth i record) record-out))
    );end if
    (setq i (+ 1 i))
   )
   (reverse record-out)
)



(define (db-update db key field new-data, row-number i length1 db-out)
   (setq row-number (db-get-row-number db key)) ;get the row index
   (setq i 0)
   (setq db-out '())
   (setq length1 (length db)) ;number of records in db

   ;copy the first set of records
   (while (< i row-number)
        (setq db-out (cons (nth i db) db-out))
        (setq i (+ 1 i))
   )
    
   ;update the record:
   (setq db-out (cons (db-update-record db (nth row-number db) field new-data) db-out))
   (setq i (+ 1 i)); advances one row

   ;copy the rest of records
   (while (< i length1)
        (setq db-out (cons (nth i db) db-out))
        (setq i (+ 1 i))
   )
   (reverse db-out)
)


(define (db-filter db expr, i length1 header record field-id lst-out str)
   (setq i 1)
   (setq lst-out '())
   (setq lst-out (cons (nth 0 db) lst-out))
   (setq length1 (length db))
   (setq header (first db));name of fields
   (setq field-id (find (nth 1 expr) header)) ;field index

   (while (< i length1)
      (setq record (nth i db));current record
      (setq str (nth field-id record))
      ;if (last expr) is a number, convert str into number:
      (if (number? (last expr))
         (setq str (float str))
      );if
    
      (if (eval (list (eval '(first expr)) str (last expr)))
         (setq lst-out (cons record lst-out))
      );if
    (setq i (+ 1 i))
   );while
   (reverse lst-out)
)



(define (db-stats db field, header field-id i lst length1)
   (setq header (first db));get name of fields
   (setq field-id (find field header));find position of field
   (setq lst '())
   (setq i 1)
   (setq length1 (length db))

   (while (< i length1)
            (setq lst (cons (eval-string (nth field-id (nth i db))) lst))
            (setq i (+ 1 i))
   );while
   (stats (reverse lst)) ;return the statistics
)




(define (db-save db, i j length2 lenght2 record)
    (setq f1 (open "/Users/research/Desktop/Out.csv" "write"))
    (println "saving database.....")

    (setq j 0)
    (setq length1 (length db)) ;number of records in db

    (while (< j length1)
        (setq record (nth j db)) ;loads a record
        (setq length2 (length record)) ;number of fields in the record
        (setq i 0)

    ;now, for each record:
        (while (< i length2)
              (write f1 (nth i record))
                (if (< i (- length2 1))
                     (write f1 ",")
                );end if
                (setq i (+ 1 i))
        );internal while
 
        (write f1 "\n")
        (setq j (+ 1 j))
    );enf while j
    (close f1)
    (println "Database saved Ok")
    (println j " records sucessfully saved.")
    true
)
  

(define (ddb-tell db id)
    (assoc id db)
)


;code 4-18b, a shorter, more elegant version than code 4-12
(define (ddb-tell2 db id a-field)
    (nth (find a-field (first db)) (assoc id db))
)


(define (db-menu)
    (print "\nCSV database manager\n\n")
    (print "1. Load a database into memory\n")
    (print "2. Get the name of fields in a database\n")
    (print "3. Query the database\n")
    (print "4. Add a new field to the database\n")
    (print "5. Update the database\n")
    (print "6. Filter the database\n")
    (print "7. Calculate statistics\n")
    (print "8. Save the database\n")
    (print "\nChoose an option (1-8): ")
    (read-line)
)

;From the book "A Practical Introduction to Fuzzy Logic using Lisp
;(C) Luis Argüelles Mendez, 2015
