(define NULL 'null)

;====================================
;=            Cerința 1             =
;= Definirea elementelor de control =
;=          20 de puncte            =
;====================================

;= Funcțiile de acces
(define init-database
  (λ ()
    '()))

(define create-table
  (λ (table columns-name)
    (cons table 
          (let columns-list((columns columns-name))
            (if (null? columns)
                null
                (cons (list (car columns)) (columns-list (cdr columns))))))))

(define get-name
  (λ (table)
    (car table)))

(define get-columns
  (λ (table)
    (let columns-name-list((columns (cdr table)))
      (if (null? columns)
          null
          (cons (caar columns) (columns-name-list (cdr columns)))))))

(define get-tables
  (λ (db)
    db))

(define get-table
  (λ (db table-name)
    (car (filter (λ (lst)
              (if (equal? (get-name lst) table-name)
                  #t
                  #f)) db))))

(define add-table
  (λ (db table)
    (if (null? db)
        (list table)
        (append db (list table)))))

(define remove-table
  (λ (db table-name)
    (filter (λ(lst)
              (if (string=? (get-name lst) table-name)
                 #f
                 #t)) db)))

;= Pentru testare, va trebui să definiți o bază de date (având identificatorul db) cu următoarele tabele

;============================================================================================
;=                         Tabela Studenți                                                   =
;= +----------------+-----------+---------+-------+-------+                                  =
;= | Număr matricol |   Nume    | Prenume | Grupă | Medie |                                  =
;= +----------------+-----------+---------+-------+-------+                                  =
;= |            123 | Ionescu   | Gigel   | 321CA |  9.82 |                                  =
;= |            124 | Popescu   | Maria   | 321CB |  9.91 |                                  =
;= |            125 | Popa      | Ionel   | 321CC |  9.99 |                                  =
;= |            126 | Georgescu | Ioana   | 321CD |  9.87 |                                  =
;= +----------------+-----------+---------+-------+-------+                                  =
;=                                                                                           =
;=                                         Tabela Cursuri                                    =
;= +------+----------+-----------------------------------+---------------+------------+      =
;= | Anul | Semestru |            Disciplină             | Număr credite | Număr teme |      =
;= +------+----------+-----------------------------------+---------------+------------+      =
;= | I    | I        | Programarea calculatoarelor       |             5 |          2 |      =
;= | II   | II       | Paradigme de programare           |             6 |          3 |      =
;= | III  | I        | Algoritmi paraleli și distribuiți |             5 |          3 |      =
;= | IV   | I        | Inteligență artificială           |             6 |          3 |      =
;= | I    | II       | Structuri de date                 |             5 |          3 |      =
;= | III  | II       | Baze de date                      |             5 |          0 |      =
;= +------+----------+-----------------------------------+---------------+------------+      =
;============================================================================================

(define db (list
  (list "Studenți"
        (list "Număr matricol" 123 124 125 126)
        (list "Nume" "Ionescu" "Popescu" "Popa" "Georgescu")
        (list "Prenume" "Gigel" "Maria" "Ionel" "Ioana")
        (list "Grupă" "321CA" "321CB" "321CC" "321CD")
        (list "Medie" 9.82 9.91 9.99 9.87)
     )
  (list "Cursuri"
	(list "Anul" "I" "II" "III" "IV" "I" "III")
        (list "Semestru" "I" "II" "I" "I" "II" "II")
        (list "Disciplină" "Programarea calculatoarelor" "Paradigme de programare" "Algoritmi paraleli și distribuiți" "Inteligență artificială" "Structuri de date" "Baze de date")
        (list "Număr credite" 5 6 5 6 5 5)
        (list "Număr teme" 2 3 3 3 3 0)
     )
))

;====================================
;=            Cerința 2             =
;=         Operația insert          =
;=            10 puncte             =
;====================================
(define insert
  (λ (db table-name record)
    (let-values (((db1 db2) (splitf-at db (λ (table)
                                             (if (string=? (get-name table) table-name)
                                                 #f
                                                 #t)))))
    (append db1 (list (let get-new-table((columns (cdr (get-table db table-name))) (new-table (list table-name)))
                        (if (null? columns)
                            new-table
                            (get-new-table (cdr columns) (append new-table (list (append (car columns) (list (let insert-record((find-record record) (column-name (caar columns)))
                                                                                                               (if (null? find-record)
                                                                                                                   NULL
                                                                                                                   (if (string=? (caar find-record) column-name)
                                                                                                                       (cdar find-record)
                                                                                                                       (insert-record (cdr find-record) column-name))))))))))))
            (cdr db2)))))


;====================================
;=            Cerința 3 a)          =
;=     Operația simple-select       =
;=             10 puncte            =
;====================================
(define simple-select
  (λ (db table-name columns)
    (let ((table (get-table db table-name)))
      (let get-values((find-columns columns) (result-list null))
        (if (null? find-columns)
            (if (null? result-list)
                '()
                (if (null? (car result-list))
                    '()
                    result-list))
            (if (member (car find-columns) (get-columns table))
                (get-values (cdr find-columns) (append result-list (map cdr (filter (λ (column)
                                                                                   (if (string=? (car column) (car find-columns))
                                                                                       #t
                                                                                       #f)) (cdr table)))))
                (get-values (cdr find-columns) result-list)))))))

;====================================
;=            Cerința 3 b)          =
;=           Operația select        =
;=            30 de puncte          =
;====================================

;Returneaza lista de coloane care trebuiesc selectate
(define searched-columns
  (λ (columns)
    (let get-col((columns-result null) (col columns))
      (if (null? col)
          columns-result
          (if (pair? (car col))
              (get-col (append columns-result (list (cdar col))) (cdr col))
              (get-col (append columns-result (list (car col))) (cdr col)))))))

;Intersectia a doua liste
(define (intersection a b)
  (if (null? a)
      null
      (if (member (car a) b)
          (cons (car a) (intersection (cdr a) b))
          (intersection (cdr a) b))))

;Intersectia liniilor pe care trebuie sa le returnez
(define intersect-rows
  (λ(first-intersection rows-list)
    (let intersect-lists((result first-intersection) (rows rows-list))
      (if (null? rows)
          result
          (if (list? (cdr rows))
                 (intersect-lists (intersection result (car rows)) (cdr rows))
                 (intersection result rows))))))

;Returneaza pozitiile care respecta o conditie data
(define return-rows
  (λ (condition column compare-with)
    (let return-position((positions '()) (current-pos 1) (col column))
      (if (null? col)
          positions
          (if (condition (car col) compare-with)
              (return-position (append positions (list current-pos)) (+ current-pos 1) (cdr col))
              (return-position positions (+ current-pos 1) (cdr col)))))))

;Returneaza lista de linii care ma intereseaza
(define return-rows-list
  (λ (db table-name columns-check conditions)
    (if (null? conditions)
        (let all((col (car (simple-select db table-name (list (car columns-check))))) (result '()) (counter 1))
          (if (null? col)
              (list result)
              (all (cdr col) (append result (list counter)) (+ counter 1))))     
        (let each-cond((op (caar conditions))
                   (column-name (cadar conditions))
                   (column (if (null? (simple-select db table-name (list (cadar conditions))))
                               '()
                               (car (simple-select db table-name (list (cadar conditions))))))
                   (compare-with (caddar conditions))
                   (result-rows '())
                   (conds (cdr conditions)))
          (if (null? conds)
              (append result-rows (list (return-rows op column compare-with)))
              (each-cond (caar conds) (cadar conds) (car (simple-select db table-name (list (cadar conds))))
                     (caddar conds) (append result-rows (list (return-rows op column compare-with))) (cdr conds)))))))
      

;Obtine lista finala, dar fara operatiile din columns
(define first-select
  (λ (db table-name columns conditions)
    (letrec((col-ret (searched-columns columns))
            (positions-list (return-rows-list db table-name col-ret conditions))
            (positions (match (length positions-list)
                         [0 '()]
                         [1 (car positions-list)]
                         [2 (intersection (car positions-list) (cadr positions-list))]
                         [default (intersect-rows (intersection (car positions-list)
                                                                (cadr positions-list))
                                                  (cddr positions-list))])))
      (let select-return((col columns)
                         (result-list '()))
        (if (null? col)
            result-list
            (select-return (cdr col)
                           (append result-list
                                   (list (let get-values((col-val (car (simple-select db table-name
                                                                                     (list (if (pair? (car col))
                                                                                         (cdar col)
                                                                                         (car col))))))
                                                        (pos 1)
                                                        (result '()))
                                         (if (null? col-val)
                                             result
                                             (if (member pos positions)
                                                 (get-values (cdr col-val) (+ pos 1) (append result (list (car col-val))))
                                                 (get-values (cdr col-val) (+ pos 1) result))))))))))))

;Aplic operatiile din columns
(define select
  (λ (db table-name columns conditions)
    (let get-result((almost-result (first-select db table-name columns conditions))
         (result '())
         (col columns))
      (if (null? almost-result)
          result
          (if (pair? (car col))
              (get-result (cdr almost-result) (append result (list (match (caar col)
                                                                     ['min (car (sort (car almost-result) <))]
                                                                     ['max (car (sort (car almost-result) >))]
                                                                     ['count (length (remove-duplicates (car almost-result)))]
                                                                     ['sum (foldl + 0 (car almost-result))]
                                                                     ['avg (/ (foldl + 0 (car almost-result))
                                                                                (length (remove-duplicates (car almost-result))))]
                                                                     ['sort-asc (sort (car almost-result) <)]
                                                                     ['sort-desc (sort (car almost-result) >)]))) (cdr col))
              (get-result (cdr almost-result) (append result (list (car almost-result))) (cdr col)))))))

;====================================
;=             Cerința 4            =
;=           Operația update        =
;=            20 de puncte          =
;====================================
;Formez noua tabela in care modific
(define new-table
  (λ(db table-name values conditions)
    (letrec((columns (get-columns (get-table db table-name)))
            (positions-list (return-rows-list db table-name columns conditions))
            (positions (match (length positions-list)
                         [0 '()]
                         [1 (car positions-list)]
                         [2 (intersection (car positions-list) (cadr positions-list))]
                         [default (intersect-rows (intersection (car positions-list)
                                                                (cadr positions-list))
                                                  (cddr positions-list))])))
      (let form-table((col columns) (final-table (list table-name)))
        (if (null? col)
            final-table
            (if (assoc (car col) values)
                (form-table (cdr col)
                            (append
                             final-table (list
                              (let form-column((final-column (list (car col)))
                                               (column-values (car (simple-select db table-name (list (car col)))))
                                               (value (cdr (assoc (car col) values)))
                                               (current-position 1))
                                (if (null? column-values)
                                    final-column
                                    (if (member current-position positions)
                                        (form-column (append final-column (list value))
                                                     (cdr column-values) value (+ current-position 1))
                                        (form-column (append final-column (list (car column-values)))
                                                     (cdr column-values) value (+ current-position 1))))))))
                (form-table (cdr col)
                            (append final-table (list (append (list (car col))
                                                              (car (simple-select db table-name (list (car col))))))))))))))

(define update
  (λ (db table-name values conditions)
    (let-values (((db1 db2) (splitf-at db (λ (table)
                                             (if (string=? (get-name table) table-name)
                                                 #f
                                                 #t)))))
    (append db1 (list (new-table db table-name values conditions)) (cdr db2)))))

;====================================
;=             Cerința 5            =
;=           Operația remove        =
;=              10 puncte           =
;====================================
;Formez tabela fara liniile care respecta conditiile
(define delete-rows
  (λ(db table-name conditions)
    (letrec ((columns (get-columns (get-table db table-name)))
            (positions-list (return-rows-list db table-name columns conditions))
            (positions (match (length positions-list)
                         [0 '()]
                         [1 (car positions-list)]
                         [2 (intersection (car positions-list) (cadr positions-list))]
                         [default (intersect-rows (intersection (car positions-list)
                                                                (cadr positions-list))
                                                  (cddr positions-list))])))
      (let form-table((col columns) (final-table (list table-name)))
        (if (null? col)
            final-table
            (form-table (cdr col)
                            (append
                             final-table (list
                              (let form-column((final-column (list (car col)))
                                               (column-values (car (simple-select db table-name (list (car col)))))
                                               (current-position 1))
                                (if (null? column-values)
                                    final-column
                                    (if (member current-position positions)
                                        (form-column final-column
                                                     (cdr column-values) (+ current-position 1))
                                        (form-column (append final-column (list (car column-values)))
                                                     (cdr column-values) (+ current-position 1)))))))))))))


(define delete
  (λ (db table-name conditions)
    (let-values (((db1 db2) (splitf-at db (λ (table)
                                             (if (string=? (get-name table) table-name)
                                                 #f
                                                 #t)))))
    (append db1 (list (delete-rows db table-name conditions)) (cdr db2)))))

;====================================
;=               Bonus              =
;=            Natural Join          =
;=            20 de puncte          =
;====================================
;Retuneaza coloanele comune
(define comun-columns
  (λ(db tables)
    (letrec((table1 (car tables)) (table2 (cadr tables))
            (columns1 (get-columns (get-table db table1)))
            (columns2 (get-columns (get-table db table2))))
      (let result-columns((col columns1) (result '()))
        (if (null? col)
            result
            (if (member (car col) columns2)
                (result-columns (cdr col) (append result (list (car col))))
                (result-columns (cdr col) result)))))))

;Returneaza o lista cu elementele comune
(define columns-intersection
  (λ(db tables column)
    (letrec((table1 (car tables)) (table2 (cadr tables))
            (column1 (car (simple-select db table1 (list column))))
            (column2 (car (simple-select db table2 (list column)))))
      (let comun-elem((col column1) (result '()))
        (if (null? col)
            (remove-duplicates result)
            (if (member (car col) column2)
                (comun-elem (cdr col) (append result (list (car col))))
                (comun-elem (cdr col) result)))))))

(define little-list
  (λ(elem column1 column2)
    (let create-list((counter (max (count (λ(x) (equal? elem x)) column1)
                                   (count (λ(x) (equal? elem x)) column2))) (result '()))
      (if (= counter 0)  
          result
          (create-list (- counter 1) (append result (list elem)))))))

;Returneaza lista cu elemetele dupa care voi face afisarea
;ex: Am urmatoarele liste: '(1 1 2 3) si '(1 2 2 3) => '(1 1 2 2 3)
(define comun-list
  (λ(db tables column)
    (letrec((table1 (car tables)) (table2 (cadr tables))
            (column1 (car (simple-select db table1 (list column))))
            (column2 (car (simple-select db table2 (list column))))
            (comun-pos (columns-intersection db tables column)))
      (let result-list((result '()) (pos comun-pos))
        (if (null? pos)
            result
            (result-list (append result (little-list (car pos) column1 column2)) (cdr pos)))))
      ))

;searched-elements: lista cu elementele comune
;column: datele din coloana
;Returneaza liniile(pozitiile din coloana) de pe care trebuie sa extragem elemente
(define get-index
  (λ (searched-elements column)
    (let get-elem((elem-list searched-elements) (col column) (result '()) (position 1))
      (if (null? elem-list)
          result
          (if (member (car col) searched-elements)
              (let find-elem((elem col) (pos position) (target (car elem-list)))
                (if (null? elem)
                    (let search-more((el column) (index 1))
                      (if (equal? (car el) target)
                          (get-elem (cdr elem-list) col (append result (list index)) position)
                          (search-more (cdr el) (+ index 1))))
                    (if (equal? (car elem) target)
                        (get-elem (cdr elem-list) (cdr col) (append result (list pos)) (+ position 1))
                        (find-elem (cdr elem) (+ pos 1) target))))
              (get-elem elem-list (cdr col) result (+ position 1)))))))

;Lista cu elementele de pe anumite pozitii
(define get-pos
  (λ(index column)
    (let final-column((i index) (col column) (pos 1) (result '()))
      (if (null? i)
          result
          (if (equal? pos (car i))
              (final-column (cdr i) column 1 (append result (list (car col))))
              (final-column i (cdr col) (+ pos 1) result))))))

(define natural-join
  (λ (db tables columns conditions)
    (letrec((table1 (get-table db (car tables))) (table2 (get-table db (cadr tables)))
            (columns1 (get-columns table1)) (columns2 (get-columns table2))
            (column (car (comun-columns db tables)))
            (comun-elem (if (null? conditions)
                            (comun-list db tables column)
                            (filter (λ(x) ((caar conditions) x (caddar conditions))) (comun-list db tables column))))
            (column-index1 (get-index comun-elem (car (simple-select db (car tables) (list column)))))
            (column-index2 (get-index comun-elem (car (simple-select db (cadr tables) (list column))))) )
      (let result-list((col columns) (result '()))
        (if (null? col)
            result
            (if (member (car col) columns1)
                (result-list (cdr col) (append result (list (get-pos column-index1 (car (simple-select db (car tables) (list (car col))))))))
                (result-list (cdr col) (append result (list (get-pos column-index2 (car (simple-select db (cadr tables) (list (car col))))))))))))))