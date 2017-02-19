;; пропускает все символы до знака дизэюнкции или конца строки
(defun skip_next_disj (l)
	(cond 
		((null l) l)
		((eql 'v (car l)) l)
		(t (skip_next_disj (cdr l)))
	))

;; возвращает следующую дизъюнкцию в скобках (до знака дизъюнкции или конца строки)
(defun search_next_disj (l disj)
	(print 'search_next_disj)
	(print l)
	(print disj)
	(cond
		((null l) disj)
		((eql 'v (car l)) disj)
		((eql '- (car l)) 
			(search_next_disj (cdr(cdr l)) (cons (cons (car (cdr l)) ()) disj)) )
		(t  (search_next_disj (cdr l) (cons (car l) disj)))
	))

;;выделяет следущую дизъюнкцию
(defun disj_parse (l res)
	(print 'in_disj_parse)
	(print l)
	(print res)
	(cond 
		((null l) (print (cons 'end_of_parse res)) res)

		((eql 'v (car l)) 
			(print (cons 'start_search_new_disj res)) 
				(disj_parse 
					(skip_next_disj (cdr l)) 
					(cons (search_next_disj (cdr l) ()) res) 
				)
		)

		(t (print (cons 'append_first_disj l)) (disj_parse (cdr l) (cons (cons (car l) (car res)) ()))) 
	))

;;парсинг входной строки
(defun input_parse(l)
	(cond 
		((null l) (empty_l))
		(t (print 'start_parse) (disj_parse l '(()) ))
	))

;;(print (atom ()))
;;(print '(a . nil))
;;(print (Check_Lisp '((a . nil) . (a)) ))
;;(print (Check_Lisp '(a . ((c . s) . nil))))
(print (input_parse '(A v B - C v C Р) ))
; (print (input_parse (read)))
; (print (Blake_method_1 '(a . ((c . s) . nil))))