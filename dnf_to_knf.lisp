;; пропускает все символы до знака дизэюнкции или конца строки
(defun skip_next_conj (l)
	(cond 
		((null l) l)
		((eql 'v (car l)) l)
		(t (skip_next_conj (cdr l)))
	))

;; возвращает следующую коньюнкцию в скобках (до знака дизъюнкции или конца строки)
(defun search_next_conj (l conj)
	(print 'search_next_conj)
	(print l)
	(print conj)
	(cond
		((null l) conj)
		((eql 'v (car l)) conj)
		((eql '- (car l)) 
			(search_next_conj (cdr(cdr l)) (cons (cons (car (cdr l)) ()) conj)) )
		(t  (search_next_conj (cdr l) (cons (car l) conj)))
	))

;;парсинг ДНФ - выделяет следущую элементарную конъюнкцию и добавляет к результату
(defun dnf_parse (l res)
	(print 'in_disj_parse)
	(print l)
	(print res)
	(cond 
		((null l) (print (cons 'end_of_parse res)) res)

		((eql 'v (car l)) 
			(print (cons 'start_search_new_conj res)) 
				(dnf_parse 
					(skip_next_conj (cdr l)) 
					(cons (search_next_conj (cdr l) ()) res) 
				)
		)

		(t (dnf_parse (cdr l) (cons (cons (car l) (car res)) ()))) 
	))

;;парсинг входной строки во внутреннее предствление
;;(A v B - C v C Р) => ((Р C) ((C) B) (A)) 
(defun input_parse(l)
	(cond 
		((null l) (empty_l))
		(t (print 'start_parse) (dnf_parse l '(()) ))
	))

;;Перевод из ДНФ в КНФ -> двойное отрицание

;;1-ое отрицание для перевода в КНФ фактически не изменяет внутреннее представление
;;теперь 1-ый уровень вложенности означает коньюнкцию дизъюнкций,
;;причем раньше переменные в скобках означали отрицание, теперь наоборот.

;;раскрытие скобок после 1-го отрицания
;;после этого этапа снова получается дизъюнкция коньюнкций
(defun removal_of_brackets (l)
	(print 'removal_of_brackets)
	(reduce 'multy_2_brackets l)
	)

;;аналог mapcar, но используюет append вместо cons
(defun map_append (F L)
	(cond 
		((null L) nil)
	(t (append (funcall F (car L)) (map_append F (cdr L) )) )
	))

;;list с проверкой на список
(defun list_check (l1 l2)
	(cond 
		((listp l1) (cons l2 l1))
		(t (list l1 l2))
	))

;;перемножение 2-х скобок
(defun multy_2_brackets (br1 br2)
	(print 'multy_2_brackets)
	(map_append #'(lambda (var1)
		(mapcar #'(lambda (var2)(list_check var1 var2)) br2 )) br1)
	)



(print (removal_of_brackets '((x y) (y (z)) ((p) c)) ))
(print (multy_2_brackets '(x y) '(y (z)) ) )
(print (input_parse '(A v B - C v C Р) ))
; (print (input_parse (read)))
(print (car '(Y (Z) . C)))
(print (cadr '(Y (Z) . C)))
(print (cddr '(Y (Z) . C)))
; ((X Y) (X (Z)) (Y Y) (Y (Z))) ((P) C)