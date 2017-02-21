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

;раскрытие скобок после 1-го отрицания
;после этого этапа снова получается дизъюнкция коньюнкций
(defun removal_of_brackets (l)
	(print 'removal_of_brackets)
	(reduce 'multy_2_brackets l)
)


;;упрощение выражения:
;;1. Упрощение коньюнкций
;;2. Применение правила Блэйка
;;3. Упрощение дизъюнкций 
(defun reduction(l)
	(print 'start_reduction)
	(reduction_conjs l)
	(reduction_dizj l)
	; (blake_rule l)
)

;;правило поглощения(Блэйка): k1 v k1k2 = k1
(defun reduction_dizj (l res is_end))
	(cond 
		((null l) l)
		()

(defun blake_rule (conj conjs)
	(mapappend_2 'search_intersect conj conjs)
)


;;TODO - ускорить
(defun search_intersect (l1 l2)
	(let (intersect (intersection l1 l2))
		(cond
			((eql (length intersect) (length l1)) l1)
			((eql (length intersect) (length l2)) l2)
		)
	)
)

;;aналог mapcar c параметром
(defun mapappend_2(F x l)
	(cond
		((null l) nil)
		(t (append (funcall F x (car L)) (mapcar_2 F x (cdr L))))
	)
)

(defun reduction_conjs (conjs)
	(print (mapcar 'reduction_conj conjs))
)

;;принимает переменную и коньюнкцию, выполняет упрощение: x&x = x, x&-x = 0
(defun reduction_conj (conj)
	(print (map_append_2 'var_conj_check conj))
	 ; (map_append_2 'var_conj_check conj)
)

;;принимает переменную и коньюнкцию. Если переменная есть в коньюнкции - возвращает nill.
;;Если в коньюкции есть отрицание переменной - возвращает \0
(defun var_conj_check(var conj)
	(reduce 'var_var_check conj ::initial-value var)
)

(defun var_var_check(var1 var2)
	; (print 'var_var_check)
	; (print var1)
	; (print var2)
	(cond 
		((null var1) nil)
		((null var2) nil)
		((and (listp var1) (eql (car var1) var2)) '\0)
		((and (listp var1) (listp var2) (eql (car var1) (car var2))) nil)
		((and (listp var2) (eql (car var2) var1)) '\0)
		((and (atom var1) (atom var2) (eql var1 var2)) nil)
		(t var1)
	))


;;аналог mapcar, но используюет append вместо cons и передает функции F еще и список
(defun map_append_2 (F L)
	(print L)
	(cond 
		((null L) nil)
		(t (map_append_2_tmp F (funcall F (car L) (cdr L)) L))
	)
)

(defun map_append_2_tmp (F x L)
	(cond 
		((eql x '\0) nil)
		((null x) (map_append_2 F (cdr L))) 
		(t (cons x (map_append_2 F (cdr L)))) 
	)
)

(defun check_zero (l)
	(print 'check_zero) 
	(print l)
	(reduce #'(lambda (x y) 
		(cond 
			((eql '\0 x) nil)
			((eql '\0 y) nil)
			((null x) nil)
			((null y) nil)
			(t x)
		 ))
		l	
	) 
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

(defun dnf_to_knf(l) 
	(reduction (removal_of_brackets (input_parse l)))
)



(print (removal_of_brackets '((x y) (y (z)) ((p) c)) ))
(print (multy_2_brackets '(x y) '(y (z)) ) )
(print (input_parse '(A v B - C v C Р) ))

; (print (input_parse (read)))
(print (car '(Y (Z) . C)))
(print (cadr '(Y (Z) . C)))
(print (cddr '(Y (Z) . C)))
; ((X Y) (X (Z)) (Y Y) (Y (Z))) ((P) C)
(print (var_conj_check 'P '(P H A)))
(print (reduction '(((P) X Y) (C X (C)) ((P) X (Z)) (C X (Z)) ((P) Y Y) (C Y Y) ((P) Y (Z)) (C Y (Z))) ))

(print (intersection '(B N) '(D B N)))