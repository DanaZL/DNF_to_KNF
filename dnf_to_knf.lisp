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


;; Упрощение выражения:
;;1. Упрощение коньюнкций
;;2. Упрощение дизъюнкции:
;;   2а) правила поглощения: X v -X = 1; X v X = X
;;	 2б) правило Блэйка: K1 v K1K2 = K1
;; Упрощения не надо зацикливать!! пока я так думаю
(defun reduction(l)
	(print 'start_reduction)
	(reduction_conjs l)
	(reduction_dizj l)
	; (blake_rule l)
)

;; если строка изменилась - первый елемент nil, иначе 0
;; должны пройти по всем коньюкциям и каждую сравнить со всем оставшемися   
;; коньюнкциями 
;; l - исходный список коньюнкций, res - результирующий
(defun reduction_dizj (l res)
	(cond 
		((null l) res)
		((in_conjs (car l) res) ;; есть ли эта коньюнкция до сих поре в результате? 
			(reduction_dizj (cdr l) (cons (car l) (blake_rule ((car l) res)))))
		(t (reduction_dizj (blake_rule (car l) res)))
	)
)


;; правило поглощения + правило Блэйка для конкретной 
;; коньюнкции и всех коньюкций в списке
(defun blake_rule (conj conjs)
	(let ((check_intersect_res (check_intersect conj (car conjs)))) 
		(cond
			((null conjs) null)
			;;не включаем коньюнкцию
			((eql check_intersect_res 1)
				 (blake_rule conj (cdr conjs)))
			((eql check_intersect_res 2)
				 (cons 1 (blake_rule conj (cdr conjs))))
			;;включаем коньюнкцию
			(t (cons (car conjs) (blake_rule conj (cdr conjs))))		
		)
	)
)

;;анализирует взаимное пересечение коньюнкций
(defun check_intersect (l1 l2)
	(cond
		;; 1 коньюнкция содержится во второй - можем не включать вторую 
		;;коньюнкцию в результат по правилу Блэйка 
		;;+ автоматически сокращаем одинаковые коньюнкции 
		((is_conj_subset l1 l2) 1)
		;; если l1 - отрицание l2, то вместо l1 ставится 1 и функция 
		;; тождественно равна 1 - актуально только для конкретных переменных
		((and (atom l1) (listp l2) (eql l1 (car l2))) 2)
		((and (listp l1) (atom l2) (eql (car l1) l2)) 2)
	)
)

; ;;поиск переменной в списке - переменная может быть в скобках
; ;;nil - если не найдено
; (defun find_var (var l)
; 	(cond 
; 		((atom var) (find var l))
; 		(t (cond 
; 				((reduce #'(lambda (v x) (cond 
; 									((eq v t) t) 
; 									((atom x) v)
; 									((eq (car v) (car x)) t)
; 									(t v)
; 									)) l ::initial-value v) 
; 												t)
; 				(t nil)))
; 	)
; )

;;поиск коньюнкции в списке коньюнкций
;; - фактически сравнение вложенных списков с точностью до вложенных списков
; (defun find_conj (conj conjs)
; 	(cond 
; 		((null (find conj conjs :test 'compare_conj)) t)
; 		(t nil) 
; 	)
; )


;;поиск коньюнкции в списке коньюнкций
(defun in_conjs (conj conjs)
	(reduce #'(lambda (c x) (cond
								((eq c t) t)
								((is_conjs_equal c x) t)
								(t c)
								)) :: initial-value conj conjs)
)

;;проверка на то, что 2 коньюнкции равны с точностью до перестановки переменных 
(defun is_conjs_equal (conj1 conj2)
	(cond
		((eql 0 conj2) nil)
		((and (null conj1) (null conj2)) t)
		((null conj1) nil)
		(t (is_conjs_equal(cdr conj1) (find_var_in_conj_with_delete (car conj1) conj2)))
	)
)
  
;;проверка на то, что 1-ая коньюнкция является подмножеством 2-ой
(defun is_conj_subset(conj1 conj2)
	(cond
		((eql 0 conj2) nil)
		((null conj1) t)
		(t (is_conj_subset(cdr conj1) (find_var_in_conj_with_delete (car conj1) conj2)))
	)
)

;;функция, которая ищет переменную var (переменная может быть в скобках)
;; в коньюнкции и, в случае успеха
;;удаляет ее. Иначе, меняет 1-элемент conj на 0.
(defun find_var_in_conj_with_delete (var conj)
	(cond 
		((null var) conj);; нашли переменную
		((null conj) 0);; не нашли и дошли до конца коньюнкции
		; ((numberp (car conj)) (car conj) (find_var_in_conj_with_delete var (cdr conj)))
		((equal var (car conj)) (find_var_in_conj_with_delete nil (cdr conj)))
		(t (cons (car conj)  (find_var_in_conj_with_delete var (cdr conj)))) 
	)
)


;;проверка на то, является одна коньюнкция является отрицанием второй
;;актуально только для конкретных 
(defun is_not (с1 с2)
	(is_conjs_equal (create_not_conj c1) c2)
)

;;взятие отрицания от коньюнкции
(defun create_not_conj (c)
	(mapcar #'(lambda (x) (cond
							((atom x) (list x))
							(t (car x))
						)) c) 
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
; (print (reduction '(((P) X Y) (C X (C)) ((P) X (Z)) (C X (Z)) ((P) Y Y) (C Y Y) ((P) Y (Z)) (C Y (Z))) ))

(print (member '(B) '(D (B) N)))
(print (member '(a y) '(g (a y) c a d e a f)))
; (print (stable-sort '(A (B) C (D))  #'))
; (print (intersection '((B) N) '(D (B) N)) #'<)

(print (equal 'B 'B ))
(print (is_conj_subset '((A) B C C) '(C B (A) C) ))
(print (is_conjs_equal '((A) B C C) '(C B (A) C D) ))
(print (is_not '((A) B (C) C) '(A (B) (C) C) ))