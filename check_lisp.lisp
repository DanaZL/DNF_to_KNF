(defun Check_Lisp(l) 
	(cond	
			; ((null l) t)
			((atom l) t)
			((listp (cdr l)) (and (Check_Lisp (cdr l)) (Check_Lisp (car l)) )) 
			(t NIL) ))
(print (atom ()))
(print '(a . nil))
(print (Check_Lisp '((a . nil) . (a)) ))
(print (Check_Lisp '(a . ((c . s) . nil))))