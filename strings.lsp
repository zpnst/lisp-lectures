;;;; Работа со строками

;;; В Lisp строки - это просто массивы символов. Для них даже нет специального типа

(defvar str (copy-seq "Floppa")) ; copy-seq создаёт новую копию строки, которая может быть изменяемой.
(print (arrayp str))
(print (aref str 0))
(print (char str 1))
(setf (aref str 2) #\a)
(print str)