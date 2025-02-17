;;;; Работа со строками

;;; В Lisp строки - это просто массивы символов. Для них даже нет специального типа

(defvar str (copy-seq "Floppa")) ; copy-seq создаёт новую копию строки, которая может быть изменяемой.
(print (arrayp str))
(print (aref str 0))
(print (char str 1))
(setf (aref str 2) #\a)
(print str)

(format t "~%")

(print (array-total-size str))
(print (array-dimensions str))

(format t "~%") 

(setf s1 "Big" s2 " " s3 "Floppa" s4 "!")
(print (concatenate 'string s1 s2 s3 s4))

(print (string-upcase "high"))
(print (string-downcase "LOW"))
(print (string-capitalize "cAMWL cASE"))
(print (string-capitalize (reverse "!dlorw ,olleh")))

(format t "~%") 

;;; Поиск первого вхождения символа

(print (position #\p "Big Floppa"))
(print (position #\F "Big Floppa"))

(format t "~%") 

(print (search "Flo" "Big Floppa"))
(print (search "Ploob" "Big Floppa"))

(format t "~%") 

(print (remove #\. "I.S.O. O.S.I."))
(print (string-left-trim " " "   42"))
(print (string-right-trim " " "   42"))

(print (string-right-trim "|&%" "|&%%|42||&%%|%%|%|&&"))
(print (string-trim "|&%" "|&%%|42||&%%|%%|%|&&"))

(print (substitute #\a #\. "I.S.O. O.S.I."))

(format t "~%") 

(setf *random-state* (make-random-state t))

(defvar x 42)
(print (write-to-string x))

(format t
    "~%~a is picket randomly~%"
    (random 1000))

(print (parse-integer "42"))
; (print (parse-integer "42 is the Answer")) ; *** - PARSE-INTEGER: substring "42 is the Answer" does not have integer syntax at position 3
(print (parse-integer "42 is the Answer" :junk-allowed t))

(format t "~%") 

;;; Преобразования вещественных чисел

(print (read-from-string "3.14"))
(print (read-from-string "#x2A !kasjdfklj"))

(setf xx (read-from-string "1/42"))
(print (numberp x))
(print xx)