;;;; comment at the begining of the file

(format t "~r" (+ 111 333))
(format t "~%")
(format t "~r" (+ 25 88)) ; returns atomic value nil with side effect as STDOUT

;;; macroces of reading

#|
 | functions with not
 | patameters
 |#
 
(print (machine-instance))
(print (machine-type))
(format t "~%")

#|
 | Puts N random 
 | numbers to terminal
 |#

;;; comment before(outside) the function body
(defun print-n-randoms (n max-limit)
    ;; comment inside the function body
    (dotimes (i n)                              ; dotimes macros
        (format t "~r~%" (random max-limit))))  ; comment at the end od the line

#|
 | function always retutns a value! even function don't have a return instruction. 
 | functions in Lisp MUST return some value
 | 
 | for example print-n-randoms returns NIL
 |#

(print (print-n-randoms 5 1000)) ; NIL 

#|
 | Форма dotimes - это последняя форма, которая вычисляется 
 | внутри функции => она становится возвращаемым значением функции print-n-randoms
 |#

;;; (describe 'random) ; описывает любую стандарттную функцию

(print (macro-function 'format)) ; NIL --> не являетя макросом
(print (macro-function 'defun))  ; #<COMPILED-FUNCTION DEFUN> --> является стандартным макросом

#|
 | Если форма не является ни вызовом функции, ни макросом, значит это специальная форма
 | Спциальные формы это функции, которые имеют особый порядок вычисления аргументов или исполняются ядром Lisp особым образом
 | Например форма условного выбора (if условие форма1 форма2)
 | В отличие от вызова функции, в которой слева направо выичисляются все аргументы, 
 | в форме if сначала вычисляется форма, которая задаёт условие и в зависимости от получившегося 
 | рещультата вычисляется либо форма1, либо форма2
 | Именно поэтому выводится лишт одна строка(форма) "Don't Panic", а не обе
 | форму if можно также использовать в качестве аргумента функции
 |#

(if (= (+ 2 2) 4)
    (format t "Don't Panic~%")
    (format t "Panic!~%"))

;;; можно и без else, только с than
; (if (> n 100)
;     (print "A")) 

#|
 | progn последовтаельно вычисляется все указанные формы
 | progn является функциональным аналогом операторных скобок в императивных языках, таких как C
 |#

(if (= (+ 2 2) 4)
    (progn 
        (format t "Don't Panic 1~%")
        (format t "Don't Panic 2~%"))
    (print "Panic!~%"))

#|
 | (when условие форма1 форма2 ... формаN)  - аналог комбинации if и progn. Вычисляет формы слева направо, если улсовие истинно
 | (unless условие форма1 форма2 ... формаN)  - аналог комбинации if и progn. Вычисляет формы слева направо, если улсовие ложно
 |#

(when (= 362880 (! 9))
    (format t "9! = 362880~%"))

(unless (= 362881 (! 9))
    (format t "9! != 362881~%"))

;;; macroexpand - показывает раскрытие макосра

(format t "~%")

(format t "~s~%" (macroexpand '(when (= 362880 (! 9))
    (format t "9! = 362880~%"))))

;;; (IF (= 362880 (! 9)) (PROGN (FORMAT T "9! = 362880~%"))) 
(IF (= 362880 (! 9)) (PROGN (FORMAT T "9! = 362880~%"))) 

(if (= 362880 (! 9))
    (progn
        (format t "9! != 362880~%")))

    
(format t "~s~%" (macroexpand '(unless (= 362881 (! 9))
    (format t "9! != 362881~%"))))

;;; (IF (NOT (= 362881 (! 9))) (PROGN (FORMAT T "9! != 362881~%"))) 
(IF (NOT (= 362881 (! 9))) (PROGN (FORMAT T "9! != 362881~%"))) 

(if (not (= 362881 (! 9)))
    (progn
        (format t "9! != 362881~%")))

(defvar a 1)
(if (/= a 2)
    (print "Hello"))

;;; variables

(defvar *aa* 42) ; переменные через devar видны всем функциям пакета или даже другим пакета, обычно называются как *varname*(в шутку называются наушниками)
(print *aa*)
(setf *aa* 45)   ; универсальный макрос setf аналогичен присваиванию в императивных языках прогнраммирования
(print *aa*)

;;; types, Lisp -м язык с динамической типизацией

;;; Узнать тип объекта с которым связан символ можно с помощью функции type-of

(print (type-of *aa*)) ; (INTEGER 0 281474976710655), 281474976710655 = 2^48
(print (type-of pi))
(print (type-of "Hello, World!"))

(defvar e 0)
(defvar g 0)

(print pi)
(print e)
(print g)

;;; можно изменять значения сразу нескольких переменных
(setf pi 3.14 e 2.71 g 9.81)

(print pi)
(print e)
(print g)

;;; последнее, чем может быть форма, если она не является список или символом - литералом, то есть константой. числовые и символьные константы - атомарные значения

(print 42)
(print 42e1)
(print 1/5)
(print #b1111)
(print #\a)