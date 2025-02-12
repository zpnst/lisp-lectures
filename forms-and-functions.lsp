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
 | Если форма не является ни вызовом функции, ни макросом,
 | значит это специальная форма
 |#