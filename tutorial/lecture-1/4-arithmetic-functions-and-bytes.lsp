;;; инкремент
(setf x 42)

(print (incf x))
(print (decf x))

(print (incf x 10))
(print (decf x 10))

(format t "~%")

;;; Множество стандратных математичсекиз функций, полная справка по ним - http://clhs.lisp.se
(print (max 12 54398 0.134))
(print (expt 2 8))
(print (expt 65536 1/16))
(print (sqrt 361))

(format t "~%")

;;; Округления
(print (floor -7.5))    ; returns -8  0.5 --> в сторону -oo
(print (ceiling -7.5))  ; returns -7 -0.5 --> в сторону +oo
(print (truncate -7.5)) ; returns -7 -0.5 --> в сторону 0
(print (round -7.5))    ; retunrs -8  0.5 --> в сторону ближайшего целого

(format t "~%")

;;; Модульная арифметика

(print (mod 5 3)) ; 2
(print (rem 5 3)) ; 2

;;; mod использует floor для округления результата,    a mod b = a - floor(a/b)*b
;;; rem использует truncate для округления результата, a rem b = a - truncare(a/b)*b

(print (mod 5 -3)) ; -1, 5 rem -3 = 5 - (5/(-3))*(-3) = 5-(-2)*(-3) = -1
(print (rem 5 -3)) ; 2,  5 rem -3 = 5 - (5/(-3))*(-3) = 5-(-1)*(-3) = 2

(format t "~%")

;;; В Lisp с числами не связан кокретный размер в байтах. В Lisp арифметика произвольной точности
(print (integer-length 15))
(print (integer-length 16))
(print (integer-length #xFFF))
(print (integer-length 1635834058690536))

;;; Зная, что в байте 8 бит количстево байт можно найти так
(print (ceiling (integer-length (expt 128 3)) 8))

(format t "~%")

;;; Побитовые операции
(print (lognot 1))
(print (logand 7 8)) ; 0111 & 1000 = 0000
(print (logior #xAC #xDC))
(print (logxor  #xAC #xDC))

(format t "~%#x~x"
    (logxor #xAC #xDC))

(print (logbitp 1 7)) ; какое значение в чисел имеет бит с заданным номером ((номер бита (число))

(format t "~%")

;;; Сдвиги 
;;; (ash число количество_разрядов)
(print (ash 1 1)) ; на один влево
(print (ash 1 7)) ; на один вправо

(print (ash 128 -5))
(print (ash 2 -1))

(format t "~%")

;;; (byte размер позиция)
(print (byte 9 4)) ; #S(BYTE :SIZE 9 :POSITION 4) 

;;; чтение битов

;;; ldb - load byte. (ldb спецификация число), то есть (ldb (byte размер позиция) число)
(print (ldb (byte 3 0) #2R10100111)) ; из числа #2R10100111 нужно извелчь значения битов #S(BYTE :SIZE 8 :POSITION 0) 
(print (ldb (byte 8 16) #xCAFEBABE))

(format t "~%~%")

;;; запись битов (dbp значение спецификация число)
(defvar y #2R111000)
(format t "~d~%" y)
(format t "~d~%" (logior y (dpb #2R111 (byte 3 0) y)))

(format t "~%~%")

#|
 | Прочитать беззнаковое целое число из STDIN и изменить порядок
 | следования битов в байтах числа на обратный. Результат вывестив STDOUT
 | Исходное число 345 = 00000001 01011001
 | Результат 10000000 10011010 = 32922
 |#

;;; Чтение из stdin read

; (defvar num 0)
; (setf num (read))
; (format t "~b~%" num)

;;; Циклы (loop форма1 форма2 ... формаN). loop - макрос
(setf nnum 3)

(print (loop 
    (when (< nnum 0) (return 1111)) ; (unless (>= nnum 0) (return))
    (format t "~d~%" nnum)
    (decf nnum))) ; decf тоже макрос (setf nnum (- nnum 1)

; 3
; 2
; 1
; 0
; 1111 - значение, которое вернла форма (return)

(format t "~%")

;;; (dotimes форма_инициализации форма1 ... формаN)
;;; вторая форма в простейшем случае просто константа
(print 
    (dotimes (i 3) (print i)))

; 0 
; 1 
; 2 
; NIL --> значение, возвращаемое из цкила (форма возврата отсутствует)

(format t "~%")

(print 
    (dotimes (i 3 (* i 2)) (print i)))

; 0 
; 1 
; 2 
; 6 --> значение, возвращаемое из цкила формой (* i 2)

(format t "~%~%")

; ;; b - байт
; ;; i - номер бита в байте
; (not (eq
;     (logbitp i b)
;     (logbitp (- 7 i) b)))

; ;;; формирование маски (значение 1 на симметричных позициях в байте при их не совпадении)
; (logxor 
;     b
;     (+ (ash 1 i) (ash 1 (- 7 i))))

;;; Вернёмся к задаче

(defvar res 0)

(setf res (read)) ; прочитать число из STDIN

(defun change-byte (b)
    (dotimes (i 4 b)  ; форма возврата dotimes - это b. b и станет возвращаемой формой change-byte
        (when 
            (not (eq
                (logbitp i b)
                (logbitp (- 7 i) b)))
            (setf b (logxor 
                        b
                        (+ (ash 1 i) (ash 1 (- 7 i))))))))

(defun invert-bytes (n)
    (dotimes (i (ceiling (integer-length n) 8) n) ; цикл по байтам числа
        (setf n
            (dpb
                (change-byte (ldb (byte 8 (* i 8)) n))
                (byte 8 (* i 8)) n)))
)

(format t "~d~%" (invert-bytes res))