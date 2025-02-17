#|
 | В качестве ложного значения в Lisp выступает список. Все отсальные значения истинные
 | Истинна - T
 | Ложь(синоним пустого списка) - NIL
 |#

(print (> 20 10))
(print (> 20 30))

;;; 0 не является ложным значением
(print (if 0 'true 'false))

(format t "~%")

;;; элемнетарные логические функции
(print (not nil))
(print (and t nil))
(print (or t nil))
(print (xor t t))
(print (xor nil nil))

(format t "~%")

;;; операции and и or реалищованы как макросы или как специальные формы (это нужно для оптимизации вычисления логических выражений)
(print (and (> 20 30) (> 10 5)) ) ; (> 10 5) просто не будет вычисляться, так как (> 20 30) уже NIL(false)
(print (or (> 30 20) (> 10 500))) ; (> 10 500) просто не будет вычисляться, так как (> 30 20) уже T(true)

;;; не обязательно два аргумента (значением всей формы становится значение последнего вычисленного выражения)
(print 
    (and 
        (> 10 5)
        (> 20 10)
        (> 100 42)
        (> 1029420395 394035)))

(format t "~%")

#| 
 | Предикат - функция, которая проверяет некоторое условие и возвращает логическое значение (NIL или отличное от NIL значение)
 | Названия функций предикатов обычно заканчивается на 'p' (zerop, oddp, evenp)
 |#
 
(print (zerop 10))
(print (zerop 0))
(print (oddp 7))
(print (evenp 42))

(print (string-lessp "abc" "abd"))

(format t "~%")

;;; Создадим новый предикат. Проверим, является ли бидлет из 6-ти 10-ных цифр - счастливым

;;; Функции, которые нам понадобятся

(print (abs -1234))
(print (expt 10 2))
(print (truncate 12.34)) ; возвращает два значения, целую и дробную части. тут 12 и 0.34
(print (rem 12 10))
(print (/ 12 10))

(format t "~%")

;;; Создадим функцию для получения n-нной цифры числа
(defun nth-dec-digit (num n)
    (rem
        (truncate
            (/ (abs num) (expt 10 n)))
    10))

(print (nth-dec-digit 1234 0)) ; 4
(print (nth-dec-digit 1234 1)) ; 3
(print (nth-dec-digit 1234 2)) ; 2
(print (nth-dec-digit 1234 3)) ; 1

(format t "~%")

(defun happy-ticket-p (num) 
    (and (>= num 0) (<= num 999999)
        (=  (+ (nth-dec-digit num 0)
               (nth-dec-digit num 1)
               (nth-dec-digit num 2)) 
            (+ (nth-dec-digit num 3)
               (nth-dec-digit num 4)
               (nth-dec-digit num 5)))))

(print (happy-ticket-p 124322)) ; T
(print (happy-ticket-p 001002)) ; NIL (1002 => 001002)

(format t "~%")

;;; Предикаты, которые используются чаще всего

;;; Для проверки на равенство символов (symbols) используется функция eq:
(print (eq t nil))
(print (eq nil ()))
(print (eq 10 10))
(print (eq 10 10.0))
(print (eq #\a #\a))
(print (eq "string" "string"))

(format t "~%")

;;; equal использует eq для сравнения символов и coil для спавнения чисел и знаков, а списки, строки и битовые ыектора сравнивает поэлементно. Сложные структуры, такие как хэши или массивы данных предикат сранвить не может

(print (equal '(1 2 3) '(1 2 3)))
(print (equal #4*1101 #4*1100))  ; битовые вектора
(print (equal "big-floppa-string" "big-floppa-string"))

(format t "~%")

;;; предикат = для проверки равенства чисел, пусть даже и разных типов
(print (= 123 123))
(print (= 10 #C(10.0 0.0)))

(print (= 10 100/10))
(print (= 10 (/ 100 10)))

(print(= #xFF 255))

(format t "~%")

;;; Для проверки равеснтва символов (characters) без учёта регистра сипользуется функция char-equal
(print (char-equal #\a #\A))
(print (char-equal #\a #\b))

;;; Для проверки равенства символов (characters) с учётом ргеистра используется фукнция char=
(print (char= #\A #\a))
(print (char= #\A #\A))

(format t "~%")

;;; string-equal сравнивает строки без учёта регистра
(print (string-equal "Lisp FloPPA" "LiSp FlOpPa"))
(print (string-equal "flip" "flop"))

;;; string= сравнивает строки с учётом регистра
(print (string= "Lisp FlopPPA" "LiSp FlOpPa"))
(print (string= "FlopPPA" "FlopPPA"))

(format t "~%")

;;; equalp - наиболее общая функция для проверки на равенство, которая подходит и для протсых типов, и для составных. Символы и строки сравнивает без учёта регистра
;;; Но из-за многочисленных проверок equalp работает медленнее, чем специализированные проверки
(print (equalp 10 10.0))
(print (equalp #\A #\a))
(print (equalp "lisp" "LISP"))
(print (equalp (quote (1 2 3)) '(1 2 3)))
(print (equalp 10 "10"))

#|
 | Предикаты и парные к ним
 | eq           | NOT eq
 | eql          | NOT eql
 | equal        | NOT equal
 | =            | /=
 | char=        | char/=
 | char-equal   | char-not-equal
 | string=      | string\=         | string >=
 | string-equal | string-not-equal
 | equalp       | NOT equalp
 |#

(format t "~%")

;;; функциям сравнения чисел и символов можно передавать больше одного аргумнета. Однако предикаты, которые сравнивают строки - требуют для работы ровно две строки
(print (= 42 42 42))
(print (< 0 1 2 3))  ; 0 < 1 < 2 < 3 
(print (< 1 2 3 0))  ; 1 < 2 < 3 \< 0

(format t "~%")

;;; Стандартные предикаты

(setf x 42)
(print (typep x 'number))
(print (numberp x))     ; является ли 'x' числом 
(print (null x))        ; является ли 'x' пустым списком
(print (and (null nil) (null ())))
(print (listp '(1 2 3)))

(format t "~%")

(print (upper-case-p #\A))
(print (upper-case-p #\a))
(print (yes-or-no-p "Lisp rocks?")) ; yes --> T. no --> NIL


