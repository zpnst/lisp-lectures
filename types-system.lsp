#|
 | Стандарт Common Lisp требует, чтобы компилятор поддреживал как минимум эти ьипы данных:
 |
 | Целые (integer)
 | Рациональные (rational)
 | Вещественные (floating-point)
 | Комплексные (comples)
 |#

;;; Целые числа не могут не иметь фикисрованного размера в байтах и могут хранить сколь угодно большие значения
(print (* 4513483935984590687587 348758347598))

;;; Разные системы счисления: <base>R<number>. Максимальное основание - 36
(print #10R42)
(print #2R101010)
(print #16R2A)
(print #25R1H)

;;; Дроби
(print (+ 3/5 1/7))
(print (+ 1/2 1/2))
(print (- #xA/AA #xA/FF))

;;; Floating Point Numbers (обычный или научный формаь)

(print pi)
(print 3.14)
(print 0.031415e2)
(print (exp 1))

;;; Комплексные числа
(print #C(100 -100))
(print (* #C(1 2) #C(3 5)))
(print #C(3.14 0))
(print (expt #c(0 1) 2)) ; i^2 = -1

;;; Символьный тип
(print #\a)

(print (format t "~c~%" #\a))
(print (format t "~c~%" #\NOT_EQUAL_TO))

;;; Полезные функции для работы с символами

(format t "~%~%")

(format t "char-upcase #\\a:     ~c~%" (char-upcase #\a))
(format t "char-downcase #\\B:   ~c~%" (char-downcase #\B))
(format t "char-code #\\A:       ~d~%" (char-code #\A))
(format t "code-char 65:        ~c~%" (code-char 65))
(format t "code-char (- 97 32): ~c~%" (code-char (- 97 32)))

(format t "char-name #\\A:       ~d~%" (char-name #\A))
(format t "char-name #\\ё:       ~d~%" (char-name #\ё))
(format t "char-name #\\片:      ~d~%" (char-name #\片))

(format t "~c~%"
    (char-upcase
        (name-char "CYRILLIC_SMALL_LETTER_IO")))

#|
 | Списки - один из самых важных типов (Lisp - List Processing language)
 | Формы по сути и являются списками, просто интерприатор Lisp трактует первый 
 | элемент формы как команду, а отсальные - как аргументы.
 | Если же мы хотим, чтобы элменеты внутри круглых скобок трактовались как данные, то нужно потсавить одинакрную кавычку (quote)
 |
 | Элементами списков могут быть данные любых типов, в том числе и другие списки
 |#

(print '(1 2 3))
(print (quote (1 2 3)))
(print '(1 two "three" (() () () ())))

(print ())
(print nil)

#|
 | Векторы (vetctors)
 | Многомерные массивы (multi-dimentional arrays)
 | Строки (strings)
 | Битовые вектора (bit-vectors)
 |#

;;; Хэш-таблица
(print (setf h (make-hash-table)))      ; cjplfybt хэш-таблицы
(setf (gethash 'cat h) 'russian-floppa) ; добавление новых паре в таблицу или изменение существующих

(print (gethash 'cat h))                ; получение значения по ключу   
