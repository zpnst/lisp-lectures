;;; read about declaim, declare and proclaim


; (defun sum-n (arr n)
;     (let ((res 0))
;         (dotimes (i n)
;             (incf res (aref arr i)))
;         res))

; (print (sum-n '(1 2 3) 3)) ; *** - AREF: argument (1 2 3) is not an array

;;; Лучше использовать elt для того, чтобы функция обрабатывала не только массивы, но и списки (обеспечиваем независимость функции от типа контейнера, который надо просуммировать)
(defun sum-n (seq n)
    "Суммирует n первых элементов 
        последовательности seq"
    (let ((res 0))
        (dotimes (i n)
            (incf res (elt seq i)))
        res))

;;; Возвращаемым значением функции является значение последний из вычесленных форм внутри функции. В данном случе формы let
(print (sum-n #(40 2 3 5 67) 2))
(print (sum-n '(40 2 3 6 7) 2))

(print (documentation 'sum-n 'function))

(format t "~%")

;;; Для определения фукнкции использууется макрос defun
(defun имя-функции (список-аргументов)
    "Строка с документацией функции"

    ; тело функции
)

;;; Будем называть формальными аргументами те, которые указаны в её списке аргументов при определении функции
;;; Формальные аргументы - локалные переменный функции
;;; С именами формальных аргументов связываются имена фактических аргументов, то есть тех, что были переданны в функцию в месте её вызова

(defun func (a b c) ; формальные аргументы a, b & с
    (* a b c)) 

(print (func 2 3 7)) ; фактические аргументы 2, 3 & 7

(format t "~%")

;;; Необязательные (опциональные) аргументы в функции
;;; По традиции ключевые слова в списке формальных аргументов записывются с амперсандом(&) вначале
(defun func-opt (a1 a2 &optional o1 o2)
    (list a1 a2 o1 o2))

; (print (func-opt 1)) ; *** - EVAL/APPLY: Too few arguments (1 instead of at least 2) given to FUNC-OPT
(print (func-opt 1 2))
(print (func-opt 1 2 3))
(print (func-opt 3 4 5 6))

(format t "~%")

;;; Необязаиельные аргументы по умолчанию инициализируются значением NIL, однако мы можем указать их значения вручную
(defun func-opt2 (a1 a2 &optional (o1 42 o1-p) (o2 52))
    (list a1 a2 o1 o2 o1-p)) ; o1-p - переменная, которая помогает понять, была ли выполнена инициализация фактиечского аргумнета значнеием по умолчанию, или тем значением, которое было указано при вызоые функции

(print (func-opt2 1 2))
(print (func-opt2 1 2 3))
(print (func-opt2 1 2 3 4))

(format t "~%")

;;; Аналог стандратной функции сложения
(defun my-plus (&optional (o1 0) (o2 0) (o3 0))
    (+ o1 o2 o3))

(print (my-plus 1))
(print (my-plus 1 2))
(print (my-plus 1 2 3))
; (print (my-plus 1 2 3 4)) ; *** - EVAL/APPLY: too many arguments given to MY-PLUS

; Во всех реализациях Common Lisp константа call-arguments-limit должна быть больше 50

; sbcl REPL
; * call-arguments-limit
; 4611686018427387903

; clisp REPL
; [1]> call-arguments-limit
; 4096

(format t "~%")

;;; Передача произвольного числа аргументов осуществляется с помощью ключевого слова &rest
;;; Все эти аргументы агрегрируются в список
(defun my-func (a1 a2 &rest r)
    (list a1 a2 r))

(print(my-func 1 2))
(print(my-func 1 2 3 4 5 6 7 8 9 10 11))

(format t "~%")

;;; Теперь реализуем фукнкцию сложения произвольно числа чисел
(defun m-plus (&rest args)
    (let ((res 0))
        (dolist (arg args)
            (incf res arg))
    res))

(print (m-plus))
(print (m-plus 1 2 3))
(print (m-plus 1 2 3 4 5 6 7 8 9 10 11))

(format t "~%")

;;; Ключевые(именованные) аргументы (они тоже необязательные)
(defun some-func (&key (k1 1 k1-p) (k2 11 k2-p))
    (list k1 k2 k1-p k2-p))

(print (some-func))
(print (some-func :k2 42))
(print (some-func :k2 11 :k1 1))
(print (some-func :k2 21 :k1 7))

(format t "~%")

;;; Преимущество ключевых необязательных аргументов над обычнми необязательными аргументами
;;; Порядок передачи аргументов не важен, так как инициалищируем по имени

(setf *random-state* (make-random-state t))

(defun some-foo (&key
        ((:some-arg k1) (random 100) k1-p)
        ((:another-arg k2) (random 100) k2-p))
    (list k1 k2 k1-p k2-p))

(print (some-foo))
(print (some-foo))
(print (some-foo :another-arg 42))
(print (some-foo :some-arg 26 :another-arg 42))

(format t "~%")

(defun woo (&key k1 k2 &allow-other-keys)
    (list k1 k2))

(print (woo :k1 42))
(print (woo :k1 42 :some-other-key 26))

(format t "~%")

;;; Список аргументов - лямбда-список
; clisp
; [1]> lambda-list-keywords (&OPTIONAL &REST &KEY &ALLOW-OTHER-KEYS &AUX &BODY &WHOLE &ENVIRONMENT)
; sbcl
; * lambda-list-keywords (&ALLOW-OTHER-KEYS &AUX &BODY &ENVIRONMENT &KEY SB-INT:&MORE &OPTIONAL &REST &WHOLE)

;;; Пример использования ключевого слова &allow-other-keys

(defun make-string-array
    (str dims
        &rest keyword-pairs
        &key ((:start start) 0) ((:end end)) &allow-other-keys)

    "Облочка для make-array, которая позволяет создать массив строк 
        инициализированного подстрокой, определяемой с помощью индексов start и end"
    
    (print keyword-pairs)


    (apply #'make-array dims
        :initial-element
            (subseq str start end)
        :allow-other-keys t
    keyword-pairs))

(print (make-string-array "Hello, World!" '(2 6) :start 7 :end 13 :element-type 'string))

;;; &allow-other-keys пропускает ключевой аргумент :element-type 'string, который передаётся в make-array

(format t "~%")

;;; applay vs funcall
;;;
;;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Calling-Functions.html
;;; apply calls function with arguments , just like funcall but with one difference: 
;;; the last of arguments is a list of objects, which are passed to function as separate arguments,
;;; rather than a single list. We say that apply spreads this list so that each individual element becomes an argument.


