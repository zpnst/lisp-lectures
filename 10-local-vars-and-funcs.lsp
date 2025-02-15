;;;; Локальные переменные и функции (формы связывания)

;;; Lisp - первый язык, в котором была реализована сборка мусора! Джон Маккрти сделал это в 1959 году! 65 лет назад...
;;; Lisp опередил время, так как массовое применение в языках программирования сборки мусора нашла только через четверть века!

;;; (let список_переменных список_форм)
(print (let (x y) (list x y)))
(print (let ((x 0) (y 1)) (list x y)))

(print(let ((x (random 100)) (y (random 100))) (list x y)))

(format t "~%")

;;; Значение связывается с именем переменной и остаётся доустпным до конца формы let, то есть фо всех формах, входящих в let
;;; Формы в let выполняются последовательно, let обеспечивает неявный progn
;;; Значением формы let становится, как ни странно, значение последней вычисленно формы в let <(0_0)>
;;; Значения переменных в let можно изменять, в отличие от того же Haskell
(let ((x 42)) 
    (print x)
    (format t "~r~%" x))

(let ((string "Hello, World!"))
    (format t "~a~%" string))

(format t "~%")

;;; Если внутри одной формы let содержится другая, то переменные с одинаковыми именами будут скрывать переменные связанные во внешних формах
;;; При выходе из let восстанавливаются значения предыдущего связывания
(let ((x 1) (y 2))
    (format t "x = ~d, y = ~d~%" x y)
    (let ((x 11) (y 22))
        (format t "x = ~d, y = ~d~%" x y))
    (format t "x = ~d, y = ~d~%" x y)
    (setf x 0)
    (format t "x = ~d, y = ~d~%" x y))
;;; То есть значения переменныз зависят от того, в каком месте программы и в какой конструкции let они были заданы, поэтому такие переменные часто называются лексическими

;;; *** - LET: variable DISC has no value
;;; Так как в форме let сначала вычисляются все формы инициализации переменных и только потом они присваиваются соответствующим переменным
;;; Во время вычисления x1 с переменной disc ещё не связанно никакое значение

; (defun square-eqn-roots (&key a b c) ; (defun square-eqn-roots (&key ((:a a)) ((:b b)) ((:c c)))
;     "Функция возвращает список
;         корней квадратного уравнения
;         с коэффицентами a b c"
;     (let ((disc (- (* b b) (* 4 a c))) 
;             (x1 (/ (- (- 0 b) (sqrt disc)) (* 2 a))) 
;             (x2 (/ (+ (- 0 b) (sqrt disc)) (* 2 a))))
;         (list x1 x2)))

;;; В форме let* инициализация и присваивание значений переменным происходит последовтаельно
(defun square-eqn-roots (&key a b c) ; (defun square-eqn-roots (&key ((:a a)) ((:b b)) ((:c c)))
    "Функция возвращает список
        корней квадратного уравнения
        с коэффицентами a b c"
    (let* ((disc (- (* b b) (* 4 a c))) 
            (x1 (/ (- (- 0 b) (sqrt disc)) (* 2 a))) 
            (x2 (/ (+ (- 0 b) (sqrt disc)) (* 2 a))))
        (list x1 x2)))

(defun square-eqn-roots-check (&key a b c) ; (defun square-eqn-roots (&key ((:a a)) ((:b b)) ((:c c)))
    "Функция возвращает список 
        корней квадратного уравнения
        с коэффицентами a b c"
    (if (= a 0)
        (error "Не квадратное уравнение (a = 0)")
        (let* ((disc (- (* b b) (* 4 a c))) 
            (x1 (/ (- (- 0 b) (sqrt disc)) (* 2 a))) 
            (x2 (/ (+ (- 0 b) (sqrt disc)) (* 2 a))))
        (list x1 x2))))

; (print (square-eqn-roots-check :a 0 :b 1 :c 2)) ; *** - Не квадратное уравнение (a = 0)
(print (square-eqn-roots-check :a 1 :b 0 :c -4))
(print (square-eqn-roots-check :a 3 :b -2 :c 6))

(format t "~%")

;;; Можно объявить локальные переменные прмяо в лямбда-списке(списке аргумнетов функции) после ключевого слова &aux
;;; Так как каждая форма let или let* занимает как миниум одну строчку и добавляет лишний уровень вложенности
(defun sum-n-1 (seq n)
    (let ((res 0))
        (dotimes (i n)
            (incf res (elt seq i)))
    res))

(print (sum-n-1 '(1 2 3) 3))
(print (sum-n-1 #(1 2 3) 3))

(format t "~%")

(defun sum-n-2 (seq n &aux (res 0))
    (dotimes (i n)
        (incf res (elt seq i)))
    res)

;;; переменная res за аргумент не считается, ей не должен соответствовать фактический аргумент в месте вызова функции
;(print (sum-n-2 #(1 2 3) 3 10)) ; *** - EVAL/APPLY: too many arguments given to SUM-N-2
(print (sum-n-2 '(1 2 3) 3))
(print (sum-n-2 #(1 2 3) 3))

(format t "~%~%")

;;; Ещё одной полезной формой связывания является макрос destructuring-bind
; (destructuring-bind список_переменных 
;     исходный_список список_форм)

;;; Деконструкция списка
;;; Например, функция возвращает результат в виде списка (например square-eqn-roots-check)
;;; Если мы хотим получить отдельные значения их списка корней и связать их с переменными, то мы можем использовать макрос destructuring-bind
(destructuring-bind (x1 x2) (square-eqn-roots-check :a 1 :b 0 :c -4)
    (format t "x1 = ~a. x2 = ~a~%" x1 x2))

; (destructuring-bind (x1 x2) '(1 2 3) ; *** - The object to be destructured should be a list with 2 elements, not (1 2 3).
;     (format t "x1 = ~a. x2 = ~a~%" x1 x2))

;;; можно использовать &optional и &key!
(destructuring-bind (x1 &optional (x2 -1 x2-p)) '(22 55)
    (format t "x1 = ~a. x2 = ~a x2-p = ~a~%" x1 x2 x2-p))

(destructuring-bind (x1 &optional (x2 -1 x2-p)) '(22)
    (format t "x1 = ~a. x2 = ~a x2-p = ~a~%" x1 x2 x2-p))

;;; Макрос destructuring-bing интересен тем, что можем подвергнуть деконструкции список любой сложности
;;; Для этого структура списка переменных должна повторять структуру исходного списка
(print 
    (destructuring-bind (x1 ((x2)(x3)(x4(((x5))))(x6))((((((((((x7 x8 x9))))x10)))))))
        '(11 ((22)(33)(44 (((55))))(66))((((((((((77 88 99)))) 111)))))))
        (list x1 x2 x3 x4 x5 x6 x7 x8 x9 x10)))

(format t "~%~%")

;;; Можно создавать не только локальные переменные, но и локальные функции c помощью flet и labels! 
;;; flet - аналог let, но только для определения функций, lables - аналог let*, но только для определения функций
(defun square-eqn-roots-2 (&key a b c)
    (labels 
        ((discr (a b c)
            (- (* b b) (* 4 a c)))
        (root (a b c func)
            (/ (funcall func (- b)
                (sqrt (discr a b c))) (* 2 a))))
    (list
        (root a b c #'+)
        (root a b c #'-))))

(print (square-eqn-roots-2 :a 1 :b 0 :c -4))

(format t "~%~%")

(flet ((hello-world () 
    (format t "Hello, World!~%")))
    
(hello-world))