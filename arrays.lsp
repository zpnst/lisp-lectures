;;;; Lisp Arrays

;;; В массиве могут хранится элемнеты разного типа данных
;;; Имеет смысл использовать там, где число элементов последовательности, которую надо хрнаить в памяти может привысить эффективную по скорости величину для списков
;;; Массивы динамиечские(размер модет менятся в runtime) 


;;; Максимальный индекс в массиве
(print array-dimension-limit) ; 4294967296

;;; Максимальное количество элементов в массиве
(print array-total-size-limit)

;;; Создадим массив
(defvar array1 (make-array 5)) ; одномерные массивы
(print array1)

(defvar array2 (make-array '(3 3 #|... n|#))) ; многомерные массивы
(print array2)

(format t "~%")

(defvar a (make-array '(3 3)))
(print (aref a 1 1)) ; в Си a[1][1], *(*(a + 1) + 1) =)

(setf (aref a 1 1) 42) ; a[1][1] = 42
(print (aref a 1 1))
(print a)

(format t "~%")

;;; дополнительные или ключевые аргуметы
(setf arr1 (make-array 3 
    :initial-element 42))

(print arr1)

(setf arr2 (make-array 5
    :initial-contents '(1 2 3 4 5)))

(print arr2)

(setf arr3 (make-array '(2 2)
    :initial-contents (list (make-array 2 :initial-contents '(1 2))
                            (make-array 2 :initial-contents '(3 4)))))

(print arr3)

(setf arr4 (vector 11 22 33 44 55)) ; более быстрый сопсоб создания одномерного массива с заданным количеством аргументов
(print arr4)

(format t "~%")

;;; Хранит ли переменная ссылку на массив
(defvar somevar 1)
(print (arrayp somevar))
(print (arrayp arr4))

(defvar *a* #2A(
    (1 #\2)
    ("3" '(4 4 4 4))))

(defvar *b* #1A(
    (1 #\2)
    ("3" '(4 4 4 4))))

(format t "~%")
(print (array-dimensions *a*))
(print (array-dimensions *b*))

(print *a*)
(print *b*)

(format t "~%")

;;; Изменение массива в runtime

(defvar rarray (make-array 3
    :initial-contents '(1 2 3)))

(print rarray)

(setf rarray (adjust-array rarray 6))

(print rarray)

(defvar *<[0_0]>* (make-array '(2 2)))
(print (array-total-size *<[0_0]>*)) ; общее количество элементов в массиве

(format t "~%")

;;; Реализация стека с помощью вектора с указателем заполгнения
(defvar stack (make-array 5
    :fill-pointer 0))

(vector-push 42 stack)
(vector-push "string" stack)

(print stack)

(print (vector-pop stack))
(print (vector-pop stack))

(print stack)

(format t "~%")

(defvar hyper (make-array '(5 5 5 5)))

(setf (aref hyper 4 3 1 3) 42)

(print 
    (block outer
        (dotimes (i 5)
            (dotimes (j 5)
                (dotimes (k 5)
                    (dotimes (s 5)
                        (when (eq 42 (aref hyper i j k s))
                            (return-from outer
                                (list i j k s)))))))))




