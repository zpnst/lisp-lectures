(defun ext-square-eq-root (&key ((:a a)) ((:b b)) ((:c c)) )
    "Формирует список массивов длины 'floor(|x|)' 
        для каждого 'x', где 'x' - корень уравнения"
    (if (= a 0)
        (error "'a = 0 => Not a square equation!'")
    (labels 
        ((discr (a b c)
            (- (* b b) (* 4 a c)))    
        (root (a b c foo)
            (/ (funcall foo (- b)
                    (sqrt (discr a b c))) (* 2 a))))
        (let ((rt1 (root a b c #'+)) (rt2 (root a b c #'-)))
            (list (make-array (floor (abs rt1))
                    :initial-element rt1) 
              (make-array (floor (abs rt2))
                    :initial-element rt2))))))
    
(print (ext-square-eq-root :a 1 :b 1 :c -6))
(print (ext-square-eq-root :a 1 :b 2 :c -6))
(print (ext-square-eq-root :a 1 :b 2 :c 3))
