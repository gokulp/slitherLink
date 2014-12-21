(setq flag 0)

(defun 2d-array-to-list (array)
  (loop for i below (array-dimension array 0)
        collect (loop for j below (array-dimension array 1)
                      collect (aref array i j))))

(defun list-to-2d-array (list)
  (make-array (list (length list)
                    (length (first list)))
              :initial-contents list))

(defun print-board (array)
  (terpri)
  (loop for i below (array-dimension array 0)
        :do (loop for j below (array-dimension array 1)
                  ;:do (if (or (equal (aref array i j) #\x) (equal (aref array i j) NIL)) (princ #\Space) (princ (aref array i j)))) (terpri)))
                  :do (if (equal (aref array i j) NIL) (princ #\Space) (princ (aref array i j)))) (terpri)))

(defun reset-grid(array string)
  (setq in (open string))
  (loop for i below (array-dimension array 0)
        :do (loop for j below (array-dimension array 1)
           :do(setq temp (read-char in))
              (when (equal temp #\+)     (setf (aref array i j) #\+))
              (when (equal temp #\Space) (setf (aref array i j) #\Space))
              (when (numberp(temp))      (setf (aref array i j) temp))))
  (print-board(array))
  (close in))

(defun at-corner(array x y)
   (setq ylim (array-dimension array 1) xlim (array-dimension array 0))
   (if (and (equal x 1) (or (equal y 1) (equal y (- ylim 2))))              (return-from at-corner T))
   (if (and (equal x (- xlim 2)) (equal y (- ylim 2)))                      (return-from at-corner T))
   (if (and (equal x (- xlim 2)) (equal y 1))                               (return-from at-corner T))
   (return-from at-corner NIL))
   
(defun at-top-left(array x y xlim ylim ele)
    (if(equal (aref array 1 1) ele)
                (return-from at-top-left T) (return-from at-top-left NIL)))

(defun at-top-right(array x y xlim ylim ele)
    (if(equal (aref array 1 (- ylim 2) ) ele)
                 (return-from at-top-right T) (return-from at-top-right NIL)))

(defun at-bottom-left(array x y xlim ylim ele)
    (if(equal (aref array (- xlim 2) 1) ele)          
                (return-from at-bottom-left T) (return-from at-bottom-left NIL)))

(defun at-bottom-right(array x y xlim ylim ele)
    (if(equal (aref array (- xlim 2) (- ylim 2)) ele) 
                (return-from at-bottom-right T) (return-from at-bottom-right NIL)))

(defun check-diagonal-ele-BR(array x y xlim ylim ele)
    (if(and (< y (- ylim 2)) (< x (- xlim 2)) (equal (aref array (+ x 2) (+ y 2)) ele)) 
                (return-from check-diagonal-ele-BR T) (return-from check-diagonal-ele-BR NIL)))

(defun check-diagonal-ele-BL(array x y xlim ylim ele)
    (if(and (> y 2) (< x (- xlim 2)) (equal (aref array (+ x 2) (- y 2)) ele))
                (return-from check-diagonal-ele-BL T) (return-from check-diagonal-ele-BL NIL)))

(defun check-diagonal-ele-TR(array x y xlim ylim ele)
    (if(and (< y (- ylim 2)) (>= x 3) (equal (aref array (- x 2) (+ y 2)) ele)) 
                (return-from check-diagonal-ele-TR T) (return-from check-diagonal-ele-TR NIL)))

(defun check-diagonal-ele-TL(array x y xlim ylim ele)
    (if(and (>= y 3) (>= x 3) (equal (aref array (- x 2) (- y 2)) ele)) 
                (return-from check-diagonal-ele-TL T) (return-from check-diagonal-ele-TL NIL)))

(defun check-adj-ele-top(array x y xlim ylim ele)
    (if(and (> x 2) (equal (aref array (- x 2) y) ele))
                (return-from check-adj-ele-top T) (return-from check-adj-ele-top NIL)))

(defun check-adj-ele-bottom(array x y xlim ylim ele)
    (if(and (< x (- xlim 2)) (equal (aref array (+ x 2) y) ele)) 
                (return-from check-adj-ele-bottom T) (return-from check-adj-ele-bottom NIL)))

(defun check-adj-ele-right(array x y xlim ylim ele)
    (if(and (< y (- ylim 2)) (equal (aref array x (+ y 2)) ele)) 
                (return-from check-adj-ele-right T) (return-from check-adj-ele-right NIL)))

(defun check-adj-ele-left(array x y xlim ylim ele)
    (if(and (> y 2) (equal (aref array x (- y 2)) ele)) 
                (return-from check-adj-ele-left T) (return-from check-adj-ele-left NIL)))

(defun zero-at-diagonal(array x y xlim ylim)
       (if (or (check-diagonal-ele-BR array x y xlim ylim 0) (check-diagonal-ele-BL array x y xlim ylim 0) 
                (check-diagonal-ele-TR array x y xlim ylim 0) (check-diagonal-ele-TL array x y xlim ylim 0)) 
               (return-from zero-at-diagonal T) (return-from zero-at-diagonal NIL)))

(defun place-3s-constraints(array)
   (setq ylim (array-dimension array 1) xlim (array-dimension array 0))
   (loop for i below (array-dimension array 0)
      :do (loop for j below (array-dimension array 1)
         :do (when (equal (aref array i j) 3)
               (get-3-constraints array i j)))))

(defun get-3-constraints(array x y)
   (setq ylim (array-dimension array 1) xlim (array-dimension array 0))
   ;(print "applying 3s constraints")
   (when (at-corner array x y)
        ;(print "3 at-corner")
        (when (at-top-left array x y xlim ylim 3) ;top-left corner                  
                  (setf (aref array 0 1) #\-     
                        (aref array 1 0) #\|))
        (when (at-top-right array x y xlim ylim 3) ;top-right corner
                  (setf (aref array 0 (- ylim 2)) #\-     
                        (aref array 1 (- ylim 1)) #\|))
        (when (at-bottom-left array x y xlim ylim 3) ;bottom-left corner
                  (setf (aref array (- xlim 2) 0) #\|     
                        (aref array (- xlim 1) 1) #\-))
        (when (at-bottom-right array x y xlim ylim 3) ;bottom-right corner
                  (setf (aref array (- xlim 2) (- ylim 1)) #\|     
                        (aref array (- xlim 1) (- ylim 2)) #\-)))
   (when (not (at-corner array x y)) 
       (when (check-diagonal-ele-TL array x y xlim ylim 3)
                  (setf (aref array (+ x 1) (+ y 0)) #\-
                        (aref array (+ x 0) (+ y 1)) #\|
                        (aref array (- x 2) (- y 3)) #\|
                        (aref array (- x 3) (- y 2)) #\-)
                   (setq flag 1)                          )
       (when (check-diagonal-ele-BL array x y xlim ylim 3) 
                  (setf (aref array (- x 1) (- y 0)) #\-
                        (aref array (+ x 0) (+ y 1)) #\|  
                        (aref array (+ x 2) (- y 3)) #\| 
                        (aref array (+ x 3) (- y 2)) #\-) 
                   (setq flag 1)                          )
       (when (check-diagonal-ele-BR array x y xlim ylim 3)
                  (setf (aref array (- x 1) (+ y 0)) #\-
                        (aref array (+ x 0) (- y 1)) #\| 
                        (aref array (+ x 2) (+ y 3)) #\| 
                        (aref array (+ x 3) (+ y 2)) #\-)
                   (setq flag 1)                          )
       (when (check-diagonal-ele-TR array x y xlim ylim 3)
                  (setf (aref array (- x 1) (+ y 0)) #\-
                        (aref array (+ x 0) (- y 1)) #\| 
                        (aref array (- x 2) (+ y 3)) #\| 
                        (aref array (- x 3) (+ y 2)) #\-)
                   (setq flag 1)                          )
       (when (check-adj-ele-right array x y xlim ylim 3) 
                  (setf (aref array (+ x 0) (- y 1)) #\| 
                        (aref array (- x 0) (+ y 3)) #\| 
                        (aref array (- x 0) (+ y 1)) #\|)
                   (setq flag 1)                          )
       (when (check-adj-ele-left array x y xlim ylim 3) 
                  (setf (aref array (+ x 0) (- y 1)) #\|     
                        (aref array (- x 0) (- y 3)) #\|     
                        (aref array (- x 0) (+ y 1)) #\|)
                   (setq flag 1)                          )
       (when (check-adj-ele-top array x y xlim ylim 3) 
                  (setf (aref array (+ x 1) (- y 0)) #\-     
                        (aref array (- x 1) (- y 0)) #\-     
                        (aref array (- x 3) (+ y 0)) #\-)
                   (setq flag 1)                          )
       (when (check-adj-ele-bottom array x y xlim ylim 3)
                  (setf (aref array (- x 1) (- y 0)) #\-     
                        (aref array (+ x 1) (- y 0)) #\-     
                        (aref array (+ x 3) (+ y 0)) #\-) 
                   (setq flag 1)                          ))
   (when (and (not (at-corner array x y)) (zero-at-diagonal array x y xlim ylim))
        (when (check-diagonal-ele-TL array x y xlim ylim 0) (setf (aref array (- x 0) (- y 1)) #\| (aref array (- x 1) (- y 0)) #\-) (setq flag 1))
        (when (check-diagonal-ele-TR array x y xlim ylim 0) (setf (aref array (- x 0) (+ y 1)) #\| (aref array (- x 1) (+ y 0)) #\-) (setq flag 1))
        (when (check-diagonal-ele-BL array x y xlim ylim 0) (setf (aref array (+ x 0) (- y 1)) #\| (aref array (+ x 1) (- y 0)) #\-) (setq flag 1))
        (when (check-diagonal-ele-BR array x y xlim ylim 0) (setf (aref array (+ x 0) (+ y 1)) #\| (aref array (+ x 1) (+ y 0)) #\-) (setq flag 1)))
   (when (equal (aref array (- x 1) (+ y 0)) #\x) (setf (aref array (+ x 1) y) #\- (aref array x (- y 1)) #\| (aref array x (+ y 1)) #\|) (setq flag 1))
   (when (equal (aref array (+ x 1) (+ y 0)) #\x) (setf (aref array (- x 1) y) #\- (aref array x (- y 1)) #\| (aref array x (+ y 1)) #\|) (setq flag 1))
   (when (equal (aref array (+ x 0) (- y 1)) #\x) (setf (aref array (+ x 1) y) #\- (aref array (- x 1) y) #\- (aref array x (+ y 1)) #\|) (setq flag 1))
   (when (equal (aref array (+ x 0) (+ y 1)) #\x) (setf (aref array (+ x 1) y) #\- (aref array (- x 1) y) #\- (aref array x (- y 1)) #\|) (setq flag 1))
   (when (and (equal (aref array (+ x 1) y) #\-) (equal (aref array (- x 1) y) #\-) (equal (aref array x (- y 1)) #\|))
         (setf (aref array (+ x 0) (+ y 1)) #\x))
   (when (and (equal (aref array (+ x 1) y) #\-) (equal (aref array (- x 1) y) #\-) (equal (aref array x (+ y 1)) #\|))
         (setf (aref array (+ x 0) (- y 1)) #\x))
   (when (and (equal (aref array (- x 1) y) #\-) (equal (aref array x (- y 1)) #\|) (equal (aref array x (+ y 1)) #\|))
         (setf (aref array (+ x 1) (+ y 0)) #\x))
   (when (and (equal (aref array (+ x 1) y) #\-) (equal (aref array x (- y 1)) #\|) (equal (aref array x (+ y 1)) #\|))
         (setf (aref array (- x 1) (+ y 0)) #\x))
)

(defun place-0s-constraints(array)
   (loop for i below (array-dimension array 0)
      :do (loop for j below (array-dimension array 1)
         :do (when (equal (aref array i j) 0)
               (get-0-constraints array i j)))))

(defun get-0-constraints(array x y)
    ;(print "applying zero constraints at ")(print x) (print y)
    (setf (aref array (- x 1) y) #\x (aref array (+ x 1) y) #\x (aref array x (- y 1)) #\x (aref array x (+ y 1)) #\x flag 1)
    (when (check-diagonal-ele-TL array x y xlim ylim 1) (setf (aref array (- x 2) (- y 1)) #\x (aref array (- x 1) (- y 2)) #\x) flag 1)
    (when (check-diagonal-ele-TR array x y xlim ylim 1) (setf (aref array (- x 2) (+ y 1)) #\x (aref array (- x 1) (+ y 2)) #\x) flag 1)
    (when (check-diagonal-ele-BL array x y xlim ylim 1) (setf (aref array (+ x 2) (- y 1)) #\x (aref array (+ x 1) (- y 2)) #\x) flag 1)
    (when (check-diagonal-ele-BR array x y xlim ylim 1) (setf (aref array (+ x 2) (+ y 1)) #\x (aref array (+ x 1) (+ y 2)) #\x) flag 1)
)

(defun place-1s-constraints(array)
        (when (at-top-left array 0 0 xlim ylim 1) ;top-left corner                  
                  (setf (aref array 0 1) #\x     
                        flag 1
                        (aref array 1 0) #\x))
        (when (at-top-right array 0 0 xlim ylim 1) ;top-right corner
                  (setf (aref array 0 (- ylim 2)) #\x     
                        flag 1
                        (aref array 1 (- ylim 1)) #\x))
        (when (at-bottom-left array 0 0 xlim ylim 1) ;bottom-left corner
                  (setf (aref array (- xlim 2) 0) #\x     
                        flag 1
                        (aref array (- xlim 1) 1) #\x))
        (when (at-bottom-right array 0 0 xlim ylim 1) ;bottom-right corner
                  (setf (aref array (- xlim 2) (- ylim 1)) #\x     
                        flag 1
                        (aref array (- xlim 1) (- ylim 2)) #\x))
   (loop for i below (array-dimension array 0)
      :do (loop for j below (array-dimension array 1)
         :do (when (equal (aref array i j) 1)
               (apply-1-constraints array i j))))
)

(defun apply-1-constraints(array x y)
      (when (equal (aref array x (- y 1)) #\|)
          (setf (aref array x (+ y 1)) #\x (aref array (+ x 1) y) #\x (aref array (- x 1) y) #\x))
      (when (equal (aref array x (+ y 1)) #\|)
          (setf (aref array x (- y 1)) #\x (aref array (+ x 1) y) #\x (aref array (- x 1) y) #\x))
      (when (equal (aref array (+ x 1) y) #\-)
          (setf (aref array x (+ y 1)) #\x (aref array x (- y 1)) #\x (aref array (- x 1) y) #\x))
      (when (equal (aref array (- x 1) y) #\-)
          (setf (aref array x (+ y 1)) #\x (aref array x (- y 1)) #\x (aref array (+ x 1) y) #\x))
      (when (and (equal (aref array x (+ y 1)) #\x) (equal (aref array x (- y 1)) #\x) (equal (aref array (+ x 1) y) #\x))
          (setf (aref array (- x 1) y) #\-))
      (when (and (equal (aref array x (+ y 1)) #\x) (equal (aref array x (- y 1)) #\x) (equal (aref array (- x 1) y) #\x))
          (setf (aref array (+ x 1) y) #\-))
      (when (and (equal (aref array (- x 1) y) #\x) (equal (aref array x (- y 1)) #\x) (equal (aref array (+ x 1) y) #\x))
          (setf (aref array x (+ y 1)) #\|))
      (when (and (equal (aref array (- x 1) y) #\x) (equal (aref array x (+ y 1)) #\x) (equal (aref array (+ x 1) y) #\x))
          (setf (aref array x (- y 1)) #\|))
      
)

(defun place-2s-constraints(array)
        (when (at-top-left array 0 0 xlim ylim 2) ;top-left corner                  
                  (setf (aref array 0 3) #\-     
                        flag 1
                        (aref array 3 0) #\|))
        (when (at-top-right array 0 0 xlim ylim 2) ;top-right corner
                  (setf (aref array 0 (- ylim 4)) #\-     
                        flag 1
                        (aref array 3 (- ylim 1)) #\|))
        (when (at-bottom-left array 0 0 xlim ylim 2) ;bottom-left corner
                  (setf (aref array (- xlim 4) 0) #\|     
                        flag 1
                        (aref array (- xlim 1) 3) #\-))
        (when (at-bottom-right array 0 0 xlim ylim 2) ;bottom-right corner
                  (setf (aref array (- xlim 4) (- ylim 1)) #\|     
                        flag 1
                        (aref array (- xlim 1) (- ylim 4)) #\-))
   (loop for i below (array-dimension array 0)
      :do (loop for j below (array-dimension array 1)
         :do (when (equal (aref array i j) 2)
               (get-2-constraints array i j))))
)

(defun get-2-constraints(array x y)
    ;(when (and (equal (aref (+ x 1) (+ y 0)) #\x) (equal (aref (- x 1) (+ y 0)) #\x)) 
               ;(setf (aref (+ x 0) (+ y 1)) #\| (aref (+ x 0) (- y 1)) #\|))
    ;;(when (and (equal (aref (+ x 1) (+ y 0)) #\x) (equal (aref (+ x 0) (+ y 1)) #\x)) 
               ;(setf (aref (- x 1) (+ y 0)) #\| (aref (+ x 0) (- y 1)) #\|))
    ;(when (and (equal (aref (+ x 1) (+ y 0)) #\x) (equal (aref (- x 1) (+ y 0)) #\x)) 
               ;(setf (aref (+ x 0) (+ y 1)) #\| (aref (+ x 0) (- y 1)) #\|))
    ;(when (and (equal (aref (+ x 1) (+ y 0)) #\x) (equal (aref (- x 1) (+ y 0)) #\x)) 
               ;(setf (aref (+ x 0) (+ y 1)) #\| (aref (+ x 0) (- y 1)) #\|))
    ;;Two cross are present around two
    (when (and (equal (aref array (- x 1) y) #\x) (equal (aref array x (- y 1)) #\x))
          (setf (aref array (+ x 1) y) #\- (aref array x (+ y 1)) #\|) )
    (when (and (equal (aref array (- x 1) y) #\x) (equal (aref array x (+ y 1)) #\x))
          (setf (aref array (+ x 1) y) #\- (aref array x (- y 1)) #\|) )
    (when (and (equal (aref array (- x 1) y) #\x) (equal (aref array (+ x 1) y) #\x))
          (setf (aref array x (+ y 1)) #\| (aref array x (- y 1)) #\|) )
    (when (and (equal (aref array (+ x 1) y) #\x) (equal (aref array x (- y 1)) #\x))
          (setf (aref array (- x 1) y) #\- (aref array x (+ y 1)) #\|) )
    (when (and (equal (aref array (+ x 1) y) #\x) (equal (aref array x (+ y 1)) #\x))
          (setf (aref array (- x 1) y) #\- (aref array x (- y 1)) #\|) )
    (when (and (equal (aref array x (- y 1)) #\x) (equal (aref array x (+ y 1)) #\x))
          (setf (aref array (+ x 1) y) #\- (aref array (- x 1) y) #\-) )
    ;;Two edges are present around two
    (when (and (equal (aref array (- x 1) y) #\-) (equal (aref array x (- y 1)) #\|))
          (setf (aref array (+ x 1) y) #\x (aref array x (+ y 1)) #\x) )
    (when (and (equal (aref array (- x 1) y) #\-) (equal (aref array x (+ y 1)) #\|))
          (setf (aref array (+ x 1) y) #\x (aref array x (- y 1)) #\x) )
    (when (and (equal (aref array (- x 1) y) #\-) (equal (aref array (+ x 1) y) #\-))
          (setf (aref array x (+ y 1)) #\x (aref array x (- y 1)) #\x) )
    (when (and (equal (aref array (+ x 1) y) #\-) (equal (aref array x (- y 1)) #\|))
          (setf (aref array (- x 1) y) #\x (aref array x (+ y 1)) #\x) )
    (when (and (equal (aref array (+ x 1) y) #\-) (equal (aref array x (+ y 1)) #\|))
          (setf (aref array (- x 1) y) #\x (aref array x (- y 1)) #\x) )
    (when (and (equal (aref array x (- y 1)) #\|) (equal (aref array x (+ y 1)) #\|))
          (setf (aref array (+ x 1) y) #\x (aref array (- x 1) y) #\x) )
)

(defun corner-adjust(array)
   (setq xlim (array-dimension array 0) ylim (array-dimension array 1))
   (loop for i below (array-dimension array 0)
      :do (loop for j below (array-dimension array 1)
         :do ;(when (equal (aref array i j) "+") 
               (apply-joining-constraints array i j)));)
   (hborder array)
   (vborder array))

;No use of setting flag in this function
(defun apply-joining-constraints(array x y)
    (setq xlim (array-dimension array 0) ylim (array-dimension array 1))
    (when (and (> x 0) (> y 0) (< x (- xlim 1)) (< y (- ylim 1)))
          (when (and (equal (aref array (+ x 1) y) #\|) (equal (aref array x (+ y 1)) #\-)) 
                (setf (aref array (- x 1) y) #\x (aref array x (- y 1)) #\x)) 
          (when (and (equal (aref array (+ x 1) y) #\|) (equal (aref array x (- y 1)) #\-)) 
                (setf (aref array (- x 1) y) #\x (aref array x (+ y 1)) #\x)) 
          (when (and (equal (aref array (- x 1) y) #\|) (equal (aref array x (- y 1)) #\-)) 
                (setf (aref array (+ x 1) y) #\x (aref array x (+ y 1)) #\x)) 
          (when (and (equal (aref array (- x 1) y) #\|) (equal (aref array x (+ y 1)) #\-)) 
                (setf (aref array (+ x 1) y) #\x (aref array x (- y 1)) #\x)) 
          (when (and (equal (aref array (- x 1) y) #\|) (equal (aref array (+ x 1) y) #\|)) 
                (setf (aref array x (+ y 1)) #\x (aref array x (- y 1)) #\x)) 
          (when (and (equal (aref array x (+ y 1)) #\-) (equal (aref array x (- y 1)) #\-)) 
                (setf (aref array (- x 1) y) #\x (aref array (+ x 1) y) #\x)) 
))

(defun check-and-add-constraints(array)
      (place-3s-constraints array)
      (place-2s-constraints array)
      (place-1s-constraints array)
      (place-0s-constraints array)
      (corner-adjust array)
      (if(equal flag 1) (return-from check-and-add-constraints T) (return-from check-and-add-constraints NIL)))

;(defun try)
