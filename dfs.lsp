;;Check if placing edge at x and y can form a verticle T
(defun check-up-t (array x y)
     (setq xlim (array-dimension array 0) ylim (array-dimension array 1))
     (if(and (> (mod x 2) 0) (= (mod y 2) 0) (> x 0) (< x (- xlim 1)) (> y 0) (< y (- ylim 1)))
         (if(and (equal (aref array (- x 1) (- y 1)) #\-) (equal (aref array (- x 1) (+ y 1)) #\-))
            (return-from check-up-t T)))
     (return-from check-up-t NIL))

;;Check if placing edge at x and y can form a upside down T
(defun check-down-t (array x y)
     (setq xlim (array-dimension array 0) ylim (array-dimension array 1))
     (if(and (> (mod x 2) 0) (= (mod y 2) 0) (> x 0) (< x (- xlim 1)) (> y 0) (< y (- ylim 1)))
         (if(and (equal (aref array (+ x 1) (- y 1)) #\-) (equal (aref array (+ x 1) (+ y 1)) #\-))
            (return-from check-down-t T)))
     (return-from check-down-t NIL))

;;Check if placing edge at x and y can form a left side |-
(defun check-left-t(array x y)
     (setq xlim (array-dimension array 0) ylim (array-dimension array 1))
     (if(and (= (mod x 2) 0) (> (mod y 2) 0) (> x 0) (< x (- xlim 1)) (> y 0) (< y (- ylim 1)))
         (if(and (equal (aref array (- x 1) (- y 1)) #\|) (equal (aref array (- x 1) (+ y 1)) #\|))
            (return-from check-left-t T)))
     (return-from check-left-t NIL))

;;Check if placing edge at x and y can form a right side -|
(defun check-right-t(array x y)
     (setq xlim (array-dimension array 0) ylim (array-dimension array 1))
     (if(and (= (mod x 2) 0) (> (mod y 2) 0) (> x 0) (< x (- xlim 1)) (> y 0) (< y (- ylim 1)))
         (if(and (equal (aref array (+ x 1) (- y 1)) #\|) (equal (aref array (+ x 1) (+ y 1)) #\|))
            (return-from check-right-t T)))
     (return-from check-right-t NIL))

;;check if edge is present on top
(defun edge-top(array x y)
     (if (= x 0) (return-from edge-top NIL))
     (if (equal (aref (- x 1) y) #\|) (return-from edge-top T) (return-from edge-top NIL)))

;;check if edge is present on down 
(defun edge-down(array x y)
     (if (= x (- (array-dimension array 0) 1)) (return-from edge-down NIL))
     (if (equal (aref (+ x 1) y) #\|) (return-from edge-down T) (return-from edge-down NIL)))

;;check if edge is present on top
(defun edge-left(array x y)
     (if (= y 0) (return-from edge-left NIL))
     (if (equal (aref x (- y 1)) #\-) (return-from edge-left T) (return-from edge-left NIL)))

;;check if edge is present on down 
(defun edge-right(array x y)
     (if (= y (- (array-dimension array 1) 1)) (return-from edge-right NIL))
     (if (equal (aref x (+ y 1)) #\-) (return-from edge-right T) (return-from edge-right NIL)))

;;check if block is present on top
(defun block-top(array x y)
     (if (= x 0) (return-from block-top T))
     (if (equal (aref (- x 1) y) #\x) (return-from block-top T) (return-from block-top NIL)))

;;check if block is present on down 
(defun block-down(array x y)
     (if (= x (- (array-dimension array 0) 1)) (return-from block-down T))
     (if (equal (aref (+ x 1) y) #\x) (return-from block-down T) (return-from block-down NIL)))

;;check if block is present on top
(defun block-left(array x y)
     (if (= y 0) (return-from block-left T))
     (if (equal (aref x (- y 1)) #\x) (return-from block-left T) (return-from block-left NIL)))

;;check if block is present on down 
(defun block-right(array x y)
     (if (= y (- (array-dimension array 1) 1)) (return-from block-right T))
     (if (equal (aref x (+ y 1)) #\x) (return-from block-right T) (return-from block-right NIL)))

;;checks if moving up is fruitful
(defun should-we-cont-up(array x y list1)
     ;(print "should-we-cont-up")(print x)(print y)
     (setq xlim (array-dimension array 0) ylim (array-dimension array 1))
     (if(<= x 1) (return-from should-we-cont-up NIL))
    ; (if(not (is-fruitful array (- x 2) y)) (return-from should-we-cont-up NIL))
     (if(check-if-visited  (list (- x 1) y) list1) (return-from should-we-cont-up NIL))
     (if(equal (aref array (- x 1) y) #\|) (return-from should-we-cont-up T))
     (return-from should-we-cont-up NIL))
     
;;checks if moving down is fruitful
(defun should-we-cont-down(array x y list1)
     ;(print "should-we-cont-down")(print x)(print y)
     (setq xlim (array-dimension array 0) ylim (array-dimension array 1))
     (if(>= x (- xlim 2)) (return-from should-we-cont-down NIL))
    ; (if(not (is-fruitful array (+ x 2) y)) (return-from should-we-cont-down NIL))
     (if(check-if-visited (list (+ x 1) y) list1) (return-from should-we-cont-down NIL))
     (if(equal (aref array (+ x 1) y) #\|) (return-from should-we-cont-down T))
     (return-from should-we-cont-down NIL))
     
;;checks if moving left is fruitful
(defun should-we-cont-left(array x y list1)
     ;(print "should-we-cont-left")(print x)(print y)
     (setq xlim (array-dimension array 0) ylim (array-dimension array 1))
     (if(<= y 1) (return-from should-we-cont-left NIL))
    ; (if(not (is-fruitful array x (- y 2))) (return-from should-we-cont-left NIL))
     (if(check-if-visited  (list x (- y 1)) list1) (return-from should-we-cont-left NIL))
     (if(equal (aref array x (- y 1)) #\-) (return-from should-we-cont-left T))
     (return-from should-we-cont-left NIL))
     
;;checks if moving right is fruitful
(defun should-we-cont-right(array x y list1)
     ;(print "should-we-cont-right")(print x)(print y)
     (setq xlim (array-dimension array 0) ylim (array-dimension array 1))
     (if(>= y (- ylim 2)) (return-from should-we-cont-right NIL))
    ; (if(not (is-fruitful array x (+ y 2))) (return-from should-we-cont-right NIL))
     (if(check-if-visited  (list x (+ y 1)) list1) (return-from should-we-cont-right NIL))
     (if(equal (aref array x (+ y 1)) #\-) (return-from should-we-cont-right T))
     (return-from should-we-cont-right NIL))

;;check if junction is already occupied at point x & y
(defun is-occupied(array x y)
     (setq xlim (array-dimension array 0) ylim (array-dimension array 1) edges 0)
     ;lefty if y is zero then as is or y-1
     (setq leftx x lefty y rightx x righty y topx x topy y downx x downy y)
     (if(> y 0) (decf lefty))
     (if(> x 0) (decf topx))
     (if(< x (- xlim 1)) (incf downx))
     (if(< y (- ylim 1)) (incf righty))
     (if(equal (aref array leftx   lefty   ) #\-) (incf edges))
     (if(equal (aref array rightx  righty  ) #\-) (incf edges))
     (if(equal (aref array topx    topy    ) #\|) (incf edges))
     (if(equal (aref array downx   downy   ) #\|) (incf edges))
     (if(> edges 1) (return-from is-occupied T) (return-from is-occupied NIL)))
                  
;;check if three cross exists at any point then fourth will always be x
(defun is-fruitful(array x y)
     (setq xlim (array-dimension array 0) ylim (array-dimension array 1) cnt 0)
     (setq leftx x lefty y rightx x righty y topx x topy y downx x downy y)
     ;lefty if y is zero then as is or y-1
     (if(> y 0) (decf lefty))
     (if(> x 0) (decf topx))
     (if(< x (- xlim 1)) (incf downx))
     (if(< y (- ylim 1)) (incf righty))
     (if(equal (aref array leftx   lefty   ) #\x) (incf cnt))
     (if(equal (aref array rightx  righty  ) #\x) (incf cnt))
     (if(equal (aref array topx    topy    ) #\x) (incf cnt))
     (if(equal (aref array downx   downy   ) #\x) (incf cnt))
     (if(< cnt 3) (return-from is-fruitful T) (return-from is-fruitful NIL)))
                  
;;checks if moving up is fruitful
(defun should-we-go-up(array x y list1)
     (setq xlim (array-dimension array 0) ylim (array-dimension array 1))
     (if(= x 0) (return-from should-we-go-up NIL))
     (if(not (equal (aref array (- x 1) y) #\Space)) (return-from should-we-go-up NIL))
     (if(is-occupied array x y) (return-from should-we-go-up NIL) (return-from should-we-go-up T))
     (if(not (check-satisfiability array x y 1)) (return-from should-we-go-up NIL))
     (if(check-if-visited  (list (- x 2) y) list1) (return-from should-we-go-up NIL))
     (if(is-occupied array (- x 2) y) (return-from should-we-go-up NIL) (return-from should-we-go-up T)))
     
;;checks if moving down is fruitful
(defun should-we-go-down(array x y list1)
     (setq xlim (array-dimension array 0) ylim (array-dimension array 1))
     (if(= x (- xlim 1)) (return-from should-we-go-down NIL))
     (if(not (equal (aref array (+ x 1) y) #\Space)) (return-from should-we-go-down NIL))
     (if(is-occupied array x y) (return-from should-we-go-down NIL) (return-from should-we-go-down T))
     (if(not (check-satisfiability array x y 2)) (return-from should-we-go-down NIL))
     (if(check-if-visited  (list (+ x 2) y) list1) (return-from should-we-go-down NIL))
     (if(is-occupied array (+ x 2) y) (return-from should-we-go-down NIL) (return-from should-we-go-down T)))
     
;;checks if moving left is fruitful
(defun should-we-go-left(array x y list1)
     (setq xlim (array-dimension array 0) ylim (array-dimension array 1))
     (if(= y 0) (return-from should-we-go-left NIL))
     (if(not (equal (aref array x (- y 1)) #\Space)) (return-from should-we-go-left NIL))
     (if(is-occupied array x y) (return-from should-we-go-left NIL) (return-from should-we-go-left T))
     (if(not (check-satisfiability array x y 3)) (return-from should-we-go-left NIL))
     (if(check-if-visited  (list x (- y 2)) list1) (return-from should-we-go-left NIL))
     (if(is-occupied array x (- y 2)) (return-from should-we-go-left NIL) (return-from should-we-go-left T)))
     
;;checks if moving right is fruitful
(defun should-we-go-right(array x y list1)
     (setq xlim (array-dimension array 0) ylim (array-dimension array 1))
     (if(= y (- ylim 1)) (return-from should-we-go-right NIL))
     (if(not (equal (aref array x (+ y 1)) #\Space)) (return-from should-we-go-right NIL))
     (if(is-occupied array x y) (return-from should-we-go-right NIL) (return-from should-we-go-right T))
     (if(not (check-satisfiability array x y 4)) (return-from should-we-go-right NIL))
     (if(check-if-visited  (list x (+ y 2)) list1) (return-from should-we-go-right NIL))
     (if(is-occupied array x (+ y 2)) (return-from should-we-go-right NIL) (return-from should-we-go-right T)))

;;advance edge and check call path formation with new x y coordinates
;;check if the point given in inputs are starting cordinates of logic if yes return True.
;;This section will traverse edge in top, down, left, right + pushing the direction in list1 after making decision
;;if path cannot be continued we have to put new path for that follow
;;     check blockades if only one option exists put edge there and push direction in list1
;;     otherwise if multiple options are available try all of them in following order down, right, left, top
;;     Check return value of direction calls if return-value is true then no need to call other direction
;;     before calling other direction 
;;     if no direction gives the correct result take car of list1 and 
;;          form a new function create-path-dir (dir is DRLT) 
;;               Put a edge on respective direction. Push the direction element in list1 and call the function path formation from here.
;;               remove the edge and assign cdr of list1 to itself and return false

;;check if we can continue existing path 
;;This function checks for connection from recently visited nodes if present push direction value in list1 and return True
;;otherwise return NIL.
(defun cont-existing-path(array x y list1)
    ;;Check needed here to end recursion-some sort of check against starting position
    ;(print "in cont-existing-path")
    ;(print x)(print y)(print (list-length list1))
    (when (and (= x xstart) (= y ystart) (> (list-length list1) edgeCount)) 
       (if(verify-loop array list1) 
          (return-from cont-existing-path T)  ;if part
          (return-from cont-existing-path NIL))) ;else part
    (when(should-we-cont-up    array x y list1) (if(go-up    array x y list1) (return-from cont-existing-path T)))
    (when(should-we-cont-left  array x y list1) (if(go-left  array x y list1) (return-from cont-existing-path T)))
    (when(should-we-cont-down  array x y list1) (if(go-down  array x y list1) (return-from cont-existing-path T)))
    (when(should-we-cont-right array x y list1) (if(go-right array x y list1) (return-from cont-existing-path T)))
    (when(form-possible-path   array x y list1) (return-from cont-existing-path T) (return-from cont-existing-path NIL))
)

(defun form-possible-path(array x y list1)
    ;(print "form-possible-path")
    (when(should-we-go-up    array x y list1) (push (list (- x 1) y) list1) (setf (aref array (- x 1) y) #\|) 
                                             (if(cont-existing-path array (- x 2) y list1) (return-from form-possible-path T)
                                                (setf list1 (cdr list1) (aref array (- x 1) y) #\Space) ))
    (when(should-we-go-left  array x y list1) (push (list x (- y 1)) list1) (setf (aref array x (- y 1)) #\-) 
                                             (if(cont-existing-path array x (- y 2) list1) (return-from form-possible-path T)
                                                (setf list1 (cdr list1) (aref array x (- y 1)) #\Space) ))
    (when(should-we-go-down  array x y list1) (push (list (+ x 1) y) list1) (setf (aref array (+ x 1) y) #\|) 
                                             (if(cont-existing-path array (+ x 2) y list1) (return-from form-possible-path T)
                                                (setf list1 (cdr list1) (aref array (+ x 1) y) #\Space) ))
    (when(should-we-go-right array x y list1) (push (list x (+ y 1)) list1) (setf (aref array x (+ y 1)) #\-)
                                             (if(cont-existing-path array x (+ y 2) list1) (return-from form-possible-path T)
                                                (setf list1 (cdr list1) (aref array x (+ y 1)) #\Space) ))
    ;(print-board array)
    ;(print list1)
    (return-from form-possible-path NIL))

(defun go-up(array x y list1)
    ;(print "in go-up")(print x)(print y)
    ;(print list1)
    (push (list (- x 1) y) list1)
    (if(cont-existing-path array (- x 2) y list1) (return-from go-up T))
    (setq list1 (cdr list1))
    (return-from go-up NIL))

(defun go-down(array x y list1)
    ;(print "in go-down")(print x)(print y)
    ;(print list1)
    (push (list (+ x 1) y) list1)
    (if(cont-existing-path array (+ x 2) y list1) (return-from go-down T))
    (setq list1 (cdr list1))
    (return-from go-down NIL))

(defun go-left(array x y list1)
    ;(print "in go-left")(print x)(print y)
    ;(print list1)
    (push (list x (- y 1)) list1)
    (if(cont-existing-path array x (- y 2) list1) (return-from go-left T))
    (setq list1 (cdr list1))
    (return-from go-left NIL))

(defun go-right(array x y list1)
    ;(print "in go-right")(print x)(print y)
    ;(print list1)
    (push (list x (+ y 1)) list1)
    (if(cont-existing-path array x (+ y 2) list1) (return-from go-right T))
    (setq list1 (cdr list1))
    (return-from go-right NIL))

;;checks effect of newly added edge on existing constraints
;;dir gives direction of new edge
;; 1 - TOP 
;; 2 - DOWN
;; 3 - LEFT
;; 4 - RIGHT

(defun check-satisfiability(array x y dir)
    (format nil "in CS ~3D ~3D ~3D" x y dir)
    (cond((= dir 1)(if(check-TL array x y)(return-from check-satisfiability (check-TR array x y)) (return-from check-satisfiability NIL)))
         ((= dir 2)(if(check-DL array x y)(return-from check-satisfiability (check-DR array x y)) (return-from check-satisfiability NIL)))
         ((= dir 3)(if(check-TL array x y)(return-from check-satisfiability (check-DL array x y)) (return-from check-satisfiability NIL)))
         ((= dir 4)(if(check-TR array x y)(return-from check-satisfiability (check-DR array x y)) (return-from check-satisfiability NIL))))
    (return-from check-satisfiability NIL))

(defun check-TL(array x y)
    ;(print "TL")
    (if(or (< x 2) (< y 2)) (return-from check-TL T))
    (if(equal (aref array (- x 1) (- y 1)) #\Space) (setq num 3) (setq num (aref array (- x 1) (- y 1))))
    (setq cnt 0 leftx (- x 1) lefty (- y 2) topx (- x 2) topy (- y 1) rightx(- x 1) righty(- y 0) downx (- x 0) downy (- y 1))
    (setq xlim (array-dimension array 0) ylim (array-dimension array 1))
     (if(< leftx 0) (setq leftx 0))
     (if(< lefty 0) (setq lefty 0))
     (if(> rightx (- xlim 1)) (setq rightx (- xlim 1)))
     (if(> righty (- ylim 1)) (setq righty (- ylim 1)))
     (if(< topx 0) (setq topx 0))
     (if(< topy 0) (setq topy 0))
     (if(> downx (- xlim 1)) (setq downx (- xlim 1)))
     (if(> downy (- ylim 1)) (setq downy (- ylim 1)))
    (if(equal (aref array leftx   lefty   ) #\|) (incf cnt))
    (if(equal (aref array rightx  righty  ) #\|) (incf cnt))
    (if(equal (aref array topx    topy    ) #\-) (incf cnt))
    (if(equal (aref array downx   downy   ) #\-) (incf cnt))
    (if(<= (+ cnt 1) num) (return-from check-TL T) (return-from check-TL NIL)))
    
(defun check-DL(array x y)
    ;(print "DL")
    (setq xlim (array-dimension array 0) ylim (array-dimension array 1))
    (if(or (> x (- xlim 3)) (< y 2)) (return-from check-DL T))
    (if(equal (aref array (+ x 1) (- y 1)) #\Space) (setq num 3) (setq num (aref array (+ x 1) (- y 1))))
    (setq cnt 0 leftx (+ x 1) lefty (- y 2) topx (- x 0) topy (- y 1) rightx(+ x 1) righty(- y 0) downx (+ x 2) downy (- y 1))
     (if(< leftx 0) (setq leftx 0))
     (if(< lefty 0) (setq lefty 0))
     (if(> rightx (- xlim 1)) (setq rightx (- xlim 1)))
     (if(> righty (- ylim 1)) (setq righty (- ylim 1)))
     (if(< topx 0) (setq topx 0))
     (if(< topy 0) (setq topy 0))
     (if(> downx (- xlim 1)) (setq downx (- xlim 1)))
     (if(> downy (- ylim 1)) (setq downy (- ylim 1)))
    (if(equal (aref array leftx   lefty   ) #\|) (incf cnt))
    (if(equal (aref array rightx  righty  ) #\|) (incf cnt))
    (if(equal (aref array topx    topy    ) #\-) (incf cnt))
    (if(equal (aref array downx   downy   ) #\-) (incf cnt))
    (if(<= (+ cnt 1) num) (return-from check-DL T) (return-from check-DL NIL)))
    
(defun check-TR(array x y)
    ;(print "TR")
    (setq xlim (array-dimension array 0) ylim (array-dimension array 1))
    (if(or (> y (- ylim 3)) (< x 2)) (return-from check-TR T))
    (if(equal (aref array (- x 1) (+ y 1)) #\Space) (setq num 3) (setq num (aref array (- x 1) (+ y 1))))
    (setq cnt 0 leftx (- x 1) lefty (- y 0) topx (- x 2) topy (+ y 1) rightx(- x 1) righty(+ y 2) downx (- x 0) downy (+ y 1))
     (if(< leftx 0) (setq leftx 0))
     (if(< lefty 0) (setq lefty 0))
     (if(> rightx (- xlim 1)) (setq rightx (- xlim 1)))
     (if(> righty (- ylim 1)) (setq righty (- ylim 1)))
     (if(< topx 0) (setq topx 0))
     (if(< topy 0) (setq topy 0))
     (if(> downx (- xlim 1)) (setq downx (- xlim 1)))
     (if(> downy (- ylim 1)) (setq downy (- ylim 1)))
    (if(equal (aref array leftx   lefty   ) #\|) (incf cnt))
    (if(equal (aref array rightx  righty  ) #\|) (incf cnt))
    (if(equal (aref array topx    topy    ) #\-) (incf cnt))
    (if(equal (aref array downx   downy   ) #\-) (incf cnt))
    (if(<= (+ cnt 1) num) (return-from check-TR T) (return-from check-TR NIL)))
    
(defun check-DR(array x y)
    ;(print "DR")
    (setq xlim (array-dimension array 0) ylim (array-dimension array 1))
    (if(or (> x (- xlim 3)) (> y (- ylim 3))) (return-from check-DR T))
    (if(equal (aref array (+ x 1) (+ y 1)) #\Space) (setq num 3) (setq num (aref array (+ x 1) (+ y 1))))
    (setq cnt 0 leftx (+ x 1) lefty (- y 0) topx (- x 0) topy (+ y 1) rightx(+ x 1) righty(+ y 2) downx (- x 2) downy (+ y 1))
     (if(< leftx 0) (setq leftx 0))
     (if(< lefty 0) (setq lefty 0))
     (if(> rightx (- xlim 1)) (setq rightx (- xlim 1)))
     (if(> righty (- ylim 1)) (setq righty (- ylim 1)))
     (if(< topx 0) (setq topx 0))
     (if(< topy 0) (setq topy 0))
     (if(> downx (- xlim 1)) (setq downx (- xlim 1)))
     (if(> downy (- ylim 1)) (setq downy (- ylim 1)))
    (if(equal (aref array leftx   lefty   ) #\|) (incf cnt))
    (if(equal (aref array rightx  righty  ) #\|) (incf cnt))
    (if(equal (aref array topx    topy    ) #\-) (incf cnt))
    (if(equal (aref array downx   downy   ) #\-) (incf cnt))
    (if(<= (+ cnt 1) num) (return-from check-DR T) (return-from check-DR NIL)))
    
