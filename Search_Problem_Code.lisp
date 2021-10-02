;starting search

(defun searchProblem (start-state goal method )
  (print '____BEGIN_SEARCHING_____ )
    (findSolution (MakeFront start-state) (MakeQueue start-state) () goal method )
)

;Basic recursive function to create search tree (recursive tree expantion

(defun FindSolution (front queue closed goal method )
  (cond
    ((null front)                  'no_solution)
    ((mymember (car front) closed) (FindSolution (cdr front) (cdr queue) closed  goal method  ))
    ((equal (car front) goal)      (print "This is the solution: ") (reverse (first queue)))
    (T      (FindSolution (ExpandFront front method) (ExtendQueue queue method)  (cons (car front)  closed) goal method  ))
  )
)

;initialization of front

(defun MakeFront (node)
  (list node)
)

;expanding front

(defun ExpandFront (front method)
  (cond
    ( (eq method 'DFS)    (append  (removeNils (findchildren (car front)))    (cdr front)))
    ( (eq method 'BFS)    (append (cdr front) (removeNils (findchildren (car front)))))
	( (eq method 'Bestfs) (append (rest front) (removeNils (findchildren (first front)))))
  )
)

;initialization of queue

(defun MakeQueue (node)
  (list (list node))
)


;expanding queue


(defun ExtendQueue (queue method)
  (cond
    ( (eq method 'DFS)	  (append  (growPath (car queue))     (rest queue)  )  )
    ( (eq method 'BFS)	  (append (rest queue) (growPath (car queue))))
    ( (eq method 'BestFS) (SORT  (append (rest queue)(growPath (first queue)) ) #'best-choice2)	)
  )
)

;growing path towards each different child of the selected parent node


(defun growPath (path)
  (removecycles (grow1 path (removeNils (findchildren (car path)))))
)

(defun grow1 (path children)
  (cond
    ((null children) nil                                                            )
    ( T              (cons (cons (car children) path) (grow1 path (cdr children)))  )
  )
)


;Supportive functions


(defun mymember(x Y)
  (cond
    ((endp y)            nil                   )
    ((equal x (first y)) T                     )
    (T                  (mymember x (rest y))  )
  )
)

(defun removeNils (X)
  (cond
    ((endp x)            nil                                    )
    ((eq (first x) NIL) (removeNils (rest x))                   )
    (T                  (cons (first x) (removeNils (rest x)))  )
  )
)

(defun removecycles (paths)
  (cond
    ((null paths)                        nil                                          )
    ((member (caar paths) (cdar paths)) (removecycles (cdr paths))                    )
    (T                                  (cons (car paths) (removecycles (cdr paths))) )
  )
)

(defun best-choice2 (state1 state2)
(cond
   ((< (FIRST (FIRST state1)) (FIRST (FIRST state2)))nil)
   (T nil)
))

;operators

(setq volume 5)  

(defun elevatorup1 (state)
  (cond
	((or (= (third state) 0) (>= (third state) 3))nil)
    ((and (= (third state) 2) (<= (second state) 3))
            (list 1 (+ (second state) (third state)) 0 (fourth state) (fifth state)))
    ((and (= (second state) 4) (> (third state) 1))
            (list 1 5 (- (third state) 1) (fourth state) (fifth state)))
    ((and (= (third state) 1) (<= (second state) 4))
            (list 1 (+ (second state) (third state)) 0 (fourth state) (fifth state)))
    ;If the floor has 2 or less people they get into the elevator.
    (T NIL) ;It returns the list with next state
))


(defun elevatorup2 (state)
  (cond
	((and (= (second state) 1) (>= (fourth state) 1) (<= (fourth state) 4))
			(list 2 (+ (fourth state) (second state)) (third state) 0 (fifth state)))
	((and (= (second state) 1) (= (fourth state) 5))
			(list 2 5 (third state) 1 (fifth state)))
	((and (= (second state) 1) (= (fourth state) 6))
			(list 2 5 (third state) 2 (fifth state)))
	((and (= (second state) 2) (>= (fourth state) 1) (<= (fourth state) 3))
			(list 2 (+ (fourth state) (second state)) (third state) 0 (fifth state)))
	((and (= (second state) 2) (= (fourth state) 4))
			(list 2 5 (third state) 1 (fifth state)))
	((and (= (second state) 2) (= (fourth state) 5))
			(list 2 5 (third state) 2 (fifth state)))
	((and (= (second state) 2) (= (fourth state) 6))
			(list 2 5 (third state) 3 (fifth state)))		
	((and (= (second state) 3) (>= (fourth state) 1) (<= (fourth state) 2))
			(list 2 (+ (fourth state) (second state)) (third state) 0 (fifth state)))
	((and (= (second state) 3) (= (fourth state) 3))
			(list 2 5 (third state) 1 (fifth state)))
	((and (= (second state) 3) (= (fourth state) 4))
			(list 2 5 (third state) 2 (fifth state)))
    ((and (= (second state) 3) (= (fourth state) 5))
			(list 2 5 (third state) 3 (fifth state)))
	((and (= (second state) 3) (= (fourth state) 6))
			(list 2 5 (third state) 4 (fifth state)))
	((and (= (second state) 4) (>= (fourth state) 1) (<= (fourth state) 1))
			(list 2 (+ (fourth state) (second state)) (third state) 0 (fifth state)))
	((and (= (second state) 4) (= (fourth state) 2))
			(list 2 5 (third state) 1 (fifth state)))
	((and (= (second state) 4) (= (fourth state) 3))
			(list 2 5 (third state) 2 (fifth state)))
    ((and (= (second state) 4) (= (fourth state) 4))
			(list 2 5 (third state) 3 (fifth state)))
	((and (= (second state) 4) (= (fourth state) 5))
			(list 2 5 (third state) 4 (fifth state)))
    ((and (= (second state) 4) (= (fourth state) 6))
			(list 2 5 (third state) 5 (fifth state)))
    ((and (= (fourth state) 6) (< (second state) volume))
            (list 2 5 (third state) (- (fourth state) (- volume (second state))) (fifth state)))
    ((and (< (fourth state) 6) (> (fourth state) 0) (< (second state)  volume))
            (list 2 (- volume (- volume (fourth state))) (third state) 0 (fifth state)))
    ;If the floor has 6 or less people they get into the elevator.
    (T NIL) ;It returns the list with next state
))


(defun elevatorup3 (state)
  (cond
    ((= (fifth state) 0)nil)
    ((and (<= (fifth state) 4) (= (second state) 0))
             (list 3 (+ (second state) (fifth state)) (third state) (fourth state) 0))
    ((and (= (fifth state) 4) (<= (second state) 4))
             (list 3 (+ (second state) (- volume (second state))) (third state) (fourth state) (- (fifth state) (- volume (second state)))))
    ((and  (< (fifth state) 4) (> (fifth state) 0) (< (second state) 3))
             (list 3 (+ (second state) (fifth state)) (third state) (fourth state) 0))
	((and (< (fifth state) 4) (> (fifth state) 0) (= (second state) 4))
             (list 3 5 (third state) (fourth state) (- (fifth state) 1)))
    ((and  (< (fifth state) 3) (> (fifth state) 1) (< (second state) 4))
             (list 3 (+ (second state) (fifth state)) (third state) (fourth state) (- (fifth state) (- volume (second state))) ))
    ((and  (= (fifth state) 3)  (= (second state) 3))
             (list 3 5 (third state) (fourth state) 1))
    ((and  (= (fifth state) 1) (= (second state) 3))
             (list 3 4 (third state) (fourth state) 0))
    ;If the floor has 4 or less people they get into the elevator.
    (T NIL) ;It returns the list with next state
))

(defun empty (state)
      (cond
           ((and (>= (second state) 0) (= 0 (third state)) (= 0 (fourth state)) (= 0 (fifth state))
                (list 0 0 0 0 0)))
           ; CHECKS IF ALL THE FLOORS ARE CLEAR AND IF THERE ARE PEOPLE IN THE ELEVATOR IN ORDER TO BRING THEM TO THE GORUND FLOOR.

           ((= (second state) 5) (list 0 0 (third state) (fourth state) (fifth state)))
           ; CHECK IF THE ELEVATOR IS FULL IN ORDER TO BRING THE PEOPLE TO THE GROUND FLOOR.
           (T nil)
 ))


;function to find the children nodes of a parent state node

 (defun findchildren (state)
       (setq volume 5)
       (list  (elevatorup1 state)
              (elevatorup2 state)
              (elevatorup3 state)
 	           (empty state))
              ;IT RETURNS A LIST WITH THE RESULTS OF THE 4 FUNCTIONS.
 )