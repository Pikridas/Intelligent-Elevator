;;;    FACTS

(deffacts ini       "initial facts"
	(capacity 7); SHOWS THE MAXIMUM CAPACITY OF THE ELEVATOR
    (elevator is_at 0 has 0); SHOWS IN WHICH FLOOR THE ELEVATOR IS AND HOW MANY PEOPLE HAS INSIDE
    (floor 1 has 2 and 1); SHOWS HOW MANY PEOPLE (CHILDERN AND ADULTS) FLOOR 1 HAS
    (floor 2 has 2 and 1); SHOWS HOW MANY PEOPLE (CHILDERN AND ADULTS) FLOOR 2 HAS
    (floor 3 has 2 and 1); SHOWS HOW MANY PEOPLE (CHILDERN AND ADULTS) FLOOR 3 HAS
)

;; RULE FOR END OF EVACUATION
(defrule end_of_evacuation        "building evacuated"
(elevator is_at 0 has 0)
(floor ?floor has ?p and ?c)
	(floor ?floor2&~?floor has ?p&0 and ?c&0)
	=>
	 (printout t "END OF EVACUATION" crlf)
	 (halt)
)

;; RULE FOR GOING TO FLOOR "0" WHEN ELEVATOR IS FULL 
(defrule go_to_0_A      "elevator is full"
?i<- (elevator is_at ?x has 7) 
(floor ?floor has ?p&~0 and ?c&~0) 
	=> 
    (retract ?i)
    (assert (elevator is_at 0 has 0))
)

;; RULE FOR GOING TO FLOOR "0" WHEN ELEVATOR HAS PEOPLE BUT THE FLOORS ARE EMPTY 
(defrule go_to_base_B      "no_more people to take"
?i<- (elevator is_at ?x has ?y)
(floor ?floor&~?x has ?p&0 and ?c&0)
=>
 (if (> ?y 0)
		then
		(retract ?i)
		(assert (elevator is_at 0 has 0))
		(printout t "ALL FLOORS ARE CLEAR" crlf)
	)
)

; RULE FOR MOVING FROM ANY FLOOR TO ANY FLOOR
(defrule move_to_any		"elevator moves to any floor"
?i <- (elevator is_at floor ?x has ?y)
   (floor ?floor2&~?x has ?p and ?c)
 =>
(if (> ?p 0)
	then
	(retract ?i)
	(assert (elevator is_at floor ?x has ?y))
)
)

;RULE FOR PEOPLE ENTERING THE ELEVATOR FROM ANY FLOOR 
(defrule enter_from_any      "people from any floor enter the elevator"
 ?i<- (elevator is_at ?x has ?y)
 ?j<- (floor ?floor2 has ?p and ?c)
      (capacity ?cap)
   => 
	(if (and (neq ?x ?floor2) (> ?p 0) (> ?c 0));CASE WHERE THERE ARE ADULTS AND CHILDREN IN THE FLOOR
     then
        (if (and (> ?p 0) (> ?c 0) (>= (- ?cap (+ ?p ?y)) 0) ); CASE WHERE EVERYONE CAN FIT IN THE ELEVATOR
			then
		    (retract ?i ?j)
			(assert (floor ?floor2 has 0 and 0))
            (assert (elevator is_at ?floor2 has (+ ?p ?y)))
			
       else (if (and (neq ?x ?floor2) (> ?c 0) (>= (- ?cap (+ ?y ?c 1)) 0));CASE WHERE ALL CHILDREN PLUS ONE ADULT CAN FIT IN THE ELEVATOR
			then
		    (retract ?i ?j)
			(assert (floor ?floor2 has (- ?p ?c 1) and 0))
            (assert (elevator is_at ?floor2 has (+ ?y ?c 1)))
			)
		
		else (if (and (neq ?x ?floor2) (> ?c 0) (< (- (- ?cap ?y 1) ?c) 0));CASE WHERE ALL CHILDREN PLUS ONE ADULT CAN NOT FIT IN THE ELEVATOR
			then
			(retract ?i ?j)
			(assert (floor ?floor2 has (- ?p (- ?c (- ?cap ?y))) and (- ?c (- ?cap ?y 1))))
			(assert (elevator is_at ?floor2 has ?cap))
			)
		)
    else (if (and (neq ?x ?floor2) (> ?p 0) (= ?c 0));CASE IF THERE PEOPLE BUT NO CHILDREN
	        then
		(if (and (neq ?x ?floor2) (= ?c 0) (>= (- ?cap (+ ?p ?y)) 0)); CASE WHERE ALL ADULTS CAN FIT IN THE ELEVATOR 
			then
		    (retract ?i ?j)
			(assert (floor ?floor2 has 0 and 0))
            (assert (elevator is_at ?floor2 has (+ ?y ?p)))
		else (if (and (neq ?x ?floor2) (= ?c 0) (< (- ?cap (+ ?p ?y)) 0)); CASE WHERE ALL ADULTS CAN NOT FIT IN THE ELEVATOR
			then
			(retract ?i ?j)
			(assert (floor ?floor2 has (- ?p (- ?cap ?y)) and 0))
			(assert (elevator is_at ?floor2 has ?cap))
		)
	  )
	)
  )
)