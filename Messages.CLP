(deftemplate building
 (slot floors (type INTEGER))
 (slot capacity (type INTEGER))
 (slot filled (type INTEGER))
 (slot elevator (type INTEGER))
)

(deftemplate floor
 (slot name (type INTEGER))
 (slot people (type INTEGER))
 (slot children (type INTEGER))
 (slot adults (type INTEGER))
)

(defrule init
(declare (salience 50))
(initial-fact)

=>

(printout t "HOW MANY FLOORS DO YOU WANT?" crlf)
(bind ?f (read))
(if (< ?f 1) then
	(printout t "YOU NEED TO ADD MORE THAN" ?f "FLOORS" crlf)
	(printout t "PLEASE RESTART THE PROGRAMM" crlf)
	(halt)
)

(printout t "WHICH IS THE MAXIMUM CAPACITY OF THE ELEVATOR?" crlf)
(bind ?cap (read))
(bind ?fi 0)
(printout t "FROM WHICH FLOOR DO U WANT ME TO START THE PROCESS" crlf)
(bind ?e (read))
(while (> ?e ?f)
	(printout t "!!!ERROR!!!" crlf)
	(printout t "THE STARTING FLOOR CAN NOT BE ANY HIGHER THAN "?f clrf)
	(printout t "LETS TRY AGAIN!" crlf)
	(printout t "FROM WHICH FLOOR DO U WANT ME TO START THE PROCESS" crlf)
	(bind ?e (read))
)
(assert (building (floors ?f) (capacity ?cap) (filled ?fi) (elevator ?e)))

(bind ?i 0)
(while (< ?i ?f)
	(bind ?i (+ ?i 1))
	(printout t "WE ARE GOING TO INSERT THE DATA FOR FLOOR NUMBER " ?i crlf)
	(printout t "HOW MANY PEOPLE WILL BE ON THE FLOOR" crlf)
	(bind ?p (read))
	(if (< ?p 0) then
		(bind ?p 0)
		(printout t "!!!ERROR!!!" crlf)
		(printout t "PEOPLE CAN NOT BE LESS THAN ZERO" clrf)
		(printout t "LETS TRY AGAIN!" crlf)
		(printout t "HOW MANY PEOPLE WILL BE ON THE FLOOR" crlf)
		(bind ?i (- ?i 1))
	else (if (> ?p 0) then
		(printout t "HOW MANY CHILDREN WILL BE ON THE FLOOR?" crlf)
		(bind ?c (read))
		(if (>= ?c ?p) then
			(printout t "!!!ERROR!!!" crlf)
			(printout t "CHILDERN CAN NOT BE MORE THAN " (- ?p 1) crlf)
			(printout t "LETS TRY AGAIN!" crlf)
		)
	)
	else (if (= ?p 0) then
		(bind ?c 0)
		(bind ?a 0)
		(assert (floor (name ?i) (people 0) (children 0) (adults 0)))
	)
)
)
(bind ?a (- ?p ?c)) 
(bind ?name (+ ?i +0))
(assert (floor (name ?name) (people ?p) (children ?c) (adults ?a)))
)



(defrule end_of_evacuation        "building evacuated"
(elevator is_at 0 has 0)
?i <- (building (floors ?f) (capacity ?cap) (filled ?fi) (elevator ?e))
?j <- (floor (name ?name) (people ?p) (children ?c) (adults ?a))
(bind ?x 0)
(bind ?y 0)
		=>
(while (< ?x ?f) 
	(if (and (eq ?name ?i) (= ?p 0)) then
		(bind ?x (+ ?x 1))
		(bind ?y (+ ?y 1))
	else
		(bind ?x (= ?x ?f))
	)
)
(if (and (= ?x ?f) (= ?y ?f))
		 then
		(modify ?i (filled 0) (elevator 0))
		(modify ?j (people 0) (children 0) (adults 0))
		(printout t "END OF EVACUATION" crlf)
		(halt)
	)
)

(defrule go_to_0_A      "elevator is full"
(declare (salience 10))
(floor (name ?name) (people ?p) (children ?c) (adults ?a))
?i <- (building (floors ?f) (capacity ?cap) (filled ?fi) (elevator ?e))
   => 
	(if (= ?fi ?cap)
		 then
		(modify ?i (filled 0) (elevator 0))
	)
)

(defrule go_to_base_B      "no_more people to take"
(declare (salience 8))
?i <- (building (floors ?f) (capacity ?cap) (filled ?fi) (elevator ?e))
?j <- (floor (name ?name&~?namex) (people ?p) (children ?c) (adults ?a))
		=>
	(if (and (= ?p 0) (> ?fi 0)) then
      (modify ?i (filled 0) (elevator 0))
	  (modify ?j (people 0) (children 0) (adults 0))
	  (printout t "ALL FLOORS ARE CLEAR" crlf)
	)
)

(defrule move_to_any		"elevator moves to any floor"
(declare (salience 8))
?i <- (building (floors ?f) (capacity ?cap) (filled ?fi) (elevator ?e))
?j <- (floor (name ?name&~?name2) (people ?p) (children ?c) (adults ?a))
   =>
	(if (and (neq ?name ?e) (> ?p 0) (< ?fi ?cap))
		 then
		(modify ?i (elevator ?name))
	)
)

(defrule enter_from_any      "people from any floor enter the elevator"
?j <- (building (floors ?f) (capacity ?cap) (filled ?fi) (elevator ?e))
?i <- (floor (name ?name) (people ?p) (children ?c) (adults ?a))
   =>
	(if (and (> ?p 0) (>= (- ?cap (+ ?p ?fi)) 0)) ;Children and adults on the floor, everyone fits
			 then
			(modify ?i (people 0) (children 0) (adults 0))
			(modify ?j (filled (+ ?fi ?p)))
      else (if (and (> ?c 0) (> ?a 0) (< (- ?cap (+ ?p ?fi)) 0)) ;Children and adults on the floor, not everyone fits
				then
				(modify ?i (people (- ?p (- ?cap ?fi))) (children (- ?cap (- ?c ?fi 1))) (adults (- ?p ?c 1)))
				(modify ?j (filled ?cap))
			)
	  else (if (and (= ?c 0) (> ?a 0) (< (- ?cap (+ ?p ?fi))0)); Adults only on the floor,not everyone fits
				then
 			    (modify ?i (people (- ?p (- ?cap ?fi))) (children 0) (adults (- ?a (- ?cap ?fi))))
 			    (modify ?j (filled ?cap))

 			)
	)
)