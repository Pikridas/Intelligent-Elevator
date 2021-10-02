;;;    FACTS

(deffacts ini       "initial facts"
    (capacity 5)
    (elevator is_at 0 has 0)
    (floor 1 has 2)
    (floor 2 has 6)
    (floor 3 has 4)
)

;;; GENERAL RULES


(defrule end_of_evacuation        "building evacuated"
    (elevator is_at 0 has 0)
    (floor 1 has 0)
    (floor 2 has 0)
    (floor 3 has 0)
   =>
   (printout t "END OF EVACUATION")
   (halt)
)



;;; GO_TO BASE FLOOR RULES

(defrule go_to_0_A      "elevator is full"
      (declare (salience 10))
 ?i<- (elevator is_at ?x has 5)  
   => 
      (retract ?i)
      (assert (elevator is_at 0 has 0))) 


(defrule go_to_base_B      "no_more people to take"
      (declare (salience 8))
 ?i<- (elevator is_at ?x has ?y)
      (test (> ?y 0))
      (floor 1 has 0)
      (floor 2 has 0)
      (floor 3 has 0)
        
   => 
      (retract ?i)
      (assert (elevator is_at 0 has 0))) 

(defrule move_to_1          "elevator moves to 1st floor"
 ?i<- (elevator is_at ?x has ?y)
      (test (not (= ?x 1)))
      (floor 1 has ?n)
       (test (not (= ?n 0)))    
   => 
      (retract ?i)
      (assert (elevator is_at 1 has ?y)))

(defrule move_to_2          "elevator moves to 2nd floor" ?i<- (elevator is_at ?x has ?y)      (test (not (= ?x 2)))       (floor 2 has ?n)      (test (not (= ?n 0)))    =>       (retract ?i)      (assert (elevator is_at 2 has ?y)))

(defrule move_to_3          "elevator moves to 3rd floor"
 ?i<- (elevator is_at ?x has ?y)
      (test (not (= ?x 3)))       (floor 3 has ?n)      (test (not (= ?n 0)))    =>       (retract ?i)      (assert (elevator is_at 3 has ?y)))


;;; RULES FOR entering lift

(defrule enter_from_1      
              "people at 1st floor enter the elevator"
      (declare (salience 5))
 ?i<- (elevator is_at 1 has ?p)
 ?j<- (floor 1 has ?r)
     (test (> ?r 0))
      (capacity ?c)
   => 
      (retract ?i ?j)
      (if (>= (- ?c (+ ?p ?r)) 0) 
          then 
          (assert (elevator is_at 1 has (+ ?p ?r))
                  (floor 1 has 0))
          else
          (assert (elevator is_at 1 has ?c)                   
                  (floor 1 has (- ?r (- ?c  ?p))))))                
  
(defrule enter_from_2                    "people at 2nd floor enter the elevator"      (declare (salience 5)) ?i<- (elevator is_at 2 has ?p) ?j<- (floor 2 has ?r)     (test (> ?r 0))      (capacity ?c)   =>       (retract ?i ?j)      (if (>= (- ?c (+ ?p ?r)) 0)           then           (assert (elevator is_at 2 has (+ ?p ?r))                  (floor 2 has 0))          else          (assert (elevator is_at 2 has ?c)                                     (floor 2 has (- ?r (- ?c  ?p))))))

(defrule enter_from_3                    "people at 3rd floor enter the elevator"      (declare (salience 5)) ?i<- (elevator is_at 3 has ?p) ?j<- (floor 3 has ?r)     (test (> ?r 0))      (capacity ?c)   =>       (retract ?i ?j)      (if (>= (- ?c (+ ?p ?r)) 0)           then           (assert (elevator is_at 3 has (+ ?p ?r))                  (floor 3 has 0))          else          (assert (elevator is_at 3 has ?c)                                     (floor 3 has (- ?r (- ?c  ?p))))))
				  
				  
;ΑΠΟΤΕΛΕΣΜΑΤΑ ΚΩΔΙΚΑ

         CLIPS (6.30 3/17/15)
CLIPS> (load "C:/Users/Admin/Desktop/Code-of-simple-evacuation-problem.CLP")
Defining deffacts: ini
Defining defrule: end_of_evacuation +j+j+j+j+j
Defining defrule: go_to_0_A +j+j
Defining defrule: go_to_base_B +j+j+j+j+j
Defining defrule: move_to_1 +j+j+j
Defining defrule: move_to_2 +j+j+j
Defining defrule: move_to_3 +j+j+j
Defining defrule: enter_from_1 +j+j+j+j
Defining defrule: enter_from_2 +j+j+j+j
Defining defrule: enter_from_3 +j+j+j+j
TRUE
CLIPS> (reset)
<== f-0     (initial-fact)
==> f-0     (initial-fact)
==> f-1     (capacity 5)
==> f-2     (elevator is_at 0 has 0)
==> f-3     (floor 1 has 2)
==> f-4     (floor 2 has 6)
==> f-5     (floor 3 has 4)
CLIPS> (run)
FIRE    1 move_to_3: f-2,f-5
<== f-2     (elevator is_at 0 has 0)
==> f-6     (elevator is_at 3 has 0)
FIRE    2 enter_from_3: f-6,f-5,f-1
<== f-6     (elevator is_at 3 has 0)
<== f-5     (floor 3 has 4)
==> f-7     (elevator is_at 3 has 4)
==> f-8     (floor 3 has 0)
FIRE    3 move_to_1: f-7,f-3
<== f-7     (elevator is_at 3 has 4)
==> f-9     (elevator is_at 1 has 4)
FIRE    4 enter_from_1: f-9,f-3,f-1
<== f-9     (elevator is_at 1 has 4)
<== f-3     (floor 1 has 2)
==> f-10    (elevator is_at 1 has 5)
==> f-11    (floor 1 has 1)
FIRE    5 go_to_0_A: f-10
<== f-10    (elevator is_at 1 has 5)
==> f-12    (elevator is_at 0 has 0)
FIRE    6 move_to_1: f-12,f-11
<== f-12    (elevator is_at 0 has 0)
==> f-13    (elevator is_at 1 has 0)
FIRE    7 enter_from_1: f-13,f-11,f-1
<== f-13    (elevator is_at 1 has 0)
<== f-11    (floor 1 has 1)
==> f-14    (elevator is_at 1 has 1)
==> f-15    (floor 1 has 0)
FIRE    8 move_to_2: f-14,f-4
<== f-14    (elevator is_at 1 has 1)
==> f-16    (elevator is_at 2 has 1)
FIRE    9 enter_from_2: f-16,f-4,f-1
<== f-16    (elevator is_at 2 has 1)
<== f-4     (floor 2 has 6)
==> f-17    (elevator is_at 2 has 5)
==> f-18    (floor 2 has 2)
FIRE   10 go_to_0_A: f-17
<== f-17    (elevator is_at 2 has 5)
==> f-19    (elevator is_at 0 has 0)
FIRE   11 move_to_2: f-19,f-18
<== f-19    (elevator is_at 0 has 0)
==> f-20    (elevator is_at 2 has 0)
FIRE   12 enter_from_2: f-20,f-18,f-1
<== f-20    (elevator is_at 2 has 0)
<== f-18    (floor 2 has 2)
==> f-21    (elevator is_at 2 has 2)
==> f-22    (floor 2 has 0)
FIRE   13 go_to_base_B: f-21,f-15,f-22,f-8
<== f-21    (elevator is_at 2 has 2)
==> f-23    (elevator is_at 0 has 0)
FIRE   14 end_of_evacuation: f-23,f-15,f-22,f-8
END OF EVACUATION[PRCCODE4] Execution halted during the actions of defrule end_of_evacuation.
CLIPS> 
