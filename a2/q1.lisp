
; Problem 1 [45 Points]. 
; The following set of problems is about shuffling of cards, 
;  particularly something called the Faro shuffle, 
;  where the stack is broken up into two, 
;  and then combined so that a card from one sub-stack is 
;   followed by one from the other, and so on.    
;
; A perfect Faro shuffle breaks up the stack into two sub-stacks 
;  of exactly the same size, 
; and then combines them in the manner described above.  
;
; An out-shuffle results in the top and the bottom cards 
;  of the stack remaining the same after the shuffle; 
; An in-shuffle results in these cards becoming the 
;  second and the second last cards of the shuffled stack.

;[Note: Do not use built-in higher-order functions for this problem]

; (a) [10 Points].  
; Write a polymorphic function, shuffle, 
;  which takes two lists l1 and l2 representing decks of cards, 
;  and returns the result of a combining of the lists.  
; Particularly, the returned list contains the first element of l1, 
;  followed by the first element of l2, 
;  followed by the second element of l1, and so on.  
; If one of the lists ends before the other, 
; the other list’s remaining elements are simply added to the end.
(defun shuffle (l1 l2)
   (cond ((null l1) l2)
         ((null l2) l1)
         (t (cons (car l1) (shuffle l2 (cdr l1))))
   )
)

(defun shuffle_test1 ()
  (let (
        (A '(1 2 3 4))
        (B '(a b c d))
       )
    (shuffle A B)
  )
)

(defun shuffle_test2 ()
  (let (
        (A '(1 2 3 4))
        (B '(a b c d X Y Z K))
       )
    (shuffle A B)
  )
)

(defun shuffle_test3 ()
  (let (
        (A '(1 2 3 4 5 6 7 8))
        (B '(a b c d))
       )
    (shuffle A B)
  )
)

(defun shuffle_test ()
  (print (shuffle '(1 2 3) '(a b c)))
  (print (shuffle '(1 2 3) '(a b c d e)))
  (print (shuffle '(1 2 3 4 5) '(a b c)))
  t
)

