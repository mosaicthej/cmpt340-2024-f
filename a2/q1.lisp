
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

(defun shuffle_test ()
  (print (shuffle '(1 2 3) '(a b c)))
  (print (shuffle '(1 2 3) '(a b c d e)))
  (print (shuffle '(1 2 3 4 5) '(a b c)))
  t
)

; made rev public since it's useful in many cases
;@args:
; L   - remaining list
; acc - progress-so-far 
(defun rev (L)
  (defun revi (L0 acc) 
    (if (null L0) acc
      (revi (cdr L0) (cons (car L0) acc) )))
  (revi L nil)
)



;(b) [7 Points]. 
; Write a polymorphic function, split, 
; which takes as parameters a list 
; and an Integer n, indicating the splitting point.  
;
; It splits the contents of the provided list into two lists, 
;  the first with the first n elements of the list (in order), 
;  and the second with the remaining elements (again, in order).  
; These two lists are then returned as a list of two lists.
(defun split (L n)
  ;@args:
  ; listPair - a dotted pair representing two stacks
  ; n - number of where from stk2 should put after stk1
  (defun split_help (listPair n)
    (cond ((eq 0 n) listPair ) ; start from idx=0, directly 
          ((null (cdr listPair)) listPair) 
      ; if stack2 is empty, don't go further; else, 
      (t (split_help 
            (cons    ; move 1 card from stk2 to stk1
              (cons (cadr listPair) (car listPair))
              (cddr listPair) )
            (- n 1)  ; mark the progress
    ) ) ) ) ; note, stk1 is in reverse order

  (let
    (   
      (pair (split_help (cons nil L) n) )   
    )
    (cons (rev (car pair)) (cons (cdr pair) nil) )
    ; return a list of both stacks... both in-order...
  )

)


; macro that counts (one 1 level) the number of things in list
(defun alen (L) 
  (defun aalen (L n) (if (null L) n (aalen (cdr L) (+ n 1))))
  (aalen L 0)
)
; (c) [8 Points]. 
; Write two polymorphic functions, 
; outshuffle and inshuffle, 
; which use shuffle and split functions described above 
; (and possibly additional intermediary functions), 
; and carry out perfect Faro shuffles of the 
;  out-shuffle and in-shuffle type.  
;
; In other words, 
; they take a single stack of an even number of cards, 
; break them up and return the result of applying the 
;  required type of Faro shuffle exactly once.
;
; say, have (1 2 3 4 5 6)
; break into (1 2 3) (4 5 6)
; out-shuffle:
; ↓
; 1  2  3
;  4  5  6 ←
;
; in-shuffle:
; → 1  2  3
; 4  5  6 ←
(defun doFaro (L outsflP)
  (let ( (brk (split L (/ (alen L) 2))) )
    ; brk is the breaked up deck
    (if outsflP
        (shuffle (car brk) (cadr brk))
        (shuffle (cadr brk) (car brk)) )
    )
)

(defun outShuffle (L)
  (doFaro L t)
)

(defun inShuffle (L)
  (doFaro L nil)
)

; (d) [10 Points]. 
; Write a polymorphic function, 
; nshuffle, 
; which takes three parameters,
; - a shuffle function (such as outshuffle or inshuffle), 
; - an integer indicating how many shuffles to carry out, 
; - and a list to shuffle, which is of even length.   
; 
; It then carries out 
; - the required number of shuffles on the list, and 
;   returns the result.
(defun nshuffle (shuffleFunc n L)
  (if (eq n 0) L  ; basecase to return L, else, do 1 shuffle
    (nshuffle shuffleFunc (- n 1) (funcall shuffleFunc L))
    )
)

;As part of your testing of howManyShuffles, try to find out:
; i) How many out-shuffles are required to 
;    return a stack of 52 cards to its original ordering?
; ii) How many in-shuffles are required to 
;    completely reverse a stack of 52 cards?
;
; I think there should be a closed-form formula...
; yes... says here: 
; - https://en.wikipedia.org/wiki/Faro_shuffle#Perfect_shuffles
; - https://oeis.org/A002326
; So, to restore the deck to original, need k shuffles where:
; - outshuffle: $ 2^k \equiv 1 (mod n-1) $
; - inshuffle : $ 2^k \equiv 1 (mod n+1) $
; k is the multiplicative order of 2.
;
;
; infact, any position for any card can be binary encoded...
; This is fun... I'll look into it...
; ... now just to demonstrate.

; first generate any number of cards...
(defun gennL (n)
  (defun genna(n accu)
    (if (eq n 0) accu
      (cons n (genna (- n 1) nil ))))
  (rev (genna n nil))
)

; also a function that finds the multiplicative order
; find x that: $ r^x \equiv 1 (mod n) $
; in successive calls, 
;   k is the count for exponent, and
;   v is the value of $r^k$
(defun mord (n r)
  (defun morda (n r k v)
    (if (> (gcd r n) 1) 0 ; mul ord exists only if n r coprime.
      (if (eq 1 (mod v n)) k ; found it!
        (morda n r (+ 1 k) (* v r) ))))
  (morda n r 1 r)
)

; testing outshuffle which restores the order of cards
; first find the k that (2^k \equiv 1 (mod n-1))
; (2 is coprime with all odd numbers, so must be a match)
; then print the original deck, the 
(defun outshuffle_res (n) ; testing with n cards
  (if (eq (mod n 2) 1) nil ; has to be even number
    (let (  (cards (gennL n) )
            (k (mord (- n 1) 2))  )
      ; now, if we shuffle k times, it will restore.
      ; return the result of outshuffle k times
      ; also return k
      (cons k (cons (nshuffle 'outshuffle k cards) nil))
    )
  )
)

; For using in-shuffle to acheive reverse-order, 
; perform x in-shuffle where: $ 2^x \equiv n (mod n+1) $
; It can proved with some Discrete logarithm theories
;  that i won't expand here....
;
; I'll call this "drom" since it's quite like the opposite
; of "mord" above.
; Basically modular arithmetic with special case.
; Given n and r, find that
; $ r^x \equiv n-1 (mod n)
(defun drom (n r)
  (defun droma (n r k v)
    (if (> k n) 0 ; at most n iterations before looping
      (if (eq (- n 1) (mod v n)) k ; found it!
        (droma n r (+ k 1) (* v r) ) ; keep going
      )))
  (droma n r 1 r)
)


; Below are code that gets you a real deck 
; (with suit and face)
;
; generating a suit (given the symbol and num per suit)
(defun genSuitN (symbo n &optional accu)
  (defun genSuita (accua n)
    (if (eq n 0) accua
      (genSuita (cons (cons n symbo) accua) ; put card in front
                (- n 1))))
  (genSuita accu n)
)

; more generic deck generation.
; provide a list of "suits" as symbols
;   and number of cards per suit
(defun genDeckGeneric (ss nc)
  (defun gdgAccu (ss nc accu)
    (if (null ss) accu
      (gdgAccu (cdr ss) nc 
               (genSuitN (car ss) nc accu))))
  (gdgAccu ss nc nil)
)
