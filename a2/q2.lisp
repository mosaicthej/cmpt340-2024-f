; Problem 2 [35 Points]. 
; Define a polymorphic Tree data type, 
; where a Tree is either a Node with a left subtree, 
;   a value, and a right subtree; or it is Null. 
;
; Also define a companion object for the Tree data type, 
; with the following functions:
;
; (a) [15 Points]. 
; Three polymorphic functions --  
; inOrder, preOrder, and postOrder -- 
;   to traverse the tree in-order, pre-order, and post-order.  
; Each function takes a tree and returns a list with the 
;   contents of the tree when traversed in that order.
;
; reverse a list
(defun rev (L)
  (defun revi (L0 acc) 
    (if (null L0) acc
      (revi (cdr L0) (cons (car L0) acc) )))
  (revi L nil)
)



; (b) [5 Points]. 
; A polymorphic function, search, 
; which takes two arguments 
; — a Tree and a key — and returns a boolean result
; based on whether the key is found in the tree.


; (c) [5 Points]. 
; A polymorphic function, replace, 
; which takes three arguments 
; — a Tree t, a value before, and a value after 
; — and returns t with ALL instances of 
;     before replaced with the value after.

; [Hint: Model your Tree data type after the List data type presented in class.]

 
