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


; They are all DFS. Need a stack
; preorder:
;  Visit the current node (in the figure: position red).
;  Recursively traverse the current node's left subtree.
;  Recursively traverse the current node's right subtree.
; so... visit current, put left on stack, then put right
(defun preOrder (Tree)
  ; [left node right]
  ; left-> car; node -> cadr ; right -> caddr
  (defun preOrderProc (visited current stack)
    (if (null (cadr current)) visited ; base case
      (let ( (noLeft (null (car current)))
             (noRight (null (caddr current)))  )
        (cond 
          ((and noLeft noRight) ;case-> leaf node.
            (preOrderProc  ; pop stack to be next to visit.
              (cons (cadr current) visited) ; visit node
              (car stack) ; pop top of stack
              (cdr stack)))
          (noLeft (preOrderProc ; case -> missing L: move R. Stack unchanged
              (cons (cadr current) visited) ;visit node
              (caddr current) ; move to right, no change to stack
              stack))
          (noRight (preOrderProc; case -> missing R: move L. Stack unchanged
              (cons (cadr current) visited) ; visit
              (car current) ; move to left. no change on stack
              stack))
          (t (preOrderProc ; last case with both childs
              (cons (cadr current) visited)
              (car current) ; move to left, push right to stack
              (cons (caddr current) stack)))))))
  (rev(preOrderProc nil Tree nil) )); initialize


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

 
