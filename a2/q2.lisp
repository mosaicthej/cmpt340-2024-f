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
; using this tree as an example to test:
; https://en.wikipedia.org/wiki/File:Sorted_binary_tree_ALL_RGB.svg
(defvar *wikitr*)
(setq *wikitr* '(
 ((nil A nil)
  B
  ((nil C nil) D (nil E nil)))
 F
 (nil G ((nil H nil) I nil))))

; some access functions:
; treeEmptyP -> returns if tree is empty
(defun treeEmptyP (Tr) (null Tr))
; create an empty tree:
(defun createEmptyTr () nil)
; create a tree (leftTr nodeVal rightTr)
(defun createTr (lTr N rTr) 
  (cons LTr (cons N (cons rTr nil))))

; nodeVal -> node of tree
(defun nodeval (Tr) (cadr Tr))
; left -> left subtree
(defun left (Tr) (car Tr))
; right -> right subtree
(defun right (Tr) (caddr Tr))

;insert -> insert a number into the tree
(defun insert (Tr num)
  (cond 
    ((treeEmptyP Tr) ; build root
      (createTr (createEmptyTr) num (createEmptyTr)))
    ((eq (nodeVal Tr) num) Tr) ; omit repeat
    ((< (nodeVal Tr) num) ; insert num to right
      (createTr (left Tr) (nodeVal Tr) (insert (right Tr) num) ))
    (t (createTr (insert (left Tr) num) (nodeVal Tr) (right Tr)))))

; insertL -> insert a list into tree. (on the order of the list)
(defun insertL (Tr L)
  (if (null L) Tr
    (insertL (insert Tr (car L)) (cdr L)) ))

; use a testing tree
(defvar *t2*)
(setq *t2* (insertL nil (list 64 32 16 8 4 48 40 36 44 56 60 62 96 112 104)))

; preorder (NLR)
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

; inorder (LNR)
;  recurse on left
;  visit node
;  recurse on right
; the current implementation is not efficient...
; using append repetitively results in O(n^2)
; Adding another stack of counting how many to pop at a time, will add 
; space usage of O(n) and make time in O(n)...
; ihgfedcba <=> 4 1 1 3: abcdefghi
;
; Need 2 visit stack + 1 pending stack (+ 1 counting stack)
; things on visit stack goes into visit queue, (VAL type)
; things on pending stack is what would be visited next. (TREE type)
(defun inOrder(Tree) 
  (defun inOrderProc (cur vis vstk1 vstk2 pendstk) 
    (if (treeEmptyP cur) vis ; base case
      (let ( (hasLeft (not (treeEmptyP (left cur)))) 
             (hasRight (not (treeEmptyP (right cur))))
             (hasBoth (and (not (treeEmptyP (left cur))) 
                           (not (treeEmptyP (right cur)))))
             (noLeft (treeEmptyP (left cur)))  
             (noRIght (treeEmptyP (right cur)))  
             (pureLeaf (and (treeEmptyP(left cur)) 
                            (treeEmptyP(right cur))))  )
      ; define the parameters to be passed to next call.
      ; this is the most headache part...
        (let (
          (curN ; next current (as TREE)
            (if hasLeft (left cur) ; has left, next goes to left
              (if noRight (car pendstk) ; pure leaf? pop pendStk
                (right cur) ))); only right, next goes to right.
          (visN ; changes to visit queue: (as NODEVAL)
            (if hasLeft vis ; if there keep going left, visQ remain same
              (append ; when has no left (on return)
                vis ;keep the visited
                (cons (nodeval cur) vstk2) ; secondary stack + self
                (if hasRight nil ; don't touch primary stack, else
                  (list (car vstk1))) )))   ; pop 1 from primary stack              
          (vstk1N ; changes to primary stack (as NODEVAL):
            (cond (hasBoth (cons (nodeVal cur) vstk1)) ; push when both childs
                  (pureLeaf (cdr vstk1)) ; pop when it's the leaf
                  (t vstk1))) ; otherwise, no change on primary stack  
          (vstk2N ; changes to secondary stack (as NODEVAL):
            (cond (noLeft nil) ; pop all secondary stack if noLeft
                  (hasBoth vstk2) ;if hasBoth, cur in prmStk already. nochange
                  (t (cons (nodeVal cur) vstk2)))); hasLeft x noRight, push 1
          (pendstkN ; changes to pending stack (as TREE):
            (cond (hasBoth (cons (right cur) pendStk)) ; push when has both
                  (pureLeaf (cdr pendStk)) ; pop when pureleaf
                  (t pendStk)))) ; otherwise, no change
        (inOrderProc curN visN vstk1N vstk2N pendStkN))))) ; call next recurse
  (inOrderProc Tree nil nil nil nil))

; postOrder 
; I might be wrong.... It might not need be that hard...
(defun postOrderX (Tr)
  (if (treeEmptyP Tr) nil
    (append (postOrderX (left Tr)) 
            (postOrderX (right Tr)) 
            (list (nodeVal Tr)))))

; bruh that acutally worked...
(defun preOrderX (Tr)
  (if (treeEmptyP Tr) nil
    (cons (nodeVal Tr) 
          (append (preorderX (left Tr)) (preorderX (right Tr))))))

; rip my sanity
(defun inOrderX (Tr)
  (if (treeEmptyP Tr) nil
    (append (inorderX (left Tr)) 
            (list (nodeVal Tr))
            (inorderX (right Tr)))))
;
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

 
