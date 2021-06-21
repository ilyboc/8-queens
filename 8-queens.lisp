; brief：check whether it's possible to put a queen in position pos knowing that
;        queens that are already put, their positions are stored in queens
; params：pos: position (x y), queens：list of existing queens
; example: 
; > (can-place '(1 2) '((1 1)))
; nil
(defun can-place (pos queens)
    (cond ((null queens) t)                                                               ; if queens is nil then return t
          ((null (cdr queens)) (and (not (equal (car pos) (caar queens)))                 ; if len(queens)＝１ then、check if the queen to put
                                    (not (equal (cadr pos) (cadar queens)))               ; does not get captured
                                    (not (equal (abs (- (car pos) (caar queens)))         ; ..diagonal check
                                                (abs (- (cadr pos) (cadar queens)))))))
          (t (and (can-place pos (list (car queens)))                                     ; if len(queens)>1 then recursion
                  (can-place pos (cdr queens))))
    )
)

; brief：in nxn chessboard it returns the position that comes after a specifed position pos.
;        note the position that comes after (n n) is (n+1 1) and it is outside the chessboard
; params：pos: position (x y), n: board size
; example: 
; > (next-pos '(3 8) 8)
; (4 1)
; > (next-pos '(8 8) 8)
; (9 1) 
(defun next-pos (pos n)
    (setq pos (list (car pos) (+ 1 (mod (cadr pos) n))))            ; y=(y mod n) + 1
    (cond ((equal (cadr pos) 1) (list (+ 1 (car pos)) (cadr pos)))  ; if y==1 then return (x+1 y)
          (t pos))
)                                                 ; else return (x y)

; brief：find a position to put one new queen. It backtracks when there is no available position.
; params：queens：list of existing queens, from: position (x y), n: board size
; example: 
; > (find-queen '((1 1)) '(1 1) 8)
; ((2 3) (1 1))
(defun next-queens (queens from n)
    (cond ((> (car from) n)                                                         ; if from.x > n then ..
                (cond ((null queens) nil)                                           ;    if queens == () return nil
                      (t (next-queens (cdr queens) (next-pos (car queens) n) n))))  ;    else return next-queens(cdr(queens), next-pos(car(queens)))
          (t (cond ((can-place from queens) (cons from queens))                     ; else if can-place(from queens) then return (from) + queens
                   (t (next-queens queens (next-pos from n) n))))                   ;      else return next-queens(queens, next-pos(from))
    )
)

; brief：solve n-queen problem
; params：n: board size, queens：list of existing queens
; example: 
; > (setq sols '())
; > (queen-puzzle 8 '())
; 92
(defun queen-puzzle (n queens)
    (prog (from)
        (setq from '(1 1))
        loop
            (setq queens (next-queens queens from n))                           ; put one queen (it can backtrack so the new list isn't necessarily bigger)
            
            (cond ((equal (length queens) n) (setq sols (cons queens sols))))   ; if len(queens)==n then save solution

            (cond ((not (null queens))                                          ; there are more soltuions
                            (setq from (next-pos (car queens) n))               ; start checking from the position that is just after the position of the last queen
                            (go loop))
            )
    )
)

; Run:
(setq sols '())
(queen-puzzle 8 '())
(print (length sols)) ; 92, check the answer: https://en.wikipedia.org/wiki/Eight_queens_puzzle
