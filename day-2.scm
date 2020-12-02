;
; --------------- Input
;
; <min>-<max> <letter>: <string-of-letters>
; ...
;
; Representation: list of '(min max letter list-of-chars)
;
; --------------- Problem
;
; check if the string contains the correct number of letters
;

(load "./miniKanren-with-symbolic-constraints/mk.scm")

(define file (open-input-file "day-2.in"))
(do ((line (get-line file) (get-line file))) ((eof-object? line))
        (display line)
        (newline))

; working with string

(run 2 (q)
  (fresh (w x y)
    (conde
      ((== `(,x ,w ,x) q)
       (== y w))
      ((== `(,w ,x ,w) q)
       (== y w)))))
