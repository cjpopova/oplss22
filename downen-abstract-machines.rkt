#lang racket
(require redex
         "downen-compiler.rkt")

(define ->
  (reduction-relation
   mech
   [--> ((μ α c) || E)
        (substitute c α E)
        μ→]
   [--> ((lam x v_1) || (v_2 · E))
        ((substitute v_1 x v_2) || E)
        β→]
   [--> (True || (ifthen c_1 else c_2))
        c_1
        β-bool-1]
   [--> (False || (ifthen c_1 else c_2))
        c_2
        β-bool-2]))

(define notλ (term (lam x (μ α (x || (ifthen (False || α) else (True || α)))))))
(define andλ (term (lam x
                        (lam y
                             (μ α (x || (ifthen (y || α) else (False || α))))))))
(define contra (mech-compile
                `(λ f (λ g (λ x (g (f x)))))))
(define dni (mech-compile
             `(λ x (λ y (y x)))))
(define tni dni)
(define tne (term (contra || (dni · α))))

#;(define lem (mech-compile
  (\mu \alpha (Left (\ x (\mu \beta
    ((Right x) || \alpha)) || \alpha)))))

; todo: probably want to generate new gensyms everytime you invoke
; the utility functions. unless you figure out capture-avoiding substitution

; (not True) ->* False
#;(traces -> (term ((lam x
                         (μ α (x
                               || (ifthen (False || α) else (True || α)))))
                    || (True · α))))
; (and True True) ->* True
#;(traces -> (term (,andλ || (True · (True · α)))))


; ====

(define M (term (lam h_2 (μ β (h_2 || (False · β))))))
(define DNE (term (lam h_1 (μ γ (h_1 || ((lam x (μ β (x || γ))) · casebrack))))))

#;(traces -> (term (,DNE || (,M · α))))


