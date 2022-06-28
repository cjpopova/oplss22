#lang racket
(require redex)
(provide mech
         mech-compile)

(define-language calc
  (v := x (v v) (λ x v) True False (if v_1 then v_2 else v_3))
  (x y z ::= variable-not-otherwise-mentioned))

; todo: can i compile from one language to the other (w/out using extended-language?)

(define-language mech
  (v ::= x (lam x v) True False (μ α c))
  (E ::= α (v · E) (ifthen c_1 else c_2) casebrack)
  (c ::= (v || E))
  (x y z f g h α β γ ::= variable-not-otherwise-mentioned))

(define (mech-compile e)
  (match e
    ; var
    [(? symbol? x)
     x]
    ; flat
    [(? boolean? b)
     b]
    ; abs
    [`(λ ,x ,M)
     (term (lam ,x ,(mech-compile M)))]
    ; app
    [`(,M ,N)
     (define α (gensym 'α_))
     (term (μ ,α (,(mech-compile M) || (,(mech-compile N) · ,α))))]
    ; ite
    [`(if ,M ,N1 ,N2)
     (define α (gensym 'α_))
     (term (μ ,α (,(mech-compile M) (ifthen [,(mech-compile N1) ,α] else [,(mech-compile N2) ,α]))))]
    [_ (error 'mech-compile "unrecognized expr ~a" e)]))

#|(mech-compile `True)
(mech-compile `(λ x True))
(mech-compile `((λ x x) False))
(mech-compile `(((λ x (λ y (if x y False))) False) False))|#