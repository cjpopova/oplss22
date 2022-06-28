#lang racket
(require redex)

(define-language mech
  (v ::= x (lam x v) True False (μ α c))
  (E ::= α (v · E) (ifthen c_1 else c_2) casebrack)
  (c ::= (v || E))
  (x y z h α β γ ::= variable-not-otherwise-mentioned))

(default-language mech)

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

#;(apply-reduction-relation* -> (term (False || α)))
#;(redex-match? mech c
                (term ((lam x (μ α (x || (ifthen (False || α) else (True || α))))) || (True · α))))
#;(traces -> (term ((lam x
                         (μ α (x
                               || (ifthen (False || α) else (True || α)))))
                    || (True · α))))
(define M (term (lam h_2 (μ β_1 (h_2 || (False · β_1))))))
(define DNE (term (lam h_1 (μ γ (h_1 || ((lam x (μ β (x || γ))) · casebrack))))))

(traces -> (term (,DNE || (,M · α))))

; ===============================

; E (or κ) ::= (app N E) | (ite [N1 E] [N2 E])

(define (mch-compile e)
  (match e
    ; var
    [(? symbol? x)
     x]
    ; flat
    [(? boolean? b)
     b]
    ; abs
    [`(λ ,x ,M)
     `(λ ,x ,(mch-compile M))]
    ; app
    [`(,M ,N)
     `(μα [,(mch-compile M) (app ,(mch-compile N) α)])] ; do we need to mch-compile N here??
    ; ite
    [`(if ,M ,N1 ,N2)
     `(μα [,(mch-compile M) (ite [,(mch-compile N1) α] [,(mch-compile N2) α])])]
    [_ (error 'mch-compiler "unrecognized expr ~a" e)]))

#|(mch-compile true)
(mch-compile `(λ x true))
(mch-compile `((λ x x) false))
(mch-compile `(((λ x (λ y (if x y false))) false) false))|#




