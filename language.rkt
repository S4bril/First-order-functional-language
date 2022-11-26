#lang plait

(module+ test
  (print-only-errors #t))

;-----------------------------------abstract syntax--------------------------------------

(define-type Op
  (add) (sub) (mul) (leq))

(define-type Exp
  (defE [functions : (Listof Exp)] [e : Exp])
  (funE [name : Symbol] [xs : (Listof Symbol)] [e : Exp])
  (numE [n : Number])
  (varE [x : Symbol])
  (opE [op : Op] [l : Exp] [r : Exp])
  (ifE [b : Exp] [l : Exp] [r : Exp])
  (letE [x : Symbol] [e1 : Exp] [e2 : Exp])
  (appE [name : Symbol] [args : (Listof Exp)]))

;-----------------------------------------parser----------------------------------------- 
;parse
(define (parse [s : S-Exp]) : Exp
  (cond
    [(s-exp-match? `{define {ANY ...} for ANY} s)
     (defE (parse-listof-exp (s-exp->list(second (s-exp->list s))))
       (parse (fourth (s-exp->list s))))]
    
    [(s-exp-match? `[fun SYMBOL (SYMBOL ...) = ANY] s)
     (funE (s-exp->symbol
            (second (s-exp->list s)))
           (parse-listof-symbol
            (s-exp->list (third (s-exp->list s))))
           (parse (fifth (s-exp->list s))))]
        
    [(s-exp-match? `NUMBER s)
     (numE (s-exp->number s))]
        
    [(s-exp-match? `SYMBOL s)
     (varE (s-exp->symbol s))]
    
    [(s-exp-match? `{ANY SYMBOL ANY} s)
     (opE (parse-op (s-exp->symbol
                     (second (s-exp->list s))))
          (parse (first (s-exp->list s)))
          (parse (third (s-exp->list s))))]
    
    [(s-exp-match? `{ifz ANY then ANY else ANY} s)
     (ifE (parse (second (s-exp->list s)))
          (parse (fourth (s-exp->list s)))
          (parse (sixth (s-exp->list s))))]

    [(s-exp-match? `{let SYMBOL be ANY in ANY} s)
     (letE (s-exp->symbol (second (s-exp->list s)))
           (parse (fourth (s-exp->list s)))
           (parse (sixth (s-exp->list s))))]
    
    [(s-exp-match? `{SYMBOL (ANY ...)} s)
     (appE (s-exp->symbol (first (s-exp->list s)))
           (parse-listof-exp (s-exp->list
                              (second (s-exp->list s)))))]
    
    [else (error 'parse "invalid input")]))

;----------------uniqueness-check-----------------
(define (list-of-names [fun : (Listof Exp)])
  (if (empty? fun) empty
      (cons (funE-name (first fun)) (list-of-names (rest fun)))))

(define (uniqueness-check [fun : (Listof Symbol)])
  (cond [(empty? fun) (void)]
        [(member (first fun) (rest fun))
         (error 'parse "multiple declaration of function")]
        [else (uniqueness-check (rest fun))]))
;-------------------------------------------------
        
;parse-listof-exp | (List of s-exp) -> (List of Exp)
(define (parse-listof-exp xs) 
  (if (empty? xs) empty
      (cons (parse (first xs)) (parse-listof-exp (rest xs)))))

;parse-listof-symbol | (List of s-exp) -> (List of Symbol)
(define (parse-listof-symbol xs) 
  (if (empty? xs) empty
      (cons (s-exp->symbol (first xs))
            (parse-listof-symbol (rest xs)))))

(define (fifth xs)
  (first (rest (rest (rest (rest xs))))))

(define (sixth xs)
  (first (rest (rest (rest (rest (rest xs)))))))

(define (parse-op [op : Symbol]) : Op
  (cond
    [(eq? op '+) (add)]
    [(eq? op '-) (sub)]
    [(eq? op '*) (mul)]
    [(eq? op '<=) (leq)]
    [else (error 'parse "unknown operator")]))

;------------------------------------eval-------------------------------------------------

;value

(define-type-alias Value Number)

(define-type Binding
  (bind [name : Symbol]
        [val : Value]))

;environments

(define-type-alias Env (Listof Binding))

(define mt-env empty)

(define (extend-env [env : Env] [x : Symbol] [v : Value]) : Env
  (cons (bind x v) env))

(define (lookup-env [n : Symbol] [env : Env]) : Value
  (type-case (Listof Binding) env
    [empty (error 'lookup "unbound variable")]
    [(cons b rst-env) (cond
                        [(eq? n (bind-name b))
                         (bind-val b)]
                        [else (lookup-env n rst-env)])]))

;primitive operations

(define (op-num-num->proc [f : (Number Number -> Number)]) : (Value Value -> Value)
  (λ (v1 v2) (f v1 v2)))

(define (op-leq->proc) : (Value Value -> Value)
  (λ (v1 v2) (if (<= v1 v2) 0 1)))

(define (op->proc [op : Op]) : (Value Value -> Value)
  (type-case Op op
    [(add) (op-num-num->proc +)]
    [(sub) (op-num-num->proc -)]
    [(mul) (op-num-num->proc *)]
    [(leq) (op-leq->proc)]))

;----------------------evaluation function (eval/apply)------------------------------------

;eval-define
(define (eval-define [def : Exp] [env : Env])
  (type-case Exp def
    [(defE f e) (eval-program e f env)]
    [else (error 'eval "program not starting with definition")]))

;eval-program
(define (eval-program [e : Exp] [functions : (Listof Exp)] [env : Env])
  (type-case Exp e
    [(defE f x) (error 'eval "multiple use of define")]
    
    [(numE n) n]
    
    [(opE o l r) ((op->proc o) (eval-program l functions env) (eval-program r functions env))]
    
    [(ifE b l r)
     (let ([v (eval-program b functions env)])
       (if (= 0 v) (eval-program l functions env) (eval-program r functions env)))]
    
    [(varE x)
     (lookup-env x env)]
    
    [(letE x e1 e2)
     (let ([v1 (eval-program e1 functions env)])
       (eval-program e2 functions (extend-env env x v1)))]
    
    [(funE x b e)
     (error 'eval "program computed to function")]
    
    [(appE f args)
     (apply (find-fun f functions) (eval-list args functions env) functions)]))

;eval-list | (Listof Exp) -> (Listof Value)
(define (eval-list [xs : (Listof Exp)] [functions : (Listof Exp)] [env : Env])
  (if (empty? xs) empty
      (cons (eval-program (first xs) functions env)
            (eval-list (rest xs) functions env))))

;find-fun
(define (find-fun [fun : Symbol] [functions : (Listof Exp)])
  (cond [(empty? functions) (error 'eval "undeclared function")]
        [(equal? fun (funE-name (first functions))) (first functions)]
        [else (find-fun fun (rest functions))]))

;extend-list | extends environment by list of arguments
(define (extend-list variables values env)
  (cond [(and (empty? variables) (empty? values)) env ]
        [(or (and (not (empty? variables)) (empty? values))
             (and (empty? variables) (not (empty? values)))) (error 'eval "too much arguments")]
        [else (extend-list (rest variables) (rest values)
                           (extend-env env (first variables) (first values)))]))

;apply
(define (apply [f : Exp] [vals : (Listof Value)] [functions : (Listof Exp)]) : Value
  (eval-program (funE-e f) functions (extend-list (funE-xs f) vals empty)))

;eval
(define (eval exp env)
  (eval-define exp env))

;run
(define (run [e : S-Exp]) : Value
  (begin (uniqueness-check (list-of-names (defE-functions (parse e))))
         (eval (parse e) mt-env)))

;----------------------------tests-----------------------------------

(module+ test
  (test (run `{define
                {[fun fact (n) = {ifz n then 1 else {n * {fact ({n - 1})}}}]}
                for
                {fact (5)}})
        120)

  (test (run `{define
                {[fun even (n) = {ifz n then 0 else {odd ({n - 1})}}]
                 [fun odd (n) = {ifz n then 42 else {even ({n - 1})}}]}
                for
                {even (1024)}})
        0)

  (test (run `{define
                {[fun gcd (m n) = {ifz n
                                       then m
                                       else {ifz {m <= n}
                                                 then {gcd (m {n - m})}
                                                 else {gcd ({m - n} n)}}}]}
                for
                {gcd (81 63)}})
        9))
