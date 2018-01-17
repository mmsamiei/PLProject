;; PL Project - Fall 2017
;; NUMEX interpreter

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for NUMEX programs

;; Add the missing ones

(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions


(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call


(struct munit   ()      #:transparent) ;; unit value -- good for ending a list
(struct ismunit (e)     #:transparent) ;; if e1 is unit then 1 else 0


;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; ######################## I add this

(struct var (string) #:transparent)
(struct mult  (e1 e2)  #:transparent)  ;; mult two expressions
(struct neg  (e)  #:transparent)  ;; neg one expressions
(struct islthan  (e1 e2)  #:transparent)
(struct ifzero  (e1 e2 e3)  #:transparent)
(struct ifgthan  (e1 e2 e3 e4)  #:transparent)
(struct mlet  (s e1 e2)  #:transparent)
(struct apair  (e1 e2)  #:transparent)
(struct first  (e)  #:transparent)
(struct second  (e)  #:transparent)

;; ######################## I added that


;; Problem 1

(define (racketlist->numexlist xs)
  (cond 
      [(not(null? xs)) (apair (car xs) (racketlist->numexlist (cdr xs)))]
      [#t (munit)]
  )
)


(define (numexlist->racketlist xs)
  (cond 
      [(not(munit? xs)) (cons (eval-exp(first xs)) (numexlist->racketlist (eval-exp(second xs))))]
      [#t null]
  )
)


;; Problem 2

;; lookup a variable in an environment
;; Complete this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
  		[(equal? (car (car env))str) (cdr (car env))]
                [else (envlookup (cdr env ) str)]
		))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of NUMEX expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]

        [(munit? e)
         e
         ]

        [(int? e)
         ;; Error handling - Type THREE ;;
         (let ([v (int-num e)])
           (if (integer? v)
               e
               (error "const should be an integer")))
         ]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "NUMEX addition applied to non-number")))]

        [(mult? e) 
         (let ([v1 (eval-under-env (mult-e1 e) env)]
               [v2 (eval-under-env (mult-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (* (int-num v1) 
                       (int-num v2)))
               (error "NUMEX multiplaction applied to non-number")))]


        [(neg? e) 
         (let ([v (eval-under-env (neg-e e) env)])
           (if (int? v)
               (int (- (int-num v)))
               (error "negate applied to non-number")))]

        
        [(islthan? e) 
         (let ([v1 (eval-under-env (islthan-e1 e) env)]
               [v2 (eval-under-env (islthan-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (if(< (int-num v1) (int-num v2))
                  (int 1)
                  (int 0))
               (error "NUMEX multiplaction applied to non-number")))]





        [(ifgthan? e)
         (let ([v1 (eval-under-env (ifgthan-e1 e) env)]
               [v2 (eval-under-env (ifgthan-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (cond [(> (int-num v1) (int-num v2)) (eval-under-env (ifgthan-e3 e) env)]
                     [else (eval-under-env (ifgthan-e4 e) env)])
               (error "NUMEX ifgthan applied to non-number")))]


        [(ifzero? e)
         (let([v1 (eval-under-env (ifzero-e1 e) env )])
           ( if ( int? v1) ()

                (error "NUMEX ifzero applied to non-number) )
          )
         ]
        

        
        [(apair? e)
         e]
        
        [(first? e)  
         (let ([p (eval-under-env (first-e e) env)])

           (if (apair? p)
               (apair-e1 p)
               (error "NUMEX first applied to non-pair" e)))]
        
        [(second? e)  
         (let ([p (eval-under-env (second-e e) env)])

           (if (apair? p)
               (apair-e2 p)
               (error "NUMEX second applied to non-pair" e)))]


        [(mlet? e)
         (let ([p (eval-under-env (mlet-e1 e) env)])
           
         (eval-under-env (mlet-e2 e) (cons(cons (mlet-s e) p) env )))
         ]

        [
         (fun? e)
         (closure env e)
         ]

        [
         (closure? e)
         e
         ]


        
        [(call? e)
         (let ([actual (eval-under-env (call-actual e) env)]
               [cloj (eval-under-env (call-funexp e) env) ]
              ;[param (fun-formal (call-funexp e))]
              ;[body (fun-body (call-funexp e))]
              ;[cloj (eval-under-env (call-funexp e) ) ]
              [param (fun-formal(closure-fun(eval-under-env (call-funexp e) env ))) ]
              [body (fun-body(closure-fun(eval-under-env (call-funexp e) env ))) ])
      

        (eval-under-env body (cons(cons param actual) (closure-env(eval-under-env (call-funexp e) env )) ))
           )
         ]
        
        ;; CHANGE add more cases here
        [#t (error (format "bad NUMEX expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifmunit e1 e2 e3) "CHANGE")

(define (mlet* bs e2) "CHANGE")

(define (ifeq e1 e2 e3 e4) "CHANGE")

;; Problem 4

(define numex-map "CHANGE")

(define numex-mapAddN
  (mlet "map" numex-map
        "CHANGE (notice map is now in NUMEX scope)"))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make grading
;; more difficult, so copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
