#lang racket

(require "../config.rkt")
(require "../common.rkt")
(require "../points.rkt")
(require "../programs.rkt")
(require "../alternative.rkt")
(require "../test.rkt")
(require "interact.rkt")
(require "../distributions.rkt")

(define *num-eval-points* (make-parameter (*num-points*)))

(define *samplers* (make-parameter '()))
(define *initial-seed* (make-parameter '()))

(define (run #:print-points [print-points? #f])
  (eprintf "; Seed: ~a\n" (get-seed))
  (*initial-seed* (get-seed))
  (define in-expr (read))
  (define out-alt
    (match in-expr
      [`(herbie-test . ,_)
       (let ([tst (parse-test in-expr)])
         (set! in-expr (test-program tst))
         (*samplers* test-samplers)
         (run-improve (test-program tst) (*num-iterations*)
                      #:samplers (test-samplers tst)))]
      [`(,(or 'λ 'lambda) ,vars ,body)
       (*samplers* (map (curryr cons (eval-sampler 'default)) vars))
       (run-improve in-expr (*num-iterations*))]
      [_ (error "did not recognize input")]))
  (set-seed! (*initial-seed*))
  (parameterize* ([*num-points* (*num-eval-points*)]
                  [*pcontext* (prepare-points in-expr (*samplers*))])
    (printf "Using ~a points to evaluate\n" (*num-points*))
    (printf "; Input error: ~a\n" (errors-score (alt-errors (make-alt in-expr))))
    (printf "; Output error: ~a\n" (errors-score (alt-errors out-alt)))
    (define in-prog (eval-prog in-expr mode:fl))
    (define out-prog (eval-prog (alt-program out-alt) mode:fl))
    (when print-points?
      (for ([(pt ex) (in-pcontext (*pcontext*))])
        (let ([in-ans (in-prog pt)] [out-ans (out-prog pt)])
          (when (not (= in-ans out-ans))
            (printf "; sample ~a exact ~a input ~a output ~a improvement ~a\n"
                    pt ex in-ans out-ans
                    (- (bit-error ex in-ans)
                       (bit-error ex out-ans))))))))
  (printf "~a\n" (alt-program out-alt)))

(module+ main
  (define print-points #f)
  (command-line
   #:program "herbie/inout.rkt"
   #:once-each
   [("-r" "--seed") rs "The random seed vector to use in point generation"
    (set-seed! (read (open-input-string rs)))]
   [("--fuel") fu "The amount of 'fuel' to use"
    (*num-iterations* (string->number fu))]
   [("--num-points") points "The number of points to use"
    (*num-points* (string->number points))]
   [("--num-eval-points") eval-points "The number of points to evaluate on"
    (*num-eval-points* (string->number eval-points))]
   [("--print-points") "Print all sampled points"
    (set! print-points #t)]
   #:multi
   [("-o" "--option") tf "Toggle flags, specified in the form category:flag"
    (let ([split-strings (string-split tf ":")])
      (when (not (= 2 (length split-strings)))
        (error "Badly formatted input " tf))
      (toggle-flag! (string->symbol (car split-strings)) (string->symbol (cadr split-strings))))]
   #:args ()
   (run #:print-points print-points)))
