#lang racket

(require "../common.rkt")
(require "../programs.rkt")
(require "../reports/datafile.rkt")
(require net/uri-codec)

(provide compile-info)

(define (fix-name name)
  (string-replace (uri-encode (~a name)) #rx"[^a-zA-Z0-9]" "_"))

(define (apply-converter conv args)
  (cond
   [(string? conv) (apply format conv args)]
   [(list? conv) (apply format (list-ref conv (length args)) args)]
   [(procedure? conv) (apply conv args)]
   [else (error "Unknown syntax entry" conv)]))

(define-table operators->c
  [+        "(~a + ~a)"]
  [-        '(#f "(-~a)" "(~a - ~a)")]
  [*        "(~a * ~a)"]
  [/        '(#f "(1.0/~a)" "(~a / ~a)")]
  [abs      "fabs(~a)"]
  [sqrt     "sqrt(~a)"]
  [hypot    "hypot(~a, ~a)"]
  [sqr      (λ (x) (format "(~a * ~a)" x x))]
  [cube     (λ (x) (format "(~a * (~a * ~a))" x x x))]
  [exp      "exp(~a)"]
  [expm1    "expm1(~a)"]
  [expt     "pow(~a, ~a)"]
  [log      "log(~a)"]
  [log1p    "log1p(~a)"]
  [sin      "sin(~a)"]
  [cos      "cos(~a)"]
  [tan      "tan(~a)"]
  [cotan    "(1.0 / tan(~a))"]
  [asin     "asin(~a)"]
  [acos     "acos(~a)"]
  [atan     "atan(~a)"]
  [sinh     "sinh(~a)"]
  [cosh     "cosh(~a)"]
  [tanh     "tanh(~a)"]
  [atan2    "atan2(~a, ~a)"]
  [if       "(~a ? ~a : ~a)"]
  [>        "(~a > ~a)"]
  [<        "(~a < ~a)"]
  [<=       "(~a <= ~a)"]
  [>=       "(~a >= ~a)"]
  [and      "(~a && ~a)"]
  [or       "(~a || ~a)"]
  [mod      "fmod(~a, ~a)"]
  [fma      "fma(~a, ~a, ~a)"]
  [cbrt     "cbrt(~a)"])

(define-table constants->c
  [pi    "atan2(1.0, 0.0)"]
  [e     "exp(1.0)"])

(define (comparison? l)
  (and (list? l) (member (car l) '(< > <= >= and or))))

(define (if? l)
  (and (list? l) (eq? (car l) 'if)))

(define (if->c expr)
  (match-define `(if ,cond ,ift ,iff) expr)
  (format "(~a ? ~a : ~a)" (expr->c cond) (expr->c ift) (expr->c iff)))

(define (value->c expr)
  (cond
   [(member expr constants) (apply-converter (car (hash-ref constants->c expr)) '())]
   [(symbol? expr) (fix-name expr)]
   [(number? expr) (real->double-flonum expr)]
   [else (error "Invalid value" expr)]))

(define (app->c expr)
  (if (list? expr)
      (let* ([rec (list-ref (hash-ref operators->c (car expr)) 0)]
             [args (map app->c (cdr expr))])
        (apply-converter rec args))
      (value->c expr)))

(define (expr->c expr)
  (match expr
    [`(if ,cond ,ift ,iff) (if->c expr)]
    [_ (app->c expr)]))

(define (program->c prog [type "double"] [fname "f"])
  (define bound-vars (program-variables prog))
  (define unused-vars (unused-variables prog))

  (match-define
   `(let* ([,vars ,vals] ...) ,retexpr)
   (program->cse (program-body prog)))

  (define var-declarations
    (for/list ([var bound-vars])
      (if (member var unused-vars)
          (format "~a __attribute__((unused)) ~a" type (fix-name var))
          (format "~a ~a" type (fix-name var)))))

  (printf "double ~a(~a) {\n" fname (string-join var-declarations ", "))

  (for ([var vars] [val vals])
    (define type* (if (comparison? (car val)) 'bool type))
    (printf "        ~a ~a = ~a;\n" type* var (app->c val)))
  (printf "        return ~a;\n" (app->c retexpr))
  (printf "}\n\n"))

(define-table operators->mpfr
  [+        "mpfr_add(~a, ~a, ~a, MPFR_RNDN)"]
  [-        '(#f #f "mpfr_neg(~a, ~a, MPFR_RNDN)" "mpfr_sub(~a, ~a, ~a, MPFR_RNDN)")]
  [*        "mpfr_mul(~a, ~a, ~a, MPFR_RNDN)"]
  [/        '(#f #f "mpfr_ui_div(~a, 1, ~a, MPFR_RNDN)" "mpfr_div(~a, ~a, ~a, MPFR_RNDN)")]
  [abs      "mpfr_abs(~a, ~a, MPFR_RNDN)"]
  [sqrt     "mpfr_sqrt(~a, ~a, MPFR_RNDN)"]
  [hypot    "mpfr_hypot(~a, ~a, ~a, MPFR_RNDN)"]
  [sqr      "mpfr_sqr(~a, ~a, MPFR_RNDN)"]
  [cube     (λ (x y)
               (format
                 "mpfr_mul(~a, ~a, ~a, MPFR_RNDN); mpfr_mul(~a, ~a, ~a, MPFR_RNDN)"
                 x y y x x y))]
  [exp      "mpfr_exp(~a, ~a, MPFR_RNDN)"]
  [expm1    "mpfr_expm1(~a, ~a, MPFR_RNDN)"]
  [expt     "mpfr_pow(~a, ~a, ~a, MPFR_RNDN)"]
  [log      "mpfr_log(~a, ~a, MPFR_RNDN)"]
  [log1p    "mpfr_log1p(~a, ~a, MPFR_RNDN)"]
  [sin      "mpfr_sin(~a, ~a, MPFR_RNDN)"]
  [cos      "mpfr_cos(~a, ~a, MPFR_RNDN)"]
  [tan      "mpfr_tan(~a, ~a, MPFR_RNDN)"]
  [cotan    "mpfr_cot(~a, ~a, MPFR_RNDN)"]
  [asin     "mpfr_asin(~a, ~a, MPFR_RNDN)"]
  [acos     "mpfr_acos(~a, ~a, MPFR_RNDN)"]
  [atan     "mpfr_atan(~a, ~a, MPFR_RNDN)"]
  [sinh     "mpfr_sinh(~a, ~a, MPFR_RNDN)"]
  [cosh     "mpfr_cosh(~a, ~a, MPFR_RNDN)"]
  [tanh     "mpfr_tanh(~a, ~a, MPFR_RNDN)"]
  [if       (lambda (r c a b)
               (format
                "if (mpfr_get_si(~a, MPFR_RNDN)) { mpfr_set(~a, ~a, MPFR_RNDN); } else { mpfr_set(~a, ~a, MPFR_RNDN); }"
                c r a r b))]
  [>        "mpfr_set_si(~a, mpfr_cmp(~a, ~a) > 0, MPFR_RNDN)"]
  [<        "mpfr_set_si(~a, mpfr_cmp(~a, ~a) < 0, MPFR_RNDN)"]
  [<=       "mpfr_set_si(~a, mpfr_cmp(~a, ~a) <= 0, MPFR_RNDN)"]
  [>=       "mpfr_set_si(~a, mpfr_cmp(~a, ~a) >= 0, MPFR_RNDN)"]
  [and      "mpfr_set_si(~a, mpfr_get_si(~a, MPFR_RNDN) && mpfr_get_si(~a, MPFR_RNDN), MPFR_RNDN)"]
  [or       "mpfr_set_si(~a, mpfr_get_si(~a, MPFR_RNDN) || mpfr_get_si(~a, MPFR_RNDN), MPFR_RNDN)"]
  [atan2    "mpfr_atan2(~a, ~a, ~a, MPFR_RNDN)"]
  [mod      "mpfr_fmod2(~a, ~a, ~a, MPFR_RNDN)"]
  [fma      "mpfr_fma(~a, ~a, ~a, ~a, MPFR_RNDN)"]
  [cbrt     "mpfr_cbrt(~a, ~a, MPFR_RNDN)"])

(define-table constants->mpfr
  [pi    "mpfr_const_pi(~a, MPFR_RNDN)"]
  [e     (λ (x) (format "mpfr_set_si(~a, 1, MPFR_RNDN); mpfr_exp(~a, ~a, MPFR_RNDN);" x x x))])

(define (program->mpfr prog [bits 128] [fname "f"])
  (define bound-vars (program-variables prog))
  (define unused-vars (unused-variables prog))
  (match-define (list out body ...) (program->register-machine (program-body prog)))
  (define registers (remove-duplicates (map car body)))

  (define (app->mpfr out expr)
    (cond
     [(list? expr)
      (let* ([rec (list-ref (hash-ref operators->mpfr (car expr)) 0)]
             [args (cdr expr)])
        (apply-converter rec (cons out args)))]
     [(number? expr)
      (format "mpfr_set_d(~a, ~a, MPFR_RNDN)" out expr)]
     [(member expr bound-vars)
      (format "mpfr_set_d(~a, ~a, MPFR_RNDN)" out (fix-name expr))]
     [(member expr constants)
      (apply-converter (car (hash-ref constants->mpfr expr)) (list out))]
     [(symbol? expr)
      (format "mpfr_set(~a, ~a, MPFR_RNDN)" out expr)]))

  (write-string
   (printf "static mpfr_t ~a;\n\n" (string-join (map ~a registers) ", "))

   (printf "void setup_mpfr_~a() {\n" fname)
   ; Some guard bits added, just in case
   (printf "        mpfr_set_default_prec(~a);\n" (+ bits 16))
   (for ([reg registers])
     (printf "        mpfr_init(~a);\n" reg))
   (printf "}\n\n")

   (define var-declarations
     (for/list ([var bound-vars])
       (if (member var unused-vars)
           (format "double __attribute__((unused)) ~a" (fix-name var))
           (format "double ~a" (fix-name var)))))

   (printf "double ~a(~a) {\n" fname (string-join var-declarations ", "))

   (for ([assignment body])
     (printf "        ~a;\n" (app->mpfr (car assignment) (cadr assignment))))

   (printf "        return mpfr_get_d(~a, MPFR_RNDN);\n" out)
   (printf "}\n\n")))

(define (compile-all name iprog fprog dprog bits)
  (printf "#include <tgmath.h>\n")
  (printf "#include <gmp.h>\n")
  (printf "#include <mpfr.h>\n")
  (printf "#include <stdio.h>\n")
  (printf "#include <stdbool.h>\n\n")
  (printf "char *name = \"~a\";\n\n" name)
  (display (program->c iprog "float" "f_if"))
  (display (program->c iprog "double" "f_id"))
  (newline)
  (display (program->c fprog "float" "f_of"))
  (display (program->c dprog "double" "f_od"))
  (printf "void mpfr_fmod2(mpfr_t r, mpfr_t n, mpfr_t d) {\n")
  (printf "        mpfr_fmod(r, n, d, MPFR_RNDN);\n")
  (printf "        if (mpfr_cmp_ui(r, 0) < 0) mpfr_add(r, r, d, MPFR_RNDN);\n")
  (printf "}\n\n")
  (newline)
  (display (program->mpfr iprog bits "f_im"))
  (display (program->mpfr fprog bits "f_fm"))
  (display (program->mpfr dprog bits "f_dm")))

(define (compile-info base-dir single-info double-info)
  (for ([single-test (report-info-tests single-info)] [double-test (report-info-tests double-info)])
    (when (and (not (member (table-row-status single-test) '("timeout" "crash")))
               (not (member (table-row-status double-test) '("timeout" "crash"))))
      (match (cons single-test double-test)
        [(cons (table-row name single-status _ _ _ _ _ _ vars _ input single-output _ single-bits dir)
               (table-row name double-status _ _ _ _ _ _ vars _ input double-output _ double-bits dir))
         (define fname (build-path base-dir dir "compiled.c"))
         (debug #:from 'compile-info "Compiling" name "to" fname)
         (write-file fname
                     (compile-all name `(λ ,vars ,input) `(λ ,vars ,single-output)
                                  `(λ ,vars ,double-output) (max single-bits double-bits)))]
        [else
         (error "Test case order, names, inputs don't match for single and double precision results."
                single-test double-test)]))))

(module+ main
  (require racket/cmdline)
  (require "../config.rkt")

  (define dir report-output-path)

  (command-line
   #:program "compile/c.rkt"
   #:once-each
   [("-d") dir* "Report output directory"
    (set! dir dir*)]
   #:args (single-json-file double-json-file)
   (compile-info dir (read-datafile single-json-file) (read-datafile double-json-file))))
