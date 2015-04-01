#lang racket

(require racket/pretty)
(require "../common.rkt")
(require "../points.rkt")
(require "../matcher.rkt")
(require "../alternative.rkt")
(require "../test.rkt")
(require "../infer-regimes.rkt")
(require "../programs.rkt")
(require "../plot.rkt")

(provide make-graph make-traceback make-timeout)

(define *line-width* 3)
(define *point-width* 2)
(define *point-opacity* .2)

(define *tick-length* 10)
(define *label-shift* 10)
(define *label-rotation* 40)
(define *label-height* 8)
(define *label-width* 15)

(define (make-graph test end-alt points start-errs end-errs target-errs bits dir profile?)
  (printf "<!doctype html>\n")
  (printf "<html>\n")
  (printf "<head>")
  (printf "<meta charset='utf-8' />")
  (printf "<title>Results for ~a</title>" (test-name test))
  (printf "<link rel='stylesheet' type='text/css' href='../graph.css' />")
  (printf "</head>\n")
  (printf "<body>\n")

  (printf "<dl id='about'>\n")
  (printf "<dt>Test:</dt><dd>~a</dd>" (test-name test))
  (printf "<dt>Logs:</dt>")
  (printf "<dd><a href='debug.txt'>Debug output</a>")
  (when profile?
    (printf ", <a href='profile.txt'>Profiling report</a>"))
  (printf "</dd>\n")
  (printf "<dt>Bits:</dt><dd>~a bits</dd>\n" bits)
  (printf "</dl>\n")

  (printf "<div id='graphs'>\n")
  (for ([var (test-vars test)] [idx (in-naturals)])
    (call-with-output-file (build-path dir (format "plot-~a.png" idx)) #:exists 'replace
        (lambda (out)
          (herbie-plot #:port out #:title (format "Error versus ~a" var) #:kind 'png
                       (reap [sow]
                             (sow (error-points start-errs points #:axis idx #:color "red"))
                             (when target-errs
                               (sow (error-points target-errs points #:axis idx #:color "green")))
                             (sow (error-points end-errs points #:axis idx #:color "blue"))))
          (printf "<img width='500' height='250' src='plot-~a.png' />\n" idx))))
  (printf "</div>\n")

  (printf "<ol id='process-info'>\n")
  (output-history end-alt)
  (printf "</ol>\n")

  (printf "</body>\n")
  (printf "</html>\n"))

(define (make-traceback test err bits profile?)
  (printf "<!doctype html>\n")
  (printf "<html>\n")
  (printf "<head>\n")
  (printf "<meta charset='utf-8' />\n")
  (printf "<title>Exception for ~a</title>" (test-name test))
  (printf "<link rel='stylesheet' type='text/css' href='../graph.css' />")
  (printf "</head>")
  (printf "<body>\n")

  (printf "<dl id='about'>\n")
  (printf "<dt>Test:</dt><dd>~a</dd>" (test-name test))
  (printf "<dt>Logs:</dt>")
  (printf "<dd><a href='debug.txt'>Debug output</a>")
  (when profile?
    (printf ", <a href='profile.txt'>Profiling report</a>"))
  (printf "</dd>\n")
  (printf "<dt>Bits:</dt><dd>~a bits</dd>\n" bits)
  (printf "</dl>\n")

  (printf "<h2 id='error-message'>~a</h2>\n" (html-escape-unsafe (exn-message err)))
  (printf "<ol id='traceback'>\n")
  (for ([tb (continuation-mark-set->context (exn-continuation-marks err))])
    (printf "<li><code>~a</code> in <code>~a</code></li>\n"
            (html-escape-unsafe (~a (car tb))) (srcloc->string (cdr tb))))
  (printf "</ol>\n")

  (printf "</body>\n")
  (printf "</html>\n"))

(define (make-timeout test bits profile?)
  (printf "<!doctype html>\n")
  (printf "<html>\n")
  (printf "<head>\n")
  (printf "<meta charset='utf-8' />\n")
  (printf "<title>Timeout for ~a</title>" (test-name test))
  (printf "<link rel='stylesheet' type='text/css' href='../graph.css' />")
  (printf "</head>")
  (printf "<body>\n")

  (printf "<dl id='about'>\n")
  (printf "<dt>Test:</dt><dd>~a</dd>" (test-name test))
  (printf "<dt>Logs:</dt>")
  (printf "<dd><a href='debug.txt'>Debug output</a>")
  (when profile?
    (printf ", <a href='profile.txt'>Profiling report</a>"))
  (printf "</dd>\n")
  (printf "<dt>Bits:</dt><dd>~a bits</dd>\n" bits)
  (printf "</dl>\n")

  (printf "<h2>Test timed out</h2>\n")

  (printf "</body>\n")
  (printf "</html>\n"))

(struct interval (alt-idx start-point end-point expr))

(define (output-history altn)
  (match altn
    [(alt-event _ 'start _)
     (printf "<li>Started with <code><pre>~a</pre></code></li>\n"
             (pretty-format (alt-program altn) 65))]

    [(alt-event prog `(start ,strategy) `(,prev))
     (output-history prev)
     (printf "<li class='event'>Using strategy <code>~a</code></li>\n" strategy)]

    [(alt-event _ `(regimes ,splitpoints) prevs)
     (let* ([start-sps (cons (sp -1 -1 -inf.0) (take splitpoints (sub1 (length splitpoints))))]
            [vars (program-variables (alt-program altn))]
            [intervals
             (for/list ([start-sp start-sps] [end-sp splitpoints])
               (interval (sp-cidx end-sp) (sp-point start-sp) (sp-point end-sp) (sp-bexpr end-sp)))]
            [interval->string
             (λ (ival)
                (format "~a < ~a < ~a" (interval-start-point ival)
                        (interval-expr ival) (interval-end-point ival)))])
       (for/list ([entry prevs] [entry-idx (range (length prevs))])
         (let* ([entry-ivals
                 (filter (λ (intrvl) (= (interval-alt-idx intrvl) entry-idx)) intervals)]
                [condition
                 (string-join (map interval->string entry-ivals) " or ")])
           (printf "<h2><code>if <span class='condition'>~a</span></code></h2>\n" condition)
           (printf "<ol>\n")
           (output-history entry)
           (printf "</ol>\n"))))]

    [(alt-event prog `(taylor ,pt ,loc) `(,prev))
     (output-history prev)
     (printf "<li>Taylor expanded <code><pre>~a</pre></code>"
             (location-get loc (alt-program prev)))
     (printf "around ~a to get <code><pre>~a</pre></code></li>" pt (pretty-format prog 65))]

    [(alt-event prog 'periodicity `(,base ,subs ...))
     (output-history base)
     (for ([sub subs])
       (printf "<hr/><li class='event'>Optimizing periodic subexpression</li>\n")
       (output-history sub))
     (printf "<hr/><li class='event'>Combined periodic subexpressions</li>\n")]

    [(alt-event prog 'removed-pows `(,alt))
     (output-history alt)
     (printf "<hr/><li class='event'>Removed slow pow expressions</li>\n")]

    [(alt-event prog 'final-simplify `(,alt))
     (output-history alt)
     (printf "<hr/><li class='event'>Applied final simplification</li>\n")]

    [(alt-delta prog cng prev)
     (output-history prev)
     (printf "<li>Applied <span class='rule'>~a</span> "
             (rule-name (change-rule cng)))
     (printf "to get <code><pre>~a</pre></code></li>\n"
             (pretty-format prog 65))]))

(define (html-escape-unsafe err)
  (string-replace (string-replace (string-replace err "&" "&amp;") "<" "&lt;") ">" "&gt;"))

(define (srcloc->string sl)
  (if sl
      (string-append
       (path->string (srcloc-source sl))
       ":"
       (number->string (srcloc-line sl))
       ":"
       (number->string (srcloc-column sl)))
      "???"))