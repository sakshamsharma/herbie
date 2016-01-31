(lambda (x)
  #:name "complicated zero"
  (- (/ x x) (* (/ 1 x) (sqrt (* x x)))))

(lambda (r d)
  #:name "cos atan frac"
  (cos (atan (/ r d))))
