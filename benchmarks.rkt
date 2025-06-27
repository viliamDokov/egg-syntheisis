"sqrt_add"  (/ 1 (+ (sqrt (+ x 1)) (sqrt x))))
"exp1x"     (/ (- (exp x) 1) x))
"exp1x_log" (/ (- (exp x) 1) (log (exp x))))
"logexp" (log (+ 1 (exp x))))
"nonlin1" (/ z (+ z 1)))
"logexp" (let ((e (exp x))) (log (+ 1 e))))
(FPCore
 (x)
 :name
 "test05_nonlin1, r4"
 :precision
 binary64
 :pre
 (< 100001/100000 x 2)
 (let ((r1 (- x 1)) (r2 (* x x))) (/ r1 (- r2 1))))


(FPCore
 (x)
 :name
 "NMSE example 3.1"
 :cite
 (hamming-1987 herbie-2015)
 :fpbench-domain
 textbook
 :pre
 (>= x 0)
 (- (sqrt (+ x 1)) (sqrt x)))

(FPCore
 (x eps)
 :name
 "NMSE example 3.3"
 :cite
 (hamming-1987 herbie-2015)
 :fpbench-domain
 textbook
 (- (sin (+ x eps)) (sin x)))

(FPCore
 (x)
 :name
 "NMSE example 3.4"
 :cite
 (hamming-1987 herbie-2015)
 :fpbench-domain
 textbook
 :pre
 (!= x 0)
 (/ (- 1 (cos x)) (sin x)))

(FPCore
 (N)
 :name
 "NMSE example 3.5"
 :cite
 (hamming-1987 herbie-2015)
 :fpbench-domain
 textbook
 (- (atan (+ N 1)) (atan N)))

(FPCore
 (x)
 :name
 "NMSE example 3.6"
 :cite
 (hamming-1987 herbie-2015)
 :fpbench-domain
 textbook
 :pre
 (>= x 0)
 (- (/ 1 (sqrt x)) (/ 1 (sqrt (+ x 1)))))

(FPCore
 (x)
 :name
 "NMSE problem 3.3.1"
 :cite
 (hamming-1987 herbie-2015)
 :fpbench-domain
 textbook
 :pre
 (!= x 0)
 (- (/ 1 (+ x 1)) (/ 1 x)))


(FPCore
 (x)
 :name
 "NMSE problem 3.3.3"
 :cite
 (hamming-1987 herbie-2015)
 :fpbench-domain
 textbook
 :pre
 (!= x 0 1 -1)
 (+ (- (/ 1 (+ x 1)) (/ 2 x)) (/ 1 (- x 1))))

(FPCore
 (x)
 :name
 "NMSE problem 3.3.4"
 :cite
 (hamming-1987 herbie-2015)
 :fpbench-domain
 textbook
 :pre
 (>= x 0)
 (- (pow (+ x 1) (/ 1 3)) (pow x (/ 1 3))))

(FPCore
 (N)
 :name
 "NMSE problem 3.3.6"
 :cite
 (hamming-1987 herbie-2015)
 :fpbench-domain
 textbook
 :pre
 (> N 0)
 (- (log (+ N 1)) (log N)))

(FPCore
 (x)
 :name
 "NMSE problem 3.3.7"
 :cite
 (hamming-1987 herbie-2015)
 :fpbench-domain
 textbook
 (+ (- (exp x) 2) (exp (- x))))

(FPCore
 (x)
 :name
 "NMSE example 3.7"
 :cite
 (hamming-1987 herbie-2015)
 :fpbench-domain
 textbook
 (- (exp x) 1))

(FPCore
 (N)
 :name
 "NMSE example 3.8"
 :cite
 (hamming-1987 herbie-2015)
 :fpbench-domain
 textbook
 :pre
 (> N 0)
 (- (- (* (+ N 1) (log (+ N 1))) (* N (log N))) 1))

(FPCore
 (x)
 :name
 "NMSE example 3.9"
 :cite
 (hamming-1987 herbie-2015)
 :fpbench-domain
 textbook
 :pre
 (!= x 0)
 (- (/ 1 x) (/ 1 (tan x))))

(FPCore
 (x)
 :name
 "NMSE example 3.10"
 :cite
 (hamming-1987 herbie-2015)
 :fpbench-domain
 textbook
 :pre
 (< -1 x 1)
 (/ (log (- 1 x)) (log (+ 1 x))))

(FPCore
 (x)
 :name
 "NMSE problem 3.4.1"
 :cite
 (hamming-1987 herbie-2015)
 :fpbench-domain
 textbook
 :pre
 (!= x 0)
 (/ (- 1 (cos x)) (* x x)))

(FPCore
 (eps)
 :name
 "NMSE problem 3.4.3"
 :cite
 (hamming-1987 herbie-2015)
 :fpbench-domain
 textbook
 :pre
 (< -1 eps 1)
 (log (/ (- 1 eps) (+ 1 eps))))

(FPCore
 (x)
 :name
 "NMSE problem 3.4.4"
 :cite
 (hamming-1987 herbie-2015)
 :fpbench-domain
 textbook
 :pre
 (!= x 0)
 (sqrt (/ (- (exp (* 2 x)) 1) (- (exp x) 1))))

(FPCore
 (x)
 :name
 "NMSE problem 3.4.5"
 :cite
 (hamming-1987 herbie-2015)
 :fpbench-domain
 textbook
 :pre
 (!= x 0)
 (/ (- x (sin x)) (- x (tan x))))


(FPCore
 (x)
 :name
 "NMSE section 3.11"
 :cite
 (hamming-1987 herbie-2015)
 :fpbench-domain
 textbook
 :pre
 (!= x 0)
 (/ (exp x) (- (exp x) 1)))



(FPCore
 (x)
 :name
 "verhulst"
 :cite
 (darulova-kuncak-2014 solovyev-et-al-2015)
 :fpbench-domain
 science
 :precision
 binary64
 :pre
 (<= 1/10 x 3/10)
 (let ((r 4) (K 111/100)) (/ (* r x) (+ 1 (/ x K)))))

(FPCore
 (x)
 :name
 "predatorPrey"
 :cite
 (darulova-kuncak-2014 solovyev-et-al-2015)
 :fpbench-domain
 science
 :precision
 binary64
 :pre
 (<= 1/10 x 3/10)
 (let ((r 4) (K 111/100)) (/ (* (* r x) x) (+ 1 (* (/ x K) (/ x K))))))

(FPCore
 (v)
 :name
 "carbonGas"
 :cite
 (darulova-kuncak-2014 solovyev-et-al-2015)
 :fpbench-domain
 science
 :precision
 binary64
 :pre
 (<= 1/10 v 1/2)
 (let ((p 35000000)
       (a 401/1000)
       (b 427/10000000)
       (t 300)
       (n 1000)
       (k 13806503/1000000000000000000000000000000))
   (- (* (+ p (* (* a (/ n v)) (/ n v))) (- v (* n b))) (* (* k n) t))))

(FPCore
 (x)
 :name
 "sine"
 :cite
 (darulova-kuncak-2014 solovyev-et-al-2015)
 :fpbench-domain
 mathematics
 :precision
 binary64
 :rosa-post
 (=> res (< -1 res 1))
 :rosa-ensuring
 1/100000000000000
 :pre
 (< -157079632679/100000000000 x 157079632679/100000000000)
 (-
  (+ (- x (/ (* (* x x) x) 6)) (/ (* (* (* (* x x) x) x) x) 120))
  (/ (* (* (* (* (* (* x x) x) x) x) x) x) 5040)))

(FPCore
 (x)
 :name
 "sqroot"
 :cite
 (darulova-kuncak-2014 solovyev-et-al-2015)
 :fpbench-domain
 mathematics
 :pre
 (<= 0 x 1)
 (-
  (+ (- (+ 1 (* 1/2 x)) (* (* 1/8 x) x)) (* (* (* 1/16 x) x) x))
  (* (* (* (* 5/128 x) x) x) x)))

(FPCore
 (x)
 :name
 "sineOrder3"
 :cite
 (darulova-kuncak-2014 solovyev-et-al-2015)
 :fpbench-domain
 mathematics
 :precision
 binary64
 :pre
 (< -2 x 2)
 :rosa-post
 (=> res (< -1 res 1))
 :rosa-ensuring
 1/100000000000000
 (-
  (* 238732414637843/250000000000000 x)
  (* 6450306886639899/50000000000000000 (* (* x x) x))))



(FPCore
 (u)
 :name
 "bspline3"
 :cite
 (darulova-kuncak-2014)
 :pre
 (<= 0 u 1)
 :rosa-post
 (=> res (<= -17/100 res 1/20))
 :rosa-ensuring
 1/100000000000
 (/ (- (* (* u u) u)) 6))
