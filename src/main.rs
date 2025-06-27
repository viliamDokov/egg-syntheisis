use egg::*;
use microlp::*;
use std::{collections::HashMap, thread::sleep, time::Duration};

define_language! {
    enum MathLanguage {
        "+" = Add([Id; 2]),
        "-" = Minus([Id; 2]),
        "*" = Mul([Id; 2]),
        "/" = Div([Id; 2]),
        "neg" = Neg(Id),
        "pow" = Pow([Id; 2]),
        "sqrt" = Sqrt(Id),
        "cbrt" = Cbrt(Id),
        "fabs" = Fabs(Id),
        "copysign" = Copysign([Id; 2]),
        "fmin" = Fmin([Id; 2]),
        "fmax" = Fmax([Id; 2]),
        "exp" = Exp(Id),
        "log" = Log(Id),
        "sinh" = Sinh(Id),
        "cosh" = Cosh(Id),
        "tanh" = Tanh(Id),
        "sin" = Sin(Id),
        "cos" = Cos(Id),
        "tan" = Tan(Id),
        "atan2" = Atan2([Id; 2]),
        "asinh" = ASinh(Id),
        "acosh" = ACosh(Id),
        "atanh" = ATanh(Id),
        "asin" = ASin(Id),
        "acos" = ACos(Id),
        "atan" = ATan(Id),
        "thefunc" = Thefunc(Id),
        "remainder" = Remainder([Id; 2]),

        Num(i32),
        "(PI)" = Pi,
        "(E)" = E,
        Symbol(Symbol),
    }
}

fn make_rules() -> Vec<Rewrite<MathLanguage, ComposionTracking>> {
    vec![
        rewrite!("associate-+r+"; "(+ ?a (+ ?b ?c))" => "(+ (+ ?a ?b) ?c)"),
        rewrite!("associate-+l+"; "(+ (+ ?a ?b) ?c)" => "(+ ?a (+ ?b ?c))"),
        rewrite!("associate-+r-"; "(+ ?a (- ?b ?c))" => "(- (+ ?a ?b) ?c)"),
        rewrite!("associate-+l-"; "(+ (- ?a ?b) ?c)" => "(- ?a (- ?b ?c))"),
        rewrite!("associate--r+"; "(- ?a (+ ?b ?c))" => "(- (- ?a ?b) ?c)"),
        rewrite!("associate--l+"; "(- (+ ?a ?b) ?c)" => "(+ ?a (- ?b ?c))"),
        rewrite!("associate--l-"; "(- (- ?a ?b) ?c)" => "(- ?a (+ ?b ?c))"),
        rewrite!("associate--r-"; "(- ?a (- ?b ?c))" => "(+ (- ?a ?b) ?c)"),
        rewrite!("associate-*r*"; "(* ?a (* ?b ?c))" => "(* (* ?a ?b) ?c)"),
        rewrite!("associate-*l*"; "(* (* ?a ?b) ?c)" => "(* ?a (* ?b ?c))"),
        rewrite!("associate-*r/"; "(* ?a (/ ?b ?c))" => "(/ (* ?a ?b) ?c)"),
        rewrite!("associate-*l/"; "(* (/ ?a ?b) ?c)" => "(/ (* ?a ?c) ?b)"),
        rewrite!("associate-/r*"; "(/ ?a (* ?b ?c))" => "(/ (/ ?a ?b) ?c)"),
        rewrite!("associate-/r/"; "(/ ?a (/ ?b ?c))" => "(* (/ ?a ?b) ?c)"),
        rewrite!("associate-/l/"; "(/ (/ ?b ?c) ?a)" => "(/ ?b (* ?c ?a))"),
        rewrite!("associate-/l*"; "(/ (* ?b ?c) ?a)" => "(* ?b (/ ?c ?a))"),
        rewrite!("remove-double-div"; "(/ 1 (/ 1 ?a))" => "?a"),
        rewrite!("rgt-mult-inverse"; "(* ?a (/ 1 ?a))" => "1"),
        rewrite!("lft-mult-inverse"; "(* (/ 1 ?a) ?a)" => "1"),
        rewrite!("+-inverses"; "(- ?a ?a)" => "0"),
        rewrite!("div0"; "(/ 0 ?a)" => "0"),
        rewrite!("mul0-lft"; "(* 0 ?a)" => "0"),
        rewrite!("mul0-rgt"; "(* ?a 0)" => "0"),
        rewrite!("*-inverses"; "(/ ?a ?a)" => "1"),
        rewrite!("+-lft-identity"; "(+ 0 ?a)" => "?a"),
        rewrite!("+-rgt-identity"; "(+ ?a 0)" => "?a"),
        rewrite!("--rgt-identity"; "(- ?a 0)" => "?a"),
        rewrite!("sub0-neg"; "(- 0 ?a)" => "(neg ?a)"),
        rewrite!("remove-double-neg"; "(neg (neg ?a))" => "?a"),
        rewrite!("*-lft-identity"; "(* 1 ?a)" => "?a"),
        rewrite!("*-rgt-identity"; "(* ?a 1)" => "?a"),
        rewrite!("/-rgt-identity"; "(/ ?a 1)" => "?a"),
        rewrite!("mul-1-neg"; "(* -1 ?a)" => "(neg ?a)"),
        rewrite!("count-2"; "(+ ?x ?x)" => "(* 2 ?x)"),
        rewrite!("2-split"; "2" => "(+ 1 1)"),
        rewrite!("count-2-rev"; "(* 2 ?x)" => "(+ ?x ?x)"),
        rewrite!("distribute-lft-in"; "(* ?a (+ ?b ?c))" => "(+ (* ?a ?b) (* ?a ?c))"),
        rewrite!("distribute-rgt-in"; "(* ?a (+ ?b ?c))" => "(+ (* ?b ?a) (* ?c ?a))"),
        rewrite!("distribute-lft-out"; "(+ (* ?a ?b) (* ?a ?c))" => "(* ?a (+ ?b ?c))"),
        rewrite!("distribute-lft-out--"; "(- (* ?a ?b) (* ?a ?c))" => "(* ?a (- ?b ?c))"),
        rewrite!("distribute-rgt-out"; "(+ (* ?b ?a) (* ?c ?a))" => "(* ?a (+ ?b ?c))"),
        rewrite!("distribute-rgt-out--"; "(- (* ?b ?a) (* ?c ?a))" => "(* ?a (- ?b ?c))"),
        rewrite!("distribute-lft1-in"; "(+ (* ?b ?a) ?a)" => "(* (+ ?b 1) ?a)"),
        rewrite!("distribute-rgt1-in"; "(+ ?a (* ?c ?a))" => "(* (+ ?c 1) ?a)"),
        rewrite!("distribute-lft-neg-in"; "(neg (* ?a ?b))" => "(* (neg ?a) ?b)"),
        rewrite!("distribute-rgt-neg-in"; "(neg (* ?a ?b))" => "(* ?a (neg ?b))"),
        rewrite!("distribute-lft-neg-out"; "(* (neg ?a) ?b)" => "(neg (* ?a ?b))"),
        rewrite!("distribute-rgt-neg-out"; "(* ?a (neg ?b))" => "(neg (* ?a ?b))"),
        rewrite!("distribute-neg-in"; "(neg (+ ?a ?b))" => "(+ (neg ?a) (neg ?b))"),
        rewrite!("distribute-neg-out"; "(+ (neg ?a) (neg ?b))" => "(neg (+ ?a ?b))"),
        rewrite!("distribute-frac-neg"; "(/ (neg ?a) ?b)" => "(neg (/ ?a ?b))"),
        rewrite!("distribute-frac-neg2"; "(/ ?a (neg ?b))" => "(neg (/ ?a ?b))"),
        rewrite!("distribute-neg-frac"; "(neg (/ ?a ?b))" => "(/ (neg ?a) ?b)"),
        rewrite!("distribute-neg-frac2"; "(neg (/ ?a ?b))" => "(/ ?a (neg ?b))"),
        rewrite!("fp-cancel-sign-sub"; "(- ?a (* (neg ?b) ?c))" => "(+ ?a (* ?b ?c))"),
        rewrite!("fp-cancel-sub-sign"; "(+ ?a (* (neg ?b) ?c))" => "(- ?a (* ?b ?c))"),
        rewrite!("fp-cancel-sign-sub-inv"; "(+ ?a (* ?b ?c))" => "(- ?a (* (neg ?b) ?c))"),
        rewrite!("fp-cancel-sub-sign-inv"; "(- ?a (* ?b ?c))" => "(+ ?a (* (neg ?b) ?c))"),
        rewrite!("sub-flip"; "(- ?a ?b)" => "(+ ?a (neg ?b))"),
        rewrite!("sub-flip-reverse"; "(+ ?a (neg ?b))" => "(- ?a ?b)"),
        rewrite!("sub-negate"; "(neg (- ?b ?a))" => "(- ?a ?b)"),
        rewrite!("sub-negate-rev"; "(- ?a ?b)" => "(neg (- ?b ?a))"),
        rewrite!("add-flip"; "(+ ?a ?b)" => "(- ?a (neg ?b))"),
        rewrite!("add-flip-rev"; "(- ?a (neg ?b))" => "(+ ?a ?b)"),
        rewrite!("swap-sqr"; "(* (* ?a ?b) (* ?a ?b))" => "(* (* ?a ?a) (* ?b ?b))"),
        rewrite!("unswap-sqr"; "(* (* ?a ?a) (* ?b ?b))" => "(* (* ?a ?b) (* ?a ?b))"),
        rewrite!("difference-of-squares"; "(- (* ?a ?a) (* ?b ?b))" => "(* (+ ?a ?b) (- ?a ?b))"),
        rewrite!("difference-of-sqr-1"; "(- (* ?a ?a) 1)" => "(* (+ ?a 1) (- ?a 1))"),
        rewrite!("difference-of-sqr--1"; "(+ (* ?a ?a) -1)" => "(* (+ ?a 1) (- ?a 1))"),
        rewrite!("pow-sqr"; "(* (pow ?a ?b) (pow ?a ?b))" => "(pow ?a (* 2 ?b))"),
        rewrite!("sum-square-pow"; "(pow (+ ?a ?b) 2)" => "(+ (+ (pow ?a 2) (* 2 (* ?a ?b))) (pow ?b 2))"),
        rewrite!("sub-square-pow"; "(pow (- ?a ?b) 2)" => "(+ (- (pow ?a 2) (* 2 (* ?a ?b))) (pow ?b 2))"),
        rewrite!("sum-square-pow-rev"; "(+ (+ (pow ?a 2) (* 2 (* ?a ?b))) (pow ?b 2))" => "(pow (+ ?a ?b) 2)"),
        rewrite!("sub-square-pow-rev"; "(+ (- (pow ?a 2) (* 2 (* ?a ?b))) (pow ?b 2))" => "(pow (- ?a ?b) 2)"),
        rewrite!("difference-of-sqr-1-rev"; "(* (+ ?a 1) (- ?a 1))" => "(- (* ?a ?a) 1)"),
        rewrite!("difference-of-sqr--1-rev"; "(* (+ ?a 1) (- ?a 1))" => "(+ (* ?a ?a) -1)"),
        rewrite!("difference-of-squares-rev"; "(* (+ ?a ?b) (- ?a ?b))" => "(- (* ?a ?a) (* ?b ?b))"),
        rewrite!("mult-flip"; "(/ ?a ?b)" => "(* ?a (/ 1 ?b))"),
        rewrite!("mult-flip-rev"; "(* ?a (/ 1 ?b))" => "(/ ?a ?b)"),
        rewrite!("div-flip-rev"; "(/ 1 (/ ?b ?a))" => "(/ ?a ?b)"),
        rewrite!("sum-to-mult-rev"; "(* (+ 1 (/ ?b ?a)) ?a)" => "(+ ?a ?b)"),
        rewrite!("sub-to-mult-rev"; "(* (- 1 (/ ?b ?a)) ?a)" => "(- ?a ?b)"),
        rewrite!("add-to-fraction"; "(+ ?c (/ ?b ?a))" => "(/ (+ (* ?c ?a) ?b) ?a)"),
        rewrite!("add-to-fraction-rev"; "(/ (+ (* ?c ?a) ?b) ?a)" => "(+ ?c (/ ?b ?a))"),
        rewrite!("sub-to-fraction"; "(- ?c (/ ?b ?a))" => "(/ (- (* ?c ?a) ?b) ?a)"),
        rewrite!("sub-to-fraction-rev"; "(/ (- (* ?c ?a) ?b) ?a)" => "(- ?c (/ ?b ?a))"),
        rewrite!("common-denominator"; "(+ (/ ?a ?b) (/ ?c ?d))" => "(/ (+ (* ?a ?d) (* ?c ?b)) (* ?b ?d))"),
        rewrite!("sum-cubes"; "(+ (pow ?a 3) (pow ?b 3))" => "(* (+ (* ?a ?a) (- (* ?b ?b) (* ?a ?b))) (+ ?a ?b))"),
        rewrite!("difference-cubes"; "(- (pow ?a 3) (pow ?b 3))" => "(* (+ (* ?a ?a) (+ (* ?b ?b) (* ?a ?b))) (- ?a ?b))"),
        rewrite!("difference-cubes-rev"; "(* (+ (* ?a ?a) (+ (* ?b ?b) (* ?a ?b))) (- ?a ?b))" => "(- (pow ?a 3) (pow ?b 3))"),
        rewrite!("sum-cubes-rev"; "(* (+ (* ?a ?a) (- (* ?b ?b) (* ?a ?b))) (+ ?a ?b))" => "(+ (pow ?a 3) (pow ?b 3))"),
        rewrite!("div-sub"; "(/ (- ?a ?b) ?c)" => "(- (/ ?a ?c) (/ ?b ?c))"),
        rewrite!("times-frac"; "(/ (* ?a ?b) (* ?c ?d))" => "(* (/ ?a ?c) (/ ?b ?d))"),
        rewrite!("div-add"; "(/ (+ ?a ?b) ?c)" => "(+ (/ ?a ?c) (/ ?b ?c))"),
        rewrite!("div-add-rev"; "(+ (/ ?a ?c) (/ ?b ?c))" => "(/ (+ ?a ?b) ?c)"),
        rewrite!("sub-div"; "(- (/ ?a ?c) (/ ?b ?c))" => "(/ (- ?a ?b) ?c)"),
        rewrite!("frac-add"; "(+ (/ ?a ?b) (/ ?c ?d))" => "(/ (+ (* ?a ?d) (* ?b ?c)) (* ?b ?d))"),
        rewrite!("frac-sub"; "(- (/ ?a ?b) (/ ?c ?d))" => "(/ (- (* ?a ?d) (* ?b ?c)) (* ?b ?d))"),
        rewrite!("frac-times"; "(* (/ ?a ?b) (/ ?c ?d))" => "(/ (* ?a ?c) (* ?b ?d))"),
        rewrite!("frac-2neg"; "(/ ?a ?b)" => "(/ (neg ?a) (neg ?b))"),
        rewrite!("frac-2neg-rev"; "(/ (neg ?a) (neg ?b))" => "(/ ?a ?b)"),
        rewrite!("rem-square-sqrt"; "(* (sqrt ?x) (sqrt ?x))" => "?x"),
        rewrite!("rem-sqrt-square"; "(sqrt (* ?x ?x))" => "(fabs ?x)"),
        rewrite!("rem-sqrt-square-rev"; "(fabs ?x)" => "(sqrt (* ?x ?x))"),
        rewrite!("sqr-neg"; "(* (neg ?x) (neg ?x))" => "(* ?x ?x)"),
        rewrite!("sqr-abs"; "(* (fabs ?x) (fabs ?x))" => "(* ?x ?x)"),
        rewrite!("sqr-abs-rev"; "(* ?x ?x)" => "(* (fabs ?x) (fabs ?x))"),
        rewrite!("sqr-neg-rev"; "(* ?x ?x)" => "(* (neg ?x) (neg ?x))"),
        rewrite!("sqrt-cbrt"; "(sqrt (cbrt ?x))" => "(cbrt (sqrt ?x))"),
        rewrite!("cbrt-sqrt"; "(cbrt (sqrt ?x))" => "(sqrt (cbrt ?x))"),
        rewrite!("fabs-fabs"; "(fabs (fabs ?x))" => "(fabs ?x)"),
        rewrite!("fabs-sub"; "(fabs (- ?a ?b))" => "(fabs (- ?b ?a))"),
        rewrite!("fabs-add"; "(fabs (+ (fabs ?a) (fabs ?b)))" => "(+ (fabs ?a) (fabs ?b))"),
        rewrite!("fabs-neg"; "(fabs (neg ?x))" => "(fabs ?x)"),
        rewrite!("fabs-sqr"; "(fabs (* ?x ?x))" => "(* ?x ?x)"),
        rewrite!("fabs-mul"; "(fabs (* ?a ?b))" => "(* (fabs ?a) (fabs ?b))"),
        rewrite!("fabs-div"; "(fabs (/ ?a ?b))" => "(/ (fabs ?a) (fabs ?b))"),
        rewrite!("neg-fabs"; "(fabs ?x)" => "(fabs (neg ?x))"),
        rewrite!("mul-fabs"; "(* (fabs ?a) (fabs ?b))" => "(fabs (* ?a ?b))"),
        rewrite!("div-fabs"; "(/ (fabs ?a) (fabs ?b))" => "(fabs (/ ?a ?b))"),
        rewrite!("sqrt-fabs"; "(fabs (sqrt ?a))" => "(sqrt ?a)"),
        rewrite!("sqrt-fabs-rev"; "(sqrt ?a)" => "(fabs (sqrt ?a))"),
        rewrite!("fabs-lhs-div"; "(/ (fabs ?x) ?x)" => "(copysign 1 ?x)"),
        rewrite!("fabs-rhs-div"; "(/ ?x (fabs ?x))" => "(copysign 1 ?x)"),
        rewrite!("fabs-cbrt"; "(fabs (/ (cbrt ?a) ?a))" => "(/ (cbrt ?a) ?a)"),
        rewrite!("fabs-cbrt-rev"; "(/ (cbrt ?a) ?a)" => "(fabs (/ (cbrt ?a) ?a))"),
        rewrite!("copysign-neg"; "(copysign ?a (neg ?b))" => "(neg (copysign ?a ?b))"),
        rewrite!("neg-copysign"; "(neg (copysign ?a ?b))" => "(copysign ?a (neg ?b))"),
        rewrite!("copysign-other-neg"; "(copysign (neg ?a) ?b)" => "(copysign ?a ?b)"),
        rewrite!("copysign-fabs"; "(copysign ?a (fabs ?b))" => "(fabs ?a)"),
        rewrite!("copysign-other-fabs"; "(copysign (fabs ?a) ?b)" => "(copysign ?a ?b)"),
        rewrite!("fabs-copysign"; "(fabs (copysign ?a ?b))" => "(fabs ?a)"),
        rewrite!("sqrt-pow2"; "(pow (sqrt ?x) ?y)" => "(pow ?x (/ ?y 2))"),
        rewrite!("sqrt-unprod"; "(* (sqrt ?x) (sqrt ?y))" => "(sqrt (* ?x ?y))"),
        rewrite!("sqrt-undiv"; "(/ (sqrt ?x) (sqrt ?y))" => "(sqrt (/ ?x ?y))"),
        rewrite!("rem-cube-cbrt"; "(pow (cbrt ?x) 3)" => "?x"),
        rewrite!("rem-cbrt-cube"; "(cbrt (pow ?x 3))" => "?x"),
        rewrite!("rem-3cbrt-lft"; "(* (* (cbrt ?x) (cbrt ?x)) (cbrt ?x))" => "?x"),
        rewrite!("rem-3cbrt-rft"; "(* (cbrt ?x) (* (cbrt ?x) (cbrt ?x)))" => "?x"),
        rewrite!("cube-neg"; "(pow (neg ?x) 3)" => "(neg (pow ?x 3))"),
        rewrite!("cube-neg-rev"; "(neg (pow ?x 3))" => "(pow (neg ?x) 3)"),
        rewrite!("cube-prod"; "(pow (* ?x ?y) 3)" => "(* (pow ?x 3) (pow ?y 3))"),
        rewrite!("cube-div"; "(pow (/ ?x ?y) 3)" => "(/ (pow ?x 3) (pow ?y 3))"),
        rewrite!("cube-mult"; "(pow ?x 3)" => "(* ?x (* ?x ?x))"),
        rewrite!("cube-prod-rev"; "(* (pow ?x 3) (pow ?y 3))" => "(pow (* ?x ?y) 3)"),
        rewrite!("cube-div-rev"; "(/ (pow ?x 3) (pow ?y 3))" => "(pow (/ ?x ?y) 3)"),
        rewrite!("cbrt-prod"; "(cbrt (* ?x ?y))" => "(* (cbrt ?x) (cbrt ?y))"),
        rewrite!("cbrt-div"; "(cbrt (/ ?x ?y))" => "(/ (cbrt ?x) (cbrt ?y))"),
        rewrite!("cbrt-unprod"; "(* (cbrt ?x) (cbrt ?y))" => "(cbrt (* ?x ?y))"),
        rewrite!("cbrt-undiv"; "(/ (cbrt ?x) (cbrt ?y))" => "(cbrt (/ ?x ?y))"),
        rewrite!("pow-cbrt"; "(pow (cbrt ?x) ?y)" => "(pow ?x (/ ?y 3))"),
        rewrite!("cbrt-pow"; "(cbrt (pow ?x ?y))" => "(pow ?x (/ ?y 3))"),
        rewrite!("add-cube-cbrt"; "?x" => "(* (* (cbrt ?x) (cbrt ?x)) (cbrt ?x))"),
        rewrite!("add-cbrt-cube"; "?x" => "(cbrt (* (* ?x ?x) ?x))"),
        rewrite!("cube-unmult"; "(* ?x (* ?x ?x))" => "(pow ?x 3)"),
        rewrite!("cbrt-neg"; "(cbrt (neg ?x))" => "(neg (cbrt ?x))"),
        rewrite!("cbrt-neg-rev"; "(neg (cbrt ?x))" => "(cbrt (neg ?x))"),
        rewrite!("cbrt-fabs"; "(cbrt (fabs ?x))" => "(fabs (cbrt ?x))"),
        rewrite!("cbrt-fabs-rev"; "(fabs (cbrt ?x))" => "(cbrt (fabs ?x))"),
        rewrite!("cbrt-div-cbrt"; "(/ (cbrt ?x) (fabs (cbrt ?x)))" => "(copysign 1 ?x)"),
        rewrite!("cbrt-div-cbrt2"; "(/ (fabs (cbrt ?x)) (cbrt ?x))" => "(copysign 1 ?x)"),
        rewrite!("fmin-swap"; "(fmin ?a ?b)" => "(fmin ?b ?a)"),
        rewrite!("fmax-swap"; "(fmax ?a ?b)" => "(fmax ?b ?a)"),
        rewrite!("add-log-exp"; "?x" => "(log (exp ?x))"),
        rewrite!("rem-exp-log"; "(exp (log ?x))" => "?x"),
        rewrite!("rem-log-exp"; "(log (exp ?x))" => "?x"),
        rewrite!("exp-0"; "(exp 0)" => "1"),
        rewrite!("exp-1-e"; "(exp 1)" => "(PI)"),
        rewrite!("1-exp"; "1" => "(exp 0)"),
        rewrite!("e-exp-1"; "(PI)" => "(exp 1)"),
        rewrite!("exp-fabs"; "(exp ?x)" => "(fabs (exp ?x))"),
        rewrite!("fabs-exp"; "(fabs (exp ?x))" => "(exp ?x)"),
        rewrite!("exp-sum"; "(exp (+ ?a ?b))" => "(* (exp ?a) (exp ?b))"),
        rewrite!("exp-neg"; "(exp (neg ?a))" => "(/ 1 (exp ?a))"),
        rewrite!("exp-diff"; "(exp (- ?a ?b))" => "(/ (exp ?a) (exp ?b))"),
        rewrite!("prod-exp"; "(* (exp ?a) (exp ?b))" => "(exp (+ ?a ?b))"),
        rewrite!("rec-exp"; "(/ 1 (exp ?a))" => "(exp (neg ?a))"),
        rewrite!("div-exp"; "(/ (exp ?a) (exp ?b))" => "(exp (- ?a ?b))"),
        rewrite!("exp-prod"; "(exp (* ?a ?b))" => "(pow (exp ?a) ?b)"),
        rewrite!("exp-sqrt"; "(exp (/ ?a 2))" => "(sqrt (exp ?a))"),
        rewrite!("exp-cbrt"; "(exp (/ ?a 3))" => "(cbrt (exp ?a))"),
        rewrite!("exp-lft-sqr"; "(exp (* ?a 2))" => "(* (exp ?a) (exp ?a))"),
        rewrite!("exp-lft-cube"; "(exp (* ?a 3))" => "(pow (exp ?a) 3)"),
        rewrite!("exp-cbrt-rev"; "(cbrt (exp ?a))" => "(exp (/ ?a 3))"),
        rewrite!("exp-lft-cube-rev"; "(pow (exp ?a) 3)" => "(exp (* ?a 3))"),
        rewrite!("exp-sqrt-rev"; "(sqrt (exp ?a))" => "(exp (/ ?a 2))"),
        rewrite!("exp-lft-sqr-rev"; "(* (exp ?a) (exp ?a))" => "(exp (* ?a 2))"),
        rewrite!("unpow-1"; "(pow ?a -1)" => "(/ 1 ?a)"),
        rewrite!("unpow1"; "(pow ?a 1)" => "?a"),
        rewrite!("unpow0"; "(pow ?a 0)" => "1"),
        rewrite!("pow-base-1"; "(pow 1 ?a)" => "1"),
        rewrite!("pow1"; "?a" => "(pow ?a 1)"),
        rewrite!("unpow1/2"; "(pow ?a (/ 1 2))" => "(sqrt ?a)"),
        rewrite!("unpow2"; "(pow ?a 2)" => "(* ?a ?a)"),
        rewrite!("unpow3"; "(pow ?a 3)" => "(* (* ?a ?a) ?a)"),
        rewrite!("unpow1/3"; "(pow ?a (/ 1 3))" => "(cbrt ?a)"),
        rewrite!("pow-base-0"; "(pow 0 ?a)" => "0"),
        rewrite!("inv-pow"; "(/ 1 ?a)" => "(pow ?a -1)"),
        rewrite!("pow1/2"; "(sqrt ?a)" => "(pow ?a (/ 1 2))"),
        rewrite!("pow2"; "(* ?a ?a)" => "(pow ?a 2)"),
        rewrite!("pow1/3"; "(cbrt ?a)" => "(pow ?a (/ 1 3))"),
        rewrite!("pow3"; "(* (* ?a ?a) ?a)" => "(pow ?a 3)"),
        rewrite!("exp-to-pow"; "(exp (* (log ?a) ?b))" => "(pow ?a ?b)"),
        rewrite!("pow-plus"; "(* (pow ?a ?b) ?a)" => "(pow ?a (+ ?b 1))"),
        rewrite!("pow-exp"; "(pow (exp ?a) ?b)" => "(exp (* ?a ?b))"),
        rewrite!("pow-prod-down"; "(* (pow ?b ?a) (pow ?c ?a))" => "(pow (* ?b ?c) ?a)"),
        rewrite!("pow-prod-up"; "(* (pow ?a ?b) (pow ?a ?c))" => "(pow ?a (+ ?b ?c))"),
        rewrite!("pow-flip"; "(/ 1 (pow ?a ?b))" => "(pow ?a (neg ?b))"),
        rewrite!("pow-div"; "(/ (pow ?a ?b) (pow ?a ?c))" => "(pow ?a (- ?b ?c))"),
        rewrite!("log-rec"; "(log (/ 1 ?a))" => "(neg (log ?a))"),
        rewrite!("log-E"; "(log (PI))" => "1"),
        rewrite!("log-pow-rev"; "(* ?b (log ?a))" => "(log (pow ?a ?b))"),
        rewrite!("sum-log"; "(+ (log ?a) (log ?b))" => "(log (* ?a ?b))"),
        rewrite!("diff-log"; "(- (log ?a) (log ?b))" => "(log (/ ?a ?b))"),
        rewrite!("neg-log"; "(neg (log ?a))" => "(log (/ 1 ?a))"),
        rewrite!("sin-0"; "(sin 0)" => "0"),
        rewrite!("cos-0"; "(cos 0)" => "1"),
        rewrite!("tan-0"; "(tan 0)" => "0"),
        rewrite!("sin-neg"; "(sin (neg ?x))" => "(neg (sin ?x))"),
        rewrite!("cos-neg"; "(cos (neg ?x))" => "(cos ?x)"),
        rewrite!("cos-fabs"; "(cos (fabs ?x))" => "(cos ?x)"),
        rewrite!("tan-neg"; "(tan (neg ?x))" => "(neg (tan ?x))"),
        rewrite!("cos-neg-rev"; "(cos ?x)" => "(cos (neg ?x))"),
        rewrite!("cos-fabs-rev"; "(cos ?x)" => "(cos (fabs ?x))"),
        rewrite!("sin-neg-rev"; "(neg (sin ?x))" => "(sin (neg ?x))"),
        rewrite!("tan-neg-rev"; "(neg (tan ?x))" => "(tan (neg ?x))"),
        rewrite!("sqr-sin-b"; "(* (sin ?x) (sin ?x))" => "(- 1 (* (cos ?x) (cos ?x)))"),
        rewrite!("sqr-cos-b"; "(* (cos ?x) (cos ?x))" => "(- 1 (* (sin ?x) (sin ?x)))"),
        rewrite!("sqr-cos-b-rev"; "(- 1 (* (sin ?x) (sin ?x)))" => "(* (cos ?x) (cos ?x))"),
        rewrite!("sqr-sin-b-rev"; "(- 1 (* (cos ?x) (cos ?x)))" => "(* (sin ?x) (sin ?x))"),
        rewrite!("sin-asin"; "(sin (asin ?x))" => "?x"),
        rewrite!("cos-acos"; "(cos (acos ?x))" => "?x"),
        rewrite!("tan-atan"; "(tan (atan ?x))" => "?x"),
        rewrite!("atan-tan"; "(atan (tan ?x))" => "(remainder ?x (PI))"),
        rewrite!("asin-sin"; "(asin (sin ?x))" => "(- (fabs (remainder (+ ?x (/ (PI) 2)) (* 2 (PI)))) (/ (PI) 2))"),
        rewrite!("acos-cos"; "(acos (cos ?x))" => "(fabs (remainder ?x (* 2 (PI))))"),
        rewrite!("acos-cos-rev"; "(fabs (remainder ?x (* 2 (PI))))" => "(acos (cos ?x))"),
        rewrite!("asin-sin-rev"; "(- (fabs (remainder (+ ?x (/ (PI) 2)) (* 2 (PI)))) (/ (PI) 2))" => "(asin (sin ?x))"),
        rewrite!("cos-sin-sum"; "(+ (* (cos ?a) (cos ?a)) (* (sin ?a) (sin ?a)))" => "1"),
        rewrite!("1-sub-cos"; "(- 1 (* (cos ?a) (cos ?a)))" => "(* (sin ?a) (sin ?a))"),
        rewrite!("1-sub-sin"; "(- 1 (* (sin ?a) (sin ?a)))" => "(* (cos ?a) (cos ?a))"),
        rewrite!("-1-add-cos"; "(+ (* (cos ?a) (cos ?a)) -1)" => "(neg (* (sin ?a) (sin ?a)))"),
        rewrite!("-1-add-sin"; "(+ (* (sin ?a) (sin ?a)) -1)" => "(neg (* (cos ?a) (cos ?a)))"),
        rewrite!("sub-1-cos"; "(- (* (cos ?a) (cos ?a)) 1)" => "(neg (* (sin ?a) (sin ?a)))"),
        rewrite!("sub-1-sin"; "(- (* (sin ?a) (sin ?a)) 1)" => "(neg (* (cos ?a) (cos ?a)))"),
        rewrite!("sin-PI/6"; "(sin (/ (PI) 6))" => "(/ 1 2)"),
        rewrite!("sin-PI/4"; "(sin (/ (PI) 4))" => "(/ (sqrt 2) 2)"),
        rewrite!("sin-PI/3"; "(sin (/ (PI) 3))" => "(/ (sqrt 3) 2)"),
        rewrite!("sin-PI/2"; "(sin (/ (PI) 2))" => "1"),
        rewrite!("sin-PI"; "(sin (PI))" => "0"),
        rewrite!("sin-+PI"; "(sin (+ ?x (PI)))" => "(neg (sin ?x))"),
        rewrite!("sin-+PI/2"; "(sin (+ ?x (/ (PI) 2)))" => "(cos ?x)"),
        rewrite!("cos-PI/6"; "(cos (/ (PI) 6))" => "(/ (sqrt 3) 2)"),
        rewrite!("cos-PI/4"; "(cos (/ (PI) 4))" => "(/ (sqrt 2) 2)"),
        rewrite!("cos-PI/3"; "(cos (/ (PI) 3))" => "(/ 1 2)"),
        rewrite!("cos-PI/2"; "(cos (/ (PI) 2))" => "0"),
        rewrite!("cos-PI"; "(cos (PI))" => "-1"),
        rewrite!("cos-+PI"; "(cos (+ ?x (PI)))" => "(neg (cos ?x))"),
        rewrite!("cos-+PI/2"; "(cos (+ ?x (/ (PI) 2)))" => "(neg (sin ?x))"),
        rewrite!("tan-PI/6"; "(tan (/ (PI) 6))" => "(/ 1 (sqrt 3))"),
        rewrite!("tan-PI/4"; "(tan (/ (PI) 4))" => "1"),
        rewrite!("tan-PI/3"; "(tan (/ (PI) 3))" => "(sqrt 3)"),
        rewrite!("tan-PI"; "(tan (PI))" => "0"),
        rewrite!("tan-+PI"; "(tan (+ ?x (PI)))" => "(tan ?x)"),
        rewrite!("hang-0p-tan"; "(/ (sin ?a) (+ 1 (cos ?a)))" => "(tan (/ ?a 2))"),
        rewrite!("hang-0m-tan"; "(/ (neg (sin ?a)) (+ 1 (cos ?a)))" => "(tan (/ (neg ?a) 2))"),
        rewrite!("hang-p0-tan"; "(/ (- 1 (cos ?a)) (sin ?a))" => "(tan (/ ?a 2))"),
        rewrite!("hang-m0-tan"; "(/ (- 1 (cos ?a)) (neg (sin ?a)))" => "(tan (/ (neg ?a) 2))"),
        rewrite!("hang-p-tan"; "(/ (+ (sin ?a) (sin ?b)) (+ (cos ?a) (cos ?b)))" => "(tan (/ (+ ?a ?b) 2))"),
        rewrite!("hang-m-tan"; "(/ (- (sin ?a) (sin ?b)) (+ (cos ?a) (cos ?b)))" => "(tan (/ (- ?a ?b) 2))"),
        rewrite!("1-sub-sin-rev"; "(* (cos ?a) (cos ?a))" => "(- 1 (* (sin ?a) (sin ?a)))"),
        rewrite!("hang-0m-tan-rev"; "(tan (/ (neg ?a) 2))" => "(/ (neg (sin ?a)) (+ 1 (cos ?a)))"),
        rewrite!("hang-0p-tan-rev"; "(tan (/ ?a 2))" => "(/ (sin ?a) (+ 1 (cos ?a)))"),
        rewrite!("tan-+PI-rev"; "(tan ?x)" => "(tan (+ ?x (PI)))"),
        rewrite!("cos-+PI/2-rev"; "(neg (sin ?x))" => "(cos (+ ?x (/ (PI) 2)))"),
        rewrite!("sin-+PI/2-rev"; "(cos ?x)" => "(sin (+ ?x (/ (PI) 2)))"),
        rewrite!("sin-+PI-rev"; "(neg (sin ?x))" => "(sin (+ ?x (PI)))"),
        rewrite!("cos-+PI-rev"; "(neg (cos ?x))" => "(cos (+ ?x (PI)))"),
        rewrite!("neg-tan-+PI/2-rev"; "(/ -1 (tan ?x))" => "(tan (+ ?x (/ (PI) 2)))"),
        rewrite!("tan-+PI/2-rev"; "(/ 1 (tan ?x))" => "(tan (+ (neg ?x) (/ (PI) 2)))"),
        rewrite!("sin-sum"; "(sin (+ ?x ?y))" => "(+ (* (sin ?x) (cos ?y)) (* (cos ?x) (sin ?y)))"),
        rewrite!("cos-sum"; "(cos (+ ?x ?y))" => "(- (* (cos ?x) (cos ?y)) (* (sin ?x) (sin ?y)))"),
        rewrite!("sin-diff"; "(sin (- ?x ?y))" => "(- (* (sin ?x) (cos ?y)) (* (cos ?x) (sin ?y)))"),
        rewrite!("cos-diff"; "(cos (- ?x ?y))" => "(+ (* (cos ?x) (cos ?y)) (* (sin ?x) (sin ?y)))"),
        rewrite!("sin-2"; "(sin (* 2 ?x))" => "(* 2 (* (sin ?x) (cos ?x)))"),
        rewrite!("sin-3"; "(sin (* 3 ?x))" => "(- (* 3 (sin ?x)) (* 4 (pow (sin ?x) 3)))"),
        rewrite!("2-sin"; "(* 2 (* (sin ?x) (cos ?x)))" => "(sin (* 2 ?x))"),
        rewrite!("3-sin"; "(- (* 3 (sin ?x)) (* 4 (pow (sin ?x) 3)))" => "(sin (* 3 ?x))"),
        rewrite!("cos-2"; "(cos (* 2 ?x))" => "(- (* (cos ?x) (cos ?x)) (* (sin ?x) (sin ?x)))"),
        rewrite!("cos-3"; "(cos (* 3 ?x))" => "(- (* 4 (pow (cos ?x) 3)) (* 3 (cos ?x)))"),
        rewrite!("2-cos"; "(- (* (cos ?x) (cos ?x)) (* (sin ?x) (sin ?x)))" => "(cos (* 2 ?x))"),
        rewrite!("3-cos"; "(- (* 4 (pow (cos ?x) 3)) (* 3 (cos ?x)))" => "(cos (* 3 ?x))"),
        rewrite!("cos-diff-rev"; "(+ (* (cos ?x) (cos ?y)) (* (sin ?x) (sin ?y)))" => "(cos (- ?x ?y))"),
        rewrite!("sin-diff-rev"; "(- (* (sin ?x) (cos ?y)) (* (cos ?x) (sin ?y)))" => "(sin (- ?x ?y))"),
        rewrite!("sin-sum-rev"; "(+ (* (sin ?x) (cos ?y)) (* (cos ?x) (sin ?y)))" => "(sin (+ ?x ?y))"),
        rewrite!("tan-sum-rev"; "(/ (+ (tan ?x) (tan ?y)) (- 1 (* (tan ?x) (tan ?y))))" => "(tan (+ ?x ?y))"),
        rewrite!("cos-sum-rev"; "(- (* (cos ?x) (cos ?y)) (* (sin ?x) (sin ?y)))" => "(cos (+ ?x ?y))"),
        rewrite!("sqr-sin-a"; "(* (sin ?x) (sin ?x))" => "(- (/ 1 2) (* (/ 1 2) (cos (* 2 ?x))))"),
        rewrite!("sqr-cos-a"; "(* (cos ?x) (cos ?x))" => "(+ (/ 1 2) (* (/ 1 2) (cos (* 2 ?x))))"),
        rewrite!("diff-sin"; "(- (sin ?x) (sin ?y))" => "(* 2 (* (sin (/ (- ?x ?y) 2)) (cos (/ (+ ?x ?y) 2))))"),
        rewrite!("diff-cos"; "(- (cos ?x) (cos ?y))" => "(* -2 (* (sin (/ (- ?x ?y) 2)) (sin (/ (+ ?x ?y) 2))))"),
        rewrite!("sum-sin"; "(+ (sin ?x) (sin ?y))" => "(* 2 (* (sin (/ (+ ?x ?y) 2)) (cos (/ (- ?x ?y) 2))))"),
        rewrite!("sum-cos"; "(+ (cos ?x) (cos ?y))" => "(* 2 (* (cos (/ (+ ?x ?y) 2)) (cos (/ (- ?x ?y) 2))))"),
        rewrite!("cos-mult"; "(* (cos ?x) (cos ?y))" => "(/ (+ (cos (+ ?x ?y)) (cos (- ?x ?y))) 2)"),
        rewrite!("sin-mult"; "(* (sin ?x) (sin ?y))" => "(/ (- (cos (- ?x ?y)) (cos (+ ?x ?y))) 2)"),
        rewrite!("sin-cos-mult"; "(* (sin ?x) (cos ?y))" => "(/ (+ (sin (- ?x ?y)) (sin (+ ?x ?y))) 2)"),
        rewrite!("diff-atan"; "(- (atan ?x) (atan ?y))" => "(atan2 (- ?x ?y) (+ 1 (* ?x ?y)))"),
        rewrite!("sum-atan"; "(+ (atan ?x) (atan ?y))" => "(atan2 (+ ?x ?y) (- 1 (* ?x ?y)))"),
        rewrite!("tan-quot"; "(tan ?x)" => "(/ (sin ?x) (cos ?x))"),
        rewrite!("quot-tan"; "(/ (sin ?x) (cos ?x))" => "(tan ?x)"),
        rewrite!("2-tan"; "(/ (* 2 (tan ?x)) (- 1 (* (tan ?x) (tan ?x))))" => "(tan (* 2 ?x))"),
        rewrite!("diff-cos-rev"; "(* -2 (* (sin (/ (- ?x ?y) 2)) (sin (/ (+ ?x ?y) 2))))" => "(- (cos ?x) (cos ?y))"),
        rewrite!("diff-sin-rev"; "(* 2 (* (sin (/ (- ?x ?y) 2)) (cos (/ (+ ?x ?y) 2))))" => "(- (sin ?x) (sin ?y))"),
        rewrite!("diff-atan-rev"; "(atan2 (- ?x ?y) (+ 1 (* ?x ?y)))" => "(- (atan ?x) (atan ?y))"),
        rewrite!("sum-sin-rev"; "(* 2 (* (sin (/ (+ ?x ?y) 2)) (cos (/ (- ?x ?y) 2))))" => "(+ (sin ?x) (sin ?y))"),
        rewrite!("sum-cos-rev"; "(* 2 (* (cos (/ (+ ?x ?y) 2)) (cos (/ (- ?x ?y) 2))))" => "(+ (cos ?x) (cos ?y))"),
        rewrite!("sum-atan-rev"; "(atan2 (+ ?x ?y) (- 1 (* ?x ?y)))" => "(+ (atan ?x) (atan ?y))"),
        rewrite!("sqr-cos-a-rev"; "(+ (/ 1 2) (* (/ 1 2) (cos (* 2 ?x))))" => "(* (cos ?x) (cos ?x))"),
        rewrite!("sqr-sin-a-rev"; "(- (/ 1 2) (* (/ 1 2) (cos (* 2 ?x))))" => "(* (sin ?x) (sin ?x))"),
        rewrite!("cos-mult-rev"; "(/ (+ (cos (+ ?x ?y)) (cos (- ?x ?y))) 2)" => "(* (cos ?x) (cos ?y))"),
        rewrite!("sin-mult-rev"; "(/ (- (cos (- ?x ?y)) (cos (+ ?x ?y))) 2)" => "(* (sin ?x) (sin ?y))"),
        rewrite!("sin-cos-mult-rev"; "(/ (+ (sin (- ?x ?y)) (sin (+ ?x ?y))) 2)" => "(* (sin ?x) (cos ?y))"),
        rewrite!("cos-asin"; "(cos (asin ?x))" => "(sqrt (- 1 (* ?x ?x)))"),
        rewrite!("tan-asin"; "(tan (asin ?x))" => "(/ ?x (sqrt (- 1 (* ?x ?x))))"),
        rewrite!("sin-acos"; "(sin (acos ?x))" => "(sqrt (- 1 (* ?x ?x)))"),
        rewrite!("tan-acos"; "(tan (acos ?x))" => "(/ (sqrt (- 1 (* ?x ?x))) ?x)"),
        rewrite!("sin-atan"; "(sin (atan ?x))" => "(/ ?x (sqrt (+ 1 (* ?x ?x))))"),
        rewrite!("cos-atan"; "(cos (atan ?x))" => "(/ 1 (sqrt (+ 1 (* ?x ?x))))"),
        rewrite!("asin-acos"; "(asin ?x)" => "(- (/ (PI) 2) (acos ?x))"),
        rewrite!("acos-asin"; "(acos ?x)" => "(- (/ (PI) 2) (asin ?x))"),
        rewrite!("asin-neg"; "(asin (neg ?x))" => "(neg (asin ?x))"),
        rewrite!("acos-neg"; "(acos (neg ?x))" => "(- (PI) (acos ?x))"),
        rewrite!("atan-neg"; "(atan (neg ?x))" => "(neg (atan ?x))"),
        rewrite!("acos-asin-rev"; "(- (/ (PI) 2) (asin ?x))" => "(acos ?x)"),
        rewrite!("asin-acos-rev"; "(- (/ (PI) 2) (acos ?x))" => "(asin ?x)"),
        rewrite!("asin-neg-rev"; "(neg (asin ?x))" => "(asin (neg ?x))"),
        rewrite!("atan-neg-rev"; "(neg (atan ?x))" => "(atan (neg ?x))"),
        rewrite!("acos-neg-rev"; "(- (PI) (acos ?x))" => "(acos (neg ?x))"),
        rewrite!("cos-atan-rev"; "(/ 1 (sqrt (+ 1 (* ?x ?x))))" => "(cos (atan ?x))"),
        rewrite!("tan-acos-rev"; "(/ (sqrt (- 1 (* ?x ?x))) ?x)" => "(tan (acos ?x))"),
        rewrite!("tan-asin-rev"; "(/ ?x (sqrt (- 1 (* ?x ?x))))" => "(tan (asin ?x))"),
        rewrite!("cos-asin-rev"; "(sqrt (- 1 (* ?x ?x)))" => "(cos (asin ?x))"),
        rewrite!("sin-atan-rev"; "(/ ?x (sqrt (+ 1 (* ?x ?x))))" => "(sin (atan ?x))"),
        rewrite!("sin-acos-rev"; "(sqrt (- 1 (* ?x ?x)))" => "(sin (acos ?x))"),
        rewrite!("sinh-def"; "(sinh ?x)" => "(/ (- (exp ?x) (exp (neg ?x))) 2)"),
        rewrite!("cosh-def"; "(cosh ?x)" => "(/ (+ (exp ?x) (exp (neg ?x))) 2)"),
        rewrite!("tanh-def-a"; "(tanh ?x)" => "(/ (- (exp ?x) (exp (neg ?x))) (+ (exp ?x) (exp (neg ?x))))"),
        rewrite!("tanh-def-b"; "(tanh ?x)" => "(/ (- (exp (* 2 ?x)) 1) (+ (exp (* 2 ?x)) 1))"),
        rewrite!("tanh-def-c"; "(tanh ?x)" => "(/ (- 1 (exp (* -2 ?x))) (+ 1 (exp (* -2 ?x))))"),
        rewrite!("sinh-cosh"; "(- (* (cosh ?x) (cosh ?x)) (* (sinh ?x) (sinh ?x)))" => "1"),
        rewrite!("sinh-+-cosh"; "(+ (cosh ?x) (sinh ?x))" => "(exp ?x)"),
        rewrite!("sinh---cosh"; "(- (cosh ?x) (sinh ?x))" => "(exp (neg ?x))"),
        rewrite!("tanh-def-b-rev"; "(/ (- (exp (* 2 ?x)) 1) (+ (exp (* 2 ?x)) 1))" => "(tanh ?x)"),
        rewrite!("tanh-def-c-rev"; "(/ (- 1 (exp (* -2 ?x))) (+ 1 (exp (* -2 ?x))))" => "(tanh ?x)"),
        rewrite!("sinh-def-rev"; "(/ (- (exp ?x) (exp (neg ?x))) 2)" => "(sinh ?x)"),
        rewrite!("cosh-def-rev"; "(/ (+ (exp ?x) (exp (neg ?x))) 2)" => "(cosh ?x)"),
        rewrite!("sinh-+-cosh-rev"; "(exp ?x)" => "(+ (cosh ?x) (sinh ?x))"),
        rewrite!("sinh---cosh-rev"; "(exp (neg ?x))" => "(- (cosh ?x) (sinh ?x))"),
        rewrite!("sinh-undef"; "(- (exp ?x) (exp (neg ?x)))" => "(* 2 (sinh ?x))"),
        rewrite!("cosh-undef"; "(+ (exp ?x) (exp (neg ?x)))" => "(* 2 (cosh ?x))"),
        rewrite!("tanh-undef"; "(/ (- (exp ?x) (exp (neg ?x))) (+ (exp ?x) (exp (neg ?x))))" => "(tanh ?x)"),
        rewrite!("cosh-sum"; "(cosh (+ ?x ?y))" => "(+ (* (cosh ?x) (cosh ?y)) (* (sinh ?x) (sinh ?y)))"),
        rewrite!("cosh-diff"; "(cosh (- ?x ?y))" => "(- (* (cosh ?x) (cosh ?y)) (* (sinh ?x) (sinh ?y)))"),
        rewrite!("cosh-2"; "(cosh (* 2 ?x))" => "(+ (* (sinh ?x) (sinh ?x)) (* (cosh ?x) (cosh ?x)))"),
        rewrite!("cosh-1/2"; "(cosh (/ ?x 2))" => "(sqrt (/ (+ (cosh ?x) 1) 2))"),
        rewrite!("sinh-sum"; "(sinh (+ ?x ?y))" => "(+ (* (sinh ?x) (cosh ?y)) (* (cosh ?x) (sinh ?y)))"),
        rewrite!("sinh-diff"; "(sinh (- ?x ?y))" => "(- (* (sinh ?x) (cosh ?y)) (* (cosh ?x) (sinh ?y)))"),
        rewrite!("sinh-2"; "(sinh (* 2 ?x))" => "(* 2 (* (sinh ?x) (cosh ?x)))"),
        rewrite!("sinh-1/2"; "(sinh (/ ?x 2))" => "(/ (sinh ?x) (sqrt (* 2 (+ (cosh ?x) 1))))"),
        rewrite!("tanh-2"; "(tanh (* 2 ?x))" => "(/ (* 2 (tanh ?x)) (+ 1 (* (tanh ?x) (tanh ?x))))"),
        rewrite!("tanh-1/2"; "(tanh (/ ?x 2))" => "(/ (sinh ?x) (+ (cosh ?x) 1))"),
        rewrite!("sum-sinh"; "(+ (sinh ?x) (sinh ?y))" => "(* 2 (* (sinh (/ (+ ?x ?y) 2)) (cosh (/ (- ?x ?y) 2))))"),
        rewrite!("sum-cosh"; "(+ (cosh ?x) (cosh ?y))" => "(* 2 (* (cosh (/ (+ ?x ?y) 2)) (cosh (/ (- ?x ?y) 2))))"),
        rewrite!("diff-sinh"; "(- (sinh ?x) (sinh ?y))" => "(* 2 (* (cosh (/ (+ ?x ?y) 2)) (sinh (/ (- ?x ?y) 2))))"),
        rewrite!("diff-cosh"; "(- (cosh ?x) (cosh ?y))" => "(* 2 (* (sinh (/ (+ ?x ?y) 2)) (sinh (/ (- ?x ?y) 2))))"),
        rewrite!("tanh-sum"; "(tanh (+ ?x ?y))" => "(/ (+ (tanh ?x) (tanh ?y)) (+ 1 (* (tanh ?x) (tanh ?y))))"),
        rewrite!("sinh-undef-rev"; "(* 2 (sinh ?x))" => "(- (exp ?x) (exp (neg ?x)))"),
        rewrite!("cosh-undef-rev"; "(* 2 (cosh ?x))" => "(+ (exp ?x) (exp (neg ?x)))"),
        rewrite!("diff-cosh-rev"; "(* 2 (* (sinh (/ (+ ?x ?y) 2)) (sinh (/ (- ?x ?y) 2))))" => "(- (cosh ?x) (cosh ?y))"),
        rewrite!("diff-sinh-rev"; "(* 2 (* (cosh (/ (+ ?x ?y) 2)) (sinh (/ (- ?x ?y) 2))))" => "(- (sinh ?x) (sinh ?y))"),
        rewrite!("cosh-diff-rev"; "(- (* (cosh ?x) (cosh ?y)) (* (sinh ?x) (sinh ?y)))" => "(cosh (- ?x ?y))"),
        rewrite!("sinh-diff-rev"; "(- (* (sinh ?x) (cosh ?y)) (* (cosh ?x) (sinh ?y)))" => "(sinh (- ?x ?y))"),
        rewrite!("tanh-1/2-rev"; "(/ (sinh ?x) (+ (cosh ?x) 1))" => "(tanh (/ ?x 2))"),
        rewrite!("tanh-1/2*-rev"; "(/ (- (cosh ?x) 1) (sinh ?x))" => "(tanh (/ ?x 2))"),
        rewrite!("tanh-2-rev"; "(/ (* 2 (tanh ?x)) (+ 1 (* (tanh ?x) (tanh ?x))))" => "(tanh (* 2 ?x))"),
        rewrite!("sinh-1/2-rev"; "(/ (sinh ?x) (sqrt (* 2 (+ (cosh ?x) 1))))" => "(sinh (/ ?x 2))"),
        rewrite!("cosh-1/2-rev"; "(sqrt (/ (+ (cosh ?x) 1) 2))" => "(cosh (/ ?x 2))"),
        rewrite!("sinh-2-rev"; "(* 2 (* (sinh ?x) (cosh ?x)))" => "(sinh (* 2 ?x))"),
        rewrite!("cosh-2-rev"; "(+ (* (sinh ?x) (sinh ?x)) (* (cosh ?x) (cosh ?x)))" => "(cosh (* 2 ?x))"),
        rewrite!("sinh-sum-rev"; "(+ (* (sinh ?x) (cosh ?y)) (* (cosh ?x) (sinh ?y)))" => "(sinh (+ ?x ?y))"),
        rewrite!("tanh-sum-rev"; "(/ (+ (tanh ?x) (tanh ?y)) (+ 1 (* (tanh ?x) (tanh ?y))))" => "(tanh (+ ?x ?y))"),
        rewrite!("cosh-sum-rev"; "(+ (* (cosh ?x) (cosh ?y)) (* (sinh ?x) (sinh ?y)))" => "(cosh (+ ?x ?y))"),
        rewrite!("sum-cosh-rev"; "(* 2 (* (cosh (/ (+ ?x ?y) 2)) (cosh (/ (- ?x ?y) 2))))" => "(+ (cosh ?x) (cosh ?y))"),
        rewrite!("sum-sinh-rev"; "(* 2 (* (sinh (/ (+ ?x ?y) 2)) (cosh (/ (- ?x ?y) 2))))" => "(+ (sinh ?x) (sinh ?y))"),
        rewrite!("sinh-neg"; "(sinh (neg ?x))" => "(neg (sinh ?x))"),
        rewrite!("sinh-0"; "(sinh 0)" => "0"),
        rewrite!("sinh-0-rev"; "0" => "(sinh 0)"),
        rewrite!("cosh-neg"; "(cosh (neg ?x))" => "(cosh ?x)"),
        rewrite!("cosh-0"; "(cosh 0)" => "1"),
        rewrite!("cosh-0-rev"; "1" => "(cosh 0)"),
        rewrite!("cosh-neg-rev"; "(cosh ?x)" => "(cosh (neg ?x))"),
        rewrite!("sinh-neg-rev"; "(neg (sinh ?x))" => "(sinh (neg ?x))"),
        rewrite!("asinh-def"; "(asinh ?x)" => "(log (+ ?x (sqrt (+ (* ?x ?x) 1))))"),
        rewrite!("acosh-def"; "(acosh ?x)" => "(log (+ ?x (sqrt (- (* ?x ?x) 1))))"),
        rewrite!("atanh-def"; "(atanh ?x)" => "(/ (log (/ (+ 1 ?x) (- 1 ?x))) 2)"),
        rewrite!("sinh-asinh"; "(sinh (asinh ?x))" => "?x"),
        rewrite!("sinh-acosh"; "(sinh (acosh ?x))" => "(sqrt (- (* ?x ?x) 1))"),
        rewrite!("sinh-atanh"; "(sinh (atanh ?x))" => "(/ ?x (sqrt (- 1 (* ?x ?x))))"),
        rewrite!("cosh-asinh"; "(cosh (asinh ?x))" => "(sqrt (+ (* ?x ?x) 1))"),
        rewrite!("cosh-acosh"; "(cosh (acosh ?x))" => "?x"),
        rewrite!("cosh-atanh"; "(cosh (atanh ?x))" => "(/ 1 (sqrt (- 1 (* ?x ?x))))"),
        rewrite!("tanh-asinh"; "(tanh (asinh ?x))" => "(/ ?x (sqrt (+ 1 (* ?x ?x))))"),
        rewrite!("tanh-acosh"; "(tanh (acosh ?x))" => "(/ (sqrt (- (* ?x ?x) 1)) ?x)"),
        rewrite!("tanh-atanh"; "(tanh (atanh ?x))" => "?x"),
        rewrite!("asinh-def-rev"; "(log (+ ?x (sqrt (+ (* ?x ?x) 1))))" => "(asinh ?x)"),
        rewrite!("atanh-def-rev"; "(/ (log (/ (+ 1 ?x) (- 1 ?x))) 2)" => "(atanh ?x)"),
        rewrite!("acosh-def-rev"; "(log (+ ?x (sqrt (- (* ?x ?x) 1))))" => "(acosh ?x)"),
        rewrite!("tanh-asinh-rev"; "(/ ?x (sqrt (+ 1 (* ?x ?x))))" => "(tanh (asinh ?x))"),
        rewrite!("cosh-asinh-rev"; "(sqrt (+ (* ?x ?x) 1))" => "(cosh (asinh ?x))"),
        rewrite!("sinh-atanh-rev"; "(/ ?x (sqrt (- 1 (* ?x ?x))))" => "(sinh (atanh ?x))"),
        rewrite!("cosh-atanh-rev"; "(/ 1 (sqrt (- 1 (* ?x ?x))))" => "(cosh (atanh ?x))"),
        rewrite!("asinh-2"; "(acosh (+ (* 2 (* ?x ?x)) 1))" => "(* 2 (asinh (fabs ?x)))"),
        rewrite!("acosh-2-rev"; "(* 2 (acosh ?x))" => "(acosh (- (* 2 (* ?x ?x)) 1))"),
    ]
}

// in this case, our analysis itself doesn't require any data, so we can just
// use a unit struct and derive Default
#[derive(Default)]
struct ComposionTracking;
impl Analysis<MathLanguage> for ComposionTracking {
    type Data = Vec<(usize, usize)>;

    fn merge(&mut self, to: &mut Self::Data, from: Self::Data) -> DidMerge {
        let old_to = to.len();
        let old_from = from.len();
        for node in from {
            if !to.contains(&node) {
                to.push(node);
            }
        }
        DidMerge(old_to != to.len(), old_from != to.len())
    }

    fn make(egraph: &mut EGraph<MathLanguage, Self>, enode: &MathLanguage) -> Self::Data {
        vec![]
    }

    fn modify(egraph: &mut EGraph<MathLanguage, Self>, id: Id) {}
}

fn get_composed<'a>(
    egraph: &'a EGraph<MathLanguage, ComposionTracking>,
    expr: &RecExpr<MathLanguage>,
) -> &'a Vec<(usize, usize)> {
    let eClass = egraph.lookup_expr(expr).unwrap();
    return &egraph[eClass].data;
}

// You could use egg::AstSize, but this is useful for debugging, since
// it will really try to get rid of the Diff operator
pub struct CostFn;
impl egg::CostFunction<MathLanguage> for CostFn {
    type Cost = usize;
    fn cost<C>(&mut self, enode: &MathLanguage, mut costs: C) -> Self::Cost
    where
        C: FnMut(Id) -> Self::Cost,
    {
        // We really like using thefunc
        let op_cost = match enode {
            MathLanguage::Thefunc(..) => 1 + enode.fold(0, |sum, i| sum + costs(i)),
            _ => 10 + 10 * enode.fold(0, |sum, i| sum + costs(i)),
        };
        op_cost
    }
}

fn extract_expressions(start_pattern: Pattern<MathLanguage>) -> Vec<RecExpr<MathLanguage>> {
    let mut thefunc_rules = make_rules();
    let thefunc_pattern: Pattern<MathLanguage> = "(thefunc ?x)".parse().unwrap();
    let rule_to =
        Rewrite::new("to-thefunc", start_pattern.clone(), thefunc_pattern.clone()).unwrap();
    let rule_from = Rewrite::new(
        "from-thefunc",
        thefunc_pattern.clone(),
        start_pattern.clone(),
    )
    .unwrap();
    thefunc_rules.push(rule_to);
    thefunc_rules.push(rule_from);

    println!("Finidng Equivalences!");
    let start = "(thefunc x)".parse().unwrap();
    let inital_runner = Runner::default()
        .with_node_limit(1_000_000)
        .with_iter_limit(7)
        // .with_time_limit(Duration::new(2, 0))
        .with_explanations_enabled()
        .with_expr(&start)
        .run(&thefunc_rules);
    println!("{}", inital_runner.report());

    let egraph = &inital_runner.egraph;
    let root = inital_runner.roots[0];
    let class = &egraph[root];

    let extractor = Extractor::new(&egraph, CostFn);
    let get_node = |id| {
        let node = extractor.find_best_node(id);
        // sleep(Duration::from_millis(100));
        // println!("Getting: {id} {node:?}");
        return node.clone();
    };

    fn contains_thefun(expr: &RecExpr<MathLanguage>) -> bool {
        expr.iter().any(|node| match node {
            MathLanguage::Thefunc(..) => true,
            _ => false,
        })
    }

    println!("Starting node {root}");
    let expressions: Vec<RecExpr<MathLanguage>> = class
        .iter()
        .map(|node| node.build_recexpr(get_node))
        .filter(contains_thefun)
        .collect();

    println!("Found {} base expressions", expressions.len());
    for res in expressions.iter() {
        println!("EXPR: {res}");
    }

    let mut duplicate_runner = Runner::default()
        .with_node_limit(1_000_000)
        .with_time_limit(Duration::new(10, 0));

    for node in expressions.iter() {
        duplicate_runner = duplicate_runner.with_expr(&node);
    }

    println!("Deduplicating expressions...");
    duplicate_runner = duplicate_runner.run(&make_rules());
    println!("{}", duplicate_runner.report());

    let duplicate_egg = duplicate_runner.egraph;

    let deduplicated_exressions: Vec<RecExpr<MathLanguage>> = expressions
        .iter()
        .filter(|expr| duplicate_egg.equivs(&expr, &start).len() == 0)
        .cloned()
        .collect();

    println!(
        "Found {} deduplicated expressions",
        deduplicated_exressions.len()
    );

    let src = &deduplicated_exressions[0];
    let mut composition_runner = Runner::default()
        .with_node_limit(1_000_000)
        .with_time_limit(Duration::new(10, 0));

    let mut composition_egraph = EGraph::default().with_explanations_enabled();
    let mut compositions = vec![];

    for (idx1, node1) in deduplicated_exressions.iter().enumerate() {
        for (idx2, node2) in deduplicated_exressions.iter().enumerate() {
            let comp = compose_expressions(node1, node2);
            let id = composition_egraph.add_expr(&comp);
            composition_egraph.set_analysis_data(id, vec![(idx1, idx2)]);
            compositions.push(((idx1, idx2), comp));
        }
    }
    composition_egraph.rebuild();

    println!("Deduplicating compositions");
    composition_runner = composition_runner
        .with_explanations_enabled()
        .with_egraph(composition_egraph)
        .run(&make_rules());
    println!("{}", composition_runner.report());

    let N = deduplicated_exressions.len();
    let mut problem = Problem::new(OptimizationDirection::Minimize);
    let mut I = vec![];
    let mut CI = vec![];
    let mut AGE = vec![];
    let mut GOAL = vec![];
    let two = problem.add_integer_var(0.0, (0, i32::MAX));
    problem.add_constraint([(two, 1.0)], ComparisonOp::Eq, 2.0);

    composition_egraph = composition_runner.egraph;
    for (i, node) in deduplicated_exressions.iter().enumerate() {
        // We need to express a constraint for each node that
        // CI[idx] == (I[idx] /\ AGE[idx] == 1) \/ ( CI[c1] /\ CI[c2] /\ AGE[idx] = AGE[c1] + AGE[c2])
        // where we need to add a ( CI[c1] /\ CI[c2] /\ AGE[idx] = AGE[c1] + AGE[c2]) term
        // for every possible composition pair (c1,c2) that is equivalent to our index

        // For this first iteration we just prepare the generic variables
        I.push(problem.add_binary_var(1.0));
        CI.push(problem.add_binary_var(0.0));
        AGE.push(problem.add_integer_var(0.0, (1, i32::MAX)));

        // This is equivalent to the constraint that Age[i] == 1
        // since: Age[i] == 1 <=> (Age[i] >= 1 /\ Age[i] < 2)
        let age_equals_one = add_lt_var(&mut problem, AGE[i], two);
        // this is the (I[idx] /\ AGE[idx] == 1)
        let I_and_age = add_and_var(&mut problem, I[i], age_equals_one);
        GOAL.push(I_and_age);
    }

    // Now in this pass we add the constrants that allow us to derive expressions from compositions
    for (i, node) in deduplicated_exressions.iter().enumerate() {
        println!("Calculating constraint: {i}/{N}");
        // let mut equivs = vec![];
        // for ((idx1, idx2), cmp) in compositions.iter() {
        //     let eq_len = composition_egraph.equivs(&cmp, &node).len();
        //     if eq_len != 0 {
        //         equivs.push(((idx1, idx2)));
        //     }
        // }
        let equivs2 = get_composed(&composition_egraph, node);
        // println!("EQ1: {:?}", equivs.len());
        println!("EQ2: {:?}", equivs2.len());

        for (c1, c2) in equivs2.into_iter() {
            println!("Adding equivalence ({c1}, {c2})");
            if *c1 != *c2 {
                // The constraint solver does not like cases where the ids are the same

                // The this variable AGE = AGE[c1] + AGE[c2]
                let age_sum = problem.add_integer_var(0.0, (0, i32::MAX));
                problem.add_constraint(
                    [(age_sum, 1.0), (AGE[*c1], -1.0), (AGE[*c2], -1.0)],
                    ComparisonOp::Eq,
                    -1.0,
                );
                // this is the constraint that AGE[i] < AGE[c1] + AGE[c2]
                let age_lt_sum = add_lt_var(&mut problem, age_sum, AGE[i]);
                // This is the constraint that (CI[c1] /\ CI[c2] /\ AGE[i] < AGE[c1] + AGE[c2])
                let and_1 = add_and_var(&mut problem, CI[*c1], age_lt_sum);
                let and_2 = add_and_var(&mut problem, CI[*c2], and_1);
                GOAL[i] = add_or_var(&mut problem, GOAL[i], and_2);
            } else {
                // The this variable AGE = AGE[c1] + AGE[c1] (since c1 == c2)
                let age_sum = problem.add_integer_var(0.0, (0, i32::MAX));
                problem.add_constraint([(age_sum, 1.0), (AGE[*c1], -2.0)], ComparisonOp::Eq, -1.0);
                // this is the constraint that AGE[i] < AGE[c1] + AGE[c2]
                let age_lt_sum = add_lt_var(&mut problem, age_sum, AGE[i]);
                // This is the constraint that (CI[c1] /\ AGE[i] < AGE[c1] + AGE[c2])
                let and_c: Variable = add_and_var(&mut problem, CI[*c1], age_lt_sum);
                GOAL[i] = add_or_var(&mut problem, GOAL[i], and_c);
            }
        }

        // Actually make sure that the goals are there!
        problem.add_constraint([(GOAL[i], 1.0)], ComparisonOp::Eq, 1.0);
        problem.add_constraint([(GOAL[i], 1.0), (CI[1], -1.0)], ComparisonOp::Eq, 0.0);
    }

    println!("Solving!");
    let sol = problem.solve().unwrap();
    println!("SOLUTION {sol:?}");
    for (i, _) in deduplicated_exressions.iter().enumerate() {
        println!(
            "Idx: {} I:{} CI{} AI;{}",
            i, sol[I[i]], sol[CI[i]], sol[AGE[i]],
        )
    }

    let composed_exressions: Vec<RecExpr<MathLanguage>> = deduplicated_exressions
        .iter()
        .enumerate()
        .filter_map(|(id, expr)| if sol[I[id]] == 1.0 { Some(expr) } else { None })
        .cloned()
        .collect();

    println!("Found {} decomposed expressions", composed_exressions.len());

    return composed_exressions;
}

fn compose_expressions(
    src: &RecExpr<MathLanguage>,
    pattern: &RecExpr<MathLanguage>,
) -> RecExpr<MathLanguage> {
    let mut res = RecExpr::default();
    let x = Symbol::from("x");
    let mut ids: HashMap<Id, Id> = HashMap::with_capacity(src.len());

    for (outer_idx, node) in src.iter().enumerate() {
        match node {
            MathLanguage::Thefunc(thefun_id) => {
                let offset = res.len();
                let mut x_idx = None;
                for (idx, pattern_node) in pattern.iter().enumerate() {
                    let new_node = match pattern_node {
                        MathLanguage::Symbol(sym) => {
                            if sym.clone() == x {
                                x_idx = Some(idx);
                            }
                            pattern_node.clone()
                        }
                        _ => pattern_node.clone().map_children(|id| match x_idx {
                            Some(x_id) => {
                                if (Id::from(x_id) == id) {
                                    ids[thefun_id]
                                } else {
                                    Id::from(offset + usize::from(id))
                                }
                            }
                            None => id,
                        }),
                    };
                    // println!("{:?} adding {:?}", res, new_node);
                    res.add(new_node);
                }
                ids.insert(Id::from(outer_idx), Id::from(res.len() - 1));
            }
            _rest => {
                // println!("Doing node: {node:?} {ids:?} {outer_idx}");
                let node_moved = node.clone().map_children(|id| ids[&id]);
                res.add(node_moved);
                ids.insert(Id::from(outer_idx), Id::from(res.len() - 1));
            }
        }
    }
    return res;
}

fn add_and_var(problem: &mut Problem, a: Variable, b: Variable) -> Variable {
    let and = problem.add_binary_var(0.0);
    problem.add_constraint([(a, 1.0), (b, 1.0), (and, -2.0)], ComparisonOp::Ge, 0.0);
    problem.add_constraint([(a, 1.0), (b, 1.0), (and, -2.0)], ComparisonOp::Le, 1.0);
    return and;
}

fn add_or_var(problem: &mut Problem, a: Variable, b: Variable) -> Variable {
    let or = problem.add_binary_var(0.0);
    problem.add_constraint([(a, 1.0), (b, 1.0), (or, -2.0)], ComparisonOp::Ge, -1.0);
    problem.add_constraint([(a, 1.0), (b, 1.0), (or, -2.0)], ComparisonOp::Le, 0.0);
    return or;
}

fn add_lt_var(problem: &mut Problem, a: Variable, b: Variable) -> Variable {
    let lt = problem.add_binary_var(0.0);
    let k = 100000000.0;
    problem.add_constraint([(a, 1.0), (b, -1.0), (lt, k)], ComparisonOp::Ge, 0.0);
    problem.add_constraint([(a, 1.0), (b, -1.0), (lt, k)], ComparisonOp::Le, k - 1.0);
    return lt;
}

// fn ilp_tests() {
//     let mut problem = Problem::new(OptimizationDirection::Minimize);
//     // let c1 = add_bool(&mut problem, 1.0);
//     // let c2 = add_bool(&mut problem, 1.0);

//     let a1 = problem.add_integer_var(1.0, (1, i32::MAX));
//     let two = problem.add_integer_var(0.0, (1, i32::MAX));

//     let lt = add_lt_var(&mut problem, a1, two);
//     problem.add_constraint([(two, 1.0)], ComparisonOp::Eq, 2.0);
//     problem.add_constraint([(a1, 1.0)], ComparisonOp::Eq, 5.0);
//     let solution = problem.solve().unwrap();
//     println!("Solution: {}", solution[lt]);
//     println!("Solution: {}", solution[a1]);
//     println!("Solution: {}", solution[two]);

//     // let or = add_or_var(&mut problem, c1, c2);
//     // let and = add_and_var(&mut problem, c1, c2);
//     // problem.add_constraint([(c1, 1.0)], ComparisonOp::Eq, 0.0);
//     // problem.add_constraint([(c2, 1.0)], ComparisonOp::Eq, 1.0);

//     // let solution = problem.solve().unwrap();
//     // println!("Solution: {}", solution[or]);
//     // println!("Solution: {}", solution[and]);
//     // println!("Solution: {}", solution[c1]);
//     // println!("Solution: {}", solution[c2]);
//     // subject to constraints: x + y <= 4 and 2 * x + y >= 2.
// }

// fn paper_constraints_test() {
//     let mut problem = Problem::new(OptimizationDirection::Minimize);
//     let mut Is = vec![];
//     let mut Cs = vec![];
//     let mut As = vec![];

//     for _i in 0..3 {
//         Is.push(problem.add_binary_var(1.0));
//         Cs.push(problem.add_binary_var(0.0));
//         As.push(problem.add_integer_var(0.0, (1, i32::MAX)));
//     }

//     let two = problem.add_integer_var(0.0, (0, i32::MAX));
//     problem.add_constraint([(two, 1.0)], ComparisonOp::Eq, 2.0);
//     {
//         let a_eq_1 = add_lt_var(&mut problem, As[0], two);
//         let and_1_i = add_and_var(&mut problem, Is[0], a_eq_1);
//         let age_sum_1 = problem.add_integer_var(0.0, (0, i32::MAX));
//         problem.add_constraint([(age_sum_1, 1.0), (As[1], -2.0)], ComparisonOp::Eq, 0.0);
//         let age_lt_1 = add_lt_var(&mut problem, age_sum_1, As[0]);
//         let and_1_c = Cs[1];
//         let and_2_c = add_and_var(&mut problem, and_1_c, age_lt_1);
//         let or_1 = add_or_var((&mut problem), and_2_c, and_1_i);

//         problem.add_constraint([(or_1, 1.0)], ComparisonOp::Eq, 1.0);
//         problem.add_constraint([(or_1, 1.0), (Cs[0], -1.0)], ComparisonOp::Eq, 0.0);
//     }
//     {
//         let a_eq_2 = add_lt_var(&mut problem, As[1], two);
//         let and_2_i = add_and_var(&mut problem, Is[1], a_eq_2);
//         problem.add_constraint([(and_2_i, 1.0)], ComparisonOp::Eq, 1.0);
//         problem.add_constraint([(and_2_i, 1.0), (Cs[1], -1.0)], ComparisonOp::Eq, 0.0);
//     }
//     {
//         let a_eq_1 = add_lt_var(&mut problem, As[2], two);
//         let and_1_i = add_and_var(&mut problem, Is[2], a_eq_1);

//         let age_sum_1 = problem.add_integer_var(0.0, (0, i32::MAX));
//         problem.add_constraint([(age_sum_1, 1.0), (As[2], -2.0)], ComparisonOp::Eq, -1.0);

//         let age_lt_1 = add_lt_var(&mut problem, age_sum_1, As[2]);
//         let and_1_c = Cs[2];
//         let and_2_c = add_and_var(&mut problem, and_1_c, age_lt_1);
//         let or_1 = add_or_var((&mut problem), and_2_c, and_1_i);
//         problem.add_constraint([(or_1, 1.0)], ComparisonOp::Eq, 1.0);

//         problem.add_constraint([(or_1, 1.0), (Cs[2], -1.0)], ComparisonOp::Eq, 0.0);
//     }

//     let solution = problem.solve().unwrap();
//     println!("1: {}", solution[Is[0]]);
//     println!("2: {}", solution[Is[1]]);
//     println!("3: {}", solution[Is[2]]);
// }

fn main() {
    let expr = "(- (tan ?x) (sin ?x))".parse().unwrap();
    let results = extract_expressions(expr);
    for res in results {
        println!("EXPR: {res}");
    }

    // let expr = "(- (log (+ ?x 1)) (log ?x))".parse().unwrap();
    // let results = extract_expressions(expr);
    // for res in results {
    //     println!("EXPR: {res}");
    // }

    // let expr = "(/ (- 1 (cos ?x)) (sin ?x))".parse().unwrap();
    // let results = extract_expressions(expr);
    // for res in results {
    //     println!("EXPR: {res}");
    // }

    // paper_constraints_test();
}
