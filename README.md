# Introduction

This project is a reimplementation of the paper [Synthesizing Mathematical Identities with E-Graphs](https://dl.acm.org/doi/pdf/10.1145/3520308.3534506).
The goal of this paper is to use e-graphs to synthesize useful function identities which are used for range reduction.
Range reduction is a technique used to compute values of transcendental functions such as `sin(x)` or `ln(x)`.
This technique relies on the fact that we can easily approximate the values of such functions in a small range such as `[0, 2pi]` for `sin(x)`. However, approximating the value of `sin(x)` for an arbitrary floating point value `x` is non-trivial. That is why we can use some properties of `sin(x)` such as `sin(x) = sin(x + 2*pi)`, to help us derive the values of `sin(x)` for an arbitrary `x`. Although these formulas are well known for primitive functions like `sin(x)` finding such properties for complex expressions such as `tan(x) - sin(x)` is not simple and usually needs some mathematical knowledge to do so.

The goal of the paper and of this project is to use egg to derive useful identities which can be used for range reduction. That is given a function `f(x)` to find equivalent identities which are some function of the original such as `f(x) = -f(-x)`.

The goal of this project specifically is to reproduce the results of the paper by re-implementing it. I could not find the original source code of the paper, so this project is an attempt to recreate it from scratch. In the later sections of this README we will discuss in detail how each section of the paper was reimplemented.

# Usage

To run the project simply execute `cargo run -r`. This will run the synthesis with some sample expressions.

# Reimplementation

## Rewrite rules

The original paper mentioned that all the rewrite rules used for deriving new identities were taken from the [Herbie](https://github.com/herbie-fp/herbie) project. The paper did not mention which rules specifically were taken from that project. With this lack of information the main set of rewrite rules that I found in the Herbie project were the [following](https://github.com/herbie-fp/herbie/blob/8a1ac87be2d70463868f28a111bb95ea8c73e510/src/core/rules.rkt#L21).

The issue with these rules was, that they are written in racket and are then passed on to the egg rust library via foreign function interface (FFI). However, for the reimplementation I chose to use rust since it is the native language of the egg project. So we need to somehow translate these rules from racket to a format that can be used for the egg library in rust. To do this, I downloaded all the rules and saved them to `rule-parser/rules.rkt`. I then wrote a small parser for these rule expressions in Haskell, so they can be translated into usable rust egg rules. More information on how this parser works can be found in the `rule-parser/Main.hs` file. To run the rule parser navigate to the `rule-parser` directory and do `cabal run`

The resulting rules were then put in the `src/language.rs` file. These rules will be the backbone of the next steps of the project, since they are the main rules used for finding _equal_ expressions.

## Initial synthesis and THEFUNC

The first step of the synthesis process is to start with an input expression for which we want to synthesize range reduction rules. As a working example we will use the expression `(- (tan ?x) (sin ?x))`. We can put this expression as our starting expression in our e-graph runner directly and run it on the rules we derived above, however it will be really difficult to extract useful range reduction rules with do this directly. This is because we are interested in expressions of the form `s(f(t(x)))`, where `f(x)` is `(- (tan ?x) (sin ?x))`, however it is hard to extract expressions of this form the resulting e-class after we run the rules on the e-graph. This is why we add one more special symbol to our e-graph grammar called `(thefunc x)`, which will represent the function that we are trying to optimize (in our case `(thefunc x)` will be equal to `(- (tan ?x) (sin ?x))`. Now instead of running the e-graph with `(- (tan ?x) (sin ?x))` as an initial expression we will run it with `(thefunc x)` instead. Additionally, to indicate that the `(thefunc x)` is equal to `(- (tan ?x) (sin ?x))` we dynamically generate a 2 rewrite rules which allows as to rewrite `(thefunc x)` as `(- (tan ?x) (sin ?x))` and vice-versa.

## Extraction of candidate expressions

Now that we have run our target function and saturated an e-graph, we need to extract interesting expressions that are
equivalent to `(thefunc x)`. The classic e-graph extraction process uses the e-graph to find a SINGLE _best_ expression that is equivalent to our initial expression. For our use case, however, we need to find all expressions that are equal to `(thefunc x)` and among these expressions we need to find the _useful_ ones. To do so, we modify the extraction process as it was suggested in the paper. Namely, after we are done with applying rules to our graph we go through each e-node in the e-class that contains `(thefunc x)`. Since these e-nodes have children that are e-classes, we extract the _best_ e-node from these classes using the traditional approach of finding the lowest-cost expression from that e class. This way we get all the expressions equal to `(thefunc x)`, represented in a nice and short form.

In our case we are most interested in e-nodes that use `thefunc` as an operator, so we use a custom cost function which prioritizes e-nodes that contain `thefunc` over all other e-nodes. The source code of this function can be found in `src/language.rs` in the implementation of the `CostFn` struct.

With this extraction method in place, we can synthesize identities of `thefunc x`, which are likely to be of the form `s(f(t(x)))`, which is most useful for range reduction. However, despite everything, some identities do not contain `thefunc` in any way. These identities are not useful, so we filter them out directly.

The code for extracting these identities can be found in `src/main.rs` under the `extract_base_expressions` function.

## De-duplication

Now that we have a base of expression we need to filter them out to find the useful ones. This is because among our base expressions we have expressions like `(* (thefunc x) 1)` which are clearly equivalent to `(thefunc x)`, but they are not very interesting. The paper distinguishes these uninteresting expressions by identifying the fact they hold for any possible value of `(thefunc x)`. That is even if we did not know that `(thefunc x)` is equal to `(- (tan ?x) (sin ?x))` (for example), we would still be able to derive expressions like `(* (thefunc x) 1)` or `(+ (thefunc x) 0)`, since they do not depend on the value of `(thefunc x)`.

To remove these expressions, we implemented the following method from the paper referred to as de-duplication: First we construct a new e-graph and for each base-expression we derived in the initial step we add a new e-class containing this expression. Now we have an e-graph with a separate e-class for each of our base expressions. Next, we run this e-graph with all the base rules from Herbie, except that we omit the rules where we indicate that `(thefunc x)` is equal to our initial target expression (e.g. `(- (tan ?x) (sin ?x))`). This way after the e-graph is saturated we are able to find the expressions which are equal to `(thefunc x)` and which do not depend on knowing the target expression. All of these expressions are the uninteresting we discussed in the previous paragraph, such as `(+ (thefunc x) 0)`. Having found these expressions we filter them out from our base expressions to get a list of our de-duplicated expressions.

The code for this step can be found in `src/main.rs` under the `deduplicate_expressions` function.

## Decomposing expressions

The paper addresses one final redundancy in the derived range reduction expression. That is expressions which can be derived from the composition of two other expressions. For example consider the expressions `(thefunc (+ x (* 2 PI)))`
and `(thefunc (+ x (* 4 PI)))` where `(thefunc x)` is equal to `(- (tan ?x) (sin ?x))`. In this case the second expression `(thefunc (+ x (* 4 PI)))` can derive if we compose `(thefunc (+ x (* 2 PI)))` with itself as in
`(thefunc (+ (+ x (* 2 PI)) (* 2 PI))) <=> (thefunc (+ x (* 4 PI)))`. This means that if we know `(thefunc (+ x (* 2 PI)))` then the expression `(thefunc (+ x (* 4 PI)))` is redundant.

The steps in which we remove these redundant expressions is the following: First we add all the expressions we want to decompose as separate non-equivalent e-classes in a new e-graph, just like we did in the de-duplication step. Second, for every pair of expressions `e1` and `e2` we compute `e1` composed with `e2`. We also add all of these composed expressions as separate e-classes to the e-graph. Third, we run the e-graph with all the base Herbie rules, excluding the rules where we indicate that `(thefunc x)` is equal to our initial target expression (just like in de-duplication). After that we can check if a given expression is equivalent to the composition of two other expressions.

What remains is to find a minimal set of expressions from which we can derive the other expressions using compositions. As mentioned in Section 4 of the paper this is a form of a set-covering problem which can to be solved using integer linear programming. For each expression `i` we add 3 constraint variables `I[i]`, `CI[i]` and `AGE[i]`. The `I` varible indicates that the given expression is included in the final set of expressions. The `CI` varible is used to indicate if a given expression is derived either because it is in the final set or if it can be derived from a composition of 2 other variables. That is we give `CI[i]` the following constraint:

```
CI[i] = (I[i] /\ AGE[i] == 1) \/
        (for every pair of expressions c1,c2 that composes to i
            => (CI[c1] /\ CI[c2] /\ AGE[i] = AGE[c1] + AGE[c2])
        )
```

(The `/\` is a conjunction while `\/`is a disjunction).

Finally, the `AGE` variable is used to prevent cyclic reasoning where we can just `CI[i]` to true, if we have the case that `CI[i]` can be derived from a composition of `i` and some other expression. With these constraints we can derive a minimal cover of expression which cannot be derived from composition of any other expressions, to get our final set of candidate range-reduction identity.

For more details on how this step works (it is by far the most complex of them all), please look at the `decompose_expression` in `src/main.rs` and the last paragraphs of Section 4 of the paper. To solve the Integer linear programming program we used `russcip` which is a fast open-source ILP solver with rust bindings.

# Results and testing

The original paper took 60 expressions from the FPBench project. However, it was not clear which test cases were used specifically, nor was it clear which expression were derived. In my recreation I found 25 interesting benchmarks which can be found under the `benchmarks.txt` file. You can use these benchmarks to see which expression have been derived.

For example for the expression `(- (tan ?x) (sin ?x))` we find 55 base expressions, 20 deduplicated expressions
and 14 final decomposed expressions.

# Further work

One limitation that I noticed when recreating this project is that sometimes the e-graph would not detect properly that an expression is equivalent to a composition of two other expression, thus limiting the number of expressions that we can filter out. For example, we have `(thefunc (+ (+ (+ x PI) (/ PI 2)) (/ PI 2)))` which can be derived from the composition of `(thefunc (+ (+ x PI) PI))` and `(thefunc x)`. However, the current e-graph rules cannot detect this equality. This could be because the current e-graph analysis lacks a constant folding step which will allow us to fold `(+ (+ (+ x PI) (/ PI 2)) (/ PI 2))` into `(+ x (* 2 PI))`.

Another limitation that was also discussed in the paper was that none of the current filtering steps can filter out expression which are values of `(thefunc x)` but expressed in a complicated way. For example if `(thefunc x)` is equal to `(+ 1 (cos x))`, the rule `(- (- 1 (thefunc x)) (- (neg (thefunc x)) (cos x)))` can be simplified to `(+ 1 (cos x))` which is just a redundant way of specifying `(thefunc x)`. Such rules can be filtered out by having another e-graph analysis without giving the value of `thefunc` and then filtering out expressions which are equivalent to our target value, however there was not enough time to implement this pass.
