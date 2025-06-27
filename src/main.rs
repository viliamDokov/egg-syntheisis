pub(crate) mod language;

use egg::*;
use language::*;
use russcip::{Variable, prelude::*};
use std::{collections::HashMap, time::Duration};

fn extract_base_expressions(base_pattern: Pattern<MathLanguage>) -> Vec<RecExpr<MathLanguage>> {
    // base pattern is a an egg "pattern" that is the expression for which we want to synthesize useful
    // range reduction identities.
    // for example `(- (tan ?x) (sin ?x))`

    // dynamically generate rules that allow us to rewriute (thefunc x) to our input base pattern
    // this way we can find useful identies of our given base
    let mut thefunc_rules = make_rules();
    let thefunc_pattern: Pattern<MathLanguage> = "(thefunc ?x)".parse().unwrap();
    let rule_to =
        Rewrite::new("to-thefunc", base_pattern.clone(), thefunc_pattern.clone()).unwrap();
    let rule_from = Rewrite::new(
        "from-thefunc",
        thefunc_pattern.clone(),
        base_pattern.clone(),
    )
    .unwrap();
    thefunc_rules.push(rule_to);
    thefunc_rules.push(rule_from);

    // Run an e-graph runner with our new derived rules +  "(thefunc x)" as our inital expression
    // Note that we have a small timeout since we want to limit the number of expressions we get
    // Most of the useful expressions we find are found instantly and the rest of the expressions are bogus
    // which only has a negative impact on our perforamnce
    println!("Finidng Equivalences!");
    let start = "(thefunc x)".parse().unwrap();
    let runner = Runner::default()
        .with_node_limit(1_000_000)
        // .with_iter_limit(7)
        .with_time_limit(Duration::new(2, 0))
        .with_explanations_enabled()
        .with_expr(&start)
        .run(&thefunc_rules);
    println!("{}", runner.report());

    // Extract the identies from our e-graph that are equal to `(thefunc x)`
    let egraph = &runner.egraph;
    let class = &egraph[runner.roots[0]];

    // Egg extractor for our custom cost function
    let extractor = Extractor::new(&egraph, CostFn);
    // This function is used to find the best identies for a given e-class
    let get_node = |id| extractor.find_best_node(id).clone();

    // function that filters out rules that do not contain (thefunc)
    fn contains_thefun(expr: &RecExpr<MathLanguage>) -> bool {
        expr.iter().any(|node| match node {
            MathLanguage::Thefunc(..) => true,
            _ => false,
        })
    }
    // For each e-node in the root eclass we find extract the best form
    // and then we filter it out if it does not contain thefunc in any wat
    let expressions: Vec<RecExpr<MathLanguage>> = class
        .iter()
        .map(|node| node.build_recexpr(get_node))
        .filter(contains_thefun)
        .collect();

    return expressions;
}

fn deduplicate_exressions(
    base_expressions: Vec<RecExpr<MathLanguage>>,
) -> Vec<RecExpr<MathLanguage>> {
    // Remove expressions that can derived from any possible value of (thefunc x)

    let mut duplicate_runner = Runner::default()
        .with_node_limit(1_000_000)
        .with_time_limit(Duration::new(10, 0));

    // For each expressions in our base expression we add it to a new e-class in the runner's e-graph
    for node in base_expressions.iter() {
        duplicate_runner = duplicate_runner.with_expr(&node);
    }

    println!("Deduplicating expressions...");
    duplicate_runner = duplicate_runner.run(&make_rules());
    println!("{}", duplicate_runner.report());

    // Get the e-graph after applying all of the rewrite rules
    let duplicate_egg = duplicate_runner.egraph;
    let start = "(thefunc x)".parse().unwrap();

    // Filter out the base expressions which are equivalent to (thefunc x) in the deduplicated egraph
    // these expressions are not needed since they are equal to (thefunc x) for every possible value of (thefunc x)
    let deduplicated_exressions: Vec<RecExpr<MathLanguage>> = base_expressions
        .iter()
        .filter(|expr| duplicate_egg.equivs(&expr, &start).len() == 0)
        .cloned()
        .collect();

    return deduplicated_exressions;
}

fn decompose_expressions(
    deduplicated_exressions: Vec<RecExpr<MathLanguage>>,
) -> Vec<RecExpr<MathLanguage>> {
    // This function removes all expressions that can be derived from compositions of other expressions
    // I.e it finds the smallest possible set of epxression that can be used to derive every other expression
    // in the input vector

    // Prepare the runner for finding which expression is equivalent to which composition
    let mut composition_runner = Runner::default()
        .with_node_limit(1_000_000)
        .with_time_limit(Duration::new(10, 0));

    let mut composition_egraph = EGraph::default().with_explanations_enabled();
    let mut compositions = vec![];

    for (idx1, node1) in deduplicated_exressions.iter().enumerate() {
        for (idx2, node2) in deduplicated_exressions.iter().enumerate() {
            // For every pair of expressions we compose them and we add it to the e-graph
            let comp = compose_expressions(node1, node2);
            let id = composition_egraph.add_expr(&comp);
            composition_egraph.set_analysis_data(id, vec![(idx1, idx2)]);
            compositions.push(((idx1, idx2), comp));
        }
        // We also add the base expression itself
        composition_egraph.add_expr(&node1);
    }
    composition_egraph.rebuild();

    println!("Decomposing expressions");
    // Run the e-graph runner to find which expressions are equivalent to which compositions
    composition_runner = composition_runner
        .with_explanations_enabled()
        .with_egraph(composition_egraph)
        .run(&make_rules());
    println!("{}", composition_runner.report());

    let N = deduplicated_exressions.len();

    // Prepare ILP constraints
    let mut problem: Model<russcip::ProblemCreated> = Model::default().minimize();
    let mut I = vec![];
    let mut CI = vec![];
    let mut AGE = vec![];
    let mut GOAL = vec![];

    let two = problem.add(var().obj(0.0).int(1..isize::MAX));
    problem.add(cons().eq(2.0).coef(&two, 1.0));

    composition_egraph = composition_runner.egraph;
    for (i, _node) in deduplicated_exressions.iter().enumerate() {
        // We need to express a constraint for each node that
        // CI[idx] == (I[idx] /\ AGE[idx] == 1) \/ ( CI[c1] /\ CI[c2] /\ AGE[idx] = AGE[c1] + AGE[c2])
        // where we need to add a ( CI[c1] /\ CI[c2] /\ AGE[idx] = AGE[c1] + AGE[c2]) term
        // for every possible composition pair (c1,c2) that is equivalent to our index

        // For this first iteration we just prepare the generic variables
        I.push(problem.add(var().bin().obj(1.0)));
        CI.push(problem.add(var().bin().obj(0.0)));
        AGE.push(problem.add(var().int(1..isize::MAX).obj(0.0)));

        // This is equivalent to the constraint that Age[i] == 1
        // since: Age[i] == 1 <=> (Age[i] >= 1 /\ Age[i] < 2)
        let age_equals_one = add_lt_var(&mut problem, &AGE[i], &two);
        // this is the (I[idx] /\ AGE[idx] == 1)
        let i_and_age = add_and_var(&mut problem, &I[i], &age_equals_one);
        GOAL.push(i_and_age);
    }

    // Now in this pass we add the constrants that allow us to derive expressions from compositions
    for (i, node) in deduplicated_exressions.iter().enumerate() {
        println!("Calculating constraint: {i}/{N}");

        let equivs2 = get_compositions_for_expr(&composition_egraph, node);

        for (c1, c2) in equivs2.into_iter() {
            println!("Adding equivalence ({c1}, {c2})");
            if *c1 != *c2 {
                // The constraint solver does not like cases where the ids are the same

                // The this variable AGE = AGE[c1] + AGE[c2]
                let age_sum = problem.add(var().int(0..isize::MAX).obj(0.0));
                // problem.add_constraint([(age_sum, 1.0), (AGE[*c1], -1.0), (AGE[*c2], -1.0)],ComparisonOp::Eq,-1.0,);
                problem.add(
                    cons()
                        .coef(&age_sum, 1.0)
                        .coef(&AGE[*c1], -1.0)
                        .coef(&AGE[*c2], -1.0)
                        .eq(-1.0),
                );

                // this is the constraint that AGE[i] < AGE[c1] + AGE[c2]
                let age_lt_sum = add_lt_var(&mut problem, &age_sum, &AGE[i]);
                // This is the constraint that (CI[c1] /\ CI[c2] /\ AGE[i] < AGE[c1] + AGE[c2])
                let and_1 = add_and_var(&mut problem, &CI[*c1], &age_lt_sum);
                let and_2 = add_and_var(&mut problem, &CI[*c2], &and_1);
                GOAL[i] = add_or_var(&mut problem, &GOAL[i], &and_2);
            } else {
                // The this variable AGE = AGE[c1] + AGE[c1] (since c1 == c2)
                let age_sum = problem.add(var().int(0..isize::MAX).obj(0.0));

                // problem.add_constraint([(age_sum, 1.0), (AGE[*c1], -2.0)], ComparisonOp::Eq, -1.0);
                problem.add(cons().coef(&age_sum, 1.0).coef(&AGE[*c1], -2.0).eq(-1.0));
                // this is the constraint that AGE[i] < AGE[c1] + AGE[c2]
                let age_lt_sum = add_lt_var(&mut problem, &age_sum, &AGE[i]);
                // This is the constraint that (CI[c1] /\ AGE[i] < AGE[c1] + AGE[c2])
                let and_c: Variable = add_and_var(&mut problem, &CI[*c1], &age_lt_sum);
                GOAL[i] = add_or_var(&mut problem, &GOAL[i], &and_c);
            }
        }

        // Actually make sure that the goals are there!
        // problem.add_constraint([(GOAL[i], 1.0)], ComparisonOp::Eq, 1.0);
        problem.add(cons().coef(&GOAL[i], 1.0).eq(1.0));
        // problem.add_constraint([(GOAL[i], 1.0), (CI[i], -1.0)], ComparisonOp::Eq, 0.0);
        problem.add(cons().coef(&GOAL[i], 1.0).coef(&CI[i], -1.0).eq(0.0));
    }

    // Actually solve the constraints and print the variables for debugging
    println!("Solving!");
    let solved_problem = problem.solve();
    let sol = solved_problem.best_sol().unwrap();
    for (i, _) in deduplicated_exressions.iter().enumerate() {
        println!(
            "Idx: {} I:{} CI{} AI;{}",
            i,
            sol.val(&I[i]),
            sol.val(&CI[i]),
            sol.val(&AGE[i]),
        )
    }

    // Filter out expressions which are not included in the final set covering
    let composed_exressions: Vec<RecExpr<MathLanguage>> = deduplicated_exressions
        .iter()
        .enumerate()
        .filter_map(|(id, expr)| {
            if sol.val(&I[id]) == 1.0 {
                Some(expr)
            } else {
                None
            }
        })
        .cloned()
        .collect();

    return composed_exressions;
}

// Utility function that is return a variable which is constrained to be equal
// to the conjunctin of two input variables a and b
// Taken from
// https://blog.adamfurmanek.pl/2015/08/22/ilp-part-1/
fn add_and_var(
    problem: &mut Model<russcip::ProblemCreated>,
    a: &Variable,
    b: &Variable,
) -> Variable {
    let and = problem.add(var().bin().obj(0.0));
    problem.add(cons().coef(&a, 1.0).coef(&b, 1.0).coef(&and, -2.0).ge(0.0));
    problem.add(cons().coef(&a, 1.0).coef(&b, 1.0).coef(&and, -2.0).le(1.0));

    return and;
}

// Utility function that is return a variable which is constrained to be equal
// to the disjuction of two input variables a and b
// Taken from
// https://blog.adamfurmanek.pl/2015/08/22/ilp-part-1/
fn add_or_var(
    problem: &mut Model<russcip::ProblemCreated>,
    a: &Variable,
    b: &Variable,
) -> Variable {
    let or = problem.add(var().bin().obj(0.0));
    problem.add(cons().coef(&a, 1.0).coef(&b, 1.0).coef(&or, -2.0).ge(-1.0));
    problem.add(cons().coef(&a, 1.0).coef(&b, 1.0).coef(&or, -2.0).le(0.0));

    return or;
}

// Utility function that is return a variable which is constrained to be equal
// to the result of the comparison of a < b
// Taken from
// https://blog.adamfurmanek.pl/2015/09/12/ilp-part-4/
fn add_lt_var(
    problem: &mut Model<russcip::ProblemCreated>,
    a: &Variable,
    b: &Variable,
) -> Variable {
    let lt = problem.add(var().bin().obj(0.0));
    let k = 10000000.0;
    // problem.add([(a, 1.0), (b, -1.0), (lt, k)], ComparisonOp::Ge, 0.0);
    problem.add(cons().coef(&a, 1.0).coef(&b, -1.0).coef(&lt, k).ge(0.0));
    // problem.add([(a, 1.0), (b, -1.0), (lt, k)], ComparisonOp::Le, k - 1.0);
    problem.add(cons().coef(&a, 1.0).coef(&b, -1.0).coef(&lt, k).le(k - 1.0));
    return lt;
}

fn extract_expressions(start_pattern: Pattern<MathLanguage>) -> Vec<RecExpr<MathLanguage>> {
    let base_expressions = extract_base_expressions(start_pattern);
    println!("Found {} base expressions", base_expressions.len());
    for res in base_expressions.iter() {
        println!("EXPR: {res}");
    }

    let deduplicated_exressions = deduplicate_exressions(base_expressions);
    println!(
        "Found {} deduplicated expressions",
        deduplicated_exressions.len()
    );

    let decomposed_expressions = decompose_expressions(deduplicated_exressions);
    println!(
        "Found {} decomposed expressions",
        decomposed_expressions.len()
    );

    return decomposed_expressions;
}

// Helper function that composes 2 expressions
// That is it replaces every occurence of the (thefunc ?x) with (pattern) and
// And substitues every occurence of x in pattern with the expression in ?x
// The function is quite complicated since we need to maintain the RecExpr invariant which is document in the egg libary
fn compose_expressions(
    src: &RecExpr<MathLanguage>,
    pattern: &RecExpr<MathLanguage>,
) -> RecExpr<MathLanguage> {
    // The resulting expression
    let mut res = RecExpr::default();
    let x = Symbol::from("x");
    // A map from old ids to new ids used for updating edges of the src
    let mut ids: HashMap<Id, Id> = HashMap::with_capacity(src.len());

    // We go for each node in the src
    for (outer_idx, node) in src.iter().enumerate() {
        match node {
            // If it is thefunc the we replace it with pattern
            MathLanguage::Thefunc(thefun_id) => {
                let offset = res.len();
                let mut x_idx = None;
                // So we add every node in pattern to result node
                // and make sure that every occurance of Symbol("x") points to the original expression in thefunc
                for (idx, pattern_node) in pattern.iter().enumerate() {
                    let new_node = match pattern_node {
                        // We get the index of Symbol("x") in pattern
                        MathLanguage::Symbol(sym) => {
                            if sym.clone() == x {
                                x_idx = Some(idx);
                            }
                            pattern_node.clone()
                        }
                        _ => pattern_node.clone().map_children(|id| match x_idx {
                            // If a node refers to "Symbol X" we instead point it to the original expression in thefunc
                            // aka thefun_id
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
                    res.add(new_node);
                }
                ids.insert(Id::from(outer_idx), Id::from(res.len() - 1));
            }
            // Not thefun so we just need to make sure that we point to the corrent child nodes
            _rest => {
                let node_moved = node.clone().map_children(|id| ids[&id]);
                res.add(node_moved);
                ids.insert(Id::from(outer_idx), Id::from(res.len() - 1));
            }
        }
    }
    return res;
}

fn run_exp(exp: &str) {
    let expr = exp.parse().unwrap();
    let results = extract_expressions(expr);
    for res in results {
        println!("EXPR: {res}");
    }
}

fn main() {
    // Run this on the examples given in the benchmarks.txt
    run_exp("(- (tan ?x) (sin ?x))");
    run_exp("(- (log (+ ?x 1)) (log ?x))");
    run_exp("(/ (- 1 (cos ?x)) (sin ?x))");
    run_exp("(/ (- 1 (cos ?x)) (sin ?x))");
}
