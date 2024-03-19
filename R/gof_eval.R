gof_eval <- function(
    X,
    a_func = function(l){pmax(l*2^(2*l/2), 1)},
    size = floor(log(length(X),2)),
    N_boot = 1000,
    f0_test = dunif){

  samples <- fit_polya_tree(x = X, a_func, size) |>
    sample_polya_tree_v2(NN = N_boot)

  f_hat <- samples |>
    evaluate_f_hat()

  l2_samples <- samples |>
    evaluate_l2_norm() |>
    with(norma_l2)

  eval <- evaluate_eval_precise_l2(f_hat, l2_samples, f0 = f0_test)

  print(eval)

  eval

}
