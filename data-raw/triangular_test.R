devtools::load_all()

N <- 1000

amostra <- (runif(N)+runif(N))/2

samples <- fit_polya_tree(amostra) |>
  sample_polya_tree(NN = 1000)

f_hat <- samples |>
  evaluate_f_hat()

l2_samples <- samples |>
  evaluate_l2_norm() |>
  with(norma_l2)

evaluate_eval_precise(
  f_hat,
  l2_samples,
  f0 = dbeta, shape1 = 2, shape2 = 2)

evals <- resample_gof(
  resamples = 10,
  sample_size = 1000,
  distribution = rbeta,
  shape1 = 2, shape2 = 2,
  f0 = function(x){dbeta(x, 2, 2)}
  )

hist(evals)
