devtools::load_all()

N <- 1000

alfa <- 3
beta <- 3

alfa_0 <- 3
beta_0 <- 3

c <- integrate(function(x){dbeta(x, alfa, beta, log = TRUE)^2*dbeta(x, alfa, beta)}, 0, 1)$value-
  integrate(function(x){dbeta(x, alfa, beta, log = TRUE)*dbeta(x, alfa, beta)}, 0, 1)$value^2

#amostra_0 <- rnorm(N, 1, 1)
#amostra_0 <- rgamma(N, 2, 2)

#estimacao_gamma <- EnvStats::egamma(amostra_0)

#amostra <- pnorm(amostra_0, mean(amostra_0), sd(amostra_0))
#amostra <- pgamma(amostra_0, estimacao_gamma$parameters[1], scale = estimacao_gamma$parameters[2])

#amostra <- (runif(N)+runif(N))/2
amostra <- rbeta(N, alfa, beta)

DistributionTest::za.test(pbeta(amostra, alfa, beta), "unif", para = list(min = 0, max = 1))

rtriangular <- function(N = 1){
  (runif(N)+runif(N))/2
}

a_func <- function(l){
  ceiling(pmax(l*2^(2*l), 1))
}

# a_func <- function(l){
#   l^2
# }

arvore <- fit_polya_tree(amostra, a_func = a_func, size = round(log(N, 2)))

samples <- arvore |>
  sample_polya_tree_v2(NN = 1000)

f_hat <- samples |>
  evaluate_f_hat()

f_hat |> plot(type = 'l', ylim = c(0,2))
lines(f_hat$x, dbeta(f_hat$x, alfa, beta), col = 'red')

l2_samples <- samples |>
  evaluate_l2_norm() |>
  with(norma_l2)

cross_entropy <- samples |>
  evaluate_cross_entropy() |>
  with(cross_entropy)

evaluate_eval_precise_l2(
  f_hat,
  l2_samples,
  f0 = dbeta, shape1 = alfa_0, shape2 = beta_0
  )

evaluate_eval_precise_l2(
  f_hat,
  l2_samples,
  f0 = dbeta, shape1 = alfa, shape2 = beta
)

evaluate_eval_precise_entropy(
  f_hat,
  cross_entropy,
  f0 = dbeta, shape1 = alfa_0, shape2 = beta_0
)

evaluate_eval_precise_entropy(
  f_hat,
  cross_entropy,
  f0 = dbeta, shape1 = alfa, shape2 = beta
)


cross_entropy |> hist()

f0 <- mean(
  f_hat$estimativa_media*
  dbeta(f_hat$x, alfa, beta, log = TRUE))


evaluate_eval_kl_divergence(
  f_hat,
  cross_entropy,
  f0 = dbeta, shape1 = alfa, shape2 = beta
)

# evaluate_eval_precise_l1(
#   f_hat,
#   l1_samples,
#   f0 = dbeta, shape1 = alfa, shape2 = beta
# )


# evaluate_eval_precise_sup(
#   f_hat,
#   sup_samples,
#   f0 = dbeta, shape1 = alfa, shape2 = beta
# )

evals <- resample_gof(
  resamples = 20,
  sample_size = 3000,
  distribution = rbeta, shape1 = alfa, shape2 = beta,
  f0 = function(x){dbeta(x, alfa, beta)}
)

media_norma_l2 <- numeric()
diferenca_f_chapeu <- numeric()

kk <- 1

X <- 30000

for(jj in X){

  print(jj)

  a_func <- function(l){
    ceiling(pmax(2^(2*l/4), 1))*100
  }

  amostra <- rbeta(jj, alfa, beta)

  arvore <- fit_polya_tree(amostra, a_func = a_func, size = round(log(N, 2)))

  samples <- arvore |>
    sample_polya_tree_v2(NN = 1000)

  f_hat <- samples |>
    evaluate_f_hat()

  l2_samples <- samples |>
    evaluate_l2_norm() |>
    with(norma_l2)

  media_norma_l2[kk] <- mean(l2_samples)
  diferenca_f_chapeu[kk] <- mean((f_hat$estimativa_media-dbeta(f_hat$x, alfa, beta))^2)

  kk <- kk+1
}

total <- c(diferenca_f_chapeu, media_norma_l2)

plot(X, media_norma_l2, type = 'l')
lines(X, 1/sqrt(X), type = 'l', col ='red')
lines(X, diferenca_f_chapeu, type = 'l', col ='blue')
lines(X, 1/X^(1/3), type = 'l', col ='red')

# hist(evals)
#
# mean(evals < 0.1)
