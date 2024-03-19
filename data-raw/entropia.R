devtools::load_all()

  N <- 1000

  alfa <- 1
  beta <- 3


  # c <- integrate(function(x){dbeta(x, alfa, beta, log = TRUE)^2*dbeta(x, alfa, beta)}, 0, 1)$value-
  #   integrate(function(x){dbeta(x, alfa, beta, log = TRUE)*dbeta(x, alfa, beta)}, 0, 1)$value^2
  #
  # entropy <- integrate(function(x){dbeta(x, alfa, beta, log = TRUE)*dbeta(x, alfa, beta)}, 0, 1)$value

  #amostra_0 <- rnorm(N, 1, 1)
  #amostra_0 <- rgamma(N, 2, 2)

  #estimacao_gamma <- EnvStats::egamma(amostra_0)

  #amostra <- pnorm(amostra_0, mean(amostra_0), sd(amostra_0))
  #amostra <- pgamma(amostra_0, estimacao_gamma$parameters[1], scale = estimacao_gamma$parameters[2])

  NN <- 100

  estatistica <- numeric(length = NN)
  outra <- numeric(length = NN)

  for(ii in 1:NN){

  #amostra <- (runif(N)+runif(N))/2
  amostra <- rbeta(N, alfa, beta)

  #DistributionTest::za.test(pbeta(amostra, alfa, beta), "unif", para = list(min = 0, max = 1))

  rtriangular <- function(N = 1){
    (runif(N)+runif(N))/2
  }

  a_func_adap <- function(l, delta = 0.01){
    round(l*2^(2*l*delta))
    #1
  }

  ln = 10

  fitted_tree <- fit_polya_tree(
    amostra,
    a_func = a_func_adap,
    size = ln)

  samples <- fitted_tree |>
    sample_polya_tree_v2(NN = 1000)

  f_chapeu <- evaluate_f_hat(samples)

  ce <- samples |>
    evaluate_cross_entropy_non_discrete() |>
    dplyr::pull(cross_entropy)

  f_0 <- mean(f_chapeu$estimativa_media*
                dbeta(f_chapeu$x, alfa, beta, log = TRUE))

  f_01 <- mean(f_chapeu$estimativa_media*log(
                (pbeta(f_chapeu$x+1/2^(ln+1), alfa, beta)-
                pbeta(f_chapeu$x-1/2^(ln+1), alfa, beta))
                ))

  eval <- mean(ce >= f_01)

  print(eval)

  print(ii)

  # entropia_chapeu <- mean(f_chapeu$estimativa_media*log(f_chapeu$estimativa_media))
  # sd_chapeu <- sqrt(mean(f_chapeu$estimativa_media*log(f_chapeu$estimativa_media)^2)-
  # entropia_chapeu^2)
  # variancia_ponte_browniana <- sqrt(mean(log(f_chapeu$estimativa_media)^2))
  #
  # print(ii)
  # outra[ii] <- (mean(dbeta(amostra, alfa, beta, log = TRUE))-entropy)
  estatistica[ii] <- eval
  }

hist(estatistica)

#   var(outra-estatistica)
#
# raiz_de_n <- sqrt(N)
#
# sqrt(var(raiz_de_n*outra)+var(raiz_de_n*estatistica)-
#   2*cov(raiz_de_n*outra,raiz_de_n*estatistica))

# entropia <- samples |>
#   dplyr::select(theta = posicao, altura, estimativa, x, peso) |>
#   tidyr::unnest(estimativa) |>
#   dplyr::group_by(x, peso, theta) |>
#   dplyr::mutate(amostra = 1:dplyr::n()
#   ) |>
#   #dplyr::group_by(amostra) |>
#   #dplyr::mutate(
#   #  x = (1:dplyr::n())*(2^(-max(altura)))-1/((2^(-max(altura)+1)))
#   #) |>
#   dplyr::ungroup() |>
#   dplyr::group_by(amostra) |>
#   dplyr::summarise(
#     entropia = sum(peso*log(estimativa))/N
#   ) |>
#   dplyr::ungroup() |>
#   dplyr::mutate(
#     entropia_padrao = (entropia-mean(entropia))/sd(entropia)
#   ) |>
#   ggplot2::ggplot(
#     ggplot2::aes(x = entropia_padrao)
#   ) +
#   ggplot2::geom_density() +
#   ggplot2::geom_line(ggplot2::aes(
#     x = entropia_padrao,
#     y = dnorm(
#       entropia,
#       0,
#       2*sqrt(c/N))), col = 'red')

hist(estatistica)
