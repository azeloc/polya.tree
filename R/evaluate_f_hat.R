evaluate_f_hat <- function(sampled_polya_tree){
  sampled_polya_tree |>
    dplyr::select(x, altura, estimativa) |>
    tidyr::unnest(estimativa) |>
    dplyr::group_by(x) |>
    dplyr::mutate(
      amostra = 1:dplyr::n()
    ) |>
    dplyr::group_by(x) |>
    dplyr::mutate(
      estimativa_media = mean(estimativa)
    ) |>
    dplyr::distinct(x, estimativa_media)
}

evaluate_f_hat_direct <- function(polya_tree){

  altura <- max(polya_tree$altura) + 1

  quebras <- cumsum(c(rep(2^(-(altura)), 2^altura)))-1/(2^(altura+1))

  possibilidades <- sapply(X = quebras, FUN = function(.x){
    expansao <- as.numeric(intToBits(which.min(.x > quebras)-1))[altura:1]
    expansao
  })

  tamanho <- ncol(possibilidades)

  posicoes <- matrix(nrow = altura, ncol = tamanho)

  posicoes[1,] <- 1

  for(jj in 2:altura){

    posicoes[jj,] <- (
      2^(jj-1) +
        apply(
          matrix(possibilidades[1:(jj-1),], nrow = jj-1, ncol = tamanho, byrow = FALSE),
          2,
          function(x){
            sum(x*(2^((jj-2):0)))
          })
    )
  }

  lista_posicoes <- as.list(as.data.frame(posicoes))

  sorteio <- polya_tree

  pontos <- tibble::tibble(
    #posicao = possibilidades,
    x = quebras,
    caminho = lista_posicoes,
    altura = altura,
    id = seq_along(quebras)
  )

  C <- numeric(length = nrow(pontos))

  for(ii in 1:nrow(pontos)){
      thetas <- sorteio$n_esquerda[posicoes[,ii]]/
        (sorteio$n_esquerda[posicoes[,ii]]+
         sorteio$n_direita[posicoes[,ii]])

      direita <- possibilidades[,ii]

      peso <- thetas^(1-direita)-(direita)*thetas

      C[ii] <- prod(peso*2)
  }

  aux <- pontos

  aux$estimativa <- C

  aux |>
    dplyr::distinct(x, estimativa_media = estimativa)
}
