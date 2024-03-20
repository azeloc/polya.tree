sample_polya_tree_v2 <- function(polya_tree, NN = 1){

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

  C <- as.list(numeric(length = nrow(pontos))+1)

  for(ii in 1:nrow(pontos)){
    for(jj in 1:altura){

      thetas <- sorteio$thetas_amostra[posicoes[,ii]][[jj]]
      direita <- possibilidades[jj,ii]

      peso <- thetas^(1-direita)-(direita)*thetas

      C[[ii]] <- C[[ii]]*peso*2

    }
  }

  aux <- pontos

  aux$estimativa <- C

  aux
}
