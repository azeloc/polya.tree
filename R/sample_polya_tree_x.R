sample_polya_tree_x <- function(polya_tree, x, NN = 1){

  altura <- max(polya_tree$altura) + 1

  quebras <- cumsum(c(rep(2^(-(altura)), 2^altura)))-1/(2^(altura+1))

  possibilidades <- sapply(X = x, FUN = function(.x){
    as.numeric(intToBits(which.min(.x > quebras)-1))[(altura):1]
  }) |>
    apply(2, paste0, simplify = FALSE, collapse = "")  |>
    purrr::map(purrr::accumulate, paste0) |>
    unlist()

  referencia <- tibble::tibble(
    possibilidades,
    x
  ) |>
    dplyr::group_by(possibilidades) |>
    dplyr::summarise(
      x = mean(x),
      peso = dplyr::n()
    )

  localizacoes <- referencia$possibilidades |>
    purrr::map(
      function(.x){
        matches <- stringr::str_detect(.x, paste0("^", polya_tree$theta))

        matches[matches] <- as.character(stringr::str_split(.x, "", simplify = TRUE))

        as.numeric(matches)
      }
    )

  sorteio <- polya_tree |>
    dplyr::mutate(
      thetas_amostra = purrr::map2(n_esquerda, n_direita, ~rbeta(NN, .x, .y))
    )

  pontos <- tibble::tibble(
    posicao = referencia$possibilidades,
    x = referencia$x,
    peso = referencia$peso,
    caminho = localizacoes,
    altura = altura,
    id = seq_along(referencia$x)
  )

  suppressMessages(
    A <- dplyr::bind_cols(pontos$caminho, .name_repair = "unique")  |> t())

  suppressMessages(
    B <- sorteio$thetas_amostra |> dplyr::bind_cols(.name_repair = "unique") |> as.matrix() |> t()
  )
  lados <- length(which(!is.na(A[1,])))

  C <- list()
  for(ii in 1:nrow(A)){
    C[ii] <- list(
      apply(
        rbind(
          B[which(A[ii,] == 0), ],
          1-B[which(A[ii,] == 1), ]),
        2,
        prod)*(2^lados)
    )
  }

  aux <- pontos

  aux$estimativa <- C

  aux
}
