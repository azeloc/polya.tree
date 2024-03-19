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
    as.numeric(intToBits(which.min(.x > quebras)-1))[(altura):1]
  }) |>
    apply(2, paste0, simplify = FALSE, collapse = "")  |>
    purrr::map(purrr::accumulate, paste0) |>
    unlist()

  localizacoes <- possibilidades |>
    purrr::map(
      function(.x){
        print(.x)

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
    posicao = possibilidades,
    x = quebras,
    caminho = localizacoes,
    altura = altura,
    id = seq_along(quebras)
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

  sampled_polya_tree |>
    dplyr::select(theta = posicao, altura, estimativa) |>
    tidyr::unnest(estimativa) |>
    dplyr::group_by(theta) |>
    dplyr::mutate(
      amostra = 1:dplyr::n()
    ) |>
    dplyr::group_by(amostra) |>
    dplyr::mutate(
      x = (1:dplyr::n())*(2^(-max(altura)))-1/2^(max(altura)+1)
    ) |>
    dplyr::group_by(x) |>
    dplyr::mutate(
      estimativa_media = mean(estimativa)
    ) |>
    dplyr::distinct(x, estimativa_media)
}
