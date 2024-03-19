fit_polya_tree <- function(x, a_func = function(l){pmax(2^(2*l/6), 8)}, size = floor(log(length(x),2))){

  quebras <- cumsum(c(rep(2^(-(size)), 2^size)))

  matriz_digitos <- sapply(X = x, FUN = function(.x){
    as.numeric(intToBits(which.min(.x > quebras)-1))[(size):1]
  })

  grupos_encontrados <- apply(matriz_digitos, 1, identity, simplify = FALSE) |>
    purrr::accumulate(paste0) |>
    unlist()

  possibilidades <- sapply(X = quebras, FUN = function(.x){
    as.numeric(intToBits(which.min(.x > quebras)-1))[(size):1]
  }) |>
    apply(2, identity, simplify = FALSE) |>
    purrr::map(purrr::accumulate, paste0) |>
    unlist() |>
    unique()

  aux <- tibble::enframe(table(unlist(grupos_encontrados))) |>
    dplyr::rename(
      grupo = name
    ) |>
    dplyr::mutate(value = as.character(value)) |>
    tidyr::complete(grupo = possibilidades, fill = list(value = "0")) |>
    dplyr::mutate(
      value = as.numeric(value), theta = stringr::str_sub(grupo, 1, nchar(grupo)-1)
    ) |>
    dplyr::arrange(theta) |>
    dplyr::group_by(theta) |>
    dplyr::mutate(
      ordem = 1:dplyr::n()
    ) |>
    tidyr::pivot_wider(
      id_cols = c(theta), names_from = ordem, values_from = value,
      values_fill = 0
    ) |>
    dplyr::mutate(nivel = nchar(theta)) |>
    dplyr::arrange(nivel) |>
    dplyr::ungroup() |>
    purrr::set_names(c("theta", "n_esquerda", "n_direita", "altura")) |>
    dplyr::mutate(
      n_esquerda = n_esquerda + a_func(altura),
      n_direita = n_direita + a_func(altura)
    )

  return(aux)
}
