evaluate_l1_norm <- function(sampled_polya_tree){
  sampled_polya_tree |>
    dplyr::select(theta = posicao, altura, estimativa) |>
    tidyr::unnest(estimativa) |>
    dplyr::group_by(theta) |>
    dplyr::mutate(
      amostra = 1:dplyr::n()
    ) |>
    dplyr::group_by(amostra) |>
    dplyr::mutate(
      x = (1:dplyr::n())*(2^(-max(altura)))-1/((2^(-max(altura)+1)))
    ) |>
    dplyr::group_by(x) |>
    dplyr::mutate(
      estimativa_media = mean(estimativa)
    ) |>
    dplyr::ungroup() |>
    dplyr::group_by(amostra) |>
    dplyr::summarise(
      norma_l1 = mean(abs(estimativa-estimativa_media))
    )
}
