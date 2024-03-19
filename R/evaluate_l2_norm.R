evaluate_l2_norm <- function(sampled_polya_tree){
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
    dplyr::ungroup() |>
    dplyr::group_by(amostra) |>
    dplyr::summarise(
      norma_l2 = mean((estimativa-estimativa_media)^2)
    )
}

evaluate_sup_norm <- function(sampled_polya_tree){
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
      norma_sup = max((estimativa-estimativa_media))
    )
}

evaluate_cross_entropy <- function(sampled_polya_tree){
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
    dplyr::ungroup() |>
    dplyr::group_by(amostra) |>
    dplyr::summarise(
      cross_entropy = mean(estimativa_media*log(estimativa))
    )
}

evaluate_kl_divergence <- function(sampled_polya_tree){
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
    dplyr::ungroup() |>
    dplyr::group_by(amostra) |>
    dplyr::summarise(
      kl_divergence = mean(estimativa_media*log(estimativa_media/estimativa))
    )
}

