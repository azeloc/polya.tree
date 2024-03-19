resample_gof <- function(
    sample_size = 100,
    distribution,
    f0,
    resamples = 100, ...){

  purrr::map_dbl(
    1:resamples,
    function(i){

      print(i)

      amostra <- distribution(sample_size, ...)

      gof_eval(amostra, f0_test = f0)

    }
  )
}
