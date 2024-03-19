evaluate_eval_precise_l1 <- function(f_hat, l1_samples, f0 = dunif, ...){
  mean(l1_samples > mean(abs(f_hat$estimativa_media-f0(f_hat$x, ...))))
}
