evaluate_eval_precise_l2 <- function(f_hat, l2_samples, f0 = dunif, ...){
  mean(l2_samples > mean((f_hat$estimativa_media-f0(f_hat$x, ...))^2))
}

evaluate_eval_precise_sup <- function(f_hat, sup_samples, f0 = dunif, ...){
  mean(sup_samples > max(abs(f_hat$estimativa_media-f0(f_hat$x, ...))))
}

evaluate_eval_precise_entropy <- function(f_hat, entropy_samples, f0 = dunif, ...){
  mean(entropy_samples > mean(f_hat$estimativa_media*f0(f_hat$x, log = TRUE, ...)))
}

evaluate_eval_kl_divergence <- function(f_hat, kl_samples, f0 = dunif, ...){
  mean(kl_samples > mean(f_hat$estimativa_media*(log(f_hat$estimativa_media)-f0(f_hat$x, log = TRUE, ...))))
}

