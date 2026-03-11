# Internal delta-method helper (nlcom equivalent)
.delta_method <- function(grad, vcov_mat) {
  sqrt(drop(t(grad) %*% vcov_mat %*% grad))
}