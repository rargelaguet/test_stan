data_summary <- function(data){
  mean <- colMeans(data)
  sd <- apply(data, 2, sd)
  return(data.table("pred_mean" = mean, "pred_sd" = sd))
}

posterior_errorbars <- function(dt) {
  p <- ggplot(dt, aes(x = true_mean, y = pred_mean)) +
    #geom_line() +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
    geom_errorbar(aes(ymin = pred_mean - pred_sd, ymax = pred_mean + pred_sd),
                  width = .005, position = position_dodge(0.05))
  return(p)
}