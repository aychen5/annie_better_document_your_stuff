rdplot_custom_fxn <- function (y, x, c = 0, p = 4, nbins = NULL, binselect = "esmv", 
          scale = NULL, kernel = "uni", weights = NULL, h = NULL, covs = NULL, 
          covs_eval = 0, covs_drop = TRUE, support = NULL, subset = NULL, 
          hide = FALSE, ci = NULL, shade = FALSE, title = NULL, x.label = NULL, 
          y.label = NULL, x.lim = NULL, y.lim = NULL, col.dots = NULL, 
          col.lines = NULL) 
{
  if (!is.null(subset)) {
    x <- x[subset]
    y <- y[subset]
  }
  na.ok <- complete.cases(x) & complete.cases(y)
  if (!is.null(covs)) {
    covs = as.matrix(covs)
    dZ = ncol(covs)
    if (!is.null(subset)) 
      covs <- subset(covs, subset)
    for (i in 1:dZ) {
      na.ok <- na.ok & complete.cases(covs[, i])
    }
  }
  x <- x[na.ok]
  y <- y[na.ok]
  if (!is.null(covs)) 
    covs <- subset(covs, na.ok)
  x_min = min(x)
  x_max = max(x)
  x_l = x[x < c]
  x_r = x[x >= c]
  y_l = y[x < c]
  y_r = y[x >= c]
  if (!is.null(support)) {
    support_l = support[1]
    support_r = support[2]
    if (support_l < x_min) 
      x_min = support_l
    if (support_r > x_max) 
      x_max = support_r
  }
  range_l = c - x_min
  range_r = x_max - c
  n_l = length(x_l)
  n_r = length(x_r)
  n = n_l + n_r
  meth = "es"
  if (is.null(scale)) {
    scale = scale_l = scale_r = 1
  }
  else {
    if (length(scale) == 1) 
      scale_l = scale_r = scale
    if (length(scale) == 2) {
      scale_l = scale[1]
      scale_r = scale[2]
    }
  }
  if (!is.null(nbins)) {
    if (length(nbins) == 1) 
      nbins_l = nbins_r = nbins
    if (length(nbins) == 2) {
      nbins_l = nbins[1]
      nbins_r = nbins[2]
    }
  }
  if (is.null(h)) {
    h_l = range_l
    h_r = range_r
  }
  else {
    if (length(h) == 1) 
      h_l = h_r = h
    if (length(h) == 2) {
      h_l = h[1]
      h_r = h[2]
    }
  }
  k = 4
  flag_no_ci <- FALSE
  if (is.null(ci)) {
    ci <- 95
    flag_no_ci <- TRUE
  }
  kernel_type = "Uniform"
  if (kernel == "epanechnikov" | kernel == "epa") 
    kernel_type = "Epanechnikov"
  if (kernel == "triangular" | kernel == "tri") 
    kernel_type = "Triangular"
  covs_drop_coll = NULL
  if (!is.null(covs)) {
    covs.check = covs_drop_fun(covs)
    if (covs.check$ncovs < dZ & covs_drop == FALSE) {
      print("Multicollinearity issue detected in covs. Please rescale and/or remove redundant covariates, or use covs_drop option.")
    }
    if (covs.check$ncovs < dZ & isTRUE(covs_drop)) {
      covs <- covs.check$covs
      dZ <- covs.check$ncovs
      covs_drop_coll <- 1
      print("Multicollinearity issue detected in covs. Redundant covariates dropped.")
    }
  }
  exit = 0
  if (c <= x_min | c >= x_max) {
    print("c should be set within the range of x")
    exit = 1
  }
  if (kernel != "uni" & kernel != "uniform" & kernel != "tri" & 
      kernel != "triangular" & kernel != "epa" & kernel != 
      "epanechnikov" & kernel != "") {
    print("kernel incorrectly specified")
    exit = 1
  }
  if (p < 0) {
    print("p should be a positive number")
    exit = 1
  }
  if (scale <= 0 | scale_l <= 0 | scale_r <= 0) {
    print("scale should be a positive number")
    exit = 1
  }
  p_ceiling = ceiling(p)/p
  if (p_ceiling != 1 & p > 0) {
    print("p should be an integer number")
    exit = 1
  }
  if (exit > 0) 
    stop()
  R_p_l = matrix(NA, n_l, p + 1)
  R_p_r = matrix(NA, n_r, p + 1)
  for (j in 1:(p + 1)) {
    R_p_l[, j] = (x_l - c)^(j - 1)
    R_p_r[, j] = (x_r - c)^(j - 1)
  }
  W_h_l = rdrobust_kweight(x_l, c, h_l, kernel)
  W_h_r = rdrobust_kweight(x_r, c, h_r, kernel)
  n_h_l = sum(W_h_l > 0)
  n_h_r = sum(W_h_r > 0)
  if (!is.null(weights)) {
    fw_l = weights[x < c]
    fw_r = weights[x >= c]
    W_h_l = fw_l * W_h_l
    W_h_r = fw_r * W_h_r
  }
  invG_p_l = qrXXinv((sqrt(W_h_l) * R_p_l))
  invG_p_r = qrXXinv((sqrt(W_h_r) * R_p_r))
  if (is.null(covs)) {
    gamma_p1_l = qrXXinv((sqrt(W_h_l) * R_p_l)) %*% crossprod(R_p_l * 
                                                                W_h_l, y_l)
    gamma_p1_r = qrXXinv((sqrt(W_h_r) * R_p_r)) %*% crossprod(R_p_r * 
                                                                W_h_r, y_r)
  }
  else {
    z_l = covs[x < c, ]
    z_r = covs[x >= c, ]
    D_l = cbind(y_l, z_l)
    D_r = cbind(y_r, z_r)
    U_p_l = crossprod(R_p_l * W_h_l, D_l)
    U_p_r = crossprod(R_p_r * W_h_r, D_r)
    beta_p_l = invG_p_l %*% crossprod(R_p_l * W_h_l, D_l)
    beta_p_r = invG_p_r %*% crossprod(R_p_r * W_h_r, D_r)
    ZWD_p_l = crossprod(z_l * W_h_l, D_l)
    ZWD_p_r = crossprod(z_r * W_h_r, D_r)
    colsZ = 2:max(c(2 + dZ - 1, 2))
    UiGU_p_l = crossprod(U_p_l[, colsZ], invG_p_l %*% U_p_l)
    UiGU_p_r = crossprod(U_p_r[, colsZ], invG_p_r %*% U_p_r)
    ZWZ_p_l = ZWD_p_l[, colsZ] - UiGU_p_l[, colsZ]
    ZWZ_p_r = ZWD_p_r[, colsZ] - UiGU_p_r[, colsZ]
    ZWY_p_l = ZWD_p_l[, 1] - UiGU_p_l[, 1]
    ZWY_p_r = ZWD_p_r[, 1] - UiGU_p_r[, 1]
    ZWZ_p = ZWZ_p_r + ZWZ_p_l
    ZWY_p = ZWY_p_r + ZWY_p_l
    gamma_p = chol2inv(chol(ZWZ_p)) %*% ZWY_p
    s_Y = c(1, -gamma_p[, 1])
    gamma_p1_l = t(s_Y %*% t(beta_p_l))
    gamma_p1_r = t(s_Y %*% t(beta_p_r))
  }
  nplot = 500
  x_plot_l = seq(c - h_l, c, length.out = nplot)
  x_plot_r = seq(c, c + h_r, length.out = nplot)
  rplot_l = matrix(NA, nplot, p + 1)
  rplot_r = matrix(NA, nplot, p + 1)
  for (j in 1:(p + 1)) {
    rplot_l[, j] = (x_plot_l - c)^(j - 1)
    rplot_r[, j] = (x_plot_r - c)^(j - 1)
  }
  y_hat_l = rplot_l %*% gamma_p1_l
  y_hat_r = rplot_r %*% gamma_p1_r
  if (covs_eval == "mean") {
    gammaZ = colMeans(covs) %*% gamma_p
    y_hat_l = rplot_l %*% gamma_p1_l + c(gammaZ)
    y_hat_r = rplot_r %*% gamma_p1_r + c(gammaZ)
  }
  rk_l = matrix(NA, n_l, (k + 1))
  rk_r = matrix(NA, n_r, (k + 1))
  for (j in 1:(k + 1)) {
    rk_l[, j] = x_l^(j - 1)
    rk_r[, j] = x_r^(j - 1)
  }
  gamma_k1_l = qrXXinv(rk_l) %*% crossprod(rk_l, y_l)
  gamma_k2_l = qrXXinv(rk_l) %*% crossprod(rk_l, y_l^2)
  gamma_k1_r = qrXXinv(rk_r) %*% crossprod(rk_r, y_r)
  gamma_k2_r = qrXXinv(rk_r) %*% crossprod(rk_r, y_r^2)
  mu0_k1_l = rk_l %*% gamma_k1_l
  mu0_k1_r = rk_r %*% gamma_k1_r
  mu0_k2_l = rk_l %*% gamma_k2_l
  mu0_k2_r = rk_r %*% gamma_k2_r
  drk_l = matrix(NA, n_l, k)
  drk_r = matrix(NA, n_r, k)
  for (j in 1:k) {
    drk_l[, j] = j * x_l^(j - 1)
    drk_r[, j] = j * x_r^(j - 1)
  }
  ind_l = order(x_l)
  ind_r = order(x_r)
  x_i_l = x_l[ind_l]
  y_i_l = y_l[ind_l]
  x_i_r = x_r[ind_r]
  y_i_r = y_r[ind_r]
  dxi_l = (x_i_l[2:length(x_i_l)] - x_i_l[1:(length(x_i_l) - 
                                               1)])
  dxi_r = (x_i_r[2:length(x_i_r)] - x_i_r[1:(length(x_i_r) - 
                                               1)])
  dyi_l = (y_i_l[2:length(y_i_l)] - y_i_l[1:(length(y_i_l) - 
                                               1)])
  dyi_r = (y_i_r[2:length(y_i_r)] - y_i_r[1:(length(y_i_r) - 
                                               1)])
  x_bar_i_l = (x_i_l[2:length(x_i_l)] + x_i_l[1:(length(x_i_l) - 
                                                   1)])/2
  x_bar_i_r = (x_i_r[2:length(x_i_r)] + x_i_r[1:(length(x_i_r) - 
                                                   1)])/2
  drk_i_l = matrix(NA, n_l - 1, k)
  rk_i_l = matrix(NA, n_l - 1, (k + 1))
  drk_i_r = matrix(NA, n_r - 1, k)
  rk_i_r = matrix(NA, n_r - 1, (k + 1))
  for (j in 1:(k + 1)) {
    rk_i_l[, j] = x_bar_i_l^(j - 1)
    rk_i_r[, j] = x_bar_i_r^(j - 1)
  }
  for (j in 1:k) {
    drk_i_l[, j] = j * x_bar_i_l^(j - 1)
    drk_i_r[, j] = j * x_bar_i_r^(j - 1)
  }
  mu1_i_hat_l = drk_i_l %*% (gamma_k1_l[2:(k + 1)])
  mu1_i_hat_r = drk_i_r %*% (gamma_k1_r[2:(k + 1)])
  mu0_i_hat_l = rk_i_l %*% gamma_k1_l
  mu0_i_hat_r = rk_i_r %*% gamma_k1_r
  mu2_i_hat_l = rk_i_l %*% gamma_k2_l
  mu2_i_hat_r = rk_i_r %*% gamma_k2_r
  mu0_hat_l = rk_l %*% gamma_k1_l
  mu0_hat_r = rk_r %*% gamma_k1_r
  mu2_hat_l = rk_l %*% gamma_k2_l
  mu2_hat_r = rk_r %*% gamma_k2_r
  mu1_hat_l = drk_l %*% (gamma_k1_l[2:(k + 1)])
  mu1_hat_r = drk_r %*% (gamma_k1_r[2:(k + 1)])
  mu1_i_hat_l = drk_i_l %*% (gamma_k1_l[2:(k + 1)])
  mu1_i_hat_r = drk_i_r %*% (gamma_k1_r[2:(k + 1)])
  sigma2_hat_l_bar = mu2_i_hat_l - mu0_i_hat_l^2
  sigma2_hat_r_bar = mu2_i_hat_r - mu0_i_hat_r^2
  sigma2_hat_l = mu2_hat_l - mu0_hat_l^2
  sigma2_hat_r = mu2_hat_r - mu0_hat_r^2
  J.fun = function(B, V) {
    ceiling((((2 * B)/V) * n)^(1/3))
  }
  var_y_l = var(y_l)
  var_y_r = var(y_r)
  B_es_hat_dw = c(((c - x_min)^2/(12 * n)) * sum(mu1_hat_l^2), 
                  ((x_max - c)^2/(12 * n)) * sum(mu1_hat_r^2))
  V_es_hat_dw = c((0.5/(c - x_min)) * sum(dxi_l * dyi_l^2), 
                  (0.5/(x_max - c)) * sum(dxi_r * dyi_r^2))
  V_es_chk_dw = c((1/(c - x_min)) * sum(dxi_l * sigma2_hat_l_bar), 
                  (1/(x_max - c)) * sum(dxi_r * sigma2_hat_r_bar))
  J_es_hat_dw = J.fun(B_es_hat_dw, V_es_hat_dw)
  J_es_chk_dw = J.fun(B_es_hat_dw, V_es_chk_dw)
  B_qs_hat_dw = c((n_l^2/(24 * n)) * sum(dxi_l^2 * mu1_i_hat_l^2), 
                  (n_r^2/(24 * n)) * sum(dxi_r^2 * mu1_i_hat_r^2))
  V_qs_hat_dw = c((1/(2 * n_l)) * sum(dyi_l^2), (1/(2 * n_r)) * 
                    sum(dyi_r^2))
  V_qs_chk_dw = c((1/n_l) * sum(sigma2_hat_l), (1/n_r) * sum(sigma2_hat_r))
  J_qs_hat_dw = J.fun(B_qs_hat_dw, V_qs_hat_dw)
  J_qs_chk_dw = J.fun(B_qs_hat_dw, V_qs_chk_dw)
  J_es_hat_mv = c(ceiling((var_y_l/V_es_hat_dw[1]) * (n/log(n)^2)), 
                  ceiling((var_y_r/V_es_hat_dw[2]) * (n/log(n)^2)))
  J_es_chk_mv = c(ceiling((var_y_l/V_es_chk_dw[1]) * (n/log(n)^2)), 
                  ceiling((var_y_r/V_es_chk_dw[2]) * (n/log(n)^2)))
  J_qs_hat_mv = c(ceiling((var_y_l/V_qs_hat_dw[1]) * (n/log(n)^2)), 
                  ceiling((var_y_r/V_qs_hat_dw[2]) * (n/log(n)^2)))
  J_qs_chk_mv = c(ceiling((var_y_l/V_qs_chk_dw[1]) * (n/log(n)^2)), 
                  ceiling((var_y_r/V_qs_chk_dw[2]) * (n/log(n)^2)))
  if (binselect == "es") {
    J_star_orig = J_es_hat_dw
    meth = "es"
    binselect_type = "IMSE-optimal evenly-spaced method using spacings estimators"
    J_IMSE = J_es_hat_dw
    J_MV = J_es_hat_mv
  }
  if (binselect == "espr") {
    J_star_orig = J_es_chk_dw
    meth = "es"
    binselect_type = "IMSE-optimal evenly-spaced method using polynomial regression"
    J_IMSE = J_es_chk_dw
    J_MV = J_es_chk_mv
  }
  if (binselect == "esmv") {
    J_star_orig = J_es_hat_mv
    meth = "es"
    binselect_type = "mimicking variance evenly-spaced method using spacings estimators"
    J_IMSE = J_es_hat_dw
    J_MV = J_es_hat_mv
  }
  if (binselect == "esmvpr") {
    J_star_orig = J_es_chk_mv
    meth = "es"
    binselect_type = "mimicking variance evenly-spaced method using polynomial regression"
    J_IMSE = J_es_chk_dw
    J_MV = J_es_chk_mv
  }
  if (binselect == "qs") {
    J_star_orig = J_qs_hat_dw
    meth = "qs"
    binselect_type = "IMSE-optimal quantile-spaced method using spacings estimators"
    J_IMSE = J_qs_hat_dw
    J_MV = J_qs_hat_mv
  }
  if (binselect == "qspr") {
    J_star_orig = J_qs_chk_dw
    meth = "qs"
    binselect_type = "IMSE-optimal quantile-spaced method using polynomial regression"
    J_IMSE = J_qs_chk_dw
    J_MV = J_qs_chk_mv
  }
  if (binselect == "qsmv") {
    J_star_orig = J_qs_hat_mv
    meth = "qs"
    binselect_type = "mimicking variance quantile-spaced method using spacings estimators"
    J_IMSE = J_qs_hat_dw
    J_MV = J_qs_hat_mv
  }
  if (binselect == "qsmvpr") {
    J_star_orig = J_qs_chk_mv
    meth = "qs"
    binselect_type = "mimicking variance quantile-spaced method using polynomial regression"
    J_IMSE = J_qs_chk_dw
    J_MV = J_qs_chk_mv
  }
  J_star_l = scale_l * J_star_orig[1]
  J_star_r = scale_r * J_star_orig[2]
  if (!is.null(nbins)) {
    J_star_l = nbins_l
    J_star_r = nbins_r
    binselect_type = "manually evenly spaced"
  }
  if (var_y_l == 0) {
    J_star_l = J_star_l_orig = 1
    print("Warning: not enough variability in the outcome variable below the threshold")
  }
  if (var_y_r == 0) {
    J_star_r = J_star_r_orig = 1
    print("Warning: not enough variability in the outcome variable above the threshold")
  }
  rscale_l = J_star_l/J_IMSE[1]
  rscale_r = J_star_r/J_IMSE[2]
  bin_x_l = rep(0, length(x_l))
  bin_x_r = rep(0, length(x_r))
  jump_l = range_l/J_star_l
  jump_r = range_r/J_star_r
  if (meth == "es") {
    jumps_l = seq(x_min, c, jump_l)
    jumps_r = seq(c, x_max, jump_r)
  }
  else if (meth == "qs") {
    jumps_l = quantile(x_l, probs = seq(0, 1, 1/J_star_l))
    jumps_r = quantile(x_r, probs = seq(0, 1, 1/J_star_r))
  }
  for (k in 1:(J_star_l - 1)) bin_x_l[x_l >= jumps_l[k] & x_l < 
                                        jumps_l[k + 1]] = -J_star_l + k - 1
  bin_x_l[x_l >= jumps_l[(J_star_l)]] = -1
  for (k in 1:(J_star_r - 1)) bin_x_r[x_r >= jumps_r[k] & x_r < 
                                        jumps_r[k + 1]] = k
  bin_x_r[x_r >= jumps_r[(J_star_r)]] = J_star_r
  rdplot_mean_bin_l = rdplot_mean_x_l = rdplot_mean_y_l = rep(0, 
                                                              J_star_l)
  rdplot_mean_bin_r = rdplot_mean_x_r = rdplot_mean_y_r = rep(0, 
                                                              J_star_r)
  if (covs_eval == "mean") {
    covs_model_l = lm(y_l ~ z_l + factor(bin_x_l))
    covs_model_r = lm(y_r ~ z_r + factor(bin_x_r))
    yhatZ_l = predict(covs_model_l)
    yhatZ_r = predict(covs_model_r)
  }
  for (k in 1:(J_star_l)) {
    rdplot_mean_bin_l[k] = mean(c(jumps_l[k], jumps_l[k + 
                                                        1]))
    rdplot_mean_x_l[k] = mean(x_l[bin_x_l == -k])
    rdplot_mean_y_l[k] = mean(y_l[bin_x_l == -k])
    if (covs_eval == "mean") 
      rdplot_mean_y_l[k] = mean(yhatZ_l[bin_x_l == -k])
  }
  rdplot_mean_y_l = rev(rdplot_mean_y_l)
  rdplot_mean_x_l = rev(rdplot_mean_x_l)
  for (k in 1:(J_star_r)) {
    rdplot_mean_bin_r[k] = mean(c(jumps_r[k], jumps_r[k + 
                                                        1]))
    rdplot_mean_x_r[k] = mean(x_r[bin_x_r == k])
    rdplot_mean_y_r[k] = mean(y_r[bin_x_r == k])
    if (covs_eval == "mean") 
      rdplot_mean_y_r[k] = mean(yhatZ_r[bin_x_r == k])
  }
  rdplot_mean_bin_l[J_star_l] = mean(c(jumps_l[J_star_l], c))
  rdplot_mean_bin_r[J_star_r] = mean(c(jumps_r[J_star_r], x_max))
  bin_x = c(bin_x_l, bin_x_r)
  rdplot_mean_bin = c(rdplot_mean_bin_l, rdplot_mean_bin_r)
  rdplot_mean_x = c(rdplot_mean_x_l, rdplot_mean_x_r)
  rdplot_mean_y = c(rdplot_mean_y_l, rdplot_mean_y_r)
  rdplot_sd_y_l = rdplot_N_l = rdplot_sd_y_r = rdplot_N_r = 0
  for (j in 1:(J_star_l)) {
    rdplot_sd_y_l[j] = sd(y_l[bin_x_l == -j])
    rdplot_N_l[j] = length(y_l[bin_x_l == -j])
  }
  for (j in 1:(J_star_r)) {
    rdplot_sd_y_r[j] = sd(y_r[bin_x_r == j])
    rdplot_N_r[j] = length(y_r[bin_x_r == j])
  }
  rdplot_sd_y_l[is.na(rdplot_sd_y_l)] = 0
  rdplot_sd_y_r[is.na(rdplot_sd_y_r)] = 0
  rdplot_sd_y = c(rev(rdplot_sd_y_l), rdplot_sd_y_r)
  rdplot_N = c(rev(rdplot_N_l), rdplot_N_r)
  quant = -qt((1 - (ci/100))/2, pmax(rdplot_N - 1, 1))
  rdplot_se_y <- rdplot_sd_y/sqrt(rdplot_N)
  rdplot_cil_bin = rdplot_mean_y - quant * rdplot_se_y
  rdplot_cir_bin = rdplot_mean_y + quant * rdplot_se_y
  temp_plot = NULL
  if (hide == "FALSE") {
    if (is.null(col.lines)) 
      col.lines = "red"
    if (is.null(col.dots)) 
      col.dots = "darkblue"
    if (is.null(title)) 
      title = "RD Plot"
    if (is.null(x.label)) 
      x.label = "X axis"
    if (is.null(y.label)) 
      y.label = "Y axis"
    data_bins <- data.frame(rdplot_mean_bin, rdplot_mean_y, 
                            rdplot_cil_bin, rdplot_cir_bin)
    data_poly <- data.frame(x_plot_l, y_hat_l, x_plot_r, 
                            y_hat_r)
    temp_plot <- ggplot() + theme_bw() + geom_point(data = data_bins, 
                                                    aes(x = rdplot_mean_bin, y = rdplot_mean_y), col = col.dots, 
                                                    na.rm = TRUE) + geom_line(data = data_poly, aes(x = x_plot_l, 
                                                                                                    y = y_hat_l), col = col.lines, na.rm = TRUE) + geom_line(data = data_poly, 
                                                                                                                                                             aes(x = x_plot_r, y = y_hat_r), col = col.lines, 
                                                                                                                                                             na.rm = TRUE)
    if (flag_no_ci == FALSE) 
      temp_plot <- temp_plot + geom_errorbar(data = data_bins, 
                                             aes(x = rdplot_mean_bin, ymin = rdplot_cil_bin, 
                                                 ymax = rdplot_cir_bin), linetype = 1)
    if (shade == TRUE) {
      temp_plot <- temp_plot + geom_ribbon(data = data_bins, 
                                           aes(x = rdplot_mean_bin, ymin = rdplot_cil_bin, 
                                               ymax = rdplot_cir_bin))
    }
    temp_plot <- temp_plot + labs(x = x.label, y = y.label) + 
      ggtitle(title) + coord_cartesian(xlim = x.lim, ylim = y.lim) + 
      theme(legend.position = "None") + geom_vline(xintercept = c, 
                                                   size = 0.5)
    print(temp_plot)
  }
  cutoffs = c(jumps_l, jumps_r[2:length(jumps_r)])
  rdplot_min_bin = cutoffs[1:(length(cutoffs) - 1)]
  rdplot_max_bin = cutoffs[2:length(cutoffs)]
  bin_length = rdplot_max_bin - rdplot_min_bin
  bin_avg_l = mean(bin_length[1:J_star_l])
  bin_med_l = median(bin_length[1:J_star_l])
  bin_avg_r = mean(bin_length[(J_star_l + 1):length(bin_length)])
  bin_med_r = median(bin_length[(J_star_l + 1):length(bin_length)])
  vars_bins = data.frame(rdplot_mean_bin = rdplot_mean_bin, 
                         rdplot_mean_x = rdplot_mean_x, rdplot_mean_y = rdplot_mean_y, 
                         rdplot_min_bin = rdplot_min_bin, rdplot_max_bin = rdplot_max_bin, 
                         rdplot_se_y = rdplot_se_y, rdplot_N = rdplot_N, rdplot_ci_l = rdplot_cil_bin, 
                         rdplot_ci_r = rdplot_cir_bin)
  vars_poly = data.frame(rdplot_x = c(x_plot_l, x_plot_r), 
                         rdplot_y = c(y_hat_l, y_hat_r))
  coef = cbind(gamma_p1_l, gamma_p1_r)
  colnames(coef) = c("Left", "Right")
  out = list(coef = coef, rdplot = temp_plot, vars_bins = vars_bins, 
             vars_poly = vars_poly, J = c(J_star_l, J_star_r), J_IMSE = J_IMSE, 
             J_MV = J_MV, scale = c(scale_l, scale_r), rscale = c(rscale_l, 
                                                                  rscale_r), bin_avg = c(bin_avg_l, bin_avg_r), bin_med = c(bin_med_l, 
                                                                                                                            bin_med_r), p = p, c = c, h = c(h_l, h_r), N = c(n_l, 
                                                                                                                                                                             n_r), N_h = c(n_h_l, n_h_r), binselect = binselect_type, 
             kernel = kernel_type)
  out$call <- match.call()
  class(out) <- "rdplot"
  return(invisible(out))
}