  // Shared state construction: club level b, season level u (AR(1) across seasons,
  // stationary with marginal sd sigma_u), and the within-season component w.
  real rho = tanh(0.5 * theta_rho);
  vector[K] b = sigma_b * b_z;
  matrix[K, NS] u;
  matrix[TS, W] w = rep_matrix(0, TS, W);
  u[, 1] = sigma_u * u_z[, 1];
  for (s in 2:NS)
    u[, s] = rho * u[, s - 1] + sqrt(1 - square(rho)) * sigma_u * u_z[, s];
  if (within_mode == 1) {
    for (r in 1:TS) {
      row_vector[W] path = append_col(0, cumulative_sum(sigma_w[1] * w_z[r]));
      w[r] = path - mean(path);
    }
  } else if (within_mode == 2) {
    w[, 1] = sigma_w[1] * w_z[, 1];
    for (t in 2:W)
      w[, t] = rho_week * w[, t - 1] + sqrt(1 - square(rho_week)) * sigma_w[1] * w_z[, t];
  }
