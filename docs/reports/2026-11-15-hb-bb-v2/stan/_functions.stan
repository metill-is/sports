  // Pointwise log density of a bivariate Student-t (nu > 0) or Gaussian (nu <= 0) with
  // residuals r1, r2, scales s1, s2 and a common correlation rho.
  vector biv_lpdf_pointwise(vector r1, vector r2, vector s1, vector s2, real rho, real nu) {
    int n = rows(r1);
    vector[n] z1 = r1 ./ s1;
    vector[n] z2 = r2 ./ s2;
    vector[n] q = (square(z1) - 2 * rho * z1 .* z2 + square(z2)) / (1 - square(rho));
    vector[n] base = -log(s1) - log(s2) - 0.5 * log1m(square(rho));
    if (nu > 0) {
      return base + lgamma(0.5 * nu + 1) - lgamma(0.5 * nu) - log(nu) - log(pi())
             - (0.5 * nu + 1) * log1p(q / nu);
    }
    return base - log(2 * pi()) - 0.5 * q;
  }
