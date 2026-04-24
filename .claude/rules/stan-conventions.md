---
paths:
  - "**/*.stan"
---

# Stan Model Conventions (Workspace-wide)

- Use `cmdstanr` (not `rstan`) for all model compilation and sampling.
- Non-centred parameterisations (`_raw` suffix) for hierarchical parameters.
- Comment expected array dimensions in the `data` block.
- `generated quantities` block should produce posterior predictive draws (`y_rep_*` or sport-specific names like `goals1_pred`/`goals2_pred`) and/or `vector[N] log_lik` for `loo::loo` comparisons.
- Test model changes by compiling with `cmdstan_model()` before running full MCMC.
- Default MCMC settings: 4 chains, 1000 warmup, 1000 sampling iterations.
- Sports models use Student's t likelihood (basketball/handball) or bivariate Poisson (football).
