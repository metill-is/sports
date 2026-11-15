# Football v2 prototype: the model variants the walk-forward compares. Each
# entry is a set of fv2_build() options; "base" is the specification validated
# against Stan.
fv2_variants <- function() {
  list(
    base = list(clock = "calendar"),
    brk = list(clock = "break"),
    brk_rev80 = list(clock = "break", rho = 0.8),
    brk_rev90 = list(clock = "break", rho = 0.9),
    brk_div = list(clock = "break", div_offsets = TRUE),
    brk_mov = list(clock = "break", movers = TRUE),
    brk_div_mov = list(clock = "break", div_offsets = TRUE, movers = TRUE),
    base_div = list(clock = "calendar", div_offsets = TRUE),
    sq_rw = list(clock = "break", param = "sq", style = "rw"),
    sq_static = list(clock = "break", param = "sq", style = "static"),
    sq_none = list(clock = "break", param = "sq", style = "none"),
    base_sq_rw = list(clock = "calendar", param = "sq", style = "rw"),
    base_sq_static = list(clock = "calendar", param = "sq", style = "static"),
    base_sq_none = list(clock = "calendar", param = "sq", style = "none"),
    brk_rev70 = list(clock = "break", rho = 0.7),
    brk_rev60 = list(clock = "break", rho = 0.6),
    rev80_mov = list(clock = "break", rho = 0.8, movers = TRUE),
    rev80_div = list(clock = "break", rho = 0.8, div_offsets = TRUE),
    rev80_sq_rw = list(clock = "break", rho = 0.8, param = "sq", style = "rw"),
    rev80_sq_static = list(clock = "break", rho = 0.8, param = "sq", style = "static"),
    rev80_sq_none = list(clock = "break", rho = 0.8, param = "sq", style = "none"),
    rev70_sq_static = list(clock = "break", rho = 0.7, param = "sq", style = "static"),
    rev70_sq_none = list(clock = "break", rho = 0.7, param = "sq", style = "none")
  )
}
