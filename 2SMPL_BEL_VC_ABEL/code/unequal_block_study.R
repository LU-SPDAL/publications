# Unequal-block simulation used for the block-ratio experiment.
# Run from the code directory after loading the main simulation engine.

source("simulation_driver.R")

build_blockratio_design <- function() {
  N <- 1728L
  pairs <- list(c(6L,24L), c(8L,18L), c(12L,12L), c(18L,8L), c(24L,6L))
  settings <- list(
    list(tag="sym_normal", p1=.6, p2=.6, i1="normal", i2="normal"),
    list(tag="asym_normal", p1=.2, p2=.8, i1="normal", i2="normal"),
    list(tag="asym_skew",   p1=.2, p2=.8, i1="normal", i2="chisq1")
  )

  z <- list(); k <- 0L
  for (ss in settings) for (mm in pairs) {
    k <- k + 1L
    z[[k]] <- new_scenario(
      id=sprintf("ratio_%s_M%d_%d", ss$tag, mm[1], mm[2]),
      group="blockratio_theory",
      N1=N, N2=N, M1=mm[1], M2=mm[2],
      p11=ss$p1, p21=ss$p2,
      innov1=ss$i1, innov2=ss$i2,
      theory_scope="unequal_blocks_main_theorem"
    )
  }
  out <- rbind_rows(z)
  out$block_ratio <- out$M1/out$M2
  out$geom_M <- sqrt(out$M1*out$M2)
  out$base_id <- out$scenario_id
  out$scenario_no <- seq_len(nrow(out))
  out
}

run_blockratio_experiment <- function(B=10000L, threads=max(1L, parallel::detectCores()-1L),
                                      overwrite=FALSE) {
  d <- build_blockratio_design()
  out_dir <- file.path(SIM_DIR, "..", "results", "unequal_blocks", "raw")
  run_design(d, out_dir=out_dir, B=B, threads=threads,
             overwrite=overwrite, allow_small_B=(B < 10000L))
  cov <- summarize_coverage(out_dir)
  diag <- summarize_diagnostics(out_dir)
  invisible(list(design=d, coverage=cov, diagnostics=diag, out_dir=out_dir))
}
