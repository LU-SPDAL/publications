# Results

The saved output used in the manuscript is kept separate from new Monte Carlo runs.

## Archived manuscript values

- `geometry/` contains the final rectangular `(x,Q)` Monte Carlo workbook.
- `vix/` contains the VIX working-model calculations used in the paper.
- `vix_reference_laws/` contains the finite-Q and local-boundary VIX reference-law calculations.
- `reported_tables/` contains compact copies of the numerical tables printed in the manuscript and Supplementary Material.

These files are not overwritten by the reproduction scripts. `scripts/11_reproduce_from_saved_results.R` uses them to rebuild the manuscript figures quickly.

## Recomputed output

Running the simulation scripts creates `recomputed/` with one subdirectory per study. This keeps a fresh run separate from the archived values used in the submitted manuscript. Monte Carlo seeds are fixed in the scripts, although small numerical differences can still occur across R, compiler, or package versions.

Three late follow-up checks were reconstructed because their original runner seeds were not present in the supplied development archives. Their exact manuscript values remain in `reported_tables/`. See `docs/provenance_notes.md`.
