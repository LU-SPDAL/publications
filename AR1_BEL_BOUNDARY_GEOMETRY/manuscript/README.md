# Manuscript sources

`main.tex` and `supplementary.tex` are the manuscript versions used when this repository was assembled.

The TeX files refer to figure PDFs by filename. Generate the figures with

```r
source("scripts/10_make_figures.R")
```

and copy the resulting PDFs from `figures/` next to the TeX files before compiling, or add `../figures/` to the LaTeX graphics path.
