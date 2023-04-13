# jrt 1.1.2

- Hannan–Quinn (HQ) Criterion is now in outputs, and can now be used for automatic selection criterion.
- Vignette improvements for clarity.

# jrt 1.1.1

* Fixes issue in `jrt()` with slope-constrained models when ordinal scales start with 0 rather than 1.
* Added a new example dataset with missing data.
* Patched issues with displaying of amount of missing data.

# jrt 1.1.0

* Fixes for compatibility with recent versions of the `mirt` package.
* Fixes for compatibility with recent versions of the `ggplot2` package.
* Update package citation.
* Added documentation of S4 classes/methods.
* Added the possibility to override the automatic facet naming in `jcc.plot()` with argument `facet.names`.
* Added the possibility to change the opacity of the different category lines in `jcc.plot()` with `line.opacity`.
* Simplified messages for situations with judges/items with unobserved categories.
* Made Akaike Information Criterion (AIC) the default option in model comparisons.


# jrt 1.0.1

* Simulated dataset is now directly included as `jrt::ratings`.
* Model weights (often called Akaike weights) are now presented in the model selection output (per reviewer suggestion).
* Calls to `mirt` can now be showed with `show.calls = TRUE` argument in the `jrt()`function.
* References were updated.


# jrt 1.0.0

* Initial release with functions jrt(), jcc.plot() and info.plot()
