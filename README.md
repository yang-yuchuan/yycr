# yycr: Some neat implementations to handle data in R.

This package contains several functions that integrate a series of often-used operations in one line. They are broadly divided into three categories: data preprocessing (start with `dpp_`), summarization (start with `sum_` or `tab_`), and statistical analyses and tests (start with `test_`).

## Data PreProcessing

- `dpp_replace_infna`: Replacing infinite values with `NA`. `R` processes `Inf` and `-Inf` in different ways from `NA`, and in functions like `mean` or `median`, the `na.rm` parameter cannot correctly handle infinite values. This function provide a easy way to replace all infinite values with `NA`.
- `dpp_hhin`: Calculating Herfindahl-Hirschman Index (HHI). It acts like its `Stata` counterpart `hhi5`.
- `dpp_ipolate`: Data interpolation. It receives two vectors and returns a completed form of the first vector.
- `dpp_pc1`: Extracting first principal component. Besides taking arithmetic mean value or weighted average, another useful way to reduce dimensionality and construct an index out of several variables is principal component (PC). It is widely used in economic empirical researches (e.g., Guriev et al., QJE 2021). This function provides a one-line solution to extract the first PC and store it in a new column.
- `dpp_renamedt`: This function is set for `data.table`. It allows users to rename variables in it just in the same way as in `tibble`, i.e., use `new_name = old_name` norm.

## Summarization

- `sum_na`: Summarising how many missing values are there in your data. The function shows the proportion of missing values in each group. This may be useful when, say, you want to know how many firms have non-missing patent application in each year.
- `sum_basic`: Providing a one-line solution for presenting basic summary statistics like in `Stata`. By default it will show 5 statistics: observation, mean, standard deviation, min and max (note that standard deviation is not presented in `summary`).
- `tab_validnum`: `round()` and `signif()` in **baseR** will produce different results when the number crosses 1. This function handles this by adding one to the digits in `signif()` if `x` is greater than 1.

## Analyses

- `test_coefdiff_welcht`: Implementing Welch' $t$ test of coefficient difference between two groups. This can be used in subsample regressions.