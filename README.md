# yycr: Some neat implementations to handle data in R.

This package, for now, provides two functions to implement some frequently (maybe) used execuses in `R`:

- `sum_na()`: summarise how many missing values are there in your data. Show the results in percentage value. It also support group by for this operation. This may be useful when, say, you want to know how many firms have non-missing patent application in each year.
    - This function also support tidy-select operation, which makes it easier to select columns you want to summarize.
- `dpp_pc1()`: extract the first principal component. Principal Component is a useful dimensionality reduction method, and it is applied to construct a comprehensive index out of several variables (especially for survey data). This function provide a one-line solution to extract the first PC from several columns in data, and store it in a new column. This also support `tidy-select` operation, facilitating you compared to typing each variable's name one by one.

