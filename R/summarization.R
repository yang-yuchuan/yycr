#' Tell you how many NAs are in your data
#'
#' `sum_na()` explicitly shows the proportion of NA for each variable you select
#' in your data (in %). You can also summarize this information by group variables.
#' If the data is wide (having a large number of columns), I strongly recommend
#' to use a `glimpse()` after using `sum_na()`.
#'
#' @param data A tibble (recommended), or a data frame.
#' @param ... <\link[dplyr]{dplyr_tidy_select}>. Columns to summarize.
#' @param by (Optional) Columns to group by for this summarize operation.
#'
#' @return A data frame with columns in `...`. Each row corresponds to a group in
#' `by` variables. The value is the number of NAs in each group divided by total
#' observations in that group.
#'
#' @examples
#' data <- tibble(x1 = c(1, 2, 2), x2 = c(1, NA, 23), y = c("a", NA, NA))
#' sum_na(data)    # All variables
#' sum_na(data, x1, x2, y)
#' sum_na(data, matches("x"))
#' sum_na(data, -y)
#' sum_na(data, x2, y, by = x1)
#' @export
sum_na = function(data, ..., by = NULL) {
  library(dplyr)
  library(rlang)

  byvar <- enquo(by)
  vars <- enquos(...)

  if (length(vars) == 0) {
    reframe(data, across(everything(), \(x) sum(is.na(x)) / n() * 100) %>% signif(2), .by = !!byvar)
  } else {
    reframe(data, across(c(!!!vars), \(x) sum(is.na(x)) / n() * 100) %>% signif(2), .by = !!byvar)
  }
}


sum_basic = function(data, vars, stats = c("obs", "mean", "sd", "min", "max"),
                     valid = 4, digits = NULL) {
  library(tidyverse)

  data %>%
    summarise(across(
      vars,
      list(
        obs = ~ sum(!is.na(.x)), mean = ~ mean(.x, na.rm = T),
        sd = ~ sd(.x, na.rm = T), min = ~ min(.x, na.rm = T),
        med = ~ median(.x, na.rm = T), max = ~ max(.x, na.rm = T),
        p10 = ~ quantile(.x, 0.1, na.rm = T), p25 = ~ quantile(.x, 0.25, na.rm = T),
        p75 = ~ quantile(.x, 0.75, na.rm = T), p90 = ~ quantile(.x, 0.9, na.rm = T)
      )
    )) %>%
    pivot_longer(
      cols = everything(),
      names_to = c("var", ".values"),
      names_pattern = "(.*)_(obs|mean|sd|min|p10|p25|med|p75|p90|max)"
    ) %>%
    pivot_wider(id_cols = var, names_from = .values, values_from = value) %>%
    mutate(across(-var, ~ tab_validnum(.x, valid = valid, digits = digits))) %>%
    select(var, any_of(stats))
}


tab_validnum = function(x, valid = 4, digits = NULL) {
  library(tidyverse)

  if (!is.null(digits)) {
    warning("Setting digits may cause different valid numbers, e.g., 0.14 & 1.14")
    round(x, digits = digits)
  } else {
    x <- as.character(x)
    if (stringr::str_detect(x, "^0|^\\-0")) {
      x %>% as.numeric() %>% round(digits = valid)
    } else {
      intnum <- stringr::str_match(x, "^(\\-|)(.*)\\.")[, 3] %>%
        stringr::str_length()
      x %>% as.numeric() %>% round(digits = valid - intnum)
    }
  }
}
