# replace infinity by NA
dpp_replace_infna = function(data, replace = NA) {
  library(tidyverse)

  data.table::fifelse(is.infinite(data), NA_integer_, data) %>%
    replace_na(replace)
}


# calculate HHI
dpp_hhin <- function(data, fml, top = 5, name = "hhi_{.col}") {
  library(dplyr)

  # 解析公式
  vars <- all.vars(fml)
  x_var <- vars[1]
  industry_var <- vars[2]  # the order of industry and year does not matter
  year_var <- vars[3]

  # 计算HHI
  hhi <- data %>%
    group_by(!!sym(industry_var), !!sym(year_var)) %>%
    arrange(desc(!!sym(x_var))) %>%
    mutate(
      total_x = sum(!!sym(x_var), na.rm = TRUE),
      across(!!sym(x_var), ~ .x / total_x)
      # x_ratio = (!!sym(x_var)) / total_x
    ) %>%
    slice_head(n = top) %>%
    summarise(across(!!sym(x_var), ~ sum(.x ^ 2, na.rm = TRUE), .names = name))

  data %>% left_join(hhi)
}


dpp_ipolate = function(y, x, epolate = F) {
  library(data.table)
  library(fixest)

  if (length(x) != length(y)) stop("Lengths of x and y are not same.")

  dt <- data.table(x, y, id = 1)
  pdt <- panel(dt, ~ id + x)
  pdt[, z := ifelse(!is.na(y), y, l(y))]
  pdt[]
}


#' Extract first principal component
#'
#' Besides taking arithmetic mean value or weighted average, another useful way
#' to reduce dimensionality or construct an index out of several variables is
#' **Principal Component** or PC. The first PC contains largest information and
#' variation of original data. `dpp_pc1()` provides a one-line solution to extract
#' the first PC and store it in a new column.
#'
#' @param data A tibble (recommended) or a data frame.
#' @param ... <\link[dplyr]{dplyr_tidy_select}>. Columns to calculate the first PC.
#' @param name A string of the new variable name. Default is "pc1".
#' @param keep Whether to keep columns, based on which the PC is calculated, in the resulted data.
#' @param center Pass to \link[stats]{prcomp}.
#' @param scale. Pass to \link[stats]{prcomp}.
#'
#' @return A data frame with a new column named `name`, which value is the first PC
#' of `...` columns.
#'
#' @examples
#' dpp_pc1(iris, matches("Petal"), name = "avg_petal", keep = F)
#'
#' @export
dpp_pc1 = function(data, ..., name = "pc1", center = T, scale. = F, keep = T) {
  # automatically na.omit

  library(dplyr)
  library(rlang)
  library(stringr)
  library(magrittr)

  vars <- enquos(...)
  newname <- sym(name)

  data %>% {
    fml <- as.formula(str_c("~", str_c(colnames(select(., !!!vars)), collapse = "+")))
    pc <- prcomp(fml, ., center = center, scale. = scale.)$rotation[, "PC1"]
    if (keep) mutate(., !!newname := (as.matrix(select(., !!!vars)) %*% pc)[, 1])
    else select(mutate(., !!newname := (as.matrix(select(., !!!vars)) %*% pc)[, 1]), -c(!!!vars))
  }
}
