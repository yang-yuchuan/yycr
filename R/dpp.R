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


# interpolation
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


# industry standard CN
dpp_indstdcn = function(ind2, ind4 = NULL,
                        std_from = c("gbt2011", "gbt2017", "csrc2012", "csrc2001"),
                        std_to = c("gbt2011", "gbt2017", "csrc2012", "csrc2001")) {
  pacman::p_load(tidyverse)

  if (std_from == std_to) stop("Same industry standard.")

  ind2 <- c("C26", "K69", "G58"); ind4 <- NULL
  # 针对C3、B1等特殊情况
  message(str_c("First detect whether have cases like B3, C2, etc. ",
                "Note that C1 contains multiple industries while B3 is just B03."))

  tibble(ind2 = ind2, ind4 = ind4) %>%
    {
      if (std_from == "gbt2011") {
        if (std_to == "gbt2017") {
          reframe(., ind = case_when(
            ind2 == "G58" ~ fun_ind4_g2011_g58(ind4),
            ind2 == "O79" ~ "O80", ind2 == "O80" ~ "O81", ind2 == "O81" ~ "O82",
            ind2 == "P82" ~ "P83", ind2 == "Q83" ~ "Q84", ind2 == "Q84" ~ "Q85",
            ind2 == "R85" ~ "R86", ind2 == "R86" ~ "R87", ind2 == "R87" ~ "R88",
            ind2 == "R88" ~ "R89", ind2 == "R89" ~ "R90", ind2 == "S90" ~ "S91",
            ind2 == "S91" ~ "S92", ind2 == "S92" ~ "S93", ind2 == "S93" ~ "S94",
            ind2 == "S94" ~ "S95", ind2 == "S95" ~ "S96", ind2 == "T96" ~ "T97",
            T ~ ind2
          ))
        } else if (std_to == "csrc2001") {
          .
        } else {  # csrc2012
          .
          # reframe(., ind = case_when(
          #   ind2
          # ))
        }
      } else if (std_from == "csrc2001") {
        if (std_to == "gbt2017") {
          .
        } else if (std_to == "gbt2011") {
          .
        } else {  # csrc2012
          reframe(., ind = case_when(
            ind2 == "A03" ~ "A02", ind2 == "A05" ~ "A03", ind2 == "A07" ~ "A04",
            ind2 == "A09" ~ "A05", ind2 == "B01" ~ "B06", ind2 == "B03" ~ "B07",
            ind2 == "B05" ~ "B08", ind2 == "B07" ~ "B09", ind2 == "B09" ~ "B10",
            ind2 == "B49" ~ "B12", ind2 == "B50" ~ "B11", ind2 == "C01" ~ "C13",
            ind2 == "C03" ~ "C14", ind2 == "C05" ~ "C15", ind2 == "C11" ~ "C17",
            ind2 == "C13" ~ fun_ind4_c2001_c13(ind4),
            ind2 == "C14" ~ "C19", ind2 == "C21" ~ "C20", ind2 == "C25" ~ "C21",
            ind2 == "C31" ~ "C22", ind2 == "C35" ~ "C23", ind2 == "C37" ~ "C24",
            ind2 == "C41" ~ "C25", ind2 == "C43" ~ "C26", ind2 == "C47" ~ "C28",
            ind2 == "C48" | ind2 == "C49" ~ "C29",
            ind2 %in% c("C51", "C55", "C57") ~ "C39",
            ind2 == "C59" ~ "O80", ind2 == "C61" ~ "C30", ind2 == "C65" ~ "C31",
            ind2 == "C67" ~ "C32", ind2 == "C69" ~ "C33", ind2 == "C71" ~ "C34",
            ind2 == "C73" ~ "C35", ind2 == "C75" ~ fun_ind4_c2001_c75(ind4),
            ind2 == "C76" ~ "C38", ind2 == "C78" ~ "C40", ind2 == "C99" ~ "C41",
            ind2 == "C81" | ind2 == "C85" ~ "C27",
            ind2 == "D01" ~ "D44", ind2 == "D03" ~ "D45", ind2 == "D05" ~ "D46",
            ind2 == "E01" ~ "E48", ind2 == "E05" ~ "E50", ind2 == "F01" ~ "G53",
            ind2 == "F03" ~ "G54", ind2 == "F05" ~ "G57", ind2 == "F07" ~ "G55",
            ind2 == "F09" ~ "G56", ind2 == "F11" ~ "G58", ind2 == "F19" ~ "G60",
            ind2 == "F21" ~ "G59", ind2 == "G81" | ind2 == "G83" ~ "C39",
            ind2 == "G85" ~ "I63", ind2 == "G87" ~ "I65",
            ind2 %in% c("H01", "H03", "H09") ~ "F51", ind2 == "H11" | ind2 == "H21" ~ "F52",
            ind2 == "I01" ~ "J66", ind2 == "I11" ~ "J68",
            ind2 == "I21" | ind2 == "I41" ~ "J67", ind2 == "I31" | ind2 == "I99" ~ "J69",
            matches(ind2, "^J") ~ "K70",
            ind2 == "K01" ~ "N78", ind2 == "K10" ~ "G60", ind2 == "K20" ~ "M74",
            ind2 == "K30" ~ "H62", ind2 == "K32" ~ "H61", ind2 == "K34" ~ "L72",
            ind2 == "K36" ~ "R89", ind2 == "K37" ~ "Q83", ind2 == "K39" ~ "L71",
            ind2 == "K99" ~ "O81", ind2 == "L01" ~ "R85", ind2 == "L05" | ind2 == "L10" ~ "R86",
            ind2 == "L15" ~ "R87", ind2 == "L20" ~ "I64", ind2 == "L99" ~ "R87"
          ))
        }
      } else if (std_from == "csrc2012") {
        if (std_to == "gbt2017") {
          .
        } else if (std_to == "gbt2011") {
          .
        } else {  # csrc2001
          .
        }
      } else {  # gbt2017
        if (std_to == "gbt2011") {
          .
        } else if (std_to == "csrc2001") {
          .
        } else {  # csrc2012
          .
        }
      }
    }

  # 专门处理四位行业代码
  fun_ind4_c2001_c13 = function(ind4) if (ind4 == "C1340" & !is.na(ind4)) "C19" else "C18"
  fun_ind4_c2001_c75 = function(ind4) if (ind4 == "C7505" & !is.na(ind4)) "C36" else "C37"
  fun_ind4_g2001_g58 = function(ind4) if (matches(ind4, "581") & !is.na(ind4)) "G59" else "G58"
  # fun_ind4_c2001_c13 = function(ind4) if (ind4 == "C1340" & !is.na(ind4)) "C19" else "C18"
  # fun_ind4_c2001_c13 = function(ind4) if (ind4 == "C1340" & !is.na(ind4)) "C19" else "C18"
}



# rename in data.table like in dplyr
dpp_renamedt = function(dt, ...) data.table::setnames(dt, names(list(...)), unlist(list(...)))












