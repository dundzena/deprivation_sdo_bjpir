library(haven)
library(dplyr)
library(lavaan)
library(kableExtra)

# this is the full combined BES panel, it is not included in the main replication files.
# you can download it from the official BES site
BES_combined <- read_dta("data_uncleaned/BES_combined.dta")


# for GRD SDO paper

bes <- BES_combined %>%
  select(id, wbEconW11, wbEconW21,
         emEconW11, emEconW21,
         al_scaleW10_W12,al_scaleW21

         )

bes[, -1][bes[, -1] == 9999] <- NA


# making the GRD scales

bes$grdW11 <- bes$wbEconW11 - bes$emEconW11

bes$grdW21 <- bes$wbEconW21 - bes$emEconW21



bes$grdW11 <-
  dplyr::recode(
    bes$grdW11,
    "-4" = -2,
    "-3" = -2,
    "-2" = -2,
    "-1" = -1,
    "0" = 0,
    "1" = 1,
    "2" = 2,
    "3" = 2,
    "4" = 2
  )

bes$grdW21 <-
  dplyr::recode(
    bes$grdW21,
    "-4" = -2,
    "-3" = -2,
    "-2" = -2,
    "-1" = -1,
    "0" = 0,
    "1" = 1,
    "2" = 2,
    "3" = 2,
    "4" = 2
  )


rev_var <- function(variable) {
  
  new_var <- (variable * -1) + max(variable, na.rm = T)
  
  return(new_var)
  
}

# reversing them as it was coded in the opposite direction in the original coding

bes$grdW21 <- rev_var(bes$grdW21)

bes$grdW11 <- rev_var(bes$grdW11)

### Cross lagged panel model

clpm_11_21 <- '
  # regressions
  grdW21 ~ grdW11 + al_scaleW10_W12
  al_scaleW21 ~ al_scaleW10_W12 + grdW11

  # variances
  grdW11 ~~ grdW11
  al_scaleW10_W12 ~~ al_scaleW10_W12
  grdW21 ~~ grdW21
  al_scaleW21 ~~ al_scaleW21

  # covariances
  grdW11 ~~ al_scaleW10_W12
  grdW21 ~~ al_scaleW21

  # intercepts
  grdW11 ~ 1
  al_scaleW10_W12 ~ 1
  grdW21 ~ 1
  al_scaleW21 ~ 1
'

# fitting

fit_11_21 <- lavaan(
  model = clpm_11_21,
  data = bes,
  missing = "fiml",
  estimator = "MLR"
)


# parameters for plotting

pe <- parameterEstimates(
  fit_11_21,
  standardized = T
)

# only the main results

table_clpm <- pe %>%
  filter(op == "~") %>%
  select(
    lhs, rhs,
    est, se, pvalue, std.all
  ) %>%
  mutate(
    pvalue = round(pvalue, 3),
    est    = round(est, 3),
    se     = round(se, 3),
    std.all = round(std.all, 3)
  )

colnames(table_clpm) <- c("DV", "IV", "Estimate", "SE", "P value", "Standardised estimate")

table_clpm$DV <- c("GRDw21", "GRDw21", "RWAw21", "RWAw21")

table_clpm$IV <- c("GRDw11", "RWAw11", "RWAw11", "GRDw11")


kbl(table_clpm,
    booktabs = T,
    linesep = "",
    row.names = F) %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  cat(., file = "figures/clpm.html")
