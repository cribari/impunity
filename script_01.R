# =====================================================================
# Reprodutibility Script
#
# Article:
# Explaining Cross-Country Variation in Impunity: Further Evidence from 
# Bounded Regression Models
#
# Authors:
# Francisco Cribari-Neto, Tatiene C. Souza, Márcia A.F. Reis 
#
# ---------------------------------------------------------------------
# Purpose
# ---------------------------------------------------------------------
# This script reproduces the descriptive statistics and empirical results
# reported in the article, including:
# - Correlation matrix
# - Descriptive statistics
# - Top 10 countries with highest and lowest impunity
# - Distribution of impunity scores by continent
# - Violin plots (overall and by continent)
# - Beta regression model fits (baseline and extended)
# - Goodness-of-fit statistics
# - Observed versus predicted values (extended model)
# - Impunity gaps (unsorted and sorted; extended model)
#
# ---------------------------------------------------------------------
# Data
# ---------------------------------------------------------------------
# Dataset file:
# data_impunity.csv
#
# Variables:
# Country   — country name
# ISO3      — ISO3 code
# continent — continent label
# IMP       — impunity index 
# IEF       — Index of Economic Freedom
# GDP       — GDP per capita
# GINI      — Gini coefficient
# HDI       — Human Development Index
# HEALTH    — Health expenditure (% of GDP)
# EDI       — Democracy Index
# PFI       — Press Freedom Index
# NORDIC    — Nordic countries dummy variable 
#
# Software:
# R statistical computing environment (https://www.r-project.org/)
# Required packages: dplyr, ggplot2, patchwork, betareg, Cairo
# =====================================================================

# ---------------------------------------------------------------------
# remove all existing objects
# ---------------------------------------------------------------------

rm(list = ls())

# ---------------------------------------------------------------------
# load required packages (install if necessary)
# ---------------------------------------------------------------------

if (!require("dplyr"))     install.packages("dplyr")
if (!require("ggplot2"))   install.packages("ggplot2")
if (!require("patchwork")) install.packages("patchwork")
if (!require("betareg"))   install.packages("betareg")
if (!require("Cairo"))     install.packages("Cairo")

library(dplyr)
library(ggplot2)
library(patchwork)
library(betareg)
library(Cairo)

# ---------------------------------------------------------------------
# load the data 
# ---------------------------------------------------------------------

data_imp <- read.csv("data_impunity.csv", stringsAsFactors = FALSE)

# ---------------------------------------------------------------------
# coerce numeric variables
# ---------------------------------------------------------------------

numeric_vars <- c("IMP","IEF","GDP","GINI","HDI","HEALTH","EDI","PFI","NORDIC")

for (v in numeric_vars) {
  if (v %in% names(data_imp)) data_imp[[v]] <- as.numeric(data_imp[[v]])
}

# ---------------------------------------------------------------------
# ensure the dependent variable values lie in (0,1)
# ---------------------------------------------------------------------

if (any(data_imp$IMP <= 0 | data_imp$IMP >= 1, na.rm = TRUE)) {
  stop("IMP values must lie in (0,1).")
}

# =====================================================================
# 1) descriptive statistics
# =====================================================================

vars <- c("IMP","IEF","GDP","HDI","HEALTH","EDI","PFI","GINI")

df_multi <- data_imp[, c("Country", vars)]

correlation_matrix <- round(
  cor(df_multi[vars], use = "pairwise.complete.obs"),
  2
)
correlation_matrix

desc <- data.frame(
  Variable = vars,
  Mean     = sapply(vars, function(v) mean(df_multi[[v]], na.rm = TRUE)),
  SD       = sapply(vars, function(v) sd(df_multi[[v]], na.rm = TRUE)),
  Min      = sapply(vars, function(v) min(df_multi[[v]], na.rm = TRUE)),
  Q1       = sapply(vars, function(v) quantile(df_multi[[v]], 0.25, na.rm = TRUE)),
  Median   = sapply(vars, function(v) median(df_multi[[v]], na.rm = TRUE)),
  Q3       = sapply(vars, function(v) quantile(df_multi[[v]], 0.75, na.rm = TRUE)),
  Max      = sapply(vars, function(v) max(df_multi[[v]], na.rm = TRUE))
)

desc$CV_percent <- with(desc, SD / abs(Mean) * 100)

desc <- within(desc, {
  Mean       <- round(Mean, 4)
  SD         <- round(SD, 4)
  Min        <- round(Min, 4)
  Q1         <- round(Q1, 4)
  Median     <- round(Median, 4)
  Q3         <- round(Q3, 4)
  Max        <- round(Max, 4)
  CV_percent <- round(CV_percent, 4)
})

desc

# =====================================================================
# 2) Top 10 countries with highest and lowest IMP 
# =====================================================================

top10_highest_imp <- data_imp %>%
  select(Country, IMP) %>%
  arrange(desc(IMP)) %>%
  mutate(IMP = round(IMP, 4)) %>%
  head(10)

top10_lowest_imp <- data_imp %>%
  select(Country, IMP) %>%
  arrange(IMP) %>%
  mutate(IMP = round(IMP, 4)) %>%
  head(10)

top10_highest_imp
top10_lowest_imp

# =====================================================================
# 3) impunity score by continent
# =====================================================================

summary_continent <- data_imp %>%
  group_by(continent) %>%
  summarise(
    Min    = min(IMP, na.rm = TRUE),
    Q1     = quantile(IMP, 0.25, na.rm = TRUE),
    Median = median(IMP, na.rm = TRUE),
    Mean   = mean(IMP, na.rm = TRUE),
    Q3     = quantile(IMP, 0.75, na.rm = TRUE),
    Max    = max(IMP, na.rm = TRUE),
    n      = n(),
    .groups = "drop"
  ) %>%
  mutate(across(Min:Max, ~ round(., 4)))

summary_continent

# =====================================================================
# 4) violin plots (overall and by continent)
# =====================================================================

continent_colors <- c(
  "Africa"       = "goldenrod1",
  "Americas"     = "hotpink3",
  "Asia-Oceania" = "cadetblue1",
  "Europe"       = "blueviolet"
)

data_imp <- data_imp %>%
  mutate(
    continent_mod = ifelse(continent %in% c("Asia","Oceania"),
                           "Asia-Oceania",
                           continent)
  )

y_lim <- c(0, 1)

p_general <- ggplot(data_imp, aes(x = "", y = IMP)) +
  geom_violin(fill = "skyblue", color = "black", trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2,
               color = "black", fill = "red") +
  coord_cartesian(ylim = y_lim) +
  labs(y = "") +
  theme_minimal(base_size = 13) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank()
  )

p_continent <- ggplot(data_imp, aes(x = continent_mod, y = IMP, fill = continent_mod)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2,
               color = "black", fill = "red") +
  scale_fill_manual(values = continent_colors) +
  coord_cartesian(ylim = y_lim) +
  labs(y = "") +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    axis.title.x = element_blank()
  )

p_final <- p_general + p_continent

p_final 

# =====================================================================
# 5) beta regression model fits (baseline and extended)
# mean submodel: log-log link
# precision submodel: log link
# =====================================================================

fit_baseline <- betareg(
  IMP ~
    I(IEF^1.5) +
    I(GDP^1.25) +
    I(HDI^0.25 * HEALTH^0.30) +
    I(EDI^1.10 * PFI^0.45) +
    I(GINI^0.7) |
    I(EDI^1.25) +
    I(HEALTH^0.25) +
    I(IEF^0.3),
  data = data_imp,
  link = "loglog",
  link.phi = "log"
)

fit_baseline_null <- betareg(
  IMP ~ 1 | 1,
  data = data_imp,
  link = "loglog",
  link.phi = "log"
)

fit_extended <- betareg(
  IMP ~
    I(IEF^1.5) +
    I(GDP^1.25) +
    I(HDI^0.25 * HEALTH^0.30) +
    I(EDI^1.10 * PFI^0.45) +
    I(GINI^0.7) +
    NORDIC |
    I(EDI^1.25) +
    I(HEALTH^0.25) +
    I(IEF^0.3),
  data = data_imp,
  link = "loglog",
  link.phi = "log"
)

fit_extended_null <- betareg(
  IMP ~ 1 | 1,
  data = data_imp,
  link = "loglog",
  link.phi = "log"
)

summary(fit_baseline)
summary(fit_extended)

# =====================================================================
# 6) goodness-of-fit statistics
# =====================================================================

compute_gof <- function(fit, fit_null, data) {

  AIC_value  <- AIC(fit)
  BIC_value  <- BIC(fit)
  Pseudo_R2  <- summary(fit)$pseudo.r.squared

  L0 <- exp(as.numeric(logLik(fit_null)))
  L1 <- exp(as.numeric(logLik(fit)))
  N  <- nobs(fit)

  Nagelkerke_R2 <- (1 - (L0 / L1)^(2 / N))

  phi_values <- predict(fit, type = "precision")
  Precision_Ratio <- max(phi_values) / min(phi_values)

  round(data.frame(
    AIC = AIC_value,
    BIC = BIC_value,
    Pseudo_R2 = Pseudo_R2,
    Nagelkerke_R2 = Nagelkerke_R2,
    Precision_Ratio = Precision_Ratio
  ), 4)
}

gof_baseline <- compute_gof(fit_baseline, fit_baseline_null, data_imp)
gof_extended <- compute_gof(fit_extended, fit_extended_null, data_imp)

gof_baseline
gof_extended

# =====================================================================
# 7) observed versus predicted (extended model) + base R plot
# =====================================================================

predicted_values <- predict(fit_extended, type = "response")

plot(data_imp$IMP, predicted_values,
     xlab = "Observed values",
     ylab = "Predicted values",
     xlim = c(0, 1),
     ylim = c(0, 1))

abline(0, 1)

# =====================================================================
# 8) impunity gaps (extended model)
# =====================================================================

impunity_gaps <- data_imp$IMP - predicted_values

gaps_table <- data.frame(
  Country   = data_imp$Country,
  Observed  = round(data_imp$IMP, 4),
  Predicted = round(predicted_values, 4),
  Gap       = round(impunity_gaps, 4),
  stringsAsFactors = FALSE
)

gaps_table_country <- gaps_table[order(gaps_table$Country), ]
gaps_table_gap     <- gaps_table[order(gaps_table$Gap), ]

gaps_table_country
gaps_table_gap