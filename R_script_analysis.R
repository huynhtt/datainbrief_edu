# ============================================================
# FULL R CODE - 5-FACTOR MODEL
# Topic: Student Evaluation of Teaching (SET)
# Final constructs:
# 1) Organizational Identification
# 2) Institutional Trust
# 3) Perceived Usefulness of SET
# 4) Constructive Evaluation Behavior
# 5) Self-Perceived Evaluation Bias
# ============================================================

# ============================================================
# 0. PACKAGES
# ============================================================
packages <- c("readxl", "dplyr", "psych", "lavaan", "semTools", "writexl", "tidyr", "stringr")

new_pkgs <- packages[!(packages %in% installed.packages()[, "Package"])]
if(length(new_pkgs) > 0) install.packages(new_pkgs)

library(readxl)
library(dplyr)
library(psych)
library(lavaan)
library(semTools)
library(writexl)
library(tidyr)
library(stringr)

# ============================================================
# 1. READ DATA
# ============================================================
df <- read_excel("dataset_SET_student_evaluation_raw.xlsx")

# Final 15 observed indicators for 5-factor model
items_5f <- c(
  "x1","x2","x3",      # Organizational Identification
  "x6","x9","x10",     # Institutional Trust
  "x11","x12","x13",   # Perceived Usefulness
  "x17","x18","x20",   # Constructive Evaluation Behavior
  "x21","x22","x23"    # Self-Perceived Evaluation Bias
)

# ============================================================
# 2. CLEAN DATA
# Rule: keep cases with fewer than 3 missing values
# ============================================================
df_5f <- df %>%
  select(all_of(items_5f)) %>%
  filter(rowSums(is.na(.)) < 3)

cat("Sample size after cleaning:", nrow(df_5f), "\n")

# ============================================================
# 3. OPTIONAL STRAIGHT-LINING CHECK
# Used later for robustness check
# ============================================================
response_sd <- apply(df_5f, 1, sd, na.rm = TRUE)

cat("\n================ STRAIGHT-LINING CHECK ================\n")
print(summary(response_sd))

possible_straightliners <- which(response_sd < 0.20)
cat("Possible straight-liners (SD < .20):", length(possible_straightliners), "\n")

# Sensitivity sample excluding potential straight-liners
df_5f_sens <- df_5f[response_sd >= 0.20, ]
cat("Sensitivity sample size:", nrow(df_5f_sens), "\n")

# ============================================================
# 4. CONSTRUCT DEFINITIONS
# ============================================================
constructs_5f <- list(
  Organizational_Identification = c("x1","x2","x3"),
  Institutional_Trust           = c("x6","x9","x10"),
  Perceived_Usefulness          = c("x11","x12","x13"),
  Constructive_Behavior         = c("x17","x18","x20"),
  Perceived_Bias                = c("x21","x22","x23")
)

construct_labels <- c(
  Organizational_Identification = "Organizational Identification",
  Institutional_Trust           = "Institutional Trust",
  Perceived_Usefulness          = "Perceived Usefulness of SET",
  Constructive_Behavior         = "Constructive Evaluation Behavior",
  Perceived_Bias                = "Self-Perceived Evaluation Bias"
)

item_labels <- c(
  x1  = "OI1", x2  = "OI2", x3  = "OI3",
  x6  = "TR1", x9  = "TR2", x10 = "TR3",
  x11 = "PU1", x12 = "PU2", x13 = "PU3",
  x17 = "CB1", x18 = "CB2", x20 = "CB3",
  x21 = "PB1", x22 = "PB2", x23 = "PB3"
)

# ============================================================
# 5. DESCRIPTIVE STATISTICS FOR OBSERVED ITEMS
# ============================================================
desc_table <- psych::describe(df_5f)[, c("mean","sd","skew","kurtosis","min","max")]
desc_table <- as.data.frame(desc_table)
desc_table$Item <- rownames(desc_table)
rownames(desc_table) <- NULL

desc_table <- desc_table %>%
  select(Item, everything()) %>%
  mutate(
    Item_Label = item_labels[Item],
    across(where(is.numeric), ~ round(.x, 3))
  ) %>%
  select(Item, Item_Label, mean, sd, skew, kurtosis, min, max)

cat("\n================ ITEM DESCRIPTIVE STATISTICS ================\n")
print(desc_table)

# ============================================================
# 6. RELIABILITY: CRONBACH ALPHA
# ============================================================
alpha_list <- lapply(names(constructs_5f), function(nm) {
  vars <- constructs_5f[[nm]]
  a <- psych::alpha(df_5f[, vars])
  data.frame(
    Construct = nm,
    Construct_Label = construct_labels[nm],
    Alpha = round(a$total$raw_alpha, 3)
  )
})

alpha_table <- bind_rows(alpha_list)

cat("\n================ CRONBACH ALPHA ================\n")
print(alpha_table)

# ============================================================
# 7. CFA MODEL (5 FACTORS)
# ============================================================
model_5f_cfa <- '
Organizational_Identification =~ x1 + x2 + x3
Institutional_Trust           =~ x6 + x9 + x10
Perceived_Usefulness          =~ x11 + x12 + x13
Constructive_Behavior         =~ x17 + x18 + x20
Perceived_Bias                =~ x21 + x22 + x23
'

fit_cfa_5f <- cfa(
  model_5f_cfa,
  data = df_5f,
  estimator = "MLR",
  std.lv = TRUE
)

cat("\n================ CFA SUMMARY (5-FACTOR) ================\n")
print(summary(fit_cfa_5f, fit.measures = TRUE, standardized = TRUE))

cfa_fit_indices <- fitMeasures(
  fit_cfa_5f,
  c("cfi","tli","rmsea","srmr","chisq","df","pvalue",
    "cfi.robust","tli.robust","rmsea.robust")
)

cat("\n================ CFA FIT INDICES ================\n")
print(round(cfa_fit_indices, 3))

# ============================================================
# 8. STANDARDIZED LOADINGS
# Table 3
# ============================================================
loading_table <- standardizedSolution(fit_cfa_5f) %>%
  filter(op == "=~") %>%
  transmute(
    Construct = lhs,
    Construct_Label = construct_labels[lhs],
    Item = rhs,
    Item_Label = item_labels[rhs],
    Std_Loading = round(est.std, 3),
    pvalue = round(pvalue, 4)
  )

cat("\n================ STANDARDIZED FACTOR LOADINGS ================\n")
print(loading_table)

# ============================================================
# 9. COMPOSITE RELIABILITY (CR) & AVE
# ============================================================
cr_vals  <- semTools::compRelSEM(fit_cfa_5f)
ave_vals <- semTools::AVE(fit_cfa_5f)

cr_ave_table <- data.frame(
  Construct = names(cr_vals),
  Construct_Label = construct_labels[names(cr_vals)],
  CR = round(as.numeric(cr_vals), 3),
  AVE = round(as.numeric(ave_vals), 3)
)

cat("\n================ CR & AVE ================\n")
print(cr_ave_table)

# ============================================================
# 10. LATENT CORRELATIONS
# Used in Table 1 and Table 2
# ============================================================
lv_cor <- inspect(fit_cfa_5f, "cor.lv")
lv_cor_round <- round(lv_cor, 3)

cat("\n================ LATENT CORRELATIONS ================\n")
print(lv_cor_round)

# ============================================================
# 11. FORNELL-LARCKER
# ============================================================
sqrt_ave <- sqrt(as.numeric(ave_vals))
names(sqrt_ave) <- names(ave_vals)

fornell_larcker <- lv_cor
diag(fornell_larcker) <- sqrt_ave
fornell_larcker_round <- round(fornell_larcker, 3)

cat("\n================ FORNELL-LARCKER MATRIX ================\n")
print(fornell_larcker_round)

# ============================================================
# 12. HTMT
# ============================================================
htmt_table <- semTools::htmt(model_5f_cfa, data = df_5f)
htmt_table_round <- round(htmt_table, 3)

cat("\n================ HTMT ================\n")
print(htmt_table_round)

# ============================================================
# 13. COMMON METHOD BIAS
# Harman single-factor CFA
# ============================================================
single_factor_model <- paste0("Common =~ ", paste(items_5f, collapse = " + "))

fit_single_5f <- cfa(
  single_factor_model,
  data = df_5f,
  estimator = "MLR",
  std.lv = TRUE
)

single_factor_fit <- fitMeasures(
  fit_single_5f,
  c("cfi","tli","rmsea","srmr","chisq","df","pvalue",
    "cfi.robust","tli.robust","rmsea.robust")
)

cat("\n================ SINGLE-FACTOR TEST (CMB CHECK) ================\n")
print(round(single_factor_fit, 3))

# ============================================================
# 14. SEM MODEL (FINAL 5-FACTOR MODEL)
# Organizational Identification -> Institutional Trust
# Institutional Trust -> Perceived Usefulness
# Perceived Usefulness -> Constructive Evaluation Behavior
# Perceived Bias -> Constructive Evaluation Behavior
# Organizational Identification ~~ Perceived Bias
# ============================================================
model_5f_sem <- '
# Measurement model
Organizational_Identification =~ x1 + x2 + x3
Institutional_Trust           =~ x6 + x9 + x10
Perceived_Usefulness          =~ x11 + x12 + x13
Constructive_Behavior         =~ x17 + x18 + x20
Perceived_Bias                =~ x21 + x22 + x23

# Structural paths
Institutional_Trust   ~ a*Organizational_Identification
Perceived_Usefulness  ~ b*Institutional_Trust
Constructive_Behavior ~ c*Perceived_Usefulness + d*Perceived_Bias

# Covariance among exogenous constructs
Organizational_Identification ~~ Perceived_Bias

# Indirect effects
ind_OI_PU      := a*b
ind_TR_CB      := b*c
indirect_total := a*b*c
'

fit_sem_5f <- sem(
  model_5f_sem,
  data = df_5f,
  estimator = "MLR",
  std.lv = TRUE
)

cat("\n================ SEM SUMMARY (FINAL 5-FACTOR MODEL) ================\n")
print(summary(fit_sem_5f, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE))

sem_fit_indices <- fitMeasures(
  fit_sem_5f,
  c("cfi","tli","rmsea","srmr","chisq","df","pvalue",
    "cfi.robust","tli.robust","rmsea.robust")
)

cat("\n================ SEM FIT INDICES ================\n")
print(round(sem_fit_indices, 3))

# ============================================================
# 15. STRUCTURAL PATHS + INDIRECT EFFECTS
# Table 4
# ============================================================
sem_paths <- parameterEstimates(
  fit_sem_5f,
  standardized = TRUE,
  ci = TRUE
) %>%
  filter(op %in% c("~", ":=")) %>%
  mutate(
    lhs_label = case_when(
      lhs == "Institutional_Trust"          ~ "Institutional Trust",
      lhs == "Perceived_Usefulness"         ~ "Perceived Usefulness of SET",
      lhs == "Constructive_Behavior"        ~ "Constructive Evaluation Behavior",
      lhs == "ind_OI_PU"                    ~ "Indirect effect: OI -> Trust -> Usefulness",
      lhs == "ind_TR_CB"                    ~ "Indirect effect: Trust -> Usefulness -> Behavior",
      lhs == "indirect_total"               ~ "Indirect effect: OI -> Trust -> Usefulness -> Behavior",
      TRUE ~ lhs
    ),
    rhs_label = case_when(
      rhs == "Organizational_Identification" ~ "Organizational Identification",
      rhs == "Institutional_Trust"           ~ "Institutional Trust",
      rhs == "Perceived_Usefulness"          ~ "Perceived Usefulness of SET",
      rhs == "Perceived_Bias"                ~ "Self-Perceived Evaluation Bias",
      rhs == "a*b"                           ~ "a*b",
      rhs == "b*c"                           ~ "b*c",
      rhs == "a*b*c"                         ~ "a*b*c",
      TRUE ~ rhs
    )
  ) %>%
  select(lhs_label, op, rhs_label, est, se, z, pvalue, ci.lower, ci.upper, std.all) %>%
  rename(
    Outcome = lhs_label,
    Predictor = rhs_label,
    Estimate = est,
    SE = se,
    Z = z,
    P = pvalue,
    CI_Lower = ci.lower,
    CI_Upper = ci.upper,
    Beta = std.all
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

cat("\n================ STRUCTURAL PATHS & INDIRECT EFFECTS ================\n")
print(sem_paths)

# ============================================================
# 16. R-SQUARE
# Included in Table 4
# ============================================================
r2_table <- inspect(fit_sem_5f, "r2")
r2_constructs <- r2_table[c("Institutional_Trust","Perceived_Usefulness","Constructive_Behavior")]

r2_table_final <- data.frame(
  Construct = c("Institutional Trust",
                "Perceived Usefulness of SET",
                "Constructive Evaluation Behavior"),
  R2 = round(as.numeric(r2_constructs), 3)
)

cat("\n================ R-SQUARE (LATENT ENDOGENOUS VARIABLES) ================\n")
print(r2_table_final)

# ============================================================
# 17. ROBUSTNESS CHECKS
# A. Sensitivity check excluding possible straight-liners
# B. Alternative single-factor model for CMB
# ============================================================
fit_cfa_sens <- cfa(model_5f_cfa, data = df_5f_sens, estimator = "MLR", std.lv = TRUE)
fit_sem_sens <- sem(model_5f_sem, data = df_5f_sens, estimator = "MLR", std.lv = TRUE)

robustness_table <- data.frame(
  Analysis = c(
    "Main CFA model (full sample)",
    "Main SEM model (full sample)",
    "CFA excluding possible straight-liners",
    "SEM excluding possible straight-liners",
    "Single-factor CFA (common method bias check)"
  ),
  N = c(
    nrow(df_5f),
    nrow(df_5f),
    nrow(df_5f_sens),
    nrow(df_5f_sens),
    nrow(df_5f)
  ),
  CFI = c(
    fitMeasures(fit_cfa_5f, "cfi"),
    fitMeasures(fit_sem_5f, "cfi"),
    fitMeasures(fit_cfa_sens, "cfi"),
    fitMeasures(fit_sem_sens, "cfi"),
    fitMeasures(fit_single_5f, "cfi")
  ),
  TLI = c(
    fitMeasures(fit_cfa_5f, "tli"),
    fitMeasures(fit_sem_5f, "tli"),
    fitMeasures(fit_cfa_sens, "tli"),
    fitMeasures(fit_sem_sens, "tli"),
    fitMeasures(fit_single_5f, "tli")
  ),
  RMSEA = c(
    fitMeasures(fit_cfa_5f, "rmsea"),
    fitMeasures(fit_sem_5f, "rmsea"),
    fitMeasures(fit_cfa_sens, "rmsea"),
    fitMeasures(fit_sem_sens, "rmsea"),
    fitMeasures(fit_single_5f, "rmsea")
  ),
  SRMR = c(
    fitMeasures(fit_cfa_5f, "srmr"),
    fitMeasures(fit_sem_5f, "srmr"),
    fitMeasures(fit_cfa_sens, "srmr"),
    fitMeasures(fit_sem_sens, "srmr"),
    fitMeasures(fit_single_5f, "srmr")
  )
) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

cat("\n================ ROBUSTNESS CHECK TABLE ================\n")
print(robustness_table)

# ============================================================
# 18. TABLE 1
# Descriptive statistics of latent constructs + correlation matrix
# Usually used in Results section 4.1
# ============================================================
latent_scores <- data.frame(
  Organizational_Identification = rowMeans(df_5f[, constructs_5f$Organizational_Identification], na.rm = TRUE),
  Institutional_Trust           = rowMeans(df_5f[, constructs_5f$Institutional_Trust], na.rm = TRUE),
  Perceived_Usefulness          = rowMeans(df_5f[, constructs_5f$Perceived_Usefulness], na.rm = TRUE),
  Constructive_Behavior         = rowMeans(df_5f[, constructs_5f$Constructive_Behavior], na.rm = TRUE),
  Perceived_Bias                = rowMeans(df_5f[, constructs_5f$Perceived_Bias], na.rm = TRUE)
)

construct_desc <- psych::describe(latent_scores)[, c("mean","sd","min","max")]
construct_desc <- as.data.frame(construct_desc)
construct_desc$Construct <- rownames(construct_desc)
rownames(construct_desc) <- NULL

construct_desc <- construct_desc %>%
  mutate(
    Construct_Label = construct_labels[Construct],
    Mean = round(mean, 3),
    SD = round(sd, 3),
    Min = round(min, 3),
    Max = round(max, 3)
  ) %>%
  select(Construct, Construct_Label, Mean, SD, Min, Max)

construct_cor <- round(cor(latent_scores, use = "pairwise.complete.obs"), 3)
construct_cor_df <- as.data.frame(construct_cor)
construct_cor_df$Construct <- rownames(construct_cor_df)
rownames(construct_cor_df) <- NULL

table1 <- construct_desc %>%
  left_join(construct_cor_df, by = "Construct")

cat("\n================ TABLE 1: DESCRIPTIVE STATISTICS AND CORRELATIONS ================\n")
print(table1)

# ============================================================
# 19. TABLE 2
# Reliability and validity
# Alpha, CR, AVE
# Fornell-Larcker and HTMT kept as separate sheets for readability
# ============================================================
table2_main <- alpha_table %>%
  left_join(cr_ave_table, by = c("Construct", "Construct_Label")) %>%
  select(Construct, Construct_Label, Alpha, CR, AVE)

cat("\n================ TABLE 2A: RELIABILITY AND CONVERGENT VALIDITY ================\n")
print(table2_main)

table2_fornell <- as.data.frame(fornell_larcker_round)
table2_fornell$Construct <- rownames(table2_fornell)
rownames(table2_fornell) <- NULL

cat("\n================ TABLE 2B: FORNELL-LARCKER MATRIX ================\n")
print(table2_fornell)

table2_htmt <- as.data.frame(htmt_table_round)
table2_htmt$Construct <- rownames(table2_htmt)
rownames(table2_htmt) <- NULL

cat("\n================ TABLE 2C: HTMT MATRIX ================\n")
print(table2_htmt)

# ============================================================
# 20. TABLE 3
# CFA standardized loadings
# ============================================================
table3 <- loading_table

cat("\n================ TABLE 3: STANDARDIZED FACTOR LOADINGS ================\n")
print(table3)

# ============================================================
# 21. TABLE 4
# Structural model results
# ============================================================
table4_paths <- sem_paths
table4_r2 <- r2_table_final

cat("\n================ TABLE 4A: STRUCTURAL PATHS AND INDIRECT EFFECTS ================\n")
print(table4_paths)

cat("\n================ TABLE 4B: EXPLAINED VARIANCE (R-SQUARE) ================\n")
print(table4_r2)

# ============================================================
# 22. TABLE 5
# Robustness checks
# ============================================================
table5 <- robustness_table

cat("\n================ TABLE 5: ROBUSTNESS CHECKS ================\n")
print(table5)

# ============================================================
# 23. OPTIONAL: MODIFICATION INDICES
# Not usually reported in the main paper, but useful for inspection
# ============================================================
mi_table <- modificationIndices(fit_cfa_5f, sort. = TRUE) %>%
  as.data.frame() %>%
  filter(mi >= 10) %>%
  arrange(desc(mi)) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

cat("\n================ TOP MODIFICATION INDICES (MI >= 10) ================\n")
print(head(mi_table, 20))

# ============================================================
# 24. EXPORT ALL TABLES TO EXCEL
# ============================================================
results_list <- list(
  Item_Descriptives = desc_table,
  Table_1_Descriptive_Correlations = table1,
  Table_2A_Reliability_ConvergentValidity = table2_main,
  Table_2B_Fornell_Larcker = table2_fornell,
  Table_2C_HTMT = table2_htmt,
  Table_3_CFA_Loadings = table3,
  Table_4A_SEM_Paths_Indirect = table4_paths,
  Table_4B_R2 = table4_r2,
  Table_5_Robustness = table5,
  Alpha = alpha_table,
  CR_AVE = cr_ave_table,
  Latent_Correlations = as.data.frame(lv_cor_round),
  Single_Factor_CMB_Check = data.frame(
    Index = names(single_factor_fit),
    Value = round(as.numeric(single_factor_fit), 3)
  ),
  CFA_Fit_Indices = data.frame(
    Index = names(cfa_fit_indices),
    Value = round(as.numeric(cfa_fit_indices), 3)
  ),
  SEM_Fit_Indices = data.frame(
    Index = names(sem_fit_indices),
    Value = round(as.numeric(sem_fit_indices), 3)
  ),
  Modification_Indices = mi_table
)

write_xlsx(results_list, "SEM_5factor_results.xlsx")

cat("\nAll done. Results exported to: SEM_5factor_results.xlsx\n")

# ============================================================
# 25. OPTIONAL: PRINT A SHORT RESULTS NARRATIVE TEMPLATE
# This helps you draft Section 4 quickly
# ============================================================
cat("\n============================================================\n")
cat("RESULTS SECTION TEMPLATE (AUTO-GUIDE)\n")
cat("============================================================\n")

cat("\n4.1 Descriptive statistics and correlations\n")
cat("Use Table 1. Report means, SDs, and inter-construct correlations.\n")

cat("\n4.2 Measurement model assessment\n")
cat("Use Table 2A, 2B, 2C, and Table 3.\n")
cat("- Report Cronbach's alpha, CR, and AVE.\n")
cat("- Report standardized factor loadings.\n")
cat("- Report discriminant validity via Fornell-Larcker and HTMT.\n")
cat("- Report CFA fit indices.\n")

cat("\n4.3 Structural model assessment\n")
cat("Use Table 4A and 4B.\n")
cat("- Report path coefficients, p-values, and indirect effects.\n")
cat("- Report R-squared values for endogenous constructs.\n")
cat("- Report SEM fit indices.\n")

cat("\n4.4 Robustness and common method bias checks\n")
cat("Use Table 5 and the single-factor test.\n")
cat("- Mention the single-factor model fit was poor, suggesting CMB is unlikely to dominate.\n")
cat("- Mention the sensitivity analysis excluding possible straight-liners did not materially change the overall interpretation.\n")
cat("============================================================\n")




#######add############
##bootstrap mediation
fit_sem_5f_boot <- sem(
  model_5f_sem,
  data = df_5f,
  estimator = "ML",
  std.lv = TRUE,
  se = "bootstrap",
  bootstrap = 5000
)

boot_indirect <- parameterEstimates(
  fit_sem_5f_boot,
  standardized = TRUE,
  ci = TRUE,
  boot.ci.type = "perc"
) %>%
  filter(op == ":=") %>%
  select(lhs, est, se, pvalue, ci.lower, ci.upper, std.all)

print(boot_indirect)
#######alternative factor model
model_4f_alt <- '
Organizational_Identification =~ x1 + x2 + x3
Trust_Usefulness =~ x6 + x9 + x10 + x11 + x12 + x13
Constructive_Behavior =~ x17 + x18 + x20
Perceived_Bias =~ x21 + x22 + x23
'

fit_cfa_4f_alt <- cfa(
  model_4f_alt,
  data = df_5f,
  estimator = "ML",
  std.lv = TRUE
)

fitMeasures(fit_cfa_4f_alt, c("cfi","tli","rmsea","srmr"))
fitMeasures(fit_cfa_5f, c("cfi","tli","rmsea","srmr"))


# ============================================================
# FIGURE 2: SAMPLE PROFILE
# Two bar charts: Gender and Year of study
# ============================================================

packages <- c("ggplot2", "dplyr", "patchwork")
new_pkgs <- packages[!(packages %in% installed.packages()[, "Package"])]
if(length(new_pkgs) > 0) install.packages(new_pkgs)

library(ggplot2)
library(dplyr)
library(patchwork)

# ------------------------------------------------------------
# 1. Data
# ------------------------------------------------------------
gender_df <- data.frame(
  Gender = c("Male", "Female"),
  Count = c(306, 56)
)

year_df <- data.frame(
  Year = c("Year 1", "Year 2", "Year 3", "Final year"),
  Count = c(72, 195, 67, 27)
)

# Add percentages
gender_df <- gender_df %>%
  mutate(Percent = round(Count / sum(Count) * 100, 1),
         Label = paste0(Count, " (", Percent, "%)"))

year_df <- year_df %>%
  mutate(Percent = round(Count / sum(Count) * 100, 1),
         Label = paste0(Count, " (", Percent, "%)"))

# ------------------------------------------------------------
# 2. Plot 1: Gender
# ------------------------------------------------------------
p1 <- ggplot(gender_df, aes(x = Gender, y = Count)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = Label), vjust = -0.4, size = 4) +
  labs(
    title = "Gender",
    x = NULL,
    y = "Number of respondents"
  ) +
  ylim(0, max(gender_df$Count) * 1.15) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank()
  )

# ------------------------------------------------------------
# 3. Plot 2: Year of study
# ------------------------------------------------------------
p2 <- ggplot(year_df, aes(x = Year, y = Count)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = Label), vjust = -0.4, size = 4) +
  labs(
    title = "Year of study",
    x = NULL,
    y = "Number of respondents"
  ) +
  ylim(0, max(year_df$Count) * 1.15) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank()
  )

# ------------------------------------------------------------
# 4. Combine and export
# ------------------------------------------------------------
fig2 <- p1 + p2 + plot_annotation(
  title = ""
)

print(fig2)

ggsave(
  filename = "Figure_2_sample_profile.png",
  plot = fig2,
  width = 12,
  height = 6,
  dpi = 300
)
