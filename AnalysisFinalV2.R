sink("Amogha.txt")
# =============================================
# Amogha Sudarshan
# =============================================

library(lavaan)
library(semTools)
library(dplyr)
library(readr)
CleanedV3 <- read_csv("CleanedV3.csv")

# Make sure LevelOfAutonomy is a factor
CleanedV3$LevelOfAutonomy <- as.factor(CleanedV3$LevelOfAutonomy)

# -------------------------------------------------
# 1. SEPARATE ONE-FACTOR CFAs (check each construct)
# -------------------------------------------------

# Define a function to run and print one-factor CFA
run_one_factor <- function(items, construct_name) {
  model <- paste(construct_name, "=~", paste(items, collapse = " + "))
  fit <- cfa(model, data = CleanedV3, estimator = "MLR")
  
  cat("========================================\n")
  cat("ONE-FACTOR CFA FOR:", construct_name, "\n")
  cat("========================================\n")
  print(summary(fit, fit.measures = TRUE, standardized = TRUE))
  print(reliability(fit))
  cat("\n\n")
}

# Run for every construct
run_one_factor(c("S1_1","S1_2","S1_3","S1_4","S1_5"), "SI")
run_one_factor(c("HM_1","HM_2","HM_3","HM_4"), "HM")
run_one_factor(c("PE_1","PE_2","PE_3","PE_4","PE_5"), "PE")
run_one_factor(c("EE_1","EE_2","EE_3","EE_4"), "EE")
run_one_factor(c("PA_1","PA_2","PA_3","PA_4","PA_5","PA_6","PA_7","PA_8","PA_9","PA_10"), "PA")
run_one_factor(c("NA_1","NA_2","NA_3","NA_4","NA_5","NA_6","NA_7","NA_8","NA_9","NA_10"), "NegAffect")
run_one_factor(c("AIUse_1","AIUse_2","AIUse_3","AIUse_4","AIUse_5","AIUse_6"), "Willingness")
run_one_factor(c("AIObj_1","AIObj_2","AIObj_3","AIObj_4","AIObj_5","AIObj_6","AIObj_7","AIObj_8"), "Objection")

# -------------------------------------------------
# 2. FULL MEASUREMENT MODEL (all constructs together)
# -------------------------------------------------

full_model <- '
  SI =~ S1_1 + S1_2 + S1_3 + S1_4 + S1_5
  HM =~ HM_1 + HM_2 + HM_3 + HM_4
  PE =~ PE_1 + PE_2 + PE_3 + PE_4 + PE_5
  EE =~ EE_1 + EE_2 + EE_3 + EE_4
  PA =~ PA_1 + PA_2 + PA_3 + PA_4 + PA_5 + PA_6 + PA_7 + PA_8 + PA_9 + PA_10
  NegAffect =~ NA_1 + NA_2 + NA_3 + NA_4 + NA_5 + NA_6 + NA_7 + NA_8 + NA_9 + NA_10
  Willingness =~ AIUse_1 + AIUse_2 + AIUse_3 + AIUse_4 + AIUse_5 + AIUse_6
  Objection   =~ AIObj_1 + AIObj_2 + AIObj_3 + AIObj_4 + AIObj_5 + AIObj_6 + AIObj_7 + AIObj_8
'

fit_full <- cfa(full_model, data = CleanedV3, estimator = "MLR", std.lv = TRUE)

cat("========================================\n")
cat("FULL MEASUREMENT MODEL (ALL CONSTRUCTS)\n")
cat("========================================\n")
print(summary(fit_full, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE))

# Reliability & AVE
print(reliability(fit_full))

# Latent correlations (for discriminant validity)
print(lavInspect(fit_full, what = "cor.lv"))


###########

#Checking the range of each construct
get_range <- function(items, name) {
  vals <- unlist(CleanedV3[, items])
  cat(name, ": Min =", min(vals, na.rm = TRUE),
      "Max =", max(vals, na.rm = TRUE), "\n")
}

get_range(c("S1_1","S1_2","S1_3","S1_4","S1_5"), "SI")
get_range(c("HM_1","HM_2","HM_3","HM_4"), "HM")
get_range(c("PE_1","PE_2","PE_3","PE_4","PE_5"), "PE")
get_range(c("EE_1","EE_2","EE_3","EE_4"), "EE")
get_range(c("PA_1","PA_2","PA_3","PA_4","PA_5","PA_6","PA_7","PA_8","PA_9","PA_10"), "PA")
get_range(c("NA_1","NA_2","NA_3","NA_4","NA_5","NA_6","NA_7","NA_8","NA_9","NA_10"), "NA")
get_range(c("AIUse_1","AIUse_2","AIUse_3","AIUse_4","AIUse_5","AIUse_6"), "Willingness")
get_range(c("AIObj_1","AIObj_2","AIObj_3","AIObj_4","AIObj_5","AIObj_6","AIObj_7","AIObj_8"), "Objection")


##########

# Function to rescale NA and PA to 1–7
rescale_1_7 <- function(x) {
  1 + (x - min(x, na.rm = TRUE)) / 
    (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) * 6
}

# Apply to PA items
pa_items <- c("PA_1","PA_2","PA_3","PA_4","PA_5","PA_6","PA_7","PA_8","PA_9","PA_10")
CleanedV3[pa_items] <- lapply(CleanedV3[pa_items], rescale_1_7)

# Apply to NA items
na_items <- c("NA_1","NA_2","NA_3","NA_4","NA_5","NA_6","NA_7","NA_8","NA_9","NA_10")
CleanedV3[na_items] <- lapply(CleanedV3[na_items], rescale_1_7)

# -------------------------------------------------
# 2. FULL MEASUREMENT MODEL (all constructs together)
# -------------------------------------------------
full_model <- '
  SI =~ S1_1 + S1_2 + S1_3 + S1_4 + S1_5
  HM =~ HM_1 + HM_2 + HM_3 + HM_4
  PE =~ PE_1 + PE_2 + PE_3 + PE_4 + PE_5
  EE =~ EE_1 + EE_2 + EE_3 + EE_4
  PA =~ PA_1 + PA_2 + PA_3 + PA_4 + PA_5 + PA_6 + PA_7 + PA_8 + PA_9 + PA_10
  NegAffect =~ NA_1 + NA_2 + NA_3 + NA_4 + NA_5 + NA_6 + NA_7 + NA_8 + NA_9 + NA_10
  Willingness =~ AIUse_1 + AIUse_2 + AIUse_3 + AIUse_4 + AIUse_5 + AIUse_6
  Objection   =~ AIObj_1 + AIObj_2 + AIObj_3 + AIObj_4 + AIObj_5 + AIObj_6 + AIObj_7 + AIObj_8
'

fit_full <- cfa(full_model, data = CleanedV3, estimator = "MLR", std.lv = TRUE)

print(summary(fit_full, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE))


# -------------------------------------------------
# 3. MULTI-GROUP CONFIGURAL CFA WITH WLSMV
# -------------------------------------------------
# -------------------------------------------------
# Auto-collapse empty categories for ordinal items
# -------------------------------------------------

collapse_empty_categories <- function(df, items, group_var) {
  for (item in items) {
    freq_table <- table(df[[item]], df[[group_var]])
    empty_levels <- as.numeric(rownames(freq_table)[apply(freq_table, 1, function(x) any(x == 0))])
    if (length(empty_levels) > 0) {
      # collapse the empty level into nearest existing level
      for (lev in empty_levels) {
        nearest <- ifelse(lev == min(df[[item]], na.rm = TRUE),
                          lev + 1,
                          lev - 1)
        df[[item]][df[[item]] == lev] <- nearest
      }
    }
    # ensure integer again
    df[[item]] <- as.integer(df[[item]])
  }
  return(df)
}

ordinal_items <- c("S1_1","S1_2","S1_3","S1_4","S1_5",
                   "HM_1","HM_2","HM_3","HM_4",
                   "PE_1","PE_2","PE_3","PE_4","PE_5",
                   "EE_1","EE_2","EE_3","EE_4",
                   "AIUse_1","AIUse_2","AIUse_3","AIUse_4","AIUse_5","AIUse_6",
                   "AIObj_1","AIObj_2","AIObj_3","AIObj_4","AIObj_5","AIObj_6",
                   "AIObj_7","AIObj_8")


# Apply to all ordinal items
CleanedV3 <- collapse_empty_categories(CleanedV3, ordinal_items, "LevelOfAutonomy")

# -------------------------------------------------
# Fit configural multi-group CFA using WLSMV
# -------------------------------------------------
# Use MLR instead of WLSMV (recommended for your data)
fit_config <- cfa(full_model,
                  data = CleanedV3,
                  group = "LevelOfAutonomy",
                  estimator = "MLR")

# Summary with fit measures and standardized loadings
print(summary(fit_config, fit.measures = TRUE, standardized = TRUE))

# =============================================
# 
# =============================================

install.packages("lme4")
install.packages("lmerTest")
install.packages("performance")
install.packages("dplyr")

library(lme4)       # mixed models
library(lmerTest)   # p-values and nice summary
library(performance) # easy ICC calculation
library(dplyr)

# Ensure Level-2 grouping variable is a factor
CleanedV3$LevelOfAutonomy <- as.factor(CleanedV3$LevelOfAutonomy)

# =============================================
# STEP 3: ICC(1) Computation
# =============================================

# Create scale scores (row means)
pa_items <- c("PA_1","PA_2","PA_3","PA_4","PA_5","PA_6","PA_7","PA_8","PA_9","PA_10")
na_items <- c("NA_1","NA_2","NA_3","NA_4","NA_5","NA_6","NA_7","NA_8","NA_9","NA_10")
CleanedV3$PA <- rowMeans(CleanedV3[pa_items], na.rm = TRUE)

CleanedV3$NegAffect <- rowMeans(CleanedV3[na_items], na.rm = TRUE)

CleanedV3$Willingness <- rowMeans(CleanedV3[c("AIUse_1","AIUse_2","AIUse_3","AIUse_4","AIUse_5","AIUse_6")], na.rm = TRUE)

CleanedV3$Objection <- rowMeans(CleanedV3[c("AIObj_1","AIObj_2","AIObj_3","AIObj_4","AIObj_5","AIObj_6","AIObj_7","AIObj_8")], na.rm = TRUE)

CleanedV3$PE <- rowMeans(CleanedV3[c("PE_1","PE_2","PE_3","PE_4","PE_5")], na.rm = TRUE)

CleanedV3$EE <- rowMeans(CleanedV3[c("EE_1","EE_2","EE_3","EE_4")], na.rm = TRUE)

CleanedV3$SI <- rowMeans(CleanedV3[c("S1_1","S1_2","S1_3","S1_4","S1_5")], na.rm = TRUE)

CleanedV3$HM <- rowMeans(CleanedV3[c("HM_1","HM_2","HM_3","HM_4")], na.rm = TRUE)

# Function to compute ICC(1) for any Level-1 variable
compute_icc <- function(var_name) {
  formula <- as.formula(paste(var_name, "~ 1 + (1 | LevelOfAutonomy)"))
  null_model <- lmer(formula, data = CleanedV3, REML = FALSE)
  icc_val <- icc(null_model)$ICC_unadjusted
  cat("ICC(1) for", var_name, "=", round(icc_val, 3), "\n")
  return(icc_val)
}

# Run ICC for all key Level-1 variables (outcomes + main mediators)
key_level1_vars <- c("PA", "NegAffect", "Willingness", "Objection",
                     "PE", "EE", "SI", "HM")

cat("=== ICC(1) RESULTS ===\n")
sapply(key_level1_vars, compute_icc)

# =============================================
# STEP 4: Unconditional (Null) Model for all outcomes
# =============================================

cat("\n=== NULL MODELS (Random Intercept Only) ===\n")

# Null model for Willingness
null_will <- lmer(Willingness ~ 1 + (1 | LevelOfAutonomy), data = CleanedV3)
summary(null_will)

# Null model for Objection
null_obj <- lmer(Objection ~ 1 + (1 | LevelOfAutonomy), data = CleanedV3)
summary(null_obj)

# Null model for Positive Affect (PA)
null_pa <- lmer(PA ~ 1 + (1 | LevelOfAutonomy), data = CleanedV3)
summary(null_pa)

# Null model for Negative Affect (NegAffect)
null_neg <- lmer(NegAffect ~ 1 + (1 | LevelOfAutonomy), data = CleanedV3)
summary(null_neg)

cat("\n=== NULL MODELS (Random Intercept Only) ===\n")

null_obj <- lmer(Objection ~ 1 + (1 | LevelOfAutonomy), data = CleanedV3)
summary(null_obj)

null_will <- lmer(Willingness ~ 1 + (1 | LevelOfAutonomy), data = CleanedV3)
print(summary(null_will))

# =============================================
# STEP 5: Random-Intercept Models (Fixed Slopes)
# =============================================

cat("\n=== RANDOM-INTERCEPT MODELS (Fixed Slopes) ===\n")

# Example 1: Level-1 main effects only (predicting Emotion from primary & secondary appraisal)
ri_emotion1 <- lmer(NegAffect ~ SI + HM + PE + EE + (1 | LevelOfAutonomy), 
                    data = CleanedV3)
summary(ri_emotion1)

# Example 2: Level-1 + Level-2 main effects (add VA levels as categorical predictor)
ri_emotion2 <- lmer(NegAffect ~ SI + HM + PE + EE + LevelOfAutonomy + 
                      (1 | LevelOfAutonomy), 
                    data = CleanedV3)
summary(ri_emotion2)

# Example 3: Predicting Willingness from Emotion (Level-1 only)
ri_will1 <- lmer(Willingness ~ PA + NegAffect + (1 | LevelOfAutonomy), 
                 data = CleanedV3)
summary(ri_will1)

# Example 4: Willingness with Level-2 VA effect
ri_will2 <- lmer(Willingness ~ PA + NegAffect + LevelOfAutonomy + 
                   (1 | LevelOfAutonomy), 
                 data = CleanedV3)
summary(ri_will2)

cat("\n=== RANDOM-INTERCEPT MODELS (Fixed Slopes) ===\n")

# 5.1 Emotion predicted by Primary + Secondary appraisal
ri_emotion <- lmer(NegAffect ~ SI + HM + PE + EE + (1 | LevelOfAutonomy), 
                   data = CleanedV3)
summary(ri_emotion)

# 5.2 Objection predicted by Emotion (main mediation path)
ri_obj_emotion <- lmer(Objection ~ PA + NegAffect + (1 | LevelOfAutonomy), 
                       data = CleanedV3)
summary(ri_obj_emotion)

# 5.3 Objection predicted by Secondary appraisal (direct effect)
ri_obj_secondary <- lmer(Objection ~ PE + EE + (1 | LevelOfAutonomy), 
                         data = CleanedV3)
summary(ri_obj_secondary)

# 5.4 Full model: Objection from Emotion + Secondary appraisal
ri_obj_full <- lmer(Objection ~ PA + NegAffect + PE + EE + (1 | LevelOfAutonomy), 
                    data = CleanedV3)
summary(ri_obj_full)

# 5.5 Add Level-2 predictor (VA levels) to Objection model
ri_obj_va <- lmer(Objection ~ PA + NegAffect + PE + EE + LevelOfAutonomy + 
                    (1 | LevelOfAutonomy), 
                  data = CleanedV3)
summary(ri_obj_va)

# 5.6 Same for Willingness (for comparison)
ri_will_full <- lmer(Willingness ~ PA + NegAffect + PE + EE + LevelOfAutonomy + 
                       (1 | LevelOfAutonomy), 
                     data = CleanedV3)
print(summary(ri_will_full))

# =============================================
# STEP 6: Random-Slope Models
# =============================================

cat("\n=== RANDOM-SLOPE MODELS ===\n")

# Example 1: Random slopes for secondary appraisal → Emotion
rs_emotion <- lmer(NegAffect ~ PE + EE + (PE + EE | LevelOfAutonomy), 
                   data = CleanedV3, control = lmerControl(optimizer = "bobyqa"))

summary(rs_emotion)

# Example 2: Random slopes for Emotion → Objection (main mediation path)
rs_obj <- lmer(Objection ~ PA + NegAffect + (PA + NegAffect | LevelOfAutonomy), 
               data = CleanedV3, control = lmerControl(optimizer = "bobyqa"))

summary(rs_obj)

# Example 3: Random slopes for Emotion → Willingness
rs_will <- lmer(Willingness ~ PA + NegAffect + (PA + NegAffect | LevelOfAutonomy), 
                data = CleanedV3, control = lmerControl(optimizer = "bobyqa"))

summary(rs_will)

# Test whether slopes vary significantly across VA levels
cat("\nVariance of random slopes (significant = slopes differ across VA levels):\n")
print(VarCorr(rs_emotion))
print(VarCorr(rs_obj))
print(VarCorr(rs_will))

# =============================================
# STEP 7: Mediation Analysis
# =============================================

cat("\n=== MEDIATION ANALYSIS (using lmer) ===\n")
cat("NOTE: Standard MLM has limitations (conflated Between/Within effects, no easy indirect effects)\n")
cat("      → See Preacher et al. (2010). We use UMM (group-mean centering) as interim step.\n")

# Create group-mean centered Level-1 variables for UMM (unconflated model)
CleanedV3 <- CleanedV3 %>%
  group_by(LevelOfAutonomy) %>%
  mutate(
    PE_c = PE - mean(PE, na.rm = TRUE),
    EE_c = EE - mean(EE, na.rm = TRUE),
    PA_c = PA - mean(PA, na.rm = TRUE),
    NegAffect_c = NegAffect - mean(NegAffect, na.rm = TRUE)
  ) %>%
  ungroup()

# 1-1-1 Mediation example: PE → NegAffect → Objection (within-level only)
med_111 <- lmer(Objection ~ PA_c + NegAffect_c + PE_c + EE_c + (1 | LevelOfAutonomy), 
                data = CleanedV3)
summary(med_111)

# 2-1-1 Mediation example (top-down): VA level → Emotion → Objection
med_211 <- lmer(Objection ~ PA_c + NegAffect_c + LevelOfAutonomy + (1 | LevelOfAutonomy), 
                data = CleanedV3)
print(summary(med_211))

# =============================================
# STEP 8: Moderation Analysis
# =============================================

cat("\n=== MODERATION ANALYSIS ===\n")

# Within-level moderation (Level-1 × Level-1)
mod_within <- lmer(Objection ~ PA_c * NegAffect_c + (1 | LevelOfAutonomy), 
                   data = CleanedV3)
print(summary(mod_within))

# Cross-level moderation (VA moderating Level-1 slope)
mod_cross <- lmer(Objection ~ PA_c + NegAffect_c + LevelOfAutonomy + 
                    PA_c:LevelOfAutonomy + NegAffect_c:LevelOfAutonomy + 
                    (1 | LevelOfAutonomy), 
                  data = CleanedV3)
print(summary(mod_cross))

# Between-level moderation (Level-2 × Level-2) - example
mod_between <- lmer(Objection ~ LevelOfAutonomy * (PE + EE) + (1 | LevelOfAutonomy), 
                    data = CleanedV3)
print(summary(mod_between))

CleanedV3$PA_mean <- CleanedV3$PA
CleanedV3$PE_mean <- CleanedV3$PE
CleanedV3$SI_mean <- CleanedV3$SI
CleanedV3$EE_mean <- CleanedV3$EE
CleanedV3$HM_mean <- CleanedV3$HM
CleanedV3$NegAffect_mean <- CleanedV3$NegAffect
CleanedV3$Willingness_mean <- CleanedV3$Willingness
CleanedV3$Objection_mean <- CleanedV3$Objection

#CleanedV3 <- CleanedV3[, !colnames(CleanedV3) %in% c("PA","PE","SI","EE","HM","NegAffect","Willingness","Objection")]

# =============================================
# STEP 9: Combined Moderated-Mediation Models
# =============================================
install.packages("emmeans")
library(emmeans)
cat("\n=== STEP 9: COMBINED MODERATED-MEDIATION ===\n")

# Full moderated-mediation model (cross-level interaction)
# VA moderates the Emotion → Objection path
mod_med <- lmer(Objection ~ PA + NegAffect + PE + EE + 
                  LevelOfAutonomy + 
                  NegAffect:LevelOfAutonomy +   # cross-level moderation of b-path
                  (1 | LevelOfAutonomy), 
                data = CleanedV3, 
                control = lmerControl(optimizer = "bobyqa"))

print(summary(mod_med))

# Conditional indirect effects (simple slopes at each VA level)
cat("\nConditional slopes of NegAffect → Objection at each VA level:\n")
emmeans::emtrends(mod_med, var = "NegAffect", specs = ~ LevelOfAutonomy)

# Figure 3: Conditional slopes of NegAffect → Objection at each VA level

library(ggplot2)

slope_data <- data.frame(
  level    = factor(c("Full\nAutonomy","High\nAutonomy","Low\nAutonomy","No\nAutonomy"),
                    levels = c("Full\nAutonomy","High\nAutonomy","Low\nAutonomy","No\nAutonomy")),
  slope    = c(0.712, 0.537, 0.490, 0.169),
  lower    = c(0.421, 0.320, 0.219, 0.030),
  upper    = c(1.004, 0.755, 0.761, 0.307),
  sig      = c(TRUE, FALSE, FALSE, TRUE)
)

fig3 <- ggplot(slope_data, aes(x = level, y = slope)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1, colour = "grey40") +
  geom_line(aes(group = 1), colour = "#2C3E50") +
  geom_point(aes(fill = sig), shape = 21, size = 4, colour = "black") +
  scale_fill_manual(values = c("TRUE" = "#2C3E50", "FALSE" = "white"),
                    labels = c("TRUE" = "p < .05", "FALSE" = "n.s."), name = NULL) +
  geom_text(aes(label = sprintf("b = %.3f", slope)), vjust = -1.2, size = 3) +
  scale_y_continuous(limits = c(-0.05, 1.15), breaks = seq(0, 1, 0.2),
                     name = "Conditional slope: Negative Affect \u2192 Objection") +
  labs(x = "Level of Variable Autonomy",
       title = "Figure 3. Conditional Slopes of Negative Affect \u2192 Objection by VA Level",
       subtitle = "95% CIs shown. Filled = p < .05; open = n.s.") +
  theme_classic(base_size = 11) +
  theme(legend.position = "bottom",
        panel.grid.major.y = element_line(colour = "grey92"))

ggsave("Figure3_ConditionalSlopes.tiff", plot = fig3,
       width = 8, height = 4, dpi = 300, compression = "lzw")

cat("Figure 3 saved: Figure3_ConditionalSlopes.tiff\n")

# Optional: Same for Willingness
mod_med_will <- lmer(Willingness ~ PA + NegAffect + PE + EE + 
                       LevelOfAutonomy + 
                       NegAffect:LevelOfAutonomy + 
                       (1 | LevelOfAutonomy), 
                     data = CleanedV3)
print(summary(mod_med_will))

# =============================================
# STEP 10: Model Comparison & Robustness Checks
# =============================================

cat("\n=== STEP 10: MODEL COMPARISON & ROBUSTNESS ===\n")

# 10.1 Information criteria (AIC, BIC, deviance)
cat("\nModel comparison (AIC / BIC):\n")
AIC(null_obj, ri_obj_full, rs_obj, mod_med)
BIC(null_obj, ri_obj_full, rs_obj, mod_med)

# 10.2 Sensitivity to centering (Grand-mean vs Group-mean)
# Re-create group-mean centered variables (already done) and grand-mean centered
CleanedV3 <- CleanedV3 %>%
  mutate(
    NegAffect_gmc = NegAffect - mean(NegAffect, na.rm = TRUE),
    PA_gmc = PA - mean(PA, na.rm = TRUE)
  )

# Compare model with group-mean centering vs grand-mean centering
mod_gmc <- lmer(Objection ~ PA + NegAffect + LevelOfAutonomy + 
                  NegAffect:LevelOfAutonomy + (1 | LevelOfAutonomy), 
                data = CleanedV3)

mod_gmc2 <- lmer(Objection ~ PA_gmc + NegAffect_gmc + LevelOfAutonomy + 
                   NegAffect_gmc:LevelOfAutonomy + (1 | LevelOfAutonomy), 
                 data = CleanedV3)

cat("\nGrand-mean vs Group-mean centering comparison:\n")
print(AIC(mod_gmc, mod_gmc2))
print(BIC(mod_gmc, mod_gmc2))


# 10.3 Multi-group equivalence tests (Manual modern approach)
# Configural, Metric, and Scalar invariance

# 1. Configural invariance
fit_config <- cfa(full_model, 
                  data = CleanedV3, 
                  group = "LevelOfAutonomy", 
                  estimator = "MLR")

# 2. Metric invariance (equal loadings)
fit_metric <- cfa(full_model, 
                  data = CleanedV3, 
                  group = "LevelOfAutonomy", 
                  estimator = "MLR",
                  group.equal = "loadings")

# 3. Scalar invariance (equal loadings + intercepts)
fit_scalar <- cfa(full_model, 
                  data = CleanedV3, 
                  group = "LevelOfAutonomy", 
                  estimator = "MLR",
                  group.equal = c("loadings", "intercepts"))

# 4. Compare the models
cat("\n=== MEASUREMENT INVARIANCE TEST RESULTS ===\n")
lavTestLRT(fit_config, fit_metric, fit_scalar)
print(lavTestLRT(fit_config, fit_metric, fit_scalar))

# 10.4 Power / sample-size note for Level-2 (VA levels)

print(nlevels(CleanedV3$LevelOfAutonomy))

sink() 














