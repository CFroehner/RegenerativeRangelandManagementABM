library(dplyr)
library(ggplot2)
library(tidyverse)

if (!dir.exists("Manuscript_Vis")) {
  dir.create("Manuscript_Vis")
}

# Loading ABM results (for specific setting) ----------------------------------

# determine experimental setting of interest
exp_setting <- "no_intervention"
# exp_setting <- "forecast_distribution_poor"
# exp_setting <- "supplemental_fodder"

# Read in ABM results (resulting avg and single runs)
SummaryStats <- read.csv(paste0("Plots_Tables/SummaryStats_", exp_setting, ".csv"))
SummaryStats

AllSimResultsDF <- read.csv(paste0("Plots_Tables/AllSimResultsDF_", exp_setting, ".csv"))
AllSimResultsDF

# Table Prep --------------------------------------------------------------
# currently hard coded removal of burn in phase (final 30 ts included only)
post_burnin_df <- AllSimResultsDF %>%
  group_by(Scenario, Rep) %>%
  filter(Time >= max(Time) - 29) %>% # last 30 ts FOR TESTING, @COSIMA
  ungroup()

# Compute summary stats as for SummaryStats table but after hard-coded burn in
post_burnin_df_summary <- post_burnin_df %>%
  group_by(Scenario) %>%
  summarize(
    AvgMoney_mean = mean(AvgMoney),
    AvgMoney_sd = sd(AvgMoney),
    AvgCows_mean = mean(AvgCows),
    AvgCows_sd = sd(AvgCows),
    AverageGrass_mean = mean(AverageGrass),
    AverageGrass_sd = sd(AverageGrass),
    Gini_mean = mean(Gini),
    Gini_sd = sd(Gini),
    .groups = "drop"
  )

finalts_long <- post_burnin_df_summary %>%
  pivot_longer(
    cols = matches("_(mean|sd)$"),
    names_to = c("Variable", ".value"),
    names_pattern = "(.*)_(mean|sd)"
  )

# absolute
comp_abs <- ggplot(finalts_long, aes(x = Scenario, y = mean)) +
  geom_col(fill = "steelblue") +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2) +
  facet_wrap(~ Variable, scales = "free_y") +
  labs(title = "Faceted Summary by Variable",
       x = "Scenario", y = "Mean ± SD")

# ggsave(paste0("Manuscript_Vis/comp_abs_", exp_setting,".png"), plot = comp_abs, width = 10, height = 6, dpi = 300)


# Incl. 95% CI ------------------------------------------------------------

# Compute summary stats with 95% Confidence Intervals instead of SD
post_burnin_df_summary <- post_burnin_df %>%
  group_by(Scenario) %>%
  summarize(
    n = n(),
    AvgMoney_mean = mean(AvgMoney),
    AvgMoney_ci = 1.96 * sd(AvgMoney) / sqrt(n),
    AvgCows_mean = mean(AvgCows),
    AvgCows_ci = 1.96 * sd(AvgCows) / sqrt(n),
    AverageGrass_mean = mean(AverageGrass),
    AverageGrass_ci = 1.96 * sd(AverageGrass) / sqrt(n),
    Gini_mean = mean(Gini),
    Gini_ci = 1.96 * sd(Gini) / sqrt(n),
    .groups = "drop"
  )

# Pivot longer for plotting
finalts_long <- post_burnin_df_summary %>%
  pivot_longer(
    cols = matches("_(mean|ci)$"),
    names_to = c("Variable", ".value"),
    names_pattern = "(.*)_(mean|ci)"
  )

# Absolute comparison plot using 95% CI
comp_abs <- ggplot(finalts_long, aes(x = Scenario, y = mean)) +
  geom_col(fill = "steelblue") +
  geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci), width = 0.2) +
  facet_wrap(~ Variable, scales = "fixed") +
  coord_cartesian(ylim = c(-70, 70)) +
  labs(title = "Faceted Summary by Variable (95% CI)",
       x = "Scenario", y = "Mean ± 95% CI")

comp_abs

ggsave(paste0("Manuscript_Vis/comp_abs_", exp_setting,".png"), plot = comp_abs, width = 10, height = 6, dpi = 300)

# Compute percent change relative to baseline using CI
baseline_means <- finalts_long %>%
  filter(Scenario == "baseline") %>%
  select(Variable, baseline_mean = mean)

finalts_pct <- finalts_long %>%
  left_join(baseline_means, by = "Variable") %>%
  mutate(
    pct_change = 100 * (mean - baseline_mean) / baseline_mean,
    pct_ci = 100 * ci / baseline_mean
  ) %>%
  filter(Scenario != "baseline")

# Fix order for plotting
finalts_pct$Scenario <- factor(finalts_pct$Scenario, levels = c("bad", "ok", "good"))

# Relative percentage change plot with 95% CI
comp_rel <- ggplot(finalts_pct, aes(x = Scenario, y = pct_change)) +
  geom_col(fill = "steelblue") +
  geom_errorbar(
    aes(ymin = pct_change - pct_ci, ymax = pct_change + pct_ci),
    width = 0.2
  ) +
  facet_wrap(~ Variable, scales = "fixed") +
  coord_cartesian(ylim = c(-70, 70)) +
  labs(
    title = "Percentage Change from Baseline by Variable (95% CI)",
    x = "Scenario", y = "% Change from Baseline"
  )

comp_rel
ggsave(paste0("Manuscript_Vis/comp_rel_", exp_setting,".png"), plot = comp_rel, width = 10, height = 6, dpi = 300)

# Non-facetted ------------------------------------------------------------

# Combine Variable and Scenario for x-axis grouping
finalts_pct <- finalts_pct %>%
  mutate(VarScenario = interaction(Variable, Scenario, sep = "_"))

# Create a custom x-axis grouping variable for plotting with space between variables
finalts_pct <- finalts_pct %>%
  mutate(
    Variable = factor(Variable),
    Scenario = factor(Scenario),
    VariableScenario = paste(Variable, Scenario, sep = "_")
  )

# Create a position dodge object to keep bars for same variable close together
dodge <- position_dodge(width = 0.8)

# Plot
comp_rel_bar <- ggplot(finalts_pct, aes(x = Variable, y = pct_change, fill = Scenario)) +
  geom_col(position = dodge, width = 0.8) +
  geom_errorbar(
    aes(ymin = pct_change - pct_ci, ymax = pct_change + pct_ci),
    position = dodge,
    width = 0.2
  ) +
  scale_fill_manual(values = c("orange", "grey", "purple")) + 
  coord_cartesian(ylim = c(-70, 70)) +
  labs(
    title = "Percentage Change from Baseline by Variable and Scenario (95% CI)",
    x = "Variable",
    y = "% Change from Baseline"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank()
  )

comp_rel_bar
ggsave(paste0("Manuscript_Vis/comp_rel_", exp_setting,".png"), plot = comp_rel_bar, width = 10, height = 6, dpi = 300)









