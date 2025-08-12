library(dplyr)
library(ggplot2)
library(tidyverse)
library(cowplot)

if (!dir.exists("Manuscript_Vis")) {
  dir.create("Manuscript_Vis")
}

# Data folder containing all resulting datasets from the ABMs 
path = "Ts_Data"


# Load Data ---------------------------------------------------------------
# Read in all files resulting from the ABM
files <- list.files(path = path, pattern = "\\.csv$", full.names = TRUE)

# Data frame containing summary statistics for all experimental settings
df_full <- data.frame()

for (fname in files){
  
  # Read in single dataset
  AllSimResultsDF <- read.csv(fname)
  AllSimResultsDF
  
# Preprocessing
# Set colouring for the different scenarios
cols <- c(
  "SSP1-2.6" = "#009E73",
  "SSP2-4.5" = "#F0E442",
  "SSP5-8.5" = "#D55E00"
)

# Rename scenarios from good, ok, bad, to actual model names
AllSimResultsDF <- AllSimResultsDF %>%
  mutate(
    Scenario = recode(
      Scenario,
      "good"     = "SSP1-2.6",
      "ok"       = "SSP2-4.5",
      "bad"      = "SSP5-8.5",
      .default   = Scenario
    ),
    Scenario = factor(Scenario, levels = names(cols))
  )

# Combine
scale_cols  <- scale_color_manual(values = cols, breaks = names(cols), drop = FALSE)
scale_fills <- scale_fill_manual(values = cols,  breaks = names(cols), drop = FALSE)

# Extract the experimental setting of the current dataset from its file name
stem <- tools::file_path_sans_ext(basename(fname))
rx <- "^FOREC_(.*?)_ADOP_(TRUE|FALSE)_FODD_(TRUE|FALSE)_PROPCONS_([0-9.]+)_FORECERR_([0-9.]+)$"
m <- regexec(rx, stem)
mm <- regmatches(stem, m)[[1]]
if (length(mm) == 0) stop("Filename doesn't match expected pattern: ", stem)

Forecasts                  <- mm[2]
Set_Adoption               <- (mm[3] == "TRUE")
Supplemental_fodder        <- (mm[4] == "TRUE")
Starting_Prop_Conservation <- as.numeric(mm[5])
ForecastError              <- as.numeric(mm[6])

# Set this as title string to use in later plots
title_string <- paste0(
  "Forecast: ", Forecasts,
  ", Adoption: ", ifelse(Set_Adoption, "Yes", "No"),
  ", Supplemental Fodder: ", ifelse(Supplemental_fodder, "Yes", "No"),
  ", Starting Prop. Conservation: ", Starting_Prop_Conservation,
  ", Forecast Error: ", ForecastError
)
title_string

# Add the experimental settings as columns
AllSimResultsDF$Forecasts                  <- Forecasts
AllSimResultsDF$Set_Adoption               <- Set_Adoption
AllSimResultsDF$Supplemental_fodder        <- Supplemental_fodder
AllSimResultsDF$Starting_Prop_Conservation <- Starting_Prop_Conservation
AllSimResultsDF$ForecastError              <- ForecastError

# Vis 1: ABM Timeseries ---------------------------------------------------

# Summarize the different runs per scenario and timestep
SummaryStats <- AllSimResultsDF %>%
  group_by(
    Scenario, Time,
    Forecasts, Set_Adoption, Supplemental_fodder,
    Starting_Prop_Conservation, ForecastError
  ) %>%
  summarize(
    across(
      c(AvgMoney, AvgCows, AvgGrass, Gini),
      list(mean = ~mean(.x, na.rm = TRUE), sd = ~sd(.x, na.rm = TRUE)),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )

# Append to the combined df
df_full <- rbind(df_full, SummaryStats)

# Facet plots
p1 <- ggplot(SummaryStats, aes(Time, AvgMoney_mean, color = Scenario, fill = Scenario)) +
  geom_line() +
  geom_ribbon(aes(ymin = AvgMoney_mean - AvgMoney_sd, ymax = AvgMoney_mean + AvgMoney_sd),
              alpha = 0.2, color = NA) +
  ggtitle("Money") + scale_cols + scale_fills + labs(color = "Scenario", fill = "Scenario")

p2 <- ggplot(SummaryStats, aes(Time, AvgCows_mean, color = Scenario, fill = Scenario)) +
  geom_line() +
  geom_ribbon(aes(ymin = AvgCows_mean - AvgCows_sd, ymax = AvgCows_mean + AvgCows_sd),
              alpha = 0.2, color = NA) +
  ggtitle("Cows") + scale_cols + scale_fills + labs(color = "Scenario", fill = "Scenario")

p3 <- ggplot(SummaryStats, aes(Time, Gini_mean, color = Scenario, fill = Scenario)) +
  geom_line() +
  geom_ribbon(aes(ymin = Gini_mean - Gini_sd, ymax = Gini_mean + Gini_sd),
              alpha = 0.2, color = NA) +
  ggtitle("Gini") + scale_cols + scale_fills + labs(color = "Scenario", fill = "Scenario")

p4 <- ggplot(SummaryStats, aes(Time, AvgGrass_mean, color = Scenario, fill = Scenario)) +
  geom_line() +
  geom_ribbon(aes(ymin = AvgGrass_mean - AvgGrass_sd, ymax = AvgGrass_mean + AvgGrass_sd),
              alpha = 0.2, color = NA) +
  ggtitle("Grass") + scale_cols + scale_fills + labs(color = "Scenario", fill = "Scenario")


# Helper function to create the facet plot with one legend only
get_legend_plot <- function(plot) {
  legend <- get_legend(
    plot + theme(legend.position = "bottom", legend.justification = "center")
  )
  return(legend)
}

# Extract legend from one plot
shared_legend <- get_legend_plot(p1)

# Remove legends from all plots
p1 <- p1 + theme(legend.position = "none")
p2 <- p2 + theme(legend.position = "none")
p3 <- p3 + theme(legend.position = "none")
p4 <- p4 + theme(legend.position = "none")

# Add burn in tag
burn_in_line <- geom_vline(xintercept = 71, linetype = "dashed", color = "black", linewidth = 0.8)
burn_in_text <- annotate("text", x = 37, y = Inf, label = "Model \n Burn-In",
                         vjust = 1.5, hjust = 0.5, size = 5, fontface = "bold")

p1 <- p1 + burn_in_line + burn_in_text
p2 <- p2 + burn_in_line + burn_in_text
p3 <- p3 + burn_in_line + burn_in_text
p4 <- p4 + burn_in_line + burn_in_text

# Combine plots without legends
plots_grid <- plot_grid(p1, p2, p3, p4, labels = NULL)

# Add title
title <- ggdraw() + 
  draw_label(title_string, fontface = "bold", size = 16, x = 0.5, hjust = 0.5)

# Stack title, plots, and legend
ABMTs_res <- plot_grid(
  title,
  plots_grid,
  shared_legend,
  ncol = 1,
  rel_heights = c(0.05, 1, 0.1)
)

print(ABMTs_res)

ggsave(
  filename = paste0("Manuscript_Vis_/ABMTS_res_", fname, ".png"),
  plot = ABMTs_res,
  width = 10, height = 8, dpi = 300
)

# Vis 2: ABM Outcomes -------------------------------------------------------

# Select timesteps from after the equilibrium is reached
# (Timeseries plot shows approx. last 30 ts)
post_burnin_df <- AllSimResultsDF %>%
  group_by(Scenario, Rep) %>%
  filter(Time >= max(Time) - 29) %>% # final 30 ts
  ungroup()

# Define main outcome variables
vars_to_normalize <- c("AvgMoney", "AvgCows", "AvgGrass", "Gini")

# Compute SSP2-4.5 median (normalizing by it / benchmark)
ok_median <- post_burnin_df %>%
  filter(Scenario == "SSP2-4.5") %>%
  summarise(across(all_of(vars_to_normalize), median, na.rm = TRUE))

# Normalize by dividing each variable by SSP2-4.5 median
normalized_df <- post_burnin_df %>%
  mutate(across(all_of(vars_to_normalize),
                ~ . / ok_median[[cur_column()]],
                .names = "{.col}_normalized"))

# Reshape for plotting
plot_df <- normalized_df %>%
  pivot_longer(cols = ends_with("_normalized"),
               names_to = "Variable",
               values_to = "Value") %>%
  mutate(Variable = str_remove(Variable, "_normalized"),
         Scenario = factor(Scenario, levels = c("SSP1-2.6", "SSP2-4.5", "SSP5-8.5")))


# Coloured boxplot
ABMres_boxplot <- ggplot(plot_df, aes(x = Scenario, y = Value, fill = Scenario)) +
  geom_boxplot() +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray30") +
  facet_wrap(~ Variable, scales = "free_y") +
  scale_fill_manual(values = c(
    "SSP1-2.6"     = "#009E73",
    "SSP2-4.5"       = "#F0E442",
    "SSP5-8.5"      = "#D55E00"
  )) +
  labs(title = title_string,
       x = "Scenario",
       y = "Normalized Outcome (SSP2-4.5 median = 1)") +
  theme(legend.position = "none")

print(ABMres_boxplot)

ggsave(
  filename = paste0("Manuscript_Vis/ABMres_boxplot_", fname, ".png"),
  plot = ABMres_boxplot,
  width = 10, height = 8, dpi = 300
)

# Coloured violin plot
ABMres_violin <- ggplot(plot_df, aes(x = Scenario, y = Value, fill = Scenario)) +
  geom_violin() +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
  facet_wrap(~ Variable, scales = "free_y") +
  labs(title = title_string,
       x = "Scenario",
       y = "Normalized Outcome Value (SSP2-4.5 median = 1)")+
  scale_fill_manual(values = c(
    "SSP1-2.6"     = "#009E73",
    "SSP2-4.5"       = "#F0E442",
    "SSP5-8.5"      = "#D55E00"
  )) + theme(legend.position = "none")

print(ABMres_violin)

ggsave(
  filename = paste0("Manuscript_Vis/ABMres_violin_", fname, ".png"),
  plot = ABMres_violin,
  width = 10, height = 8, dpi = 300
)
}

# Save combined dataframe of all outcome datasets
write.csv(
  df_full,
  file = paste0("CombinedData.csv"),
  row.names = FALSE
)

# Vis 3: Effects of conservation interventions on outcomes --------------------
# conservation interventions in different extents

df_full <- read_csv("CombinedData.csv")

# Filter to forecasts for conservationists or no one (leave out strategies such as rich / poor and forecast accuracy for now)
df_full <- df_full %>%
  dplyr::filter(Forecasts %in% c("No_one", "Everyone")) %>% # @COSIMA: Change Everyone to Conservation as soon as data is available!!!
  dplyr::filter(Set_Adoption == TRUE) %>% # allows to look at different intervention extents
  dplyr::filter(ForecastError == 0)

# take median across the post burn in time steps per setting
# @Cosima ADD SD / IQR! (and also normalize!)
df_full <- df_full %>%
  dplyr::group_by(Scenario, Forecasts, Set_Adoption, Supplemental_fodder, 
                  Starting_Prop_Conservation, ForecastError) %>%
  summarise(across(ends_with("_mean"), ~ mean(.x, na.rm = TRUE)), .groups = "drop")

vars_to_normalize <- c("AvgMoney_mean", "AvgCows_mean", "AvgGrass_mean", "Gini_mean")
var_order <- c("AvgCows_mean", "AvgGrass_mean", "AvgMoney_mean", "Gini_mean")
scenario_order <- c("SSP1-2.6", "SSP2-4.5", "SSP5-8.5")

df_long <- df_full %>%
  mutate(
    # @COSIMA: Subsitute "Everyone" for "Conservation" as soon as data is available!!!
    Forecasts_group = factor(Forecasts, levels = c("No_one", "Everyone")),
    Scenario = factor(Scenario, levels = scenario_order)
  ) %>%
  pivot_longer(
    cols = all_of(vars_to_normalize),
    names_to = "Variable",
    values_to = "RawValue"
  )

# normalize by no intervention per scenario x main variable
baseline_df <- df_long %>%
  # COSIMA: change to NO INTERVENTION Scenario as soon as respective data is available!!
  # i.e., Supplemental_fodder == FALSE, Forecasts_group == "No_one"
  filter(Supplemental_fodder == TRUE, Forecasts_group == "No_one") %>%
  group_by(Scenario, Variable) %>%
  summarise(
    Baseline = median(RawValue, na.rm = TRUE),
    .groups = "drop"
  )

df_long <- df_long %>%
  left_join(baseline_df, by = c("Scenario", "Variable")) %>%
  mutate(Value = RawValue / Baseline) %>%
  filter(is.finite(Value))

ggplot(df_long, aes(x = Starting_Prop_Conservation, y = Value,
                    color = Forecasts_group,
                    linetype = Supplemental_fodder)) +
  geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.3, color = "grey40") +
  geom_point(alpha = 0.35, size = 0.7) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 0.7) +
  facet_grid(rows = vars(Scenario), cols = vars(Variable), scales = "free_y") +
  scale_color_brewer(palette = "Set1", drop = FALSE, name = "Forecasts") +
  scale_linetype_manual(values = c("solid", "dotted"), drop = FALSE, name = "Supplemental fodder") +
  theme(
    strip.background = element_rect(fill = "grey90"),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    panel.spacing.x = unit(1.4, "lines")
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(
    x = "Proportion Conservation (PropCons)",
    y = "Value", # (normalized to baseline = median of No fodder + No forecasts in same Scenario & Variable)
    title = "PropCons vs. Main Vars (Scenario-specific baseline)"
  )

df_long <- df_long %>%
  mutate(
    Variable = factor(Variable, levels = var_order),
    Scenario = factor(Scenario, levels = scenario_order)
  )

ggplot(df_long, aes(x = Starting_Prop_Conservation, y = Value,
                    color = Forecasts_group, linetype = Supplemental_fodder)) +
  geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.3, color = "grey40") +
  geom_point(alpha = 0.35, size = 0.7) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 0.7) +
  facet_wrap(vars(Scenario, Variable),
             scales = "free_y",        # <- frees y per panel
             nrow = length(scenario_order)) +  # 3 rows (one per scenario)
  scale_color_brewer(palette = "Set1", drop = FALSE, name = "Forecasts") +
  scale_linetype_manual(values = c("solid", "dotted"), drop = FALSE, name = "Supplemental fodder") +
  theme(strip.background = element_rect(fill = "grey90"),
        strip.text = element_text(face = "bold"),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        panel.spacing.x = unit(1.4, "lines")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Proportion Conservation (PropCons)",
       y = "Value (normalized to scenario × variable baseline)",
       title = "PropCons vs. Main Vars (per-panel y-scales)")


# Vis 4: Impact of programmatic decisions on outcomes (SSP2-4.5) --------------
# Free Adoption
# @COSIMA: find unique names for dfs per plot to ensure no overlap

df_full <- read_csv("CombinedData.csv")

# Determine outcome variables of interest
main_vars <- c(
  "AvgMoney_mean",
  "AvgCows_mean",
  "Gini_mean",
  "AvgGrass_mean",
  "Starting_Prop_Conservation"
)

# Compute bottom baseline per facet, i.e., median of outcome variable under 
# no forecasts and respective supplemental fodder setting
baselines <- df_full %>%
  dplyr::filter(Scenario == "SSP2-4.5",
         Set_Adoption == TRUE, # @COSIMA: SWITCH TO FALSE once data is available!!!
         Forecasts == "Everyone") %>%  # @COSIMA: SWITCH TO No_one once data is available!!!
  dplyr::select(Supplemental_fodder, all_of(main_vars)) %>%
  pivot_longer(cols = all_of(main_vars),
               names_to = "Variable", values_to = "Value") %>%
  group_by(Supplemental_fodder, Variable) %>%
  summarise(median_baseline = median(Value, na.rm = TRUE), .groups = "drop")

plot_df <- df_full %>%
  dplyr::filter(Scenario == "SSP2-4.5",
         Set_Adoption == TRUE) %>% # COSIMA: SWITCH TO FALSE once data is available!!!
  dplyr::select(Scenario, Forecasts, Supplemental_fodder, all_of(main_vars)) %>%
  pivot_longer(cols = all_of(main_vars),
               names_to = "Variable", values_to = "Value") %>%
  left_join(baselines, by = c("Supplemental_fodder", "Variable")) %>%
  mutate(Value_norm = Value / median_baseline,
         Variable = factor(Variable,
                           levels = main_vars,
                           labels = c("AvgMoney_mean" = "AvgMoney",
                                      "AvgCows_mean" = "AvgCows",
                                      "Gini_mean" = "Gini",
                                      "AvgGrass_mean" = "AvgGrass",
                                      "Starting_Prop_Conservation" = "Start_Prop_Conservation")
         ),
         Supplemental_fodder = ifelse(Supplemental_fodder, "Fodder = TRUE", "Fodder = FALSE"))


Programm_Effect_ssp245 <- ggplot(plot_df, aes(x = Forecasts, y = Value_norm, fill = Forecasts)) +
  geom_boxplot() +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray30") +
  facet_wrap(~ Supplemental_fodder + Variable, scales = "free_y", ncol = length(main_vars)) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Impact of programmatic decisions on outcomes (SSP2-4.5)",
       x = "Forecast",
       y = "Normalized Value (median of no forecasts = 1)") +
  theme(legend.position = "none")

Programm_Effect_ssp245

ggsave(
  filename = paste0("Manuscript_Vis/Programm_Effect_ssp245.png"),
  plot = Programm_Effect_ssp245,
  width = 10, height = 8, dpi = 300
)

