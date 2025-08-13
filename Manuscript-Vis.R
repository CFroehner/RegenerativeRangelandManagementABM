library(dplyr)
library(ggplot2)
library(tidyverse)
library(cowplot)
library(ggh4x)
library(ggthemes)

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

# Append to the combined df
df_full <- rbind(df_full, AllSimResultsDF)

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
}

# Save combined dataframe of all outcome datasets post burn in phase
df_full_last30 <- df_full %>%
  ungroup() %>%
  filter(Time > max(Time) - 30)
  
  df_full %>%
  dplyr::filter(Time > max(Time) - 30) %>%
  select(-c(TotalCows, TotalGrass, Time, Rep))


write.csv(
  df_full_last30,
  file = "CombinedData_PostBurnIN.csv",
  row.names = FALSE
)


# Exp 1 -------------------------------------------------------------------
# @Cosima / Matt: Check whether any differences between these 2
# df_Exp1 <- read_csv("Ts_Data/FOREC_No_one_ADOP_FALSE_FODD_TRUE_PROPCONS_0.1_FORECERR_0.csv")
# df_Exp1 <- read_csv("Ts_Data/FOREC_No_one_ADOP_FALSE_FODD_FALSE_PROPCONS_0.1_FORECERR_0.csv")

# @Cosima / Matt: Explain y axis in these plots..?
df_Exp1 <- read_csv("CombinedData_PostBurnIN.csv")
df_Exp1 <- df_Exp1 %>%
  dplyr::filter(
    Forecasts == "No_one",
    Set_Adoption == TRUE,
    # Supplemental_fodder == TRUE, # @Matt:No real difference, would therefore go with FALSE?
    Supplemental_fodder == FALSE,
    Starting_Prop_Conservation == 0.1,
    ForecastError==0
  )

outcome_vars <- c("AvgMoney", "AvgCows", "AvgGrass", "Gini")

# Normalize both mean and sd columns by median outcome values in SSP2-4.5 scenario
ok_median <- df_Exp1 %>%
  filter(Scenario == "SSP2-4.5") %>%
  summarise(across(all_of(outcome_vars), median, na.rm = TRUE))

normalized_df <- df_Exp1 %>%
  mutate(across(all_of(outcome_vars),
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
Exp1_boxplot <- ggplot(plot_df, aes(x = Scenario, y = Value, fill = Scenario)) +
  geom_boxplot() +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray30") +
  facet_wrap(~ Variable, scales = "free_y",
             labeller = labeller(Variable = function(x) sub("^Avg", "", x))
             ) +
  scale_fill_manual(values = c(
    "SSP1-2.6"     = "#009E73",
    "SSP2-4.5"       = "#F0E442",
    "SSP5-8.5"      = "#D55E00"
  )) +
  labs(
    # title = "Outcomes under Baseline Setting (no forecasts, no supplemental food, 10% conservation) Per Climate Scenario",
       x = "Scenario",
       y = "Normalized Outcome (SSP2-4.5 median = 1)") +
  ggthemes::theme_clean()+
  theme(legend.position = "none", strip.text = element_text(size =14,face="bold"),
        axis.text=element_text(size=14),axis.title=element_text(size=16))


Exp1_boxplot

ggsave(
  filename = paste0("Manuscript_Vis/Exp1_boxplot.png"),
  plot = Exp1_boxplot,
  width = 10, height = 8, dpi = 300
)

# Coloured violin plot
# Exp1_violin <- ggplot(plot_df, aes(x = Scenario, y = Value, fill = Scenario)) +
#   geom_violin() +
#   geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
#   facet_wrap(~ Variable, scales = "free_y") +
#   labs(title = "Normalized Outcomes for No Intervention Setting",
#        x = "Scenario",
#        y = "Normalized Outcome Value (SSP2-4.5 median = 1)")+
#   scale_fill_manual(values = c(
#     "SSP1-2.6"     = "#009E73",
#     "SSP2-4.5"       = "#F0E442",
#     "SSP5-8.5"      = "#D55E00"
#   )) + theme(legend.position = "none")
# 
# print(Exp1_violin)
# 
# ggsave(
#   filename = paste0("Manuscript_Vis/Exp1_violin.png"),
#   plot = Exp1_violin,
#   width = 10, height = 8, dpi = 300)


# Exp 2 -------------------------------------------------------------------
# Effects of conservation interventions on outcomes (conservation interventions 
# in different extents)
# @COSIMA/MATT: Decide on type of interpolation, and error bars (max, min; 2SD; 1 SD etc.)
# @COSIMA: consider normalizing by values per column?? Make y axis consistent wihtin columns

df_Exp2 <- read_csv("CombinedData_PostBurnIN.csv") %>%
  dplyr::filter(
    Forecasts %in% c("No_one", "Conservation"),
    Set_Adoption == TRUE,
    Supplemental_fodder %in% c(TRUE, FALSE),
    Starting_Prop_Conservation %in% c(0.1, 0.25, 0.5, 0.75, 0.9),
    ForecastError == 0
  )
outcome_vars <- c("AvgMoney", "AvgCows", "AvgGrass", "Gini")

df_long <- df_Exp2 %>%
  mutate(
    Forecasts_group = factor(Forecasts, levels = c("No_one", "Conservation")),
    Scenario = factor(Scenario)
  ) %>%
  pivot_longer(
    cols = all_of(outcome_vars),
    names_to = "Variable",
    values_to = "RawValue"
  )

# normalize by no intervention (no fodder, no forecast) per scenario and main variable
baseline_df <- df_long %>%
  dplyr::filter(Scenario == "SSP2-4.5") %>%
  filter(Supplemental_fodder == FALSE, Forecasts_group == "No_one") %>%
  group_by(Scenario, Variable) %>%
  summarise(
    Baseline = median(RawValue, na.rm = TRUE), # normalizing by median
    .groups = "drop"
  )

baseline_df <- df_long %>%
  filter(Supplemental_fodder == FALSE, Forecasts_group == "No_one") %>%
  group_by(Variable) %>%
  summarise(
    Baseline = median(RawValue, na.rm = TRUE), # normalizing by median
    .groups = "drop"
  )

df_long <- df_long %>%
  left_join(baseline_df, by = c("Variable")) %>%
  mutate(Value = RawValue / Baseline) %>%
  filter(is.finite(Value))

df_long

## Option 1 using LOESS------------
# Exp2_Loess <- ggplot(df_long, aes(
#   x = Starting_Prop_Conservation, y = Value,
#   color = interaction(Forecasts_group, Supplemental_fodder, sep = ".", drop = TRUE)
# )) +
#   geom_hline(yintercept = 1, linetype = "dotted", linewidth = 0.3, color = "grey40") +
#   
#   # # error bars at observed x: mean ± 2 SD
#   # stat_summary(
#   #   fun.data = mean_sdl, fun.args = list(mult = 2),
#   #   geom = "errorbar", width = 0.02, linewidth = 0.6, lineend = "round",
#   #   na.rm = TRUE
#   # ) +
#   
#   # error bars at observed x: mean ± 1 SD
#   stat_summary(
#     fun.data = mean_sdl, fun.args = list(mult = 1),
#     geom = "errorbar", width = 0.02, linewidth = 0.6, lineend = "round",
#     na.rm = TRUE
#   ) +
#   
#   stat_summary(fun = mean, geom = "point", size = 1.2, na.rm = TRUE) +
#   
#   # error bars at observed x: mean ± 1 SD
#   # stat_summary(
#   #   fun.data = mean_sdl, fun.args = list(mult = 1),
#   #   geom = "errorbar", width = 0.02, linewidth = 0.6, lineend = "round",
#   #   na.rm = TRUE
#   # ) +
#   
#   # LOESS on the per-x group means (weighted by n) — inherits the combined color mapping
#   geom_smooth(
#     data = df_long %>%
#       group_by(
#         Scenario, Variable, Starting_Prop_Conservation,
#         Forecasts_group, Supplemental_fodder
#       ) %>%
#       summarise(mean = mean(Value, na.rm = TRUE),
#                 n    = dplyr::n(), .groups = "drop"),
#     aes(y = mean, weight = n),
#     method = "loess", se = TRUE, linewidth = 0.9, span = 0.75
#   ) +
#   
#   ggh4x::facet_grid2(
#     rows = vars(Scenario),
#     cols = vars(Variable),
#     scales = "free_y",
#     independent = "y",
#     labeller = labeller(
#       Scenario = label_value,
#       Variable = function(x) sub("^Avg", "", x)
#     )
#   ) +
#   theme(
#     strip.background = element_rect(fill = "grey90"),
#     strip.text = element_text(face = "bold"),
#     strip.placement = "outside",
#     panel.grid.minor = element_blank(),
#     legend.position = "bottom",
#     panel.spacing.x = unit(1.4, "lines"),
#     axis.text.x = element_text(angle = 45, hjust = 1)
#   ) +
#   labs(
#     x = "Proportion Conservation",
#     y = "Value",
#     title = "Effects of Conservation Interventions under Different Climate Scenarios"
#   ) +
#   scale_color_manual(
#     name   = "Forecasts × Fodder",
#     breaks = c("Conservation.TRUE", "Conservation.FALSE",
#                "No_one.TRUE",      "No_one.FALSE"),
#     labels = c("Conservation + fodder",
#                "Conservation + no fodder",
#                "No one + fodder",
#                "No one + no fodder"),
#     values = c(
#       "Conservation.TRUE"  = "#1b9e77",
#       "Conservation.FALSE" = "#d95f02",
#       "No_one.TRUE"        = "#7570b3",
#       "No_one.FALSE"       = "#e7298a"
#     )
#   ) +
#   guides(color = guide_legend(order = 1))
# 
# Exp2_Loess
# 
# ggsave(
#   filename = "Manuscript_Vis/Exp2_Loess.png",
#   plot = Exp2_Loess,
#   width = 10, height = 8, dpi = 300
# )

## Option 2 with Linear Interpolation-------
Exp2_LinInt <- ggplot(df_long, aes(
  x = Starting_Prop_Conservation, y = Value,
  color = interaction(Forecasts_group, Supplemental_fodder, sep = ".", drop = TRUE)
)) +
  geom_hline(yintercept = 1, linetype = "dashed", linewidth = 1, color = "#525252") +
  
  # # error bars at observed x: mean ± 2 SD
  # stat_summary(
  #   fun.data = mean_sdl, fun.args = list(mult = 2),
  #   geom = "errorbar", width = 0.02, linewidth = 0.6, lineend = "round",
  #   na.rm = TRUE
  # ) +
  
  # error bars at observed x: mean ± 1 SD
  stat_summary(
    fun.data = mean_sdl, fun.args = list(mult = 1),
    geom = "errorbar", width = 0.02, linewidth = 0.6, lineend = "round",
    na.rm = TRUE
  ) +
  
  
  # mean points
  stat_summary(fun = mean, geom = "point", size = 1.2, na.rm = TRUE) +
  # piecewise-linear interpolation between means
  stat_summary(fun = mean, geom = "line", linewidth = 0.9, na.rm = TRUE) +
  
  ggh4x::facet_grid2(
    rows = vars(Scenario),
    cols = vars(Variable),
    scales = "free_y",
    independent = "y",
    labeller = labeller(
      Scenario = label_value,
      Variable = function(x) sub("^Avg", "", x)
    )
  ) +
  theme(
    strip.background = element_rect(fill = "grey90"),
    strip.text = element_text(face = "bold"),
    strip.placement = "outside",
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    panel.spacing.x = unit(1.4, "lines"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    x = "Proportion of communities in conservation",
    y = "Normalized outcome",
    # title = "Effects of Conservation Interventions under Different Climate Scenarios"
  ) +
  scale_color_manual(
    name   = "Conservation program add-on",
    breaks = c("Conservation.TRUE", "Conservation.FALSE",
               "No_one.TRUE",      "No_one.FALSE"),
    labels = c("Forecast + fodder",
               "Forecast only",
               "Fodder only",
               "Neither"),
    values = c(
      "Conservation.TRUE"  = "#1b9e77",
      "Conservation.FALSE" = "#d95f02",
      "No_one.TRUE"        = "#7570b3",
      "No_one.FALSE"       = "#e7298a"
    )
  ) +
  #scale_y_continuous(n.breaks=4)+
  guides(color = guide_legend(order = 1))  +
  ggthemes::theme_clean()+
  theme(legend.position = "bottom", strip.text = element_text(size =14,face="bold"), panel.spacing = unit(2, "lines"),
        axis.text=element_text(size=14),axis.title=element_text(size=16))

Exp2_LinInt

ggsave(
  filename = paste0("Manuscript_Vis/Exp2_LinInt.png"),
  plot = Exp2_LinInt,
  width = 10, height = 8, dpi = 300
)

# Exp 3 -------------------------------------------------------------------

# Impact of programmatic decisions on outcomes
df_Exp3 <- read_csv("CombinedData_PostBurnIN.csv") 
df_Exp3 <- df_Exp3 %>%
  dplyr::filter(
    Forecasts %in% c("No_one", "Conservation"),
    Set_Adoption == FALSE,
    Supplemental_fodder %in% c(TRUE, FALSE),
    # Starting_Prop_Conservation == 0.1,
    Starting_Prop_Conservation == 0.25,
    ForecastError==0
  )

# Outcome var of interest
outcome_vars <- c("PropCons")

df_long <- df_Exp3 %>%
  mutate(
    Forecasts_group = factor(Forecasts, levels = c("No_one", "Conservation")),
    Scenario = factor(Scenario)
  ) %>%
  pivot_longer(
    cols = all_of(outcome_vars),
    names_to = "Variable",
    values_to = "RawValue"
  )

# normalize prop cons by no intervention (no fodder, no forecast) per scenario
baseline_df <- df_long %>%
  filter(Supplemental_fodder == FALSE, Forecasts_group == "No_one") %>%
  group_by(Scenario, Variable) %>%
  summarise(
    Baseline = median(RawValue, na.rm = TRUE), # normalizing by median
    .groups = "drop"
  )

plot_df <- df_long %>%
  filter(Variable == "PropCons") %>%
  left_join(baseline_df, by = c("Scenario", "Variable")) %>%
  mutate(
    Value = RawValue / Baseline,
    Variable = factor(Variable, levels = "PropCons", labels = "Proportion Conservation"),
    Forecasts2 = dplyr::recode(Forecasts, Conservation = "Conservation", .default = Forecasts),
    Forecasts2 = factor(Forecasts2, levels = c("No_one", "Conservation")),
    FodderFacet = ifelse(Supplemental_fodder, "Fodder = TRUE", "Fodder = FALSE")
  )

plot_df2 <- plot_df %>%
  mutate(
    Combo = interaction(Forecasts2, Supplemental_fodder, sep = ".", drop = TRUE)
  )

Exp3_plot <- ggplot(plot_df2, aes(x = Forecasts2, y = Value, fill = Combo)) +
  geom_boxplot() +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray30") +
  facet_wrap(~ Scenario, nrow = 1) + 
  scale_fill_manual(
    name   = "Conservation program add-on",
    breaks = c("Conservation.TRUE", "Conservation.FALSE",
               "No_one.TRUE",       "No_one.FALSE"),
    labels = c("Forecast + fodder",
               "Forecast only",
               "Fodder only",
               "Neither"),
    values = c(
      "Conservation.TRUE"  = "#1b9e77",
      "Conservation.FALSE" = "#d95f02",
      "No_one.TRUE"        = "#7570b3",
      "No_one.FALSE"       = "#e7298a"
    )
  ) +
  labs(
    #title = "Conservation Uptake Under Varying Conservation Interventions and Climate Scenarios",
    x = "Forecast",
    y = "Normalized conservation engagement"
  ) +
  # theme_bw() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  ) +
  theme(axis.title.x = element_blank(),
          axis.text.x  = element_blank(),
          axis.ticks.x = element_blank())+
  ggthemes::theme_clean()+
  theme(legend.position = "bottom", strip.text = element_text(size =14,face="bold"), panel.spacing = unit(2, "lines"),axis.title.x = element_blank(),
        axis.text=element_text(size=14),axis.title=element_text(size=16),axis.text.x = element_blank(),axis.ticks.x = element_blank())
  
Exp3_plot

ggsave(
  filename = paste0("Manuscript_Vis/Exp3_plot.png"),
  plot = Exp3_plot,
  width = 10, height = 8, dpi = 300
)


# Exp 4 -------------------------------------------------------------------



