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

# ABM Timeseries ---------------------------------------------------
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

ts_long <- SummaryStats %>%
  select(Scenario, Time,
         AvgMoney_mean, AvgMoney_sd,
         AvgCows_mean,  AvgCows_sd,
         AvgGrass_mean, AvgGrass_sd,
         Gini_mean,     Gini_sd) %>%
  pivot_longer(
    cols = -c(Scenario, Time),
    names_to = c("Variable", "stat"),
    names_pattern = "^(.*)_(mean|sd)$",
    values_to = "value"
  ) %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  mutate(Variable = sub("^Avg", "", Variable))

ts_long <- ts_long %>%
  mutate(Variable = factor(
    Variable,
    levels = c("Cows", "Grass", "Money", "Gini")
  ))

burn_in_line <- geom_vline(xintercept = 70, linetype = "dashed", color = "black", size = 0.8)
burn_in_text <- annotate("text", x = 35, y = Inf, label = "Model \n Burn-In",
                         vjust = 1.5, hjust = 0.5, size = 7, fontface = "bold")

ABMTs_res <- ggplot(
  ts_long,
  aes(x = Time, y = mean, color = Scenario, fill = Scenario)
) +
  geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd),
              alpha = 0.20, color = NA) +
  geom_line(linewidth = 0.9) +
  burn_in_line + 
  burn_in_text +
  ggh4x::facet_wrap2(
    ~Variable,
    scales = "free_y",
    ncol = 2,
    labeller = labeller(
      Variable = c(
        Cows = "Livestock",
        Grass = "Vegetation",
        Money = "Economic wellbeing",
        Gini = "Gini"))) +
  labs(
    x = "Timesteps",
    y = NULL,
    color = "Scenario", fill = "Scenario"
  ) +
  scale_cols + scale_fills +
  ggthemes::theme_clean() +
  theme(
    legend.position   = "bottom",
    strip.background  = element_rect(),
    strip.text        = element_text(face = "bold", size = 14),
    strip.placement   = "outside",
    panel.grid.minor  = element_blank(),
    panel.spacing     = unit(2, "lines"),
    axis.text.x       = element_text(angle = 45, hjust = 1),
    axis.text         = element_text(size = 14),
    axis.title        = element_text(size = 16)
  )

ABMTs_res<- plot_grid(ABMTs_res,
  ncol = 1, rel_heights = c(0.06, 1)
)

print(ABMTs_res)

ggsave(
  filename = paste0("Manuscript_Vis/", fname, ".png"),
  plot = ABMTs_res,
  width = 10, height = 8, dpi = 300
)

}

# Save combined dataframe of all outcome datasets post burn in phase
df_full_last30 <- df_full %>%
  ungroup() %>%
  filter(Time > max(Time) - 30) %>%
  select(-c(
    TotalCows,
    TotalGrass,
    Time, 
    Rep)) 


write.csv(
  df_full_last30,
  file = "CombinedData_PostBurnIN.csv",
  row.names = FALSE
)

# Exp 1 -------------------------------------------------------------------
df_Exp1 <- read_csv("CombinedData_PostBurnIN.csv")
df_Exp1 <- df_Exp1 %>%
  dplyr::filter(
    Forecasts == "No_one",
    Set_Adoption == TRUE,
    # Supplemental_fodder == TRUE,
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
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray30", linewidth = 0.9) +
  facet_wrap(~ Variable, scales = "free_y",
             # labeller = labeller(Variable = function(x) sub("^Avg", "", x)),
             labeller = labeller(
               Variable = c(
                 AvgMoney = "Economic wellbeing",
                 AvgCows = "Livestock",
                 AvgGrass = "Vegetation"
               )
             )) +
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

# normalize by no intervention (no fodder, no forecast) per main variable
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

# Linear Interpolation
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
    na.rm = TRUE, alpha=0.7
  ) +

  # mean points
  stat_summary(fun = mean, geom = "point", size = 2.2, na.rm = TRUE,  alpha=0.7) +
  # piecewise-linear interpolation between means
  stat_summary(fun = mean, geom = "line", linewidth = 0.9, na.rm = TRUE,  alpha=0.7) +
  
  ggh4x::facet_grid2(
    rows = vars(Variable),
    cols = vars(Scenario),
    scales = "free_y",
    independent = "y",
    labeller = labeller(
      # Scenario = label_value,
      # Variable = function(x) sub("^Avg", "", x)
        Scenario = c(
          "SSP1-2.6" = "SSP1-2.6\n(sustainability)",
          "SSP2-4.5" = "SSP2-4.5\n(middle-of-the-road)",
          "SSP5-8.5" = "SSP5-8.5\n(fossil fuel development)"
        ),
        Variable = c(
          AvgMoney = "Economic\nwellbeing",
          AvgCows = "Livestock",
          AvgGrass = "Vegetation"
        )
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
    name   = "Programmatic configuration",
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
        axis.text=element_text(size=14),axis.title=element_text(size=16)) +
  ggh4x::facetted_pos_scales(
    y = list(
      Variable == "AvgMoney" ~ scale_y_continuous(limits = c(-0.1, 3.5), n.breaks = 4),
      Variable == "AvgCows"  ~ scale_y_continuous(limits = c(-0.1, 2.6), n.breaks = 4),
      Variable == "AvgGrass" ~ scale_y_continuous(limits = c(0.2, 1.1), n.breaks = 4),
      Variable == "Gini"     ~ scale_y_continuous(limits = c(0.1, 1.2), n.breaks = 4)
    )
  )

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
    Scenario = factor(
      Scenario,
      levels = c("SSP1-2.6", "SSP2-4.5", "SSP5-8.5"),
      labels = c(
        "SSP1-2.6\n(sustainability)",
        "SSP2-4.5\n(middle-of-the-road)",
        "SSP5-8.5\n(fossil fuel development)"
      )
    )
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
    Variable = factor(Variable, levels = "PropCons", labels = "Conservation\nengagement"),
    Forecasts2 = dplyr::recode(Forecasts, Conservation = "Conservation", .default = Forecasts),
    Forecasts2 = factor(Forecasts2, levels = c("No_one", "Conservation")),
    FodderFacet = ifelse(Supplemental_fodder, "Fodder = TRUE", "Fodder = FALSE")
  )

plot_df2 <- plot_df %>%
  mutate(
    Combo = interaction(Forecasts2, Supplemental_fodder, sep = ".", drop = TRUE),
    Combo = factor(Combo, levels = c(
      "Conservation.TRUE", "Conservation.FALSE",
      "No_one.TRUE",       "No_one.FALSE"
    ))
  )

Exp3_plot <- ggplot(plot_df2, aes(x = Combo, y = Value, fill = Combo)) +
  geom_boxplot() +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray30", linewidth = 0.9) +
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

# Line Plots different ForecAcc -------------------------------------------
df_Exp4 <- read_csv("CombinedData_PostBurnIN.csv") %>%
  filter(
    Forecasts %in% c("No_one", "Conservation", "Everyone", "Rich", "Poor"),
    ForecastError %in% c(0, 10, 20, 30),
    Set_Adoption == FALSE,
    Supplemental_fodder == TRUE,
    Starting_Prop_Conservation == 0.25
  )

outcome_vars <- c("AvgMoney", "AvgCows", "AvgGrass", "Gini", "PropCons")

df_long <- df_Exp4 %>%
  mutate(
    Forecasts_group = factor(
      Forecasts,
      levels = c("No_one", "Conservation", "Poor", "Rich", "Everyone"),
      labels = c("No one", "Conservation", "Poor", "Rich", "Everyone")
    ),
    Scenario = factor(
      Scenario,
      levels = c("SSP1-2.6", "SSP2-4.5", "SSP5-8.5"),
      labels = c(
        "SSP1-2.6\n(sustainability)",
        "SSP2-4.5\n(middle-of-the-road)",
        "SSP5-8.5\n(fossil fuel development)"
      ))
  ) %>%
  pivot_longer(
    cols = all_of(outcome_vars),
    names_to = "Variable",
    values_to = "RawValue"
  )

baseline_df <- df_long %>%
  filter(Forecasts_group == "No one") %>%
  group_by(Scenario, Variable, ForecastError) %>%
  summarise(Baseline = median(RawValue, na.rm = TRUE), .groups = "drop")

plot_df <- df_long %>%
  left_join(baseline_df, by = c("Scenario", "Variable", "ForecastError")) %>%
  mutate(Value = RawValue / Baseline) %>%
  filter(is.finite(Value))


Exp4_plot <- ggplot(
  plot_df,
  aes(x = ForecastError, y = Value, color = Forecasts_group, group = Forecasts_group)
) +
  geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.9, color = "#525252") +
  
  # Error bars: mean ± 1 SD
  stat_summary(
    fun.data = mean_sdl, fun.args = list(mult = 1),
    geom = "errorbar", width = 0.8, linewidth = 0.6,
    na.rm = TRUE, alpha = 0.7
  ) +
  # Mean points
  stat_summary(fun = mean, geom = "point", size = 2.2, na.rm = TRUE, alpha = 0.7) +
  # Linear interpolation between means
  stat_summary(fun = mean, geom = "line", linewidth = 1, na.rm = TRUE, alpha = 0.7) +
  
  ggh4x::facet_grid2(
    rows = vars(Variable),
    cols = vars(Scenario),
    scales = "free_y",
    independent = "y",
    labeller = labeller(
      Scenario = label_value,
      # Variable = function(x) sub("^Avg", "", x),
        Variable = c(
          AvgCows = "Livestock",
          AvgGrass = "Vegetation",
          AvgMoney = "Economic\nwellbeing",
          PropCons = "Conservation\nengagement"
    )
  )) +
  scale_x_continuous(breaks = c(0, 10, 20, 30)) +
  labs(
    x = "Forecast error",
    y = "Normalized outcome",
    color = "Forecast"
  ) +
  scale_color_manual(
    values = c(
      "No one"        = "#1f78b4", # @COSIMA avoid color overlap TODO
      "Everyone"      = "#33a02c",
      "Poor"          = "#e31a1c",
      "Rich"          = "#ff7f00",
      "Conservation"  = "#6a3d9a"
    )
  ) +
  ggthemes::theme_clean() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    strip.text = element_text(size = 14, face = "bold"),
    panel.spacing = unit(1.4, "lines"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  ) +
  ggh4x::facetted_pos_scales(
    y = list(
      Variable == "AvgMoney"  ~ scale_y_continuous(limits = c(0, 2), n.breaks = 4),
      Variable == "AvgCows"   ~ scale_y_continuous(limits = c(0.4, 1.6), n.breaks = 4),
      Variable == "AvgGrass"  ~ scale_y_continuous(limits = c(0.8, 1.3), n.breaks = 4),
      Variable == "Gini"      ~ scale_y_continuous(limits = c(0.9, 1.15), n.breaks = 4),
      Variable == "PropCons"  ~ scale_y_continuous(limits = c(0.6, 1.3),n.breaks = 4)
    )
  )

Exp4_plot


# Boxplots ForecastAcc 0 --------------------------------------------------

df_Exp4 <- read_csv("CombinedData_PostBurnIN.csv") %>%
    filter(
      Forecasts %in% c("No_one", "Conservation", "Everyone", "Rich", "Poor"),
      ForecastError %in% c(0, 10, 20, 30),
      Set_Adoption == FALSE,
      Supplemental_fodder == FALSE,
      Starting_Prop_Conservation == 0.25
    )
  
outcome_vars <- c("AvgMoney", "AvgCows", "AvgGrass", "Gini", "PropCons")
  
df_long <- df_Exp4 %>%
    mutate(
      Forecasts_group = factor(
        Forecasts,
        levels = c("No_one", "Conservation", "Poor", "Rich", "Everyone"),
        labels = c("No one", "Conservation", "Poor", "Rich", "Everyone")
      ),
      Scenario = factor(
        Scenario,
        levels = c("SSP1-2.6", "SSP2-4.5", "SSP5-8.5"),
        labels = c(
          "SSP1-2.6\n(sustainability)",
          "SSP2-4.5\n(middle-of-the-road)",
          "SSP5-8.5\n(fossil fuel development)"
        ))
    ) %>%
    pivot_longer(
      cols = all_of(outcome_vars),
      names_to = "Variable",
      values_to = "RawValue"
    )
  
# Baseline is the No one group at the same Scenario/Variable/ForecastError
  baseline_df <- df_long %>%
    filter(Forecasts_group == "No one") %>%
    group_by(Scenario, Variable, ForecastError) %>%
    summarise(Baseline = median(RawValue, na.rm = TRUE), .groups = "drop")
  
plot_df <- df_long %>%
    left_join(baseline_df, by = c("Scenario", "Variable", "ForecastError")) %>%
    mutate(Value = RawValue / Baseline) %>%
    filter(is.finite(Value))
  

plot_df0 <- plot_df %>% filter(ForecastError == 0)
# plot_df0 <- plot_df %>% filter(ForecastError == 10)
  
lims_df <- plot_df0 %>%
  group_by(Variable) %>%
  summarise(
    lo = min(Value, na.rm = TRUE),
    hi = max(Value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    rng = hi - lo,
    pad = ifelse(rng == 0, pmax(abs(hi), 1) * 0.05, rng * 0.05),  # safe padding even if rng=0
    lo  = lo - pad,
    hi  = hi + pad
  )

y_scales <- map(
  seq_len(nrow(lims_df)),
  ~ new_formula(
    parse_expr(paste0('Variable == "', lims_df$Variable[.x], '"')),
    scale_y_continuous(limits = c(lims_df$lo[.x], lims_df$hi[.x]), n.breaks = 4)
  )
)

Exp4_box <- ggplot(
  plot_df0,
  aes(x = Forecasts_group, y = Value, fill = Forecasts_group)
) +
  geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.9, color = "#525252") +
  geom_boxplot(width = 0.7, outlier.alpha = 0.4) +
  ggh4x::facet_grid2(
    rows = vars(Variable),
    cols = vars(Scenario),
    scales = "free_y",
    independent = "y",
    labeller = labeller(
      Scenario = label_value,
      Variable = c(
        AvgCows  = "Livestock",
        AvgGrass = "Vegetation",
        AvgMoney = "Economic\nwellbeing",
        PropCons = "Conservation\nengagement",
        Gini     = "Gini"
      )
    )
  ) +
  labs(
    x = "Forecast",
    y = "Normalized outcome",
    fill = "Forecast"
  ) +
  scale_fill_manual(values = c(
    "No one"       = "#1f78b4",
    "Everyone"     = "#33a02c",
    "Poor"         = "#e31a1c",
    "Rich"         = "#ff7f00",
    "Conservation" = "#6a3d9a"
  )) +
  ggthemes::theme_clean() +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 14, face = "bold"),
    panel.spacing = unit(1.2, "lines"),
    axis.text.x = element_text(size = 12, angle = 30, hjust = 1),
    axis.text.y = element_text(size = 12),
    axis.title  = element_text(size = 14)
  ) +
  ggh4x::facetted_pos_scales(y = y_scales)

Exp4_box

