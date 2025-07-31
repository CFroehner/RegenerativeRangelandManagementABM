
# PREPROCESSING -----------------------------------------------------------

## Load Packages -----------------------------------------------------------
library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
library(terra)
library(spdep)
library(tidyterra)
library(lubridate)
library(purrr)

# Create output folder if it doesn't exist
output_dir <- "Plots_Tables"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

## Load and Transform Climate Model Data -----------------------------------

# Load precipitation rasters from CMIP6 model (MPI-ESM1-2-LR) across southern 
# African countries and time periods
# Baseline historical run: 1850–1900; Future projections: 2050–2059

baseline <- terra::rast("Data/MPI-2050to2069/pr_Amon_MPI-ESM1-2-LR_historical_r1i1p1f1_gn_18500116-19001216.nc")   # 1850 - 1900
# baseline <- baseline[[1:120]]  # first 10 years (12 × 10 = 120 layers) # for 
# visualizations uncomment because currently the sum is plotted
# (for original / longer time-period than projections, change sum to mean)

# pred1 <- terra::rast("Data/MPI-2050to2059/pr_Amon_MPI-ESM1-2-LR_ssp370_r1i1p1f1_gn_20500116-20591216.nc")  # Pessimistic (SSP3-7.0)
pred1 <- terra::rast("Data/MPI-2050to2059/pr_Amon_MPI-ESM1-2-LR_ssp585_r1i1p1f1_gn_20500116-20591216.nc")  # Dystopian (SSP5-8.5) / fossil-fueled development
pred2 <- terra::rast("Data/MPI-2050to2059/pr_Amon_MPI-ESM1-2-LR_ssp245_r1i1p1f1_gn_20500116-20591216.nc")  # Current Policy (SSP2-4.5) / middle of the road
pred3 <- terra::rast("Data/MPI-2050to2059/pr_Amon_MPI-ESM1-2-LR_ssp126_r1i1p1f1_gn_20500116-20591216.nc")  # Optimistic (SSP1-2.6) / sustainability

# Convert precipitation from kg/m²/s to kg/m²/month
convert_to_mm_per_month <- function(rast) {
  dates <- as.Date(time(rast))
  seconds_in_month <- lubridate::days_in_month(dates) * 86400
  
  for (i in seq_len(nlyr(rast))) {
    rast[[i]] <- rast[[i]] * seconds_in_month[i]
  }
  
  return(rast)
}

baseline <- convert_to_mm_per_month(baseline)
pred1 <- convert_to_mm_per_month(pred1)
pred2 <- convert_to_mm_per_month(pred2)
pred3 <- convert_to_mm_per_month(pred3)


## Define AOI and Analytical Grid ------------------------------------------

AOI <- read_sf("AOI.shp")

AOI <- st_transform(AOI) %>%
  st_buffer(dist = 0.05) %>% # buffer due to potential shapefile - raster diffs.
  st_union()

# Generate a grid over AOI with resolution 0.25
grids <- st_make_grid(AOI, cellsize = 0.25) %>%
  st_as_sf() %>%
  filter(st_intersects(AOI, ., sparse = FALSE)[1, ]) %>%
  mutate(Index = row_number()) %>%
  st_transform()

# Construct target raster for resampling using vectorized grid
grids_vect <- vect(grids)
target_raster <- terra::rast(grids_vect, resolution = 0.25)

# Diagnostic plot for visual inspection of grid overlay
AOI_grid <- ggplot(grids) +
  geom_sf() +
  geom_sf(data = AOI, color = "green", fill = NA)

ggsave("Plots_Tables/AOI_grid.png", AOI_grid, width = 10, height = 6, dpi = 300)

## Define and Extract Seasonal (JJA/DJF) Precipitation --------------------

# Helper function to subset raster layers by months
get_season_stack <- function(rast, season_months) {
  dates <- as.Date(time(rast))
  months <- month(dates)
  selected_layers <- which(months %in% season_months)
  return(rast[[selected_layers]])
}

# Define meteorological seasons
winter_months <- c(6, 7, 8)   # JJA
summer_months <- c(12, 1, 2)  # DJF

# Stack seasonal subsets for each scenario
seasonal_rasters <- list(
  baseline_winter = get_season_stack(baseline, winter_months),
  baseline_summer = get_season_stack(baseline, summer_months),
  bad_winter = get_season_stack(pred1, winter_months),
  bad_summer = get_season_stack(pred1, summer_months),
  ok_winter = get_season_stack(pred2, winter_months),
  ok_summer = get_season_stack(pred2, summer_months),
  good_winter = get_season_stack(pred3, winter_months),
  good_summer = get_season_stack(pred3, summer_months)
)

# Compute mean seasonal precipitation for each season/scenario
seasonal_sums <- lapply(seasonal_rasters, function(r) {
  terra::app(r, fun = sum, na.rm = TRUE)
})

# Resample to target grid and crop to AOI
seasonal_sums_cropped <- purrr::imap(seasonal_sums, function(r, name) {
  r_resampled <- terra::resample(r, target_raster, method = "bilinear")
  r_cropped <- terra::mask(terra::crop(r_resampled, vect(AOI)), vect(AOI))
  names(r_cropped) <- name
  r_cropped
})

# Convert raster data to tidy data frame for visualization
seasonal_df <- purrr::imap_dfr(seasonal_sums_cropped, function(r, name) {
  df <- as.data.frame(r, xy = TRUE, na.rm = TRUE)
  colnames(df)[3] <- "precip"
  parts <- unlist(strsplit(name, "_"))
  df$Scenario <- parts[1]
  df$Season <- parts[2]
  return(df)
})

# Faceted map of seasonal means
sum_precip_per_season_scenario <- ggplot(
  seasonal_df, aes(x = x, y = y, fill = precip)) +
  geom_raster() +
  facet_grid(Season ~ Scenario) +
  scale_fill_viridis_c(
    name = "Total Precip (mm)"
  ) +
  coord_sf() +
  labs(title = "Total Precipitation per Season and Scenario") +
  theme_minimal()

sum_precip_per_season_scenario

ggsave("Plots_Tables/sum_precip_per_season_scenario.png", 
       plot = sum_precip_per_season_scenario, width = 10, height = 6, dpi = 300)

# ## Summarize DJF mos only --------------------------------------------------

# extract DJF winter layers and compute mean per year
summarize_winter_djf <- function(rast, start_year) {
  dates <- as.Date(time(rast))
  months <- month(dates)
  years <- year(dates)

  # Dec of prev year, Jan + Feb of current year = winter
  winter_ids <- which(months %in% c(12, 1, 2))
  winter_stack <- rast[[winter_ids]]
  winter_dates <- dates[winter_ids]
  winter_months <- months[winter_ids]
  winter_years <- years[winter_ids]

  # Adjust Dec to the following year's winter
  winter_years[winter_months == 12] <- winter_years[winter_months == 12] + 1

  # For each winter year, average all layers (should be 3 per year)
  winter_layers <- unique(winter_years)
  winter_mean_list <- lapply(winter_layers, function(yr) {
    idx <- which(winter_years == yr)
    yr_stack <- winter_stack[[idx]]
    yr_mean <- terra::app(yr_stack, fun = mean, na.rm = TRUE)
    names(yr_mean) <- paste0("Winter_", yr)
    return(yr_mean)
  })

  rast(winter_mean_list)
}

# # Apply to each scenario
winter_baseline <- summarize_winter_djf(baseline, start_year = 1850)
winter_bad <- summarize_winter_djf(pred1, start_year = 2050)
winter_ok  <- summarize_winter_djf(pred2, start_year = 2050)
winter_good <- summarize_winter_djf(pred3, start_year = 2050)

winter_scenarios <- list(
  baseline = winter_baseline,
  bad = winter_bad,
  ok = winter_ok,
  good = winter_good
)
# 
# # Resample + crop to target grid
winter_cropped <- purrr::imap(winter_scenarios, function(r, name) {
  r_resampled <- terra::resample(r, target_raster, method = "bilinear")
  r_cropped <- terra::mask(terra::crop(r_resampled, vect(AOI)), vect(AOI))
  names(r_cropped) <- paste0(name, "_", gsub("Winter_", "", names(r_cropped)))
  r_cropped
})


# Derive PrecipTimeseries Params ------------------------------------------
# Identify pixels exhibiting highest interannual precipitation variability 
# during DJF, using robust dispersion metrics to account for potential 
# non-normality and outliers in relatively short-time frame climate model outputs

# Define robust standard deviation estimator
robust_sd_90 <- function(x, na.rm = TRUE) {
  q <- quantile(x, probs = c(0.05, 0.95), na.rm = na.rm)
  ipr_90 <- q[2] - q[1]
  return(ipr_90 / 3.29) # Converts 90% IPR to approximate robust SD assuming 
  # Gaussian-like tails
}

# Extract time series from all pixels
get_full_pixel_ts <- function(r, scenario_name) {
  df <- as.data.frame(r, xy = TRUE, na.rm = TRUE)
  df %>%
    pivot_longer(cols = starts_with(scenario_name),
                 names_to = "Year", values_to = "precip") %>%
    mutate(
      Year = as.integer(gsub(paste0(scenario_name, "_"), "", Year)),
      Scenario = scenario_name
    )
}

# Create long-format table of all pixels
winter_ts <- purrr::imap_dfr(winter_cropped, get_full_pixel_ts)

# Calculate interannual change statistics for DJF at each pixel
winter_summary_table <- winter_ts %>%
  group_by(Scenario, x, y) %>%
  filter(mean(precip, na.rm = TRUE) > 3) %>% # Filter out low-precip pixels to 
  # avoid inflated relative variability due to division by near-zero means
  arrange(Year) %>%
  mutate(
    delta_precip = precip - lag(precip),
    year_diff = Year - lag(Year)
  ) %>%
  filter(year_diff == 1) %>%
  summarize(
    mean_precip_abs = mean(precip, na.rm = TRUE),
    # sd_change = sd(delta_precip, na.rm = TRUE),
    # sd_change_percent = sd_change / mean_precip_abs,
    sd_change = robust_sd_90(delta_precip), # robust
    sd_change_percent = sd_change / mean_precip_abs, # robust
    .groups = "drop"
  )

# Identify "worst-case" location, i.e., pixel with maximum interannual 
# variability considering the 90 % IPR
extreme_pixels <- winter_summary_table %>%
  group_by(Scenario) %>%
  slice_max(sd_change, n = 1) %>%
  ungroup()

extreme_pixels

write.csv(extreme_pixels, "Plots_Tables/scenario_rainstats.csv", row.names = FALSE)

