# RegenerativeRangelandManagementABM

This repository contains the code for the agent-based model and experiments used in the study ["Eliminating climate change mitigation trade-offs in African rangelands,"](link to paper) which explores how pairing forecasts with conservation programming can reduce trade-offs among vegetation cover, livestock numbers, and wealth inequality, supporting effective adaptation to climate change at scale.

## Repository structure
- **RunModel.R**: Code for the agent-based model (ABM). Sources landscape files from `StylizedLandscape` and precipitation from `PrecipTimeseries.R` to create the initial landscape and generate precipitation scenarios. Runtime for one full setting across the three climate scenarios is ~45 minutes. It also supports a toy quickstart via environment variables.
- **GeneratePlots.R**: Code to reproduce visualizations for experiments 1-3 and supplementals from the manuscript. It expects ABM outputs in a Ts_Data-style folder (see below).
- **StylizedLandscape/**: Scripts and incputs for creating the stylized landscape, including intermediate figures used by the ABM.
- **ExperimentConditions.rds**: Predefined experimental settings for running the ABM, used in manuscript experiments 1 to 3 and supplemental material (forecast settings, provision of supplemental fodder, social learning settings, etc.).

## Prerequisites
- R ≥ 4.1
- Packages for ABM and visualizations:
```
packages ‹- c(
"data. table", "doParallel", "doRNG", "sp", "foreach", "dplyr", "sf", "raster", "in
"gstat", "spdep", "ggplot2", "tidyverse", "cowplot", "ggh4x", "ggthemes",
"vroom", "glue", "fs", "purri", "tibble"
)
install.packages (setdiff(packages, rownames(installed.packages ())))
```
## Toy example (quickstart)
The repository includes a tiny toy run to reproduce the core pipeline and generate example visualizations without the long ABM runtime.

**What it does**
- Runs a minimal ABM configuration (fewer repetitions, conditions for experiment 1 only) and writes outputs to Ts_Data_toy/.
- Generates ABM burn-in and experiment 1 plots into Manuscript_Vis_toy/.

```
  Rscript RunToy.R
```

## Reproduce the full experiments

1. Clone this repository to your local machine:
  ```bash
  git clone https://github.com/matthewclark1223/RegenerativeRangelandManagementABM.git
  cd RegenerativeRangelandManagementABM
 ```

2. Generate data

*Option A (full run, long):*
```bash
  Rscript RunModel.R
  ```

*Option B (use pregenerated data):*

Download [`Ts_Data`](https://drive.google.com/file/d/1G6POSfm8SaYC4ZBgtwkPUq5TDhat1wRN/view?usp=sharing) and place the folder in the repo root next to the scripts.

3. Analyze and plot:
```bash
  # default expects Ts_Data and writes to Manuscript_Vis
  Rscript GeneratePlots.R
  ```

## How to customize the agent-based model

1. Modify the model in `RunModel.R`. Sections in the script correspond to manuscript modules.
2. Play with settings (such as animal cost or capital gain rate) in the `RunModel.R` section `Model parameters`.
3. Adjust plots in GeneratePlots.R (facets, normalization, themes).

# Citation

If you use this code in your work, please cite:

```bibtex
@article{Clark2025Pairing,
  title     = {Eliminating climate change mitigation trade-offs in African rangelands},
  author    = {Matt Clark†, Cosima Fröhner†, Andreas Christ Sølvsten Jørgensen, Thomas Pienkowski, Sibabalo Yekela, Aamirah Isacs, Olivia Crowe, Jeffrey Andrews, Paul E. Smaldino, Iacopo Tito Gallizioli, Gina Arena, Morena Mills},
  year      = {2025},
  note      = {In review},
  doi       = {},
  url       = {},
}
