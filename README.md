# RegenerativeRangelandManagementABM

This repository contains the code for the agent-based model and experiments used in the study ["Pairing information and incentives can reduce climate impacts and trade-offs between vegetation, livelihoods, and inequality in African rangelands,"](add link) which explores how pairing forecasts with conservation programming can reduce trade-offs among vegetation, livelihoods, and inequality, supporting effective adaptation to climate change at scale.

## Repository structure
- **RunModel.R**: Code for the agent-based model. Sources landscape files from `StylizedLandscape` and precipitation data from `PrecipTimeseries.R` to create the initial landscape and generate precipitation scenarios. Runtime for one setting across the three climate scenarios is approximately 45 minutes.  
- **GeneratePlots.R**: Code to reproduce visualizations for experiments and supplementals from the manuscript. Requires running `RunModel.R` for all conditions specified in `ExperimentConditions.rds` or, to avoid long runtime, downloading [`Ts_Data`](https://drive.google.com/file/d/1G6POSfm8SaYC4ZBgtwkPUq5TDhat1wRN/view?usp=sharing) and adding the folder to the root of the repository (i.e., the top-level folder alongside the script files).
- **StylizedLandscape**: folder containing all files and data required to achieve the interactive stylized landscape based on empirical distributions for the AOI.
- **ExperimentConditions.rds**: rds file with predefined set of agent-based model conditions, required to reproduce experiments (i.e., conditions of forecasts, supplemental fodder, and social learning).

## How to reproduce experiments

1. Clone this repository to your local machine:
  ```bash
  git clone https://github.com/matthewclark1223/RegenerativeRangelandManagementABM.git
  cd RegenerativeRangelandManagementABM
 ```

2. Generate data: Run `RunModel.R` to generate agent-based model output under conditions required for the experiments.
```bash
  Rscript RunModel.R
  ```

   Alternatively, use the pregenerated data files in `Ts_Data` folder.

3. Analyze results: Run `GeneratePlots.R` to visualize results in the framework of the experiments.
```bash
  Rscript GeneratePlots.R
  ```

## How to customize the agent-based model

1. Clone this repository to your local machine:
  ```bash
  git clone https://github.com/matthewclark1223/RegenerativeRangelandManagementABM.git
  cd RegenerativeRangelandManagementABM
  ```

2. Adapt agent-based model: For instance one could,
    - adapt or extend mechanics of the model: subheadings in `RunModel.R` correspond to currently implemented and adaptable modules described in the manuscript;
    - re-define experimental conditions and settings in `conditions <- readRDS("ExperimentConditions.rds")` and `RunModel.R` code lines XX-XX.
    
3. Potentially adapt experiments or visualizations in `GeneratePlots.R`


# Citation

If you use this code or dataset in your work, please cite:

```bibtex
@article{Clark2025Pairing,
  title     = {Pairing information and incentives can reduce climate impacts and trade-offs between vegetation, livelihoods, and inequality in African rangelands},
  author    = {Clark, Matt and Fröhner, Cosima and Jørgensen, Andreas Christ Sølvsten and Pienkowski, Thomas and Yekela, Sibabalo and Isacs, Aamirah and Crowe, Olivia and Andrews, Jeffrey and Smaldino, Paul E. and Gallizioli, Iacopo Tito and Arena, Gina and Mills, Morena},
  year      = {2025},
  note      = {In review},
  doi       = {},
  url       = {},
}
