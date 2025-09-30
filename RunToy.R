## Toy example

# scripts/run_toy.R
Sys.setenv(
  CONDITIONS_RDS = "ConditionsExp1.rds",
  N_REPS         = "1",
  TIME_STEPS     = "100",
  OUTDIR_SUFFIX  = "toy",    # -> Ts_Data_toy/
  TOY_RUN        = "1"        # disable parallelization of scenarios
)

source("RunModel.R") # 3 scenarios x 100 timesteps

Sys.setenv(DATA_ROOT = "Ts_Data_toy",
           VIS_ROOT = "Manuscript_Vis_toy",
           UP_TO = "exp1")

source("GeneratePlots.R")


