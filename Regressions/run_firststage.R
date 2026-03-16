# Wrapper to run Assign + FirstStage with UTF-8 encoding
options(encoding = "UTF-8")
Sys.setenv(R_ENCODING = "UTF-8")

# Run 1951 kyoku assignment
source(here::here("Regressions", "Assign_Kyoku_By_Order_1951.R"), encoding = "UTF-8")

# Run first stage
source(here::here("Regressions", "FirstStage_FemaleManager.R"), encoding = "UTF-8")
