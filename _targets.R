# install.packages("targets")
library(pslongSim)
library(targets)

tar_option_set(packages = "pslongSim")

make_default_simulation_targets()
