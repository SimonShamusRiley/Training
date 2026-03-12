install.packages(
 c( 
  "devtools",
  "edibble",
  "emmeans",
  "glmmTMB",
  "powerutilities",
  "readxl",
  "tidyverse",
  "writexl")
)
devtools::install_github('SimonShamusRiley/powerutilities', dependencies = T,
                         build_vignettes = T)