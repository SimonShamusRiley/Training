install.packages('tidyverse')
install.packages('readxl')
install.packages('writexl')
install.packages('devtools')
install.packages('glmmTMB')
install.packages('car')
install.packages('emmeans')
install.packages('multcomp')
install.packages('multcompView')

devtools::install_github('SimonShamusRiley/powerutilities', dependencies = T, 
                         build_vignettes = T)