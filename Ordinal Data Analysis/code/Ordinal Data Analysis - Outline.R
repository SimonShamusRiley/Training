#==============================================================================#
# Ordinal Data Analysis                                                        #
#==============================================================================#

#### Housekeeping ####
# Load Required Packages
library(readxl)
library(tidyverse)
library(ordinal)
library(performance)
library(emmeans)
library(multcomp)
library(multcompView)
library(writexl)

# Global options
setwd("C:/Users/au802896/Documents/Training/Ordinal Data Analysis")

options(contrasts = c('contr.sum', 'contr.poly'),
        dplyr.width = Inf, pillar.print_min = 100, 
        pillar.print_max = 100)

# Set default aesthetics
my_theme = theme_classic()+
  theme(text = element_text(size = 14))
theme_set(my_theme)

pal = c('steelblue', 'firebrick', 'olivedrab', 'goldenrod')
options(ggplot2.discrete.fill = pal, ggplot2.discrete.colour = pal)

#### Import & Prep Data #### 
# The International Potato Center has worked for many years to promote the
# cultivation and consumption of orange-fleshed sweet potato (OFSP), which
# is richer in beta carotene (pro-Vitamin A) than the white-fleshed varieties 
# which have historically been much more popular throughout many parts of the
# world, and especially in tropical Africa. A sensory evaluation was undertaken
# in Uganda to better understand consumer preferences:
# Muzhingi, Tawanda, 2022, "Dataset for: Sensory attributes and consumer
# acceptance of sweetpotato and potato genotypes in East Africa",
# https://doi.org/10.21223/IU4LC1, International Potato Center.

# Define file
ofsp_file = 'data/OFSP Sensory Evaluation Lira Uganda.xlsx'

# Check file contents
excel_sheets(ofsp_file)

# Read-in and format data:

#### Overall Score ####
##### Data Exploration & Validation ####
##### Model Development & Evaluation ####
##### Estimation, Testing & Reporting ####

#### Firmness Score ####
##### Data Exploration & Validation ####
##### Model Development & Evaluation ####
##### Estimation, Testing & Reporting ####

