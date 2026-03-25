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
# is richer in beta carotine (pro-Vitamine A) than the white-fleshed varieties 
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

# Read-in and format data
ofsp = read_xlsx(ofsp_file, sheet = 'Sweetpotato Lira') |> 
  pivot_longer(cols = Arakaraka_Overall:Otada_Mealiness, 
               names_to = 'Assessment', 
               values_to = 'Score') |> 
  separate(col = 'Assessment', into = c('Cultivar', 'Feature'), sep = '_') |> 
  pivot_wider(names_from = 'Feature', values_from = 'Score') |> 
  mutate(Overall = ordered(Overall, levels = 1:9), 
         across(Color:Mealiness, ~ ordered(., levels = 1:3)), 
         across(where(is.character), factor), 
         Consumers = factor(Consumers))

head(ofsp)

#### Overall Score ####
##### Data Exploration & Validation ####
ggplot(ofsp, aes(x = Overall))+
  facet_wrap(~ Cultivar)+
  geom_bar()

##### Model Development & Evaluation ####
# Fit a "base" model:
ofsp_mod1 = clm(Overall ~ Cultivar, data = ofsp, link = 'logit')

# Unfortunately, there are not good tools for residual diagnostics,
# although we do have a couple of tests for the model assumptions:
nominal_test(ofsp_mod1)
scale_test(ofsp_mod1)

# Can model fit be improved by incorporating sex in the model? 
ofsp_mod2 = clm(Overall ~ Cultivar*Sex, data = ofsp, link = 'logit')
nominal_test(ofsp_mod2)
scale_test(ofsp_mod2)

# No, there is no evidence to suggest that men's and women's perceptions differ:
compare_performance(ofsp_mod1, ofsp_mod2)

# Can model fit be improved by using an alternative link function? No, these
# models are basically equivalent
ofsp_mod3 = clm(Overall ~ Cultivar, data = ofsp, link = 'probit')
compare_performance(ofsp_mod1, ofsp_mod3)

# Proceed with initial model
ofsp_mod = ofsp_mod1

##### Estimation, Testing and Reporting ####
# Wald Chi-square test (= asympotitic F-test)
(ofsp_ftest = joint_tests(ofsp_mod))

# There are many types of emmeans which can be calculated for ordinal models,
# this is controlled with the "mode = " argument, and is described in:
vignette('models', package = 'emmeans')

# For a single overall measure of score for each treatment:
(ofsp_latent_emm = emmeans(ofsp_mod, ~ Cultivar, mode = 'latent'))

# Where one of our cultivars is a "check" or a "standard", we can make 
# overall comparisons against that:
(ofsp_latent_cntr1 = contrast(ofsp_latent_emm, 'trt.vs.ctrl', ref = 'NASPOT8'))

# Or we can test each cultivar against the overall average, to see which 
# cultivars are "statistically significantly above or below average"
(ofsp_latent_cntr2 = contrast(ofsp_latent_emm, 'eff'))

# Or we can make all pairwise comparisons
(ofsp_latent_cld = cld(ofsp_latent_emm, Letters = LETTERS))

# We are often interested in the probability of achieving specific scores
# or exceeding some score. This gives us the probabilities associated with
# each score:
(ofsp_prob_emm = emmeans(ofsp_mod, ~ Cultivar:Overall, mode = 'prob'))

ofsp_prob_emm |> 
  as.data.frame() |> 
  ggplot(aes(x = Overall, colour = Cultivar))+
  facet_wrap(~ Cultivar)+
  geom_point(aes(y = prob))+
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = .1)

# For exceedance probabilities, the "psuedo-variable" is called "cut" instead
# of the name of the response variable:
(ofsp_exprob_emm = emmeans(ofsp_mod, ~ Cultivar:cut, mode = 'exc.prob'))

# Imagine we want to see/compare probabilities of 7 or higher:
ofsp_exprob_emm |> 
  as.data.frame() |> 
  filter(cut == '6|7') |> 
  ggplot(aes(x = Cultivar, color = Cultivar))+
  geom_point(aes(y = exc.prob))+
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = .1) +
  scale_y_continuous(limits = c(0, NA))

#### Firmness Score ####
##### Data Exploration & Validation ####
ggplot(ofsp, aes(x = Firmness))+
  facet_wrap(~ Cultivar)+
  geom_bar()

##### Model Development & Evaluation ####
# Fit a "base" model
firm_mod1 = clm(Firmness ~ Cultivar, data = ofsp, link = 'logit')

# Here there is some evidence that the assumption of proportional odds
# is not met
nominal_test(firm_mod1)
scale_test(firm_mod1)

# See if using an alternative link function improves fit
firm_mod2 = clm(Firmness ~ Cultivar, data = ofsp, link = 'cauchit')

# For the cauchit at least, the answer is yes
compare_performance(firm_mod1, firm_mod2)

# Select final choice of model
firm_mod = firm_mod2

##### Estimation, Testing & Reporting ####
# Overall cultivar effect is non-significant, but here 
# we are not primarily concerned with the means on latent scale, 
# because higher is not better. Instead we are interested in probability
# of firmness being "just right" (= score of 2)
(firm_ftest = joint_tests(firm_mod))

# Calculate probabilities for each cultivar for each score:
(firm_prob_emm = emmeans(firm_mod, ~ Cultivar|Firmness, mode = 'prob'))

# And we can see that Otada is statistically significantly less likely to 
# be scored as having a firmness that is "about right" than NASPOT 8
(firm_prob_cld = cld(firm_prob_emm, Letters = LETTERS))

firm_prob_cld |> 
  as.data.frame() |> 
  filter(Firmness == 2) |> 
  mutate(.group = str_trim(.group)) |> 
  ggplot(aes(x = Cultivar, color = Cultivar))+
    geom_point(aes(y = prob))+
    geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.1) +
    geom_text(aes(y = asymp.UCL+.1, label = .group)) +
    scale_y_continuous(limits = c(0, NA))





