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
#setwd("C:/Users/au802896/Documents/Training/Ordinal Data Analysis")

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

#### Example 1: Overall Score ####
##### Data Exploration & Validation ####
ggplot(ofsp, aes(x = Overall))+
  facet_wrap(~ Cultivar)+
  geom_bar()

##### Model Development & Evaluation ####
# Fit a "base" model:
overall_mod1 = clm(Overall ~ Cultivar, data = ofsp, link = 'logit')

# Unfortunately, there are not good tools for residual diagnostics,
# although we do have a couple of tests for the model assumptions:
nominal_test(overall_mod1)
scale_test(overall_mod1)

# Can model fit be improved by incorporating sex in the model? 
overall_mod2 = clm(Overall ~ Cultivar*Sex, data = ofsp, link = 'logit')
nominal_test(overall_mod2)
scale_test(overall_mod2)

# No, there is no evidence to suggest that men's and women's perceptions differ:
compare_performance(overall_mod1, overall_mod2)

# Can model fit be improved by using an alternative link function? No, these
# models are basically equivalent
overall_mod3 = clm(Overall ~ Cultivar, data = ofsp, link = 'probit')
compare_performance(overall_mod1, overall_mod3)

# Proceed with initial model
overall_mod = overall_mod1

##### Estimation, Testing and Reporting ####
# Wald Chi-square test (= asympotitic F-test)
(ofsp_ftest = joint_tests(overall_mod))

# There are many types of emmeans which can be calculated for ordinal models,
# this is controlled with the "mode = " argument, and is described in:
vignette('models', package = 'emmeans')

# For a single overall measure of score for each treatment:
(ofsp_latent_emm = emmeans(overall_mod, ~ Cultivar, mode = 'latent'))

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
(ofsp_prob_emm = emmeans(overall_mod, ~ Cultivar:Overall, mode = 'prob'))

ofsp_prob_emm |> 
  as.data.frame() |> 
  ggplot(aes(x = Overall, colour = Cultivar))+
  facet_wrap(~ Cultivar)+
  geom_point(aes(y = prob))+
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = .1)

# For exceedance probabilities, the "psuedo-variable" is called "cut" instead
# of the name of the response variable:
(ofsp_exprob_emm = emmeans(overall_mod, ~ Cultivar:cut, mode = 'exc.prob'))

# Imagine we want to see/compare probabilities of 7 or higher:
ofsp_exprob_emm |> 
  as.data.frame() |> 
  filter(cut == '6|7') |> 
  ggplot(aes(x = Cultivar, color = Cultivar))+
  geom_point(aes(y = exc.prob))+
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = .1) +
  scale_y_continuous(limits = c(0, NA))

#### Example 2: Firmness Score ####
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

#### Going Further: Incorporating Random Effects ####
# The preceding analyses implicitly assumed that each row of our data
# constitutes an independent observation, but in fact this is not the case: each
# participant was asked to assess all four varieties, and we would expect those
# four ratings from each participant to be correlated. We can think of this
# correlation in terms of each participant representing a "block" of
# observations, or we can think of this in terms of respondents' individual
# preferences (e.g., a respondent who dislikes sweet potato might give low
# ratings to all varieties, while one who loves sweet potato might rank all
# varieties  highly). Although not the case here, it is especially important
# account for this grouping pattern in the data when the data are not balanced,
# either because different participants were asked to assess different sets of
# cultivars, or because some observations are missing. Accounting for this 
# grouping structure is done using random effects.

##### Model Development & Evaluation ####
# Random effects are entered into the model formula in parentheses, with the
# grouping factor(s) on the right hand side of the "|", and the random 
# effects themselves (often, just an intercept denoted using the number "1") on
# the left hand side of the "|":
overall_mixmod = clmm(Overall ~ Cultivar + (1|Consumers), data = ofsp,
                      link = 'logit')

# Unfortunately, we have even fewer tools for model diagnostics when using clmm.
# We can still use information criteria or likelihood ratio test (not shown) 
# for model comparison. Here, we see that in this instance model fit is not
# actually improved by accounting for the random effects. Nevertheless, many 
# would argue that it should remain in the model because the grouping structure
# which the random effect represents is inherent to the design of the study.
AIC(overall_mod, overall_mixmod)

##### Estimation, Testing and Reporting ####
# Random effects are defined as having a mean of zero, so that the fixed effects 
# can be interpreted as the expected average across the whole population (known
# as "best linear unbiased estimates", or BLUEs). 
overall_raneff = ranef(overall_mixmod)$Consumers |> 
  rownames_to_column(var = 'Consumers') |> 
  arrange(`(Intercept)`)

hist(overall_raneff$`(Intercept)`)

# What is estimated is the variance among participants (in this example):
VarCorr(overall_mixmod, format = TRUE) # Variance (on logit scale)

# And from that, predictions can be made about individual participants (known 
# as "best linear unbiased predictions", or BLUPs):
head(overall_raneff) # These people are critical, or just dislike OFSP
tail(overall_raneff) # These people are not critical, or very much like OFSP

# Because the data are balanced and the variance among participants is modest,
# there is little change to the model estimates, standard errors or confidence
# intervals (but this will, of course, not always be the case):
emmeans(overall_mixmod, ~ Cultivar)
emmeans(overall_mod, ~ Cultivar)
