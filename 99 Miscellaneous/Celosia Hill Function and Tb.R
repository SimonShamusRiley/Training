#==============================================================================#
# Fitting Hill Functions in R                                                  #
#==============================================================================#

#### Prepare Software Environment ####
# Load required packages
library(readxl)
library(tidyverse)
library(nlme)
library(car)
library(emmeans)
library(marginaleffects)
library(writexl)

# Set working directory
# setwd("C:/Users/au802896/OneDrive - Aarhus universitet/Statistical Consulting/Fiona Hay/Genstat - R")

options(contrasts = c('contr.sum', 'contr.poly'))

#### Import & prepare Data ####
celosia <- read_xlsx('2026-03-01 Celosia_latest.xlsx',
                     sheet = 'Sheet2', range = 'A8:D64') |> 
  rename(pct_germ = `%germ`, cout_germ = `germination count`) |>
  mutate(temperature = factor(temperature)) |> 
  filter(!is.na(temperature))

# Since the data from temp = 15 will not be modeled, it will be 
# convenient to remove it from the data set we'll use during model fitting
celosia_set = celosia |> 
  filter(temperature != '15') |> 
  droplevels()

#### Model Fitting ####
# It is possible to fit separate models for each temperature, but we can instead
# fit a single model and simply specify to estimate separate parameter values
# for each temperature. Note that using "~ 0 + temperature" rather than 
# "~ temperature" implies that the model coefficients correspond to the parameter
# for each temp level, rather than having a coefficient for the intercept + 3 
# coefficients corresponding to differences from the intercept. This 
# parameterization is much simpler when we later want to use the delta method
# to calculate inverse times (below)
celosia_mod <- gnls(pct_germ ~ (a*time^b)/(c^b + time^b), 
                    data = celosia_set,
                    params = a + b + c ~ 0+temperature, 
                    start = c(a = c(30, 30, 30, 30), 
                              b = c(10, 10, 10, 10), 
                              c = c(5, 5, 5, 5)), 
                    na.action = na.omit)
print(celosia_mod)

#### Estimation and Reporting ####
# Calculate parameter estimates, SE's, CI's, etc., then bind them together 
# in a single table
celosia_a_ests <- emmeans(celosia_mod, ~ temperature, param = 'a')
celosia_b_ests <- emmeans(celosia_mod, ~ temperature, param = 'b')
celosia_c_ests <- emmeans(celosia_mod, ~ temperature, param = 'c')

celosia_param_ests = list('a' = as.data.frame(celosia_a_ests), 
                          'b' = as.data.frame(celosia_b_ests), 
                          'c' = as.data.frame(celosia_c_ests)) |> 
  bind_rows(.id = 'parameter') |>
  relocate(parameter, .after = 'temperature') |> 
  arrange(temperature, parameter)
print(celosia_param_ests)

# Use delta method to calculate inverse times along with SE's and CI's. Note that the formula needs to be given as a text string,
# with coefficient names as they appear in the model (look at output from line 46). Also note that the "fun" argument is just how we
# want the results named in the output. There are more elegant ways to accomplish this, but I thought this approach would be the most 
# transparent.
inv_time_ests <- rbind(
    deltaMethod(celosia_mod, '1 / ((-10 * c.temperature20^b.temperature20) / (10 - a.temperature20))^(1/b.temperature20)', fun = '20C-10'),
    deltaMethod(celosia_mod, '1 / ((-20 * c.temperature20^b.temperature20) / (20 - a.temperature20))^(1/b.temperature20)', fun = '20C-20'),
    
    deltaMethod(celosia_mod, '1 / ((-10 * c.temperature25^b.temperature25) / (10 - a.temperature25))^(1/b.temperature25)', fun = '25C-10'),
    deltaMethod(celosia_mod, '1 / ((-20 * c.temperature25^b.temperature25) / (20 - a.temperature25))^(1/b.temperature25)', fun = '25C-20'),
    deltaMethod(celosia_mod, '1 / ((-30 * c.temperature25^b.temperature25) / (30 - a.temperature25))^(1/b.temperature25)', fun = '25C-30'),
    
    deltaMethod(celosia_mod, '1 / ((-10 * c.temperature30^b.temperature30) / (10 - a.temperature30))^(1/b.temperature30)', fun = '30C-10'),
    deltaMethod(celosia_mod, '1 / ((-20 * c.temperature30^b.temperature30) / (20 - a.temperature30))^(1/b.temperature30)', fun = '30C-20'),
    deltaMethod(celosia_mod, '1 / ((-30 * c.temperature30^b.temperature30) / (30 - a.temperature30))^(1/b.temperature30)', fun = '30C-30'),
    deltaMethod(celosia_mod, '1 / ((-40 * c.temperature30^b.temperature30) / (40 - a.temperature30))^(1/b.temperature30)', fun = '30C-40'),
    deltaMethod(celosia_mod, '1 / ((-50 * c.temperature30^b.temperature30) / (50 - a.temperature30))^(1/b.temperature30)', fun = '30C-50'),
    
    deltaMethod(celosia_mod, '1 / ((-10 * c.temperature35^b.temperature35) / (10 - a.temperature35))^(1/b.temperature35)', fun = '35C-10'),
    deltaMethod(celosia_mod, '1 / ((-20 * c.temperature35^b.temperature35) / (20 - a.temperature35))^(1/b.temperature35)', fun = '35C-20'),
    deltaMethod(celosia_mod, '1 / ((-30 * c.temperature35^b.temperature35) / (30 - a.temperature35))^(1/b.temperature35)', fun = '35C-30'), 
    deltaMethod(celosia_mod, '1 / ((-40 * c.temperature35^b.temperature35) / (40 - a.temperature35))^(1/b.temperature35)', fun = '35C-40'), 
    deltaMethod(celosia_mod, '1 / ((-50 * c.temperature35^b.temperature35) / (50 - a.temperature35))^(1/b.temperature35)', fun = '35C-50'),
    deltaMethod(celosia_mod, '1 / ((-60 * c.temperature35^b.temperature35) / (60 - a.temperature35))^(1/b.temperature35)', fun = '35C-60'),
    deltaMethod(celosia_mod, '1 / ((-70 * c.temperature35^b.temperature35) / (70 - a.temperature35))^(1/b.temperature35)', fun = '35C-70')
)

inv_time_ests <- inv_time_ests |> 
  rownames_to_column(var = 'Info') |> 
  separate(col = 'Info', into = c('temperature', 'percent'), sep = 'C-') |> 
  mutate(temperature = as.numeric(temperature)) |> 
  rename(LCL = `2.5 %`, UCL = `97.5 %`)
print(inv_time_ests)

# Tbase determination for 20-35C, for 10-70% germination
inv_time_est_set1 = inv_time_ests |> 
  filter(temperature %in% c(20, 25, 30, 35)) |> 
  droplevels()

# Note that this approach treats the estimated coefficients as known values,
# rather than uncertain estimates. To account for the uncertainty in the 
# estimates, observations should be weighted by the inverse of their standard
# errors, but doing so here causes the confidence intervals around b and tb to 
# become very, very large.
tb_mod1 <- gnls(Estimate ~ b*(temperature - tb), 
                data = inv_time_est_set1, 
                params = list(b ~ 0 + percent, tb ~ 1), 
                start = c(b = c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1), 
                          tb = c(15)))

# for reasons I don't understand, emmeans is messing up the DF 
# calculations here, which leads to excessively wide CI (although
# even with manually specified correct DF, it is still large here)
emmeans(tb_mod1, ~ 1, param = 'tb', df = 6) # Doesn't look good...

# Tbase determination for 20-35C, for 10-50% germination 
inv_time_est_set2 = inv_time_ests |> 
  filter(temperature %in% c(20, 25, 30)) |> 
  droplevels()

tb_mod2 <- gnls(Estimate ~ b*(temperature - tb), 
               data = inv_time_est_set2, 
               params = list(tb ~ 1, b ~ 0+ percent), 
               start = c(b = c(0.1, 0.1, 0.1, 0.1, 0.1), 
                         tb = c(15)))

tb_emm <- emmeans(tb_mod2, ~ 1, param = 'tb', df = 2) |> 
  as.data.frame()  
print(tb_emm)

confint(tb_mod2, parm = 'tb') # here is profile CI, FYI

b_emm <- emmeans(tb_mod2, ~ percent, param = 'b', df = 2) |> 
  as.data.frame()
print(b_emm)

# Plot the results:
# first, generate predictions (with CIs) over a fine grid of time points
celosia_toplot <- celosia_set |> 
  distinct(temperature) |> 
  merge(data.frame(time = seq(0, 12, 0.25)))

celosia_toplot <- avg_predictions(celosia_mod, newdata = celosia_toplot, 
                                  by = c('temperature', 'time'))

# Plot the results as smooth line and confidence band, along with original data.
# Its possible to make this figure prettier, but I leave that up to you.
germination_plot = ggplot(celosia_toplot, aes(x = time, color = temperature, fill = temperature))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1, color = NA)+
  geom_line(aes(y = estimate), linewidth = 1) +
  geom_point(aes(y = pct_germ), data = celosia, size = 2) +
  scale_x_continuous(name = 'Days After Sowing', breaks = 0:12) +
  scale_y_continuous(name = 'Germination (%)') +
  theme_classic()  
plot(germination_plot)

# Normally, here I would also generate predictions from the fitted model 
# so that they could be plotted as trend lines, but for some 
# reason that isn't working here, possibly because there are different
# submodels for the tb and b parameters. Instead, I'll add them
# to the plot as line segments
germ_rate_toplot <- data.frame(percent = c(10, 20, 30), 
                               b = b_emm$emmean[1:3], 
                               tb = tb_emm$emmean)

germ_rate_plot = ggplot(inv_time_ests, aes(x = temperature, color = percent))+
  geom_point(aes(y = Estimate), size = 2.5)+
  geom_errorbar(aes(ymin = Estimate -SE, ymax = Estimate + SE), width = .7) +
  geom_segment(aes(x = 17, xend = 30, y = 0, yend = b*(30 - tb), 
                   color = factor(percent)), data = germ_rate_toplot, 
               linewidth = 1.1)+
  scale_x_continuous(name = 'Temperature', limits = c(0, 45), breaks = seq(0, 45, 5)) +
  scale_y_continuous(name = 'Inverse time', limits = c(0, 0.9), breaks = seq(0, 0.8, 0.2)) +
  theme_classic()

plot(germ_rate_plot)

# Export the results to excel for inclusion in a manuscript or chapter
result_list = list('Parameter Estimates' = celosia_param_ests, 
                   'Inverse Time Estimates' = inv_time_ests, 
                   'Tbase Estimate' = tb_emm)

write_xlsx(result_list, path = 'Celosia Results.xlsx')

ggsave(filename = 'Germination Plot.jpg', plot = germination_plot, 
       device = 'jpg', height = 5, width = 7, units = 'in')

ggsave(filename = 'Germination Rate Plot.jpg', plot = germ_rate_plot, 
       device = 'jpg', height = 5, width = 7, units = 'in')




