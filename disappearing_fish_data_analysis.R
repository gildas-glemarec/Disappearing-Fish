##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Disappearing fish
## Author: Gildas Glemarec (DTU Aqua)
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Testing whether the daily disappearance of fish is statistically different 
## from zero (t-test) and estimate the daily seal intake (as percent of the 
## catch taken by seal in the previous 24 hours).
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## LOAD LIBRARIES AND FUNCTIONS ####
gc()
par(mfrow = c(1,1))
Sys.setenv(LANG = "en") # force error messages to English
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse,data.table,glmmTMB,DHARMa)
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load dataset ####
TableA1 <- fread('TableA1.csv')
TableA1$f.day_since_initial_haul <- as.factor(TableA1$f.day_since_initial_haul)
TableA1$`Trial number` <- as.factor(TableA1$`Trial number`)
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Data summaries ####
## Number of fishing trips
n_distinct(TableA1$`Trial number`) ## 10
## Number of fishing days
n_distinct(TableA1$date) ## 42
## Number of fishes set vs disappeared
sum(TableA1$`cod set`);sum(TableA1$`cod lost`) ## 636 vs 293
sum(TableA1$`flounder set`);sum(TableA1$`flounder lost`) ## 239 vs 12
sum(TableA1$`plaice set`);sum(TableA1$`plaice lost`) ## 233 vs 14
sum(TableA1$`turbot set`);sum(TableA1$`turbot lost`) ## 180 vs 6
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Statistical tests and models ####
### COD DISAPPEARANCE ####

#### One-sample t-test ####
#### Is the disappearance rate for cods statistically 
#### different from zero?
t.test(TableA1[`cod set`>0]$`perc. cod lost`,
       mu = 0)

#### Boxplots ####
boxplot(data = TableA1[`cod set`>0],
        `perc. cod lost` ~ day_since_initial_haul)
boxplot(data = TableA1[year(date) == 2021][`cod set`>0],
        `perc. cod lost` ~ day_since_initial_haul)
boxplot(data = TableA1[year(date) == 2022][`cod set`>0],
        `perc. cod lost` ~ day_since_initial_haul)

#### Predict cod disappearance rates per 24 hour when we leave the nets in the 
#### same place
mod_cod_disp <- glmmTMB(`perc. cod lost` ~ 
                          f.day_since_initial_haul +
                          (1 | `Trial number`),
                        family = tweedie(),
                        data = TableA1[`cod set`>0]
)
summary(mod_cod_disp)
simout <- simulateResiduals(mod_cod_disp)
plot(simout) ## Good model (although missing samples in setting date + 5 and 
## setting date + 6)
cod_pred_plot <- emmeans::emmip(mod_cod_disp, ~ f.day_since_initial_haul, 
                                CIs = TRUE, type = "response", plotit = FALSE) %>% 
  mutate(species = "Cod")
cod_pred_plot$UCL <- ifelse(cod_pred_plot$UCL>=1,
                            1,
                            cod_pred_plot$UCL)
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### FLOUNDER DISAPPEARANCE ####

#### One-sample t-test ####
#### Is the disappearance rate for flounder statistically 
#### different from zero?
t.test(TableA1[`flounder set`>0][year(date) == 2022]$`perc. flounder lost`,
       mu = 0)

#### Boxplots ####
boxplot(data = TableA1[`flounder set`>0],
        `perc. flounder lost` ~ day_since_initial_haul)
boxplot(data = TableA1[year(date) == 2022][`flounder set`>0],
        `perc. flounder lost` ~ day_since_initial_haul)

#### Predict cod disappearance rates per 24 hour when we leave the nets in the 
#### same place
mod_flounder_disp <- glmmTMB(`perc. flounder lost` ~ 
                               f.day_since_initial_haul +
                               (1 | `Trial number`),
                             family = tweedie(),
                             data = TableA1[`flounder set`>0][year(date) == 2022]
)
summary(mod_flounder_disp)
simout_flounder <- simulateResiduals(mod_flounder_disp)
plot(simout_flounder) ## Good model (although missing samples in setting date + 5 and 
## setting date + 6)
flounder_pred_plot <- emmeans::emmip(mod_flounder_disp, ~ f.day_since_initial_haul, 
                                     CIs = TRUE, type = "response", plotit = FALSE) %>% 
  mutate(species = "Flounder")
flounder_pred_plot$UCL <- ifelse(flounder_pred_plot$UCL>=1,
                                 1,
                                 flounder_pred_plot$UCL)
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### PLAICE DISAPPEARANCE ####

#### One-sample t-test ####
#### Is the disappearance rate for plaice statistically 
#### different from zero?
t.test(TableA1[`plaice set`>0][year(date) == 2022]$`perc. plaice lost`,
       mu = 0)

#### Boxplots ####
boxplot(data = TableA1[`plaice set`>0],
        `perc. plaice lost` ~ day_since_initial_haul)
boxplot(data = TableA1[year(date) == 2022][`plaice set`>0],
        `perc. plaice lost` ~ day_since_initial_haul)

#### Predict cod disappearance rates per 24 hour when we leave the nets in the 
#### same place
mod_plaice_disp <- glmmTMB(`perc. plaice lost` ~ 
                             f.day_since_initial_haul +
                             (1 | `Trial number`),
                           family = tweedie(),
                           data = TableA1[`plaice set`>0][year(date) == 2022]
)
summary(mod_plaice_disp)
simout_plaice <- simulateResiduals(mod_plaice_disp)
plot(simout_plaice) ## Good model (although missing samples in setting date + 5 and 
## setting date + 6)
plaice_pred_plot <- emmeans::emmip(mod_plaice_disp, ~ f.day_since_initial_haul, 
                                   CIs = TRUE, type = "response", plotit = FALSE) %>% 
  mutate(species = "Plaice")
plaice_pred_plot$UCL <- ifelse(plaice_pred_plot$UCL>=1,
                               1,
                               plaice_pred_plot$UCL)
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### TURBOT DISAPPEARANCE ####

#### One-sample t-test ####
#### Is the disappearance rate for turbot statistically 
#### different from zero?
t.test(TableA1[`turbot set`>0][year(date) == 2022]$`perc. turbot lost`,
       mu = 0)

#### Boxplots ####
boxplot(data = TableA1[`turbot set`>0],
        `perc. turbot lost` ~ day_since_initial_haul)
boxplot(data = TableA1[year(date) == 2022][`turbot set`>0],
        `perc. turbot lost` ~ day_since_initial_haul)

#### Predict cod disappearance rates per 24 hour when we leave the nets in the 
#### same place
mod_turbot_disp <- glmmTMB(`perc. turbot lost` ~ 
                             f.day_since_initial_haul +
                             (1 | `Trial number`),
                           family = tweedie(),
                           data = TableA1[`turbot set`>0][year(date) == 2022]
)
summary(mod_turbot_disp) ## Does not converge
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Plot Figure 2 ####
predictions_fish <- rbind(cod_pred_plot,flounder_pred_plot,plaice_pred_plot)
dodge <- position_dodge(width=0.3)
fig2 <- ggplot(data = predictions_fish, 
               aes(x = f.day_since_initial_haul, y = yvar, 
                   group = species, colour = species, shape = species)) +
  geom_linerange( aes(ymin = LCL, ymax = UCL), position = dodge) +
  geom_point(size = 3, position = dodge) +
  scale_color_viridis(discrete = TRUE) +
  labs(x = "Number of days since initial setting", y = "Daily fish disappearance rate") +
  ylim(c(0:1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        legend.title = element_blank(),
        legend.position = "top",
        panel.grid.major.x = element_blank()
  )
fig2