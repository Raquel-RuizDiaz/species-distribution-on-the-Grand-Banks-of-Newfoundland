##################
# LOAD LIBRARIES
###################
library(dplyr)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(sdmTMB)
#install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
library(INLA)
#inla.upgrade(testing=TRUE)
library(inlabru)

#####################################
## CLIMATE MODELS PROJECTIONS
#####################################
## upload grids FOR projections
grid<- read.table("dfo-tesa-2023/data/Projections_ROM_1996_2100_grid.txt", dec=".", header=TRUE)
ROM_grid<- na.omit(grid)
ROM_grid$depth_log<- log(ROM_grid$depmean)
ROM_grid$model<- "ROM"
ROM_grid$scenario<- "RCP_370"

grid<- read.table("dfo-tesa-2023/data/Projections_IPSL370_1996_2100_grid.txt", dec=".", header=TRUE)
IPSL_grid370<- na.omit(grid)
IPSL_grid370$depth_log<- log(IPSL_grid370$depmean)
IPSL_grid370$model<- "IPSL"
IPSL_grid370$scenario<- "RCP_370"

grid<- read.table("dfo-tesa-2023/data/Projections_IPSL126_1996_2100_grid.txt", dec=".", header=TRUE)
IPSL_grid126<- na.omit(grid)
IPSL_grid126$depth_log<- log(IPSL_grid126$depmean)
IPSL_grid126$model<- "IPSL"
IPSL_grid126$scenario<- "RCP_126"

grid<- read.table("dfo-tesa-2023/data/Projections_GFDL370_1996_2100_grid.txt", dec=".", header=TRUE)
GFDL_grid370<- na.omit(grid)
GFDL_grid370$depth_log<- log(GFDL_grid370$depmean)
GFDL_grid370$model<- "GFDL"
GFDL_grid370$scenario<- "RCP_370"

grid<- read.table("dfo-tesa-2023/data/Projections_GFDL126_1996_2100_grid.txt", dec=".", header=TRUE)
GFDL_grid126<- na.omit(grid)
GFDL_grid126$depth_log<- log(GFDL_grid126$depmean)
GFDL_grid126$model<- "GFDL"
GFDL_grid126$scenario<- "RCP_126"

### UPLOAD THE PROJECTIONS
#HIGH EMISSIONS SCENARIO
preds_future_ROM370<- readRDS("dfo-tesa-2023/FINAL2024/crab/crab_ROM_simulation100.rds")
preds_future_IPSL370<- readRDS("dfo-tesa-2023/FINAL2024/crab/crab_IPSL370_simulation100.rds")
preds_future_GFDL370<- readRDS("dfo-tesa-2023/FINAL2024/crab/crab_GFDL370_simulation100.rds")

#LOW EMISSIONS SCENARIO
preds_future_IPSL126<- readRDS("dfo-tesa-2023/FINAL2024/crab/crab_IPSL126_simulation100.rds")
preds_future_GFDL126<- readRDS("dfo-tesa-2023/FINAL2024/crab/crab_GFDL126_simulation100.rds")

############################
#### HISTORICAL SIMULATIONS
##############################
## upload model
load("dfo-tesa-2023/FINAL2024/crab/crab_projection_model_offset.RData")
fit_hist<- m_crab

AIC(fit_hist)
sanity(fit_hist)

## upload historical grid
grid_hist<- read.table("dfo-tesa-2023/data/Prediction_grid2023.txt", dec=".", header=TRUE)
grid_hist$depth_log<- log(grid_hist$depmean)
summary(grid_hist)

#Predict 100 simulation drawns
set.seed(123)
preds <- predict(fit_hist, grid_hist, nsim = 100L)
saveRDS(preds, "dfo-tesa-2023/FINAL2024/crab/crab_hist_simulation_100.rds")

#######################
## COMBINE GRIDS
#######################
grid_hist$model <- "Historical"

## calculate mean and std. dev.
grid_hist$se <- apply(preds, 1, sd)
ROM_grid$se <- apply(preds_future_ROM370, 1, sd)
IPSL_grid126$se <- apply(preds_future_IPSL126, 1, sd)
GFDL_grid126$se <- apply(preds_future_GFDL126, 1, sd)
IPSL_grid370$se <- apply(preds_future_IPSL370, 1, sd)
GFDL_grid370$se <- apply(preds_future_GFDL370, 1, sd)

grid_hist$est <- apply(preds, 1, mean)
ROM_grid$est <- apply(preds_future_ROM370, 1, mean)
IPSL_grid126$est <- apply(preds_future_IPSL126, 1, mean)
GFDL_grid126$est <- apply(preds_future_GFDL126, 1, mean)
IPSL_grid370$est <- apply(preds_future_IPSL370, 1, mean)
GFDL_grid370$est <- apply(preds_future_GFDL370, 1, mean)

grid_hist$scenario <- "RCP_126"
grid_hist2<- grid_hist
grid_hist2$scenario<- "RCP_370"

grid_hist<- rbind(grid_hist, grid_hist2)
save(grid_hist, file = "dfo-tesa-2023/FINAL2024/grid_hist_crab_data_maps.RData")

#### Combine estimates and standard errors dataframes
combined_est_se <- rbind(IPSL_grid126 , GFDL_grid126, IPSL_grid370 , GFDL_grid370, ROM_grid)

save(combined_est_se, file = "dfo-tesa-2023/FINAL2024/crab/crab_projections_outputs.RData")

###############################
## BIOMASS PROJECTIONS
## Figure 3-5
################################

### CALCULATE PERCENTAGE CHANGE
load(file = "dfo-tesa-2023/FINAL2024/crab/crab_projections_outputs.RData")
load(file = "dfo-tesa-2023/FINAL2024/grid_hist_crab_data_maps.RData")

combined_final$est<- exp(combined_final$est)combined_final <- combined_final %>%  filter(depmean > 35 & depmean < 750)### CALCULATE PERCENTAGE CHANGErefDecade <- combined_final %>%  filter(model == "Historical") %>%  filter(year %in% 1996:2019) %>%  #group_by(year) %>%  dplyr::summarize(refEst = mean(est), refSe=mean(se))ChangeBio<-combined_final %>%  #filter(model_type == "hist") %>%  group_by(model, scenario, year) %>%  dplyr::summarize(est = mean(est), se=mean(se))  %>%  mutate(BioChange = (est - refDecade$refEst)/refDecade$refEst * 100,         BioChange_SE = (se / refDecade$refEst) * 100)meanval<- ChangeBio  %>%  filter(year %in% 2077:2100) %>%  group_by(model, scenario) %>%  summarise(Bio_change_mean = mean(BioChange), Se_mean = mean(BioChange_SE))###########################
CALCULATE ROLLING MEAN
##########################
library(zoo)window_size <- 10 ChangeBio_smoothed <- ChangeBio %>%  group_by(model, scenario) %>%  arrange(year) %>%  mutate(    BioChange_MA = rollmean(BioChange, k = window_size, fill = NA, align = "center"),    SD_MA = rollmean(exp(se), k = window_size, fill = NA, align = "center"),    SD_upper = BioChange_MA + SD_MA,    SD_lower = BioChange_MA - SD_MA  )

### FIGURES 3-5 (EXAMPLE CRAB)p_S<- ggplot(ChangeBio_smoothed, aes(x = year, y = BioChange, color = model, fill = model)) +  annotate("rect", xmin = 1996, xmax = 2019, ymin = -Inf, ymax = Inf, alpha = .1)+  geom_line(alpha = 0.3) +  geom_line(data = ChangeBio_smoothed[ChangeBio_smoothed$model == "Historical",], aes(y = BioChange), alpha = 0.5) +  geom_ribbon(aes(ymin = SD_lower, ymax = SD_upper), alpha = 0.2, color = NA) +  # SD band  geom_line(aes(y = BioChange_MA), size = 1) +  # Moving average  geom_line(data = ChangeBio_smoothed, aes(y = BioChange_MA), size = 1.5) +  theme(panel.grid = element_blank()) +  geom_hline(yintercept = 0, color = "grey60", size = 1, lty=2)+  #geom_point(data=mean_val, aes(x=year, y=mean_B), size = 1.5, shape = 15) +  scale_y_continuous(name = "Change in Biomass (%)") +  scale_x_continuous(limits = c(1995, 2100), breaks = pretty(ChangeBio$year, n = 10), name = "Year")+  scale_color_manual(values = c("#8B008B", "black" ,"gold", "chartreuse3"), name = "Climate model", labels = c("GFDL", "Historical", "IPSL", "ACM"))+  scale_fill_manual(values = c("#8B008B", "black" ,"gold", "chartreuse3"), name = "Climate model", labels = c("GFDL","Historical", "IPSL", "ACM"))+  facet_wrap(~scenario, labeller = labeller(scenario =                                              c("RCP_126" = "Low emissions",                                                "RCP_370" = "High emissions"))) +  theme_bw()+  theme(text = element_text(size=12),        axis.title.y = element_text(vjust=0.4, size = 12),        axis.title.x = element_text(vjust=0.3, size = 12),        axis.text.x = element_text(angle=90, hjust=0.5),        panel.grid.minor = element_blank(),        legend.background= element_rect(colour = 'black', fill = 'white', linetype='solid', size = 0.1),        panel.border = element_rect(colour = "black"),        panel.grid.major = element_blank(),        strip.text.x = element_text(face = "bold"),        legend.position = c(0.7,0.15),        legend.key.size = unit(0.3, "cm"))### FIGURE COMPARING CLIMATE MODELSa<-  ChangeBio_smoothed %>%  filter(model %in% c("Historical", "IPSL")) %>%  mutate(scenario = ifelse(model == "Historical", "Historical", scenario)) %>%  mutate(model = ifelse(model == "Historical", "IPSL", model))b<-  ChangeBio_smoothed %>%  filter(model %in% c("Historical", "GFDL")) %>%  mutate(scenario = ifelse(model == "Historical", "Historical", scenario)) %>%  mutate(model = ifelse(model == "Historical", "GFDL", model))c<- ChangeBio_smoothed %>%  filter(model %in% c("Historical","ROM")) %>%  mutate(scenario = ifelse(model == "Historical", "Historical", scenario)) %>%  mutate(model = ifelse(model == "Historical", "ROM", model))crab2<- rbind(a,b,c)p_CM<- ggplot(crab2, aes(x = year, y = BioChange, color = scenario, fill =scenario)) +  annotate("rect", xmin = 1996, xmax = 2019, ymin = -Inf, ymax = Inf, alpha = .1)+  geom_line(alpha = 0.3) +  geom_line(data = cod2[cod2$scenario == "Historical",], aes(y = BioChange), alpha = 0.5) +  geom_ribbon(aes(ymin = SD_lower, ymax = SD_upper), alpha = 0.2, color = NA) +  # SD band  geom_line(aes(y = BioChange_MA), size = 1) +  # Moving average  geom_line(data = cod2, aes(y = BioChange_MA), size = 1.5) +  theme(panel.grid = element_blank()) +  #geom_line(aes(color = scenario), size=0.3) +  #geom_vline(xintercept = 2019.2, color = "grey60", size = 1)+  geom_hline(yintercept = 0, color = "grey60", size = 1, lty=2)+  scale_y_continuous(name = "Change in Biomass (%)") +  scale_x_continuous(limits = c(1996, 2100), breaks = pretty(cod2$year, n = 10), name = "Year")+  scale_color_manual(values = c("black", "blue3","red2"), name = "Scenario", labels = c("Historical", "Low emissions", "High emissions"))+  scale_fill_manual(values = c("black", "blue3","red2"), name = "Scenario", labels = c("Historical", "Low emissions", "High emissions"))+  theme_bw()+  facet_wrap(~model, labeller = labeller(model =                                           c("GFDL" = "GFDL",                                             "IPSL" = "IPSL",                                             "ROM" = "ACM"))) +  theme(text = element_text(size=12),        axis.title.y = element_text(vjust=0.4, size = 12),        axis.title.x = element_text(vjust=0.3, size = 12),        axis.text.x = element_text(angle=90, hjust=0.5),        panel.grid.minor = element_blank(),        legend.background= element_rect(colour = 'black', fill = 'white', linetype='solid', size = 0.1),        panel.border = element_rect(colour = "black"),        panel.grid.major = element_blank(),        strip.text.x = element_text(face = "bold"),        legend.position = c(0.8, 0.15), #c(0.84, 0.15)        legend.key.size = unit(0.3, "cm"))library(patchwork)p_S/p_CM + plot_annotation(tag_levels = "a")ggsave("dfo-tesa-2023/FINAL_outputs/Figures2024/crab_BiomassChange.png", height = 6.5*1.5, width = 8.5*1.5, dpi = 600)
