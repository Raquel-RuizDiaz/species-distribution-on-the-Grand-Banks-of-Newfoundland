###### Forecasting analysis and maps #####library(INLA)
library(sdmTMB)
library(sdmTMBextra)
library(tidyverse)

################################################
######## RUN THE MODEL WITH EXTRA TIME 2020:2100
#################################################

#Upload database
data<- read.csv("./data/data_full_crab.csv", dec=";", header=TRUE)

# filter by season
data2<- data  %>%
  filter(season== "Spring")

#clean database
data<-na.omit(data2)

data$biomass<- as.numeric(data$biomass)
data$depmean<- as.numeric(data$depmean)
data$tempatfishing<- as.numeric(data$tempatfishing)
data$X.utm<- as.numeric(data$X.utm)
data$Y.utm<- as.numeric(data$Y.utm)
data$year<- data$surveyyear

data$year_f<- as.factor(data$surveyyear)

data$X.utm.km<- data$X.utm/1000
data$Y.utm.km<- data$Y.utm/1000
data$depth_log<- log(data$depmean)

data$effort<- 0.026
effort<- data$effort

###############################################################################################
## CREATE THE MESH
###############################################################################################
mesh <- make_mesh(data, xy_cols = c("X.utm.km", "Y.utm.km"), cutoff = 15)
mesh$mesh$n

###############################################################################################
# FINAL MODEL
###############################################################################################
extratime<- as.integer(2020:2100)

m_crab <- sdmTMB(
  biomass ~ poly(depth_log, 2) + poly(tempatfishing,2),
  data = data,
  mesh = mesh,
  offset=log(effort),
  family = delta_gamma(), #< new
  spatial = "on",
  spatiotemporal = "off",
  time = "year",
  extra_time = extratime # `L` for integer to match our data
)
if(max(m_crab$gradients)>0.01){
  m_crab <- sdmTMB::run_extra_optimization(m_crab, nlminb_loops = 1L)
}

save(m_crab, file = "./rds/crab_projection_model_offset.RData")

#################################
## FORECASTING
#########################################
fit<- m_crab

##### OPEN GRIDS #####
### ROM
grid<- read.table("./data/Projections_ROM_1996_2100_grid.txt", dec=".", header=TRUE)
ROM_grid<- na.omit(grid)
ROM_grid$depth_log<- log(ROM_grid$depmean)
ROM_grid$model<- "ROM"

###IPSL
grid<- read.table("./data/Projections_IPSL126_1996_2100_grid.txt", dec=".", header=TRUE)
IPSL_grid126<- na.omit(grid)
IPSL_grid126$depth_log<- log(IPSL_grid126$depmean)
IPSL_grid126$model<- "IPSL"

grid<- read.table("./data/Projections_IPSL370_1996_2100_grid.txt", dec=".", header=TRUE)
IPSL_grid370<- na.omit(grid)
IPSL_grid370$depth_log<- log(IPSL_grid370$depmean)
IPSL_grid370$model<- "IPSL"

## GFDL
grid<- read.table("./data/Projections_GFDL126_1996_2100_grid.txt", dec=".", header=TRUE)
GFDL_grid126<- na.omit(grid)
GFDL_grid126$depth_log<- log(GFDL_grid126$depmean)
GFDL_grid126$model<- "GFDL"

grid<- read.table("./data/Projections_GFDL370_1996_2100_grid.txt", dec=".", header=TRUE)
GFDL_grid370<- na.omit(grid)
GFDL_grid370$depth_log<- log(GFDL_grid370$depmean)
GFDL_grid370$model<- "GFDL"

###############################################
# Calculate future projections for each model
###############################################

set.seed(123)
preds_future_ROM <- predict(fit, ROM_grid, nsim = 100L)
preds_future_IPSL126 <- predict(fit, IPSL_grid126, nsim = 100L)
preds_future_IPSL370 <- predict(fit, IPSL_grid370, nsim = 100L)
preds_future_GFDL126 <- predict(fit, GFDL_grid126, nsim = 100L)
preds_future_GFDL370 <- predict(fit, GFDL_grid370, nsim = 100L)

saveRDS(preds_future_ROM, "./rds/crab_ROM_simulation.rds")
saveRDS(preds_future_IPSL126, "./rds/crab_IPSL126_simulation100.rds")
saveRDS(preds_future_IPSL370, "./rds/crab_IPSL370_simulation100.rds")
saveRDS(preds_future_GFDL126, "./rds/crab_GFDL126_simulation100.rds")
saveRDS(preds_future_GFDL370, "./rds/crab_GFDL370_simulation100.rds")

#### HISTORICAL SIMULATIONS## upload modelload(file = "./rds/crab_modelselection.RData")fit_hist<- m_lfocv_SPgrid_hist<- read.table("./data/Prediction_grid2023.txt", dec=".", header=TRUE)grid_hist$depth_log<- log(grid_hist$depmean)summary(grid_hist)set.seed(123)preds <- predict(fit_hist, grid_hist, nsim = 100L)saveRDS(preds, "./rds/crab_hist_simulation.rds")