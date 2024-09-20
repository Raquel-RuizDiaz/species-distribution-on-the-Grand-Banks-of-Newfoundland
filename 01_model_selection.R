##############################
#### MODEL SELECTION #########
##############################

#Libraries
#install.packages("remotes")
#remotes::install_github("pbs-assess/sdmTMBextra")

library(sdmTMB)
library(INLA)
#library(inlabru)
library(tidyverse)
library(sdmTMBextra)

library(future)
plan(multisession)

###############################################################################################
### OPEN DATA
###############################################################################################

#Upload database
data<- read.csv("dfo-tesa-2023/data/data_full_crab.csv", dec=";", header=TRUE) # EXAMPLE FOR SNOW CRAB

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
#TESTING DIFFERENT MODEL CONFIGURATIONS
###############################################################################################

#SPATIAL ON
m_lfocv_SP<- sdmTMB(
biomass ~ poly(depth_log, 2) +  poly(tempatfishing,2),
data = data,
mesh = mesh,
offset = log(effort),
family = delta_lognormal(),
spatial = "on",
spatiotemporal = "off",
time = "year")

sanity(m_lfocv_SP)

m_lfocv_SP_dg<- sdmTMB(
biomass ~ poly(depth_log, 2) +  poly(tempatfishing,2),
data = data,
mesh = mesh,
offset = log(effort),
family = delta_gamma(),
spatial = "on",
spatiotemporal = "off",
time = "year")

sanity(m_lfocv_SP_dg)

#SIMPLE (Spatial off)
m_lfocv_SIMPLE<- sdmTMB(
biomass ~  poly(depth_log, 2)  +  poly(tempatfishing,2),
data = data,
mesh = mesh,
offset = log(effort),
family = delta_gamma(),
spatial = "off",
spatiotemporal = "off",
time = "year")

sanity(m_lfocv_SIMPLE)

m_lfocv_DEPTH<- sdmTMB(
biomass ~ poly(depth_log, 2),
data = data,
mesh = mesh,
offset = log(effort),
family = delta_gamma(),
spatial = "off",
spatiotemporal = "off",
time = "year")

sanity(m_lfocv_DEPTH)

#####################
## CHECK AIC
#####################
AIC(m_lfocv_SP, m_lfocv_SP_dg, m_lfocv_SIMPLE, m_lfocv_DEPTH)

#####################
#Save models
#####################

save(m_lfocv_SP, m_lfocv_SIMPLE, m_lfocv_DEPTH, file = "dfo-tesa-2023/FINAL_outputs/crab/crab_modelselection.RData")

############################################################
## FUNCTION TO CALCULATE DEVIANCE ENSEMBLE
#############################################################

null = sdmTMB(biomass ~ 1, spatial = "off", mesh = mesh, data = data, offset = log(effort))models <- list(  null = null,  m_lfocv_SP = m_lfocv_SP,   m_lfocv_SP_dg = m_lfocv_SP_dg,  m_lfocv_SIMPLE = m_lfocv_SIMPLE,  m_lfocv_DEPTH = m_lfocv_DEPTH)#null <- models[1]null_dev <- -2 * as.numeric(logLik(null))log_lik <- unlist(lapply(models, logLik))resid_dev <- -2 * log_likdev_explained <- 100 * (null_dev - resid_dev)/null_devdev_explained

