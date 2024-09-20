################################
#### Model validations
#################################

####################
### LOAD LIBRARIES
#####################
library(TMB)
library(sdmTMB)
library(dplyr)
library(ggplot2)
library(viridis)
library(ggthemes)
library(tidyverse)
library(sdmTMB)
library(visreg)
library(sdmTMBextra)
library(patchwork)

#####################
## ADD NL SHAPE
#####################
library(sf)
library(raster)
crs_raster_format <- "+proj=utm +zone=21 +datum=WGS84 +units=km +no_defs +ellps=WGS84 +towgs84=0,0,0"

# upload land shapefile
cat_rec<- readRDS("dfo-tesa-2023/data/cat_rec.rds")

cat_rec_sf<- cat_rec %>%
  spTransform(CRS(crs_raster_format)) %>%
  st_as_sf()

plot(cat_rec_sf)

#####################
### load models (O1-RSCRIPT)
#####################

yt<- load(file = "dfo-tesa-2023/FINAL_outputs/cod/yt_finalmodel.RData")
crab<- load(file = "dfo-tesa-2023/FINAL_outputs/crab/crab_finalmodel.RData")
cod<- load(file = "dfo-tesa-2023/FINAL_outputs/crab/cod_finalmodel.RData")

fit<- m_lfocv_SP
sanity(fit)
AIC(fit)

sink(file = "dfo-tesa-2023/FINAL_outputs/cod/model_fit_output_cod.txt")
summary(fit)
sink(file = NULL)

#We can view the confidence intervals on the fixed effects by using the tidy function:
tidy(fit, conf.int = TRUE)
tidy(fit, "ran_pars", conf.int = TRUE)

###############################
##### prediction over the full domain 
################################

grid<- read.table("dfo-tesa-2023/data/Prediction_grid2023.txt", dec=".", header=TRUE)
grid$depth_log<- log(grid$depmean)
p <- predict(fit, newdata = grid)

pred<- p  %>%
  filter(depmean > 35 & depmean < 730)

#####################
#PLOTS
#####################
theme_maps_legend<- theme(legend.key.size = unit(1, 'cm'), #change legend key size
                          legend.key.height = unit(1, 'cm'), #change legend key height
                          legend.key.width = unit(1, 'cm'), #change legend key width
                          legend.title = element_text(size=12), #change legend title font size
                          legend.text = element_text(size=14), #change legend text font size
                          plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
                          plot.margin = unit(c(1, 1, 0.5, 1), "pt"),
                          strip.text = element_text(size=12, face="bold"), #change facet wrap size
                          strip.background = element_rect(color="white", fill="white", size=1.5, linetype="solid")) #change facet_wrap color



##BIOMASS (combined predictions) ###
pdf("dfo-tesa-2023//FINAL_outputs/cod/figures/combined_biomass_pred.pdf", width=5, height = 5)
ggplot() +
  geom_raster(data =pred,
              aes(x = X.utm.km,
                  y = Y.utm.km,
                  fill = plogis(est1) * exp(est2))) +
  scale_fill_viridis(option ="H", trans="log10", name= "Biomass\n(Kg/25Km2)") +

  geom_sf(data = cat_rec_sf,
          color = "black",
          fill = "darkgrey",
          size = 0.2) +

  theme_map() +
  theme(legend.position="right") +
  theme_maps_legend
dev.off()


#########################################
### Model diagnostics
#######################################
fit<-m_lfocv_SP

## plot residuals
data$resid1 <- residuals(fit, model=1, type = "mle-mvn")
data$resid2 <- residuals(fit, model=2, type = "mle-mvn")
gamma_res<- data %>%
  filter(biomass > 0)

pdf("dfo-tesa-2023//FINAL_outputs/cod/figures/residuals_cod.pdf", width=8, height = 6)
par(mfrow=c(2,2))
par(mar=c(5,5,1,1))
qqnorm(data$resid1, main = "Binomial Residuals")
qqline(data$resid1)
hist(data$resid1, main = "Binomial Residuals Histogram")
qqnorm(gamma_res$resid2, main = "Gamma Residuals") #Lognormal
qqline(gamma_res$resid2)
hist(gamma_res$resid2, main = "Gamma Residuals Histogram")
dev.off()

#####################
## check spatial patterns in the residuals
#####################

#pdf("dfo-tesa-2023//FINAL_outputs/cod/figures/spatial_res_patterns_bin.pdf", width=10, height = 15)
R1<- ggplot(data, aes(X.utm.km, Y.utm.km, colour = resid1)) +
  #facet_wrap(~year_f) +
  geom_point(size = 1) +
  coord_fixed() +
  scale_colour_gradient2(name = "Binomial\nResiduals") +
  theme_bw()


#pdf("dfo-tesa-2023//FINAL_outputs/cod/figures/spatial_res_patterns_lognormal.pdf", width=10, height = 15)
R2<- ggplot(gamma_res, aes(X.utm.km, Y.utm.km, colour = resid2)) +
  #facet_wrap(~year_f) +
  geom_point(size = 1) +
  coord_fixed() +
  scale_colour_gradient2(name = "Lognormal\nResiduals") +
  theme_bw()

R1 / R2 + plot_annotation(tag_levels = "a")
ggsave("dfo-tesa-2023/FINAL_outputs/cod/figures/spatial_effects_cod_combined.png", height = 6*1.5, width = 5*1.5)


### MCMC-based randomized-quantile residuals
#note: The above approach assumes an observation of the random effects can be approximated by a multivariate normal distribution.
#If we want to relax that assumption, we can sample the random effects with MCMC with the fixed effects held at their MLEs
r <- sdmTMBextra::predict_mle_mcmc(fit, mcmc_iter = 400, mcmc_warmup = 300)
mcmc_res <- residuals(fit, type = "mle-mcmc", mcmc_samples = r)
qqnorm(mcmc_res)
abline(0, 1)

