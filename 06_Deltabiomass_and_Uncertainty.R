##########################
#SPATIAL DELTA_BIOMASSS
#FIGURE 6
##########################


#### MAP of the change #####

theme_maps_legend<- theme(legend.key.size = unit(0.7, 'cm'), #change legend key size
                          legend.key.height = unit(0.7, 'cm'), #change legend key height
                          legend.key.width = unit(0.7, 'cm'), #change legend key width
                          legend.title = element_text(size=12), #change legend title font size
                          legend.text = element_text(size=12), #change legend text font size
                          plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
                          plot.margin = unit(c(1, 1, 0.5, 1), "pt"),
                          panel.grid.minor = element_blank(),
                          panel.border = element_rect(colour = "black"),
                          panel.grid.major = element_blank(),
                          strip.text = element_text(size=12, face="bold"), #change facet wrap size
                          strip.background = element_rect(color="white", fill="white", size=1.5, linetype="solid")) #change facet_wrap color

##################
### LOAD OUPUTS
####################
### BIOMASS ESTIMATES AND DEVIATIONS DATAFRAME
#load(file = "dfo-tesa-2023/Final2024/yellowtail/yt_projections_outputs.RData")
load(file = "dfo-tesa-2023/Final2024/crab/crab_projections_outputs.RData")
#load(file = "dfo-tesa-2023/Final2024/cod/cod_projections_outputs.RData")

### GRID
#load(file = "dfo-tesa-2023/Final2024/grid_hist_yt_data_maps.RData")
load(file = "dfo-tesa-2023/Final2024/grid_hist_crab_data_maps.RData")
#load(file = "dfo-tesa-2023/Final2024/grid_hist_cod_data_maps.RData")


## SELECT THE LAST 24 YEARS OF THE PROJECTIONS AND HIGH EMISSIONS SCENARIOS
combined_se_future <- combined_est_se %>%  filter(year %in% 2077:2100) %>%  filter(scenario == "RCP_370")combined_final <- rbind(grid_hist, combined_se_future)combined_final$est <-  exp(combined_final$est)

#FILTER DEPTH BASED ON OBSERVATIONS
combined_final <- combined_final %>%
  filter(depmean > 30 & depmean < 750)

#####################
### CALCULATE PERCENTAGE CHANGE
#####################

refDecade <- combined_final %>%  filter(model == "Historical") %>%  group_by(X.utm.km, Y.utm.km) %>%  dplyr::summarize(refEst = mean(est), refSe= mean(se))ChangeBio_all<-combined_final %>%  filter(model != "Historical") %>%  group_by(model, X.utm.km, Y.utm.km) %>%  dplyr::summarize(est = mean(est), se=mean(se))  %>%  full_join(refDecade) %>%  na.omit() %>%  group_by(model, X.utm.km, Y.utm.km) %>%  mutate(BioChange = (est - refEst)/refEst * 100,         est_delta = est - refEst)

#### SAVE OUPUTS ##############
#save(ChangeBio_all, file = "dfo-tesa-2023/Final_outputs/yellowtail/delta_bioma_data_yt.RData")
save(ChangeBio_all, file = "dfo-tesa-2023/Final_outputs/crab/delta_bioma_data_crab.RData")
#save(ChangeBio_all, file = "dfo-tesa-2023/Final_outputs/cod/delta_bioma_data_cod.RData")


#############################
#LOAD OUTPUTS
#############################
crab<- load("dfo-tesa-2023/Final_outputs/crab/delta_bioma_data_crab.RData")
crab<- ChangeBio_all
#yt<- load("dfo-tesa-2023/Final_outputs/yellowtail/delta_bioma_data_yt.RData")
#yt<- ChangeBio_all
#cod<- load(file ="dfo-tesa-2023/Final_outputs/cod/delta_bioma_data_cod.RData")
#cod<- ChangeBio_all

#####################################
#ADD NL SHAPE
#####################################
library(RColorBrewer)
library(viridis)
library(sf)
library(raster)
crs_raster_format <- "+proj=utm +zone=21 +datum=WGS84 +units=km +no_defs +ellps=WGS84 +towgs84=0,0,0"

# upload land shapefile
cat_rec<- readRDS("dfo-tesa-2023/data/cat_rec.rds")

cat_rec_sf<- cat_rec %>%
  spTransform(CRS(crs_raster_format)) %>%
  st_as_sf()

plot(cat_rec_sf)

###########################
#### DELTA BIOMASS
#### Figure 6
###########################

# Calculate the 0.1th and 99.9th percentiles
lower_limit <- quantile(crab$est_delta, 0.001)  # 0.1th percentile
upper_limit <- quantile(crab$est_delta, 0.999)  # 99.9th percentile

# Constrain the temperature variable to the specified range
crab$constrained_temperature <- pmax(pmin(crab$est_delta, upper_limit), lower_limit)

#Plots
crab_m<- ggplot(crab)+  geom_tile(aes(x = X.utm.km, y = Y.utm.km, fill =est_delta))+  #scale_fill_viridis(option ="A", name= expression(paste(Delta," Biomass", sep = ""))) +  scale_fill_gradient2(low = "dodgerblue3", mid =  "beige", high = "red", name= expression(paste(Delta," Biomass", sep = "")),                       midpoint = 0) +  facet_wrap(~model, labeller = labeller(model =                                           c("GFDL" = "GFDL",                                             "IPSL" = "IPSL",                                             "ROM" = "ACM"))) +  #scale_fill_gradientn(colours = rev(brewer.pal(9, "RdBu")), name = expression(paste("Difference \nin (%) \nchange \n")))+  geom_sf(data = cat_rec_sf,          color = "black",          fill = "darkgrey",          size = 0.2) +  xlab("")+ ylab("")+  #ggtitle("Delta Biomass") +  theme_bw()+  coord_sf(expand = FALSE) +  theme_maps_legend +  theme(    axis.text.x = element_text(angle = 45, hjust = 1), # Rotates x-axis labels 45 degrees    legend.position = "right"  )yt_m<- ggplot(yt)+  geom_tile(aes(x = X.utm.km, y = Y.utm.km, fill = est_delta))+  #scale_fill_viridis(option ="A", name= expression(paste(Delta," Biomass", sep = ""))) +  scale_fill_gradient2(low = "dodgerblue3", mid =  "beige", high = "red", name= expression(paste(Delta," Biomass", sep = "")),                        midpoint = 0) +  facet_wrap(~model, labeller = labeller(model =                                           c("GFDL" = "GFDL",                                             "IPSL" = "IPSL",                                             "ROM" = "ACM"))) +  #scale_fill_gradientn(colours = rev(brewer.pal(9, "RdBu")), name = expression(paste("Difference \nin (%) \nchange \n")))+  geom_sf(data = cat_rec_sf,          color = "black",          fill = "darkgrey",          size = 0.2) +  xlab("")+ ylab("")+  #ggtitle("Delta Biomass") +  theme_bw()+  coord_sf(expand = FALSE) +  theme_maps_legend +  theme(    axis.text.x = element_text(angle = 45, hjust = 1), # Rotates x-axis labels 45 degrees    legend.position = "right"  )cod_m<- ggplot(cod)+  geom_tile(aes(x = X.utm.km, y = Y.utm.km, fill = est_delta))+  #scale_fill_viridis(option ="D", name= expression(paste(Delta," Biomass", sep = ""))) +  scale_fill_gradient2(low = "dodgerblue3", mid =  "beige", high = "red", name= expression(paste(Delta," Biomass", sep = "")),                       midpoint = 0) +  facet_wrap(~model, labeller = labeller(model =                                           c("GFDL" = "GFDL",                                             "IPSL" = "IPSL",                                             "ROM" = "ACM"))) +  #scale_fill_gradientn(colours = rev(brewer.pal(9, "RdBu")), name = expression(paste("Difference \nin (%) \nchange \n")))+  geom_sf(data = cat_rec_sf,          color = "black",          fill = "darkgrey",          size = 0.2) +  xlab("")+ ylab("")+  #ggtitle("Delta Biomass") +  theme_bw()+  coord_sf(expand = FALSE) +  theme_maps_legend +  theme(    axis.text.x = element_text(angle = 45, hjust = 1), # Rotates x-axis labels 45 degrees    legend.position = "right"  )crab_m/yt_m/cod_m + plot_annotation(tag_levels = "a")ggsave("dfo-tesa-2023/Final2024/figures/Delta_biomass_maps_FINAL.png", height = 6*1.5, width = 5*1.5)

################################
#### SPATIAL UNCERTIANTY MAPS
#### Figure 7
#################################

library(viridis)
s1<- ggplot(crab)+  geom_tile(aes(x = X.utm.km, y = Y.utm.km, fill = exp(se)))+  scale_fill_viridis(option ="B", name= "sd", trans="log10") +  facet_wrap(~model) +  geom_sf(data = cat_rec_sf,          color = "black",          fill = "darkgrey",          size = 0.2) +  xlab("")+ ylab("")+  coord_sf(expand = FALSE) +  theme_bw()+  theme_maps_legends2<- ggplot(yt)+  geom_tile(aes(x = X.utm.km, y = Y.utm.km, fill = exp(se)))+  scale_fill_viridis(option ="B", name= "sd",trans="log10" ) +  facet_wrap(~model) +  geom_sf(data = cat_rec_sf,          color = "black",          fill = "darkgrey",          size = 0.2) +  xlab("")+ ylab("")+  coord_sf(expand = FALSE) +  theme_bw()+  theme_maps_legends3<- ggplot(cod)+  geom_tile(aes(x = X.utm.km, y = Y.utm.km, fill = exp(se)))+  scale_fill_viridis(option ="B", name= "sd", trans="log10") +  facet_wrap(~model) +  geom_sf(data = cat_rec_sf,          color = "black",          fill = "darkgrey",          size = 0.2) +  xlab("")+ ylab("")+  coord_sf(expand = FALSE) +  theme_bw()+  theme_maps_legendlibrary(patchwork)s1/s2 /s3 + plot_annotation(tag_levels = "a")ggsave("dfo-tesa-2023/Final2024/uncertainty_maps_normalscale.png", height = 6*1.5, width = 5*1.5)

#####################
## HISTORICAL uncertainty
###########################

#YT<- readRDS("dfo-tesa-2023/Final_outputs/yellowtail/yt_hist_simulation_100.rds")
CRAB<- readRDS("dfo-tesa-2023/Final_outputs/crab/crab_hist_simulation_100.rds")
#COD<- readRDS("dfo-tesa-2023/Final_outputs/cod/cod_hist_simulation_100.rds")

grid_hist<- read.table("dfo-tesa-2023/data/Prediction_grid2023.txt", dec=".", header=TRUE)
grid_hist$depth_log<- log(grid_hist$depmean)
summary(grid_hist)

##calculate mean and sdgrid_hist$se <- apply(COD, 1, sd)grid_hist$est <- apply(COD, 1, mean)#grid_hist$est_c<- exp(grid_hist$est)Cod<- grid_histgrid_hist$se <- apply(CRAB, 1, sd)grid_hist$est <- apply(CRAB, 1, mean)#grid_hist$est_c<- exp(grid_hist$est)Crab<- grid_histgrid_hist$se <- apply(YT, 1, sd)grid_hist$est <- apply(YT, 1, mean)#grid_hist$est_c<- exp(grid_hist$est)Yt<- grid_histp_SDM1<- ggplot(Crab)+  geom_tile(aes(x = X.utm.km, y = Y.utm.km, fill = se))+  scale_fill_viridis(option ="B", name= "sd", trans="log10") +  geom_sf(data = cat_rec_sf,          color = "black",          fill = "darkgrey",          size = 0.2) +  xlab("")+ ylab("")+  coord_sf(expand = FALSE) +  theme_bw()+  theme_maps_legendp_SDM2<- ggplot(Yt)+  geom_tile(aes(x = X.utm.km, y = Y.utm.km, fill = se))+  scale_fill_viridis(option ="B", name= "sd", trans="log10") +  geom_sf(data = cat_rec_sf,          color = "black",          fill = "darkgrey",          size = 0.2) +  xlab("")+ ylab("")+  coord_sf(expand = FALSE) +  theme_bw()+  theme_maps_legendp_SDM3<- ggplot(Cod)+  geom_tile(aes(x = X.utm.km, y = Y.utm.km, fill = se))+  scale_fill_viridis(option ="B", name= "sd", trans="log10") +  geom_sf(data = cat_rec_sf,          color = "black",          fill = "darkgrey",          size = 0.2) +  xlab("")+ ylab("")+  coord_sf(expand = FALSE) +  theme_bw()+  theme_maps_legendp_SDM1<- p_SDM1 + ggtitle("SDM")p_SDM2<- p_SDM2 + ggtitle("SDM")p_SDM3<- p_SDM3 + ggtitle("SDM")p_SDM1 /p_SDM2 /p_SDM3ggsave("dfo-tesa-2023/Final2024/SDM_uncertainty2_normalscale.png", height = 6*1.5, width = 5*1.5)(s1 | p_SDM1) / (s2 | p_SDM2) / (s3 | p_SDM3) + plot_annotation(tag_levels = "a")ggsave("dfo-tesa-2023/Final2024/SDM_uncertainty_FINAL_comb.png", height = 8*1.5, width = 9*1.5)