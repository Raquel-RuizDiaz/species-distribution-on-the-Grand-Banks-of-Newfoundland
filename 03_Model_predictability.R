###############################
## MODEL PREDICTABILITY
## HISTORICAL PERIOD 1996-2019 & OUT OF SAMPLE (2017-2019)
################################

data<- read.csv("dfo-tesa-2023/data/data_full_crab.csv", dec=";", header=TRUE)
data<- read.csv("dfo-tesa-2023/data/data_full_flounder.csv", dec=";", header=TRUE)
data<- read.csv("dfo-tesa-2023/data/data_full_cod.csv", dec=";", header=TRUE)

#####################
# FILTER DATA (Example snow crab)
#####################
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

###################
## testing data (HISTORICAL)
#####################
data.train<- data
data.test<- data

####################
## OR CROSS-VALIDATIONS
## TESTING DATA 2017-2019
#########################
data.train<- data %>%
  filter(year <= 2016)

data.test<- data %>%
  filter(year>= 2017)

#effort
data.train$effort<- 0.026
effort<- data.train$effort

###############################################################################################
## CREATE THE MESH
###############################################################################################
mesh.train <- make_mesh(data.train, xy_cols = c("X.utm.km", "Y.utm.km"), cutoff =15)

############################
## Run models with trainind data
#############################
extratime<- as.integer(2017:2019) #only use this in out-of-sample CV

#SPATIAL
m_lfocv_SP_crab<- sdmTMB(
  biomass ~ poly(depth_log, 2) +  poly(tempatfishing,2),
  data = data.train,
  mesh = mesh.train,
  #offset = log(effort),
  family = delta_lognormal(),
  spatial = "on",
  time= "year",
  spatiotemporal = "off")
  #extra_time = extratime) #ADD THIS FOR THE FUTURE CROSS-VALIDUATION

sanity(m_lfocv_SP_crab)
AIC(m_lfocv_SP_crab_dg, m_lfocv_SP_crab)

m_lfocv_SP_yt<- sdmTMB(
  biomass ~ poly(depth_log, 2) +  poly(tempatfishing,2),
  data = data.train,
  mesh = mesh.train,
  #offset = log(effort),
  family = delta_gamma(),
  spatial = "on",
  time= "year",
  spatiotemporal = "off")

m_lfocv_SP_cod<- sdmTMB(
  biomass ~ poly(depth_log, 2) +  poly(tempatfishing,2),
  data = data.train,
  mesh = mesh.train,
  #offset = log(effort),
  family = delta_lognormal(),
  spatial = "on",
  time= "year",
  spatiotemporal = "off")

#sanity check
sanity(m_lfocv_SP_crab)
sanity(m_lfocv_SP_yt)
sanity(m_lfocv_SP_cod)

############################
## predict over the test.data
#############################
data.test<- data

###OR for Cross validation ###
data.test<- data %>%
  filter(year>= 2017)

#####################
#Predictions
####################

sm_forecast_crab <- predict(m_lfocv_SP_crab, newdata =  data.test)
#sm_forecast_yt <- predict(m_lfocv_SP_yt, newdata =  data.test)
#sm_forecast_cod <- predict(m_lfocv_SP_cod, newdata =  data.test)

#####################################
### Calculate model performance
#####################################
sm_forecast_crab$pred<- plogis(sm_forecast_crab$est1) * exp(sm_forecast_crab$est2)
#sm_forecast_yt$pred<- plogis(sm_forecast_yt$est1) * exp(sm_forecast_yt$est2)
#sm_forecast_cod$pred<- plogis(sm_forecast_cod$est1) * exp(sm_forecast_cod$est2)

### ASSESS GAMMA/LOGNORMAL COMPONENT (Conditional-to-presence biomass)
sm_forecast_crab_pos<- sm_forecast_crab %>%
  filter(biomass > 0)

sm_forecast_yt_pos<- sm_forecast_yt %>%
  filter(biomass > 0)

sm_forecast_cod_pos<- sm_forecast_cod %>%
  filter(biomass > 0)

#####################
## GAMMA COMPONENT
#####################

## correlation

corr_crab<- cor(log(sm_forecast_crab_pos$biomass), sm_forecast_crab_pos$est2)
corr_yt<- cor(log(sm_forecast_yt_pos$biomass), sm_forecast_yt_pos$est2)
corr_cod<- cor(log(sm_forecast_cod_pos$biomass), sm_forecast_cod_pos$est2)


## plots
p_crab<- ggplot(sm_forecast_crab_pos, aes(x = log(biomass), y = est2)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  annotate("text", x = Inf, y = Inf, label = paste("R: ", round(corr_crab, 2)),
           hjust = 1.1, vjust = 2, size = 5, color = "blue") +
  labs(
    x = "Biomass observations (log)",
    y = "Biomass predictions (log)") +
  theme_bw() +
  theme(panel.grid = element_blank())

p_yt<- ggplot(sm_forecast_yt_pos, aes(x = log(biomass), y = est2)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  annotate("text", x = Inf, y = Inf, label = paste("R: ", round(corr_yt, 2)),
           hjust = 1.1, vjust = 2, size = 5, color = "blue") +
  labs(
    x = "Biomass observations (log)",
    y = "Biomass predictions (log)") +
  theme_bw() +
  theme(panel.grid = element_blank())


p_cod<- ggplot(sm_forecast_cod_pos, aes(x = log(biomass), y = est2)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  annotate("text", x = Inf, y = Inf, label = paste("R: ", round(corr_cod^2, 2)),
           hjust = 1.1, vjust = 2, size = 5, color = "blue") +
  labs(
    x = "Biomass observations (log)",
    y = "Biomass predictions (log)") +
  theme_bw() +
  theme(panel.grid = element_blank())

p_crab
ggsave("dfo-tesa-2023/FINAL2024/figures/crab_PredictionsVSObservations_hist.png", height = 3.5*1.5, width = 4.5*1.5)

#####################
### BINOMIAL COMPONENT
#####################

sm_forecast_crab$biomass_binary <- ifelse(sm_forecast_crab$biomass > 0, 1, 0)
sm_forecast_crab$Predbin<- plogis(sm_forecast_crab$est1)

sm_forecast_yt$biomass_binary <- ifelse(sm_forecast_yt$biomass > 0, 1, 0)
sm_forecast_yt$Predbin<- plogis(sm_forecast_yt$est1)

sm_forecast_cod$biomass_binary <- ifelse(sm_forecast_cod$biomass > 0, 1, 0)
sm_forecast_cod$Predbin<- plogis(sm_forecast_cod$est1)

library(pROC)

## Area under the curve (AUC)
roc_curve <- roc(sm_forecast_crab$biomass_binary, sm_forecast_crab$Predbin)
auc_value <- auc(roc_curve)
print(paste("AUC:", round(auc_value, 4)))

# Open a PNG device
png("dfo-tesa-2023/FINAL2024/figures/delta_gamma/crab_hist_AUC.png", height = 3.5*1.5, width = 4.5*1.5, units = "in", res = 300)

# Create the plot
plot(roc_curve, col = "blue", main = "ROC Curve")
abline(a = 0, b = 1, lty = 2, col = "red")  # Diagonal line for random guessing
text(x = 0, y = 0.7, labels = paste("AUC:", round(auc_value, 4)), col = "blue", cex = 1.2)

# Close the PNG device
dev.off()

####
##  CLASIFICATION ERROR
sm_forecast_crab$Predbin_threshold <- ifelse(sm_forecast_crab$Predbin >= 0.5, 1, 0)

# Assume you have your true labels (y_true) and predicted labels (y_pred)
confusion_matrix <- table(sm_forecast_crab$biomass_binary, sm_forecast_crab$Predbin_threshold)
tn <- confusion_matrix[1, 1]
fp <- confusion_matrix[1, 2]
fn <- confusion_matrix[2, 1]
tp <- confusion_matrix[2, 2]

classification_error <- (fp + fn) / (tn + fp + fn + tp)
print(paste("Classification Error:", classification_error))

#results historical : #0.186 for crab / 0.086 for yt / 0.25 for cod
#results out of sample CV: 0.22 for crab /0.079 for yt / 0.26 for cod.


