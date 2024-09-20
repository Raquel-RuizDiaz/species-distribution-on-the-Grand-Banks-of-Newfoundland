#########################
## UNCERTAINTY PARTITION
############################

##LIBRARIES:
library(dominanceanalysis)

#OPEN DATAFRAMES
preds_future_ROM370<- readRDS("dfo-tesa-2023/Final2024/crab/crab_ROM_simulation100.rds")
preds_future_IPSL370<- readRDS("dfo-tesa-2023/Final2024/crab/crab_IPSL370_simulation100.rds")
preds_future_GFDL370<- readRDS("dfo-tesa-2023/Final2024/crab/crab_GFDL370_simulation100.rds")

#RCP 2.6
preds_future_IPSL126<- readRDS("dfo-tesa-2023/Final2024/crab/crab_IPSL126_simulation100.rds")
preds_future_GFDL126<- readRDS("dfo-tesa-2023/Final2024/crab/crab_GFDL126_simulation100.rds")

#####################
## convert to a dataframe with values per interaction
#####################
preds_future_ROM370_df <- reshape2::melt(preds_future_ROM370, value.name = "est") %>%
  rename(year = Var1, iter = Var2)

preds_future_IPSL370_df <- reshape2::melt(preds_future_IPSL370, value.name = "est") %>%
  rename(year = Var1, iter = Var2)

preds_future_GFDL370_df <- reshape2::melt(preds_future_GFDL370, value.name = "est") %>%
  rename(year = Var1, iter = Var2)

preds_future_IPSL126_df <- reshape2::melt(preds_future_IPSL126, value.name = "est") %>%
  rename(year = Var1, iter = Var2)

preds_future_GFDL126_df <- reshape2::melt(preds_future_GFDL126, value.name = "est") %>%
  rename(year = Var1, iter = Var2)

#####################
## calculate mean biomass per year
#####################
preds_future_ROM370_df_f<- preds_future_ROM370_df %>%
  group_by(year) %>%
  summarize(meanBio= mean(plogis(est)))
preds_future_ROM370_df_f$model<- "ROM"
preds_future_ROM370_df_f$scenario<- "RCP_7.0"

preds_future_IPSL370_df_f<- preds_future_IPSL370_df %>%
  group_by(year) %>%
  summarize(meanBio= mean(plogis(est)))
preds_future_IPSL370_df_f$model<- "IPSL"
preds_future_IPSL370_df_f$scenario<- "RCP_7.0"

preds_future_GFDL370_df_f<- preds_future_GFDL370_df %>%
  group_by(year) %>%
  summarize(meanBio= mean(plogis(est)))
preds_future_GFDL370_df_f$model<- "GFDL"
preds_future_GFDL370_df_f$scenario<- "RCP_7.0"

preds_future_IPSL126_df_f<- preds_future_IPSL126_df %>%
  group_by(year) %>%
  summarize(meanBio= mean(plogis(est)))
preds_future_IPSL126_df_f$model<- "IPSL"
preds_future_IPSL126_df_f$scenario<- "RCP_2.6"

preds_future_GFDL126_df_f<- preds_future_GFDL126_df %>%
  group_by(year) %>%
  summarize(meanBio= mean(plogis(est)))
preds_future_GFDL126_df_f$model<- "GFDL"
preds_future_GFDL126_df_f$scenario<- "RCP_2.6"

combined_df<- rbind(preds_future_ROM370_df_f, preds_future_IPSL370_df_f, preds_future_GFDL370_df_f, preds_future_IPSL126_df_f, preds_future_GFDL126_df_f)

#####################
SAVE
#####################
save(combined_df, file = "dfo-tesa-2023/Final2024/cod/cod_uncertainty.RData")

#########################
## Upload datasets
#########################

#load(file = "dfo-tesa-2023/Final2024/cod/cod_uncertainty.RData")
#cod<- combined_df

load(file = "dfo-tesa-2023/Final2024/crab/crab_uncertainty.RData")
crab<- combined_df

#load(file = "dfo-tesa-2023/Final2024/yellowtail/yt_uncertainty.RData")
#yt<- combined_df

#####################
### Extract residuals per year and perform dominance analysis
#####################

dat<- crab #to be replace by the appropriate species

# Create an empty list to store dominance analysis results for each year
dominance_results <- list()
parameter_unc<- list()

# Loop through each year from 2020 to 2100
for (year in 1996:2100) {
  # Subset the data for the current year
  dat_year <- dat[dat$year == year, ]

  # Fit a linear regression model for the current year
  m2 <- lm(meanBio ~ model + scenario, data = dat_year)
  #m2 <- lm(BioChange ~ scenario + model + (scenario * model), data = dat_year)
  anova_results<- anova(m2)
  total_SS <- sum(anova_results$`Sum Sq`)
  residual_SS<- anova_results$`Sum Sq`[3]
  parameter_uncertainty <- residual_SS / total_SS

  # Perform dominance analysis for the current year
  da <- dominanceAnalysis(m2)

  # Store the dominance analysis results in the list
  dominance_results[[as.character(year)]] <- da
  parameter_unc[[as.character(year)]]<- parameter_uncertainty
}

#####################
##examine the results
#####################
# Access the results for a specific year, e.g., 2025
results_2100 <- dominance_results[["2100"]]
parameter_unc[["2100"]]

results_2100$contribution.average$r2[1]

##Ok, now lets put everything in a single database
merged_results_list <- list()

# Loop through each year from 2020 to 2100
for (year in 1996:2100) {
  # Extract dominance analysis results for the current year
  model_year <- dominance_results[[as.character(year)]]$contribution.average$r2[1]
  scenario_year <- dominance_results[[as.character(year)]]$contribution.average$r2[2]

  # Extract parameter uncertainty for the current year
  parameter_unc_year <- parameter_unc[[as.character(year)]]

  # Create a data frame for the current year
  dominance_df <- data.frame(
    year = rep(year, length(model_year)),
    model = model_year,
    scenario = scenario_year,
    parameter = parameter_unc_year
  )

  # Append the data frame to the list
  merged_results_list[[as.character(year)]] <- dominance_df
}

# Combine all the data frames in the list into a single data frame
merged_results_df <- do.call(rbind, merged_results_list)

#####################
## SAVE OUPUTS ###
#####################
#save(merged_results_df, file = "sdmTMB-git/dfo-tesa-2023/outputs2/rds/yellowtail/climate_projections/Simulations_Final/yt_dominance_df.RData")
#save(merged_results_df, file = "sdmTMB-git/dfo-tesa-2023/outputs2/rds/cod/climate_projections/Simulations_Final/cod_dominance_df.RData")
#save(merged_results_df, file = "sdmTMB-git/dfo-tesa-2023/outputs2/rds/crab/climate_projections/Simulations_Final/crab_dominance_df.RData")

#####################
## PLOTS
#####################
# Calculate the proportions for each year
library(ggplot2)
library(dplyr)
library(tidyr)

merged_results_long <-merged_results_df %>%
  filter(year > 2019) %>%
  #dplyr::select(-c(2:4)) %>%
  pivot_longer(
    cols = c(model, scenario, parameter),
    names_to = "Component",
    values_to = "Proportion"
  ) %>%
  mutate(year_group = 2020 + ((year - 2020) %/% 10) * 10) %>%
  group_by(Component, year_group) %>%
  summarize(Mean_Proportion = mean(Proportion, na.rm = TRUE))

merged_summary<- merged_results_long %>%
  group_by(Component) %>%
  summarize(mean= mean(Mean_Proportion)) #0.8, 0.07, 0.11

##################
## Figure 8
##################

a<- ggplot(merged_results_long, aes(fill=Component, y=Mean_Proportion, x=year_group)) +
  geom_bar(position="stack", stat="identity") +
  labs(
    x = "Year",
    y = "Relative influence on biomass uncertainty"
  ) +
  scale_fill_manual(
    values = c("model" = "blue4", "parameter" ="lightblue3" , "scenario" =  "cadetblue1"),
    labels = c("Climate Model", "SDM Parameters", "Scenario")
  ) +
  scale_x_continuous(limits = c(2015, 2105), breaks = pretty(merged_results_long$year_group, n = 9), name = "Year")+
  theme_bw() +
  theme(text = element_text(size=12),
        axis.title.y = element_text(vjust=0.4, size = 12),
        axis.title.x = element_text(vjust=0.3, size = 12),
        panel.grid.minor = element_blank()) +
  theme(legend.position = "none") + ggtitle("Snow crab")

b<- ggplot(merged_results_long, aes(fill=Component, y=Mean_Proportion, x=year_group)) +
  geom_bar(position="stack", stat="identity") +
  labs(
    x = "Year",
    y = "Relative influence on biomass uncertainty"
  ) +
  scale_fill_manual(
    values = c("model" = "blue4", "parameter" ="lightblue3" , "scenario" =  "cadetblue1"),
    labels = c("Climate Model", "SDM Parameters", "Scenario")
  ) +
  scale_x_continuous(limits = c(2015, 2105), breaks = pretty(merged_results_long$year_group, n = 9), name = "Year")+
  theme_bw() +
  theme(text = element_text(size=12),
        axis.title.y = element_text(vjust=0.4, size = 12),
        axis.title.x = element_text(vjust=0.3, size = 12),
        panel.grid.minor = element_blank()) +
  theme(legend.position = "none") + ggtitle("Yellowtail flounder")

c<- ggplot(merged_results_long, aes(fill=Component, y=Mean_Proportion, x=year_group)) +
  geom_bar(position="stack", stat="identity") +
  labs(
    x = "Year",
    y = "Relative influence on biomass uncertainty"
  ) +
  scale_fill_manual(
    values = c("model" = "blue4", "parameter" ="lightblue3" , "scenario" =  "cadetblue1"),
    labels = c("Climate Model", "SDM Parameters", "Scenario")
  ) +
  scale_x_continuous(limits = c(2015, 2105), breaks = pretty(merged_results_long$year_group, n = 9), name = "Year")+
  theme_bw() +
  theme(text = element_text(size=12),
        axis.title.y = element_text(vjust=0.4, size = 12),
        axis.title.x = element_text(vjust=0.3, size = 12),
        panel.grid.minor = element_blank()) +
  ggtitle("Atlantic cod")

library(patchwork)
a + b +c + plot_annotation(tag_levels = "a")
ggsave("dfo-tesa-2023/FINAL2024/figures/SDM_UncertaintyPartition.png", height = 3*1.5, width = 9*1.5)
