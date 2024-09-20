# Species Distribution Modeling on the Grand Banks of Newfoundland

This repository contains code for developing and evaluating species distribution models (SDMs) for the study "Climate models drive variation in projections of species distribution on the Grand Banks of Newfoundland". The workflow includes model selection, validation, prediction, and uncertainty analysis. The results are used to assess the potential impact of climate change on species biomass distribution up to 2100.

## Repository Structure

This repository includes 7 R scripts, each representing a step in the modelling process:

### 1. "01_model_selection.R"
- **Purpose**: Test different model configurations and identify the best-performing model based on various statistical criteria.
- **Description**: This script compares multiple model structures (e.g., different sets of covariates, spatial and temporal configurations) to select the most suitable model for predicting species distribution. 

### 2. "02_SDM_validation.R"
- **Purpose**: Validate the selected model by checking outputs and residuals.
- **Description**: This script evaluates the performance of the best model using validation metrics, including visual residual plots to ensure that the model adequately captures the observed patterns.

### 3. "03_Model_predictability.R"
- **Purpose**: Assess the model's predictability for historical and out-of-sample data.
- **Description**: This script tests the model's ability to predict species biomass using historical data and evaluates its performance on out-of-sample data.

### 4. "04_Climate_projections.R"
- **Purpose**: Forecast species biomass distribution under different climate scenarios up to 2100.
- **Description**: Using the selected model, this script projects species biomass under various climate change scenarios (ACM, IPSL and GFDL), generating spatial distribution maps and time series for future projections.

### 5. "05_Biomass_Change.R"
- **Purpose**: Calculate the percentage of biomass change over time and generate figures for the manuscript (Figures 3-5).
- **Description**: This script computes the relative change in biomass for the species of interest and creates visualizations to illustrate trends.

### 6. "06_Deltabiomass_and_Uncertainty.R"
- **Purpose**: Calculate spatial changes in biomass distribution and associated uncertainty (Figures 6 and 7).
- **Description**: This script quantifies spatial shifts in biomass distribution and estimates uncertainty around these predictions.

### 7. "07_Uncertainty_partition.R"
- **Purpose**: Assess different sources of uncertainty contributing to model predictions (Figure 8).
- **Description**: This script decomposes the total uncertainty in model predictions into its components (e.g., parameter uncertainty, climate model uncertainty, scenario uncertainty) and visualizes their relative contributions.

## Data

- The data used in this project includes species biomass data, environmental covariates (e.g., temperature, depth), and climate projections. Due to confidentiality, the raw data is not included in this repository. Please contact info@dfo-mpo.gc.ca for data access or further information. Use the query specified in the manuscript data availability section.

## Usage

1. **Run scripts sequentially**:
    - Follow the order of the scripts for the full workflow.

2. **R Environment**:
    - Ensure you have R version "4.3.1" and the necessary packages installed (detailed in each R-script).

## Contact

For any questions or issues, please contact:
- **Raquel Ruiz-Díaz**
- rruizdiaz@mun.ca
- Fisheries and Marine Institute. Memorial University.
