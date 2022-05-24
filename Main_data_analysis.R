############################################################## Load data #####

# Set working directory to file location
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Check if all required packages are installed, if not download them
packages = c("lavaan", "haven", "dplyr", "equaltestMI", "knitr", "semPlot", "reshape2", "ggplot2", "gridExtra")
new_packages = packages[!(packages %in% installed.packages()[, "Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load all the packages
lapply(packages, library, character.only = TRUE)
source("CFA.R")

############################################################### Analysis #####

# Make correlation matrix and save it as an image
png('correlation_matrix.png')
correlation_matrix()
dev.off()

# Set parameters for MGCFA
selection = "all" # "all" or "organization" or "individual"
ID = NaN # NaN or organization name or pairing code or "all"
type = "ML" # "ML" or "DWLS"
analysis = "configural" # "configural" or "metric" or "scalar" or "strict" or "table_all"
plot = FALSE # TRUE or FALSE

# Run the MGCFA and save the plots
MGCFA(selection, ID, type, analysis, plot)

