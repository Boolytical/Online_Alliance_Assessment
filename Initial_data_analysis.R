###################################################################################

# Please make sure this file is in the same folder as the Qualtrics export folders!
#
# If you want to use a new Qualtrics export, you need to change the folder and file
#    in the "read_spss()" funtion first.
#
# Renaming the question labels inside Qualtrics will cause errors in this code. If
#    you do this, you must change the labels below at "# Keep the required columns" 
#    too.
#
# For future data sets, check whether observations with missing data are allowed to
#    be removed before running this code (see report, "Initial data analysis").
#    This code removes them automatically.

###################################################################################



#######################################################################
############################ Data import ##############################
#######################################################################

# Set working directory to file location
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Check if all required packages are installed, if not download them
packages = c("foreign", "haven", "dplyr")
new_packages = packages[!(packages %in% installed.packages()[, "Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load all the packages
lapply(packages, library, character.only = TRUE)


#######################################################################
########################### Data cleaning #############################
#######################################################################

# Read the data from the Qualtrics export to R
counselor = read_spss("Online+hulp+(verlener)_February+21,+2022_08.39/Select_Online hulp (verlener)_February 21, 2022_08.40.sav")
client = read_spss("Online+hulp+(vrager)_February+21,+2022_08.53/Select_Online hulp (vrager)_February 21, 2022_08.53.sav")

# Keep the required columns
column_names_counselor = c('DistributionChannel', 'Q2', 'Q3', 'Q4', "Q5_1", 'Q5_2', 
                            'Q5_3', "Q5_4", "Q5_5", "Q5_6", "Q5_7", "Q5_8", "Q5_9", "Q5_10", "Q5_11", 'Q10')
counselor = counselor %>% select(all_of(column_names_counselor))

column_names_client = c('DistributionChannel', 'Q2', 'Q3', 'Q4', "Q5_1", 'Q5_2', 
                          'Q5_3', "Q5_4", "Q5_5", "Q5_6", "Q5_7", "Q5_8", "Q5_9", "Q5_10", "Q5_11")
client = client %>% select(all_of(column_names_client))

# Remove rows where Distribution Channel == preview; these are test runs by researcher
counselor = subset(counselor, DistributionChannel == 'anonymous')
client = subset(client, DistributionChannel == 'anonymous')

# Remove the Distribution Channel columns 
counselor$DistributionChannel = client$DistributionChannel = NULL


#######################################################################
##################### Data set preparation ############################
#######################################################################

########
#### Create a data set with linked client - counselor pairs ####
########

# Merge the counselor and client data set based on ID
full_set = merge(counselor, client, by = 'Q2') #.x = counselor, .y = client

# Only keep the rows where the communication was chat, create additional data frame for the MGCFA
full_set = subset(full_set, Q10 == 2)
MGCFA_full_set = data.frame(full_set)

# Rename columns
colnames(full_set) = c("ID",
                       "Organization.counselor", 
                       "Anonymity.counselor",
                       paste0("Q", 1:11, ".counselor"),
                       "OnlineContact",
                       "Organization.client",
                       "Anonymity.client",
                       paste0("Q", 1:11, ".client"))

# Remove pairs containing missing data (MCAR assumed)
full_set = full_set[-unique(which(is.na(full_set), arr.ind = T)[, 1]), ]

# Remove duplicate ID codes from client-counselor pairs
full_set = full_set %>% 
              group_by(ID) %>%
              filter(n() == 1) %>% 
              as.data.frame
  

# Export to SPSS data frame
write.csv(full_set, 'counselor_client_op_ID.sav', row.names = FALSE)


########
#### Create a data set for unpaired client and counselor groups for MGCFA ####
########

# Reformat MGCFA dataframe
MGCFA_client_set = MGCFA_full_set[, c("Q3.y", "Q4.y", "Q5_1.y", "Q5_2.y", "Q5_3.y", 
                                      "Q5_4.y", "Q5_5.y", "Q5_6.y", "Q5_7.y", "Q5_8.y", 
                                      "Q5_9.y", "Q5_10.y", "Q5_11.y", "Q2")] %>%
                   cbind(., Role = 'client') 

colnames(MGCFA_client_set) = c("Organization", 
                               "OnlineContact",
                               paste0("Q", 1:11),
                               "ID",
                               "Role")

MGCFA_counselor_set = MGCFA_full_set[, c("Q3.x", "Q4.x", "Q5_1.x", "Q5_2.x", "Q5_3.x", 
                                         "Q5_4.x", "Q5_5.x", "Q5_6.x", "Q5_7.x", "Q5_8.x", 
                                         "Q5_9.x", "Q5_10.x", "Q5_11.x", "Q2")] %>%
                      cbind(., Role = 'counselor') 

colnames(MGCFA_counselor_set) = c("Organization", 
                               "OnlineContact",
                               paste0("Q", 1:11),
                               "ID",
                               "Role")

MGCFA_full_set = rbind(MGCFA_client_set, MGCFA_counselor_set)

# Remove missing data (MCAR assumed)
MGCFA_full_set = MGCFA_full_set[-unique(which(is.na(MGCFA_full_set), arr.ind = T)[, 1]), ]

# Export to SPSS data frame
write.csv(MGCFA_full_set, 'separated_counselor_client.sav', row.names = FALSE)

# Clear environment to facilitate performance
rm(list = ls())