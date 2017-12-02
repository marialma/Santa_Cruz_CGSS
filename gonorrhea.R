# Gonorrhea Trends in Santa Cruz County
# Excel files will not be shared on the Github as they are not for public use. 
# Data in the files was picked out of the California Gonorrhea Surveillance System Survey
# 
setwd("~/Documents/Berkeley/Career/SC")
library(plyr)
# library(dplyr)
g2015 <- read.csv("2015_gonorrhea.csv")
g2016 <- read.csv("2016_gonorrhea.csv")
g2017 <- read.csv("2017_gonorrhea.csv")

# Label data with years

g2015["Year"] <- 2015
g2016["Year"] <- 2016
g2017["Year"] <- 2017

# Bind all together into one file
gonorrhea = rbind(g2015, g2016, g2017)

# Clean workspace
rm(g2015,g2016,g2017)

# Renaming from the data dictionary
gonorrhea <- rename(gonorrhea, replace = c(
                    "Sex_Partner_Gender_._Male" = "partner_male",
                    "Sex_Partner_Gender_._Female" = "partner_female",              
                    "Sex_Partner_Gender_._Transgender_.M_to_F." = "partner_mtf",
                    "Sex_Partner_Gender_._Transgender_.F_to_M." = "partner_ftm",
                    "Sex_Partner_Gender_._Unknown" = "partner_unk",             
                    "Sex_Partner_Gender_._Refused" = "partner_ref",
                    "CGSSPRFPRFSexPart3Mo" = "num_partners",
                    "CGSSPRFPRFCliHIVTest" = "rec_HIV_test",                   
                    "CGSSPRFBeh7" = "orientation",                           
                    "CGSSPRFBeh8Male" = "num_male_part",                     
                    "CGSSPRFBeh8Fem" = "num_female_part",                           
                    "CGSSPRFBeh9Bars" = "met_at_bars",                          
                    "CGSSPRFBeh9BarsVenue" = "which_bars",                     
                    "CGSSPRFBeh9Bath" = "met_at_bathhouse",                          
                    "CGSSPRFBeh9BathVenue" = "which_bathhouse",                     
                    "CGSSPRFBeh9Web" = "met_on_internet",                           
                    "CGSSPRFBeh9WebVenue" = "where_internet",                      
                    "CGSSPRFBeh9Oth" = "met_other",                           
                    "CGSSPRFBeh9OthVenue" = "how_met",                      
                    "CGSSPRFBeh18" = "last_intercourse",                            
                    "CGSSPRFBeh24" = "last_partner_again",                            
                    "CGSSPRFBeh23" = "last_partner_HIV",                            
                    "CGSSPRFHCExp31" = "HC_symptomatic",                           
                    "CGSSPRFHCExp32"  = "HC_notified_by_partner",                         
                    "testinfo" = "HC_received_info",                                 
                    "toldpart" = "HC_told_partner",                                 
                    "CGSSPRFHCExp40"  = "HC_prescription_for_partner",                         
                    "CGSSPRFHCExp40a" = "HC_prescription_who_offered",                         
                    "CGSSPRFHCExp40b" = "HC_prescription_received",                         
                    "CGSSPRFHCExp40c" = "HC_prescription_delivered",                         
                    "CGSSPRFHCExp411" = "HC_partner_treated",                          
                    "CGSSPRFHCExp412" = "HC_all_partners_treated",                         
                    "CGSSPRFHCExp42" = "HC_referral_services"))
