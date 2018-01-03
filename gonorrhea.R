# Gonorrhea Trends in Santa Cruz County
# Maria Ma, MPH
#
# Excel files will not be shared on the Github as they are not for public use. 
#
# Data in the files was picked out of the California Gonorrhea Surveillance System Survey (CGSS) and CalREDIE files
# Used SQL to combine datasets to introduce gender/age information to CGSS. 
# Datasets were joined on Incident ID numbers, but patient IDs were lost due to it being potentially identifiable.
# This in particular is unfortunate as I can no longer capture repeat patients like this.
# Zip code and clinic information was not included on the basis of it being protected health information.

# Data Import ----
rm(list=ls())
setwd("~/Documents/Berkeley/Career/SC")
library(plyr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(reshape2)
g2015 <- read.csv("2015_gonorrhea.csv", na.strings=c("","NA"),header=TRUE)
g2016 <- read.csv("2016_gonorrhea.csv", na.strings=c("","NA"),header=TRUE)
g2017 <- read.csv("2017_gonorrhea.csv", na.strings=c("","NA"),header=TRUE)
# Data Prep ----
# Label data with years 

g2015["Year"] <- 2015
g2016["Year"] <- 2016
g2017["Year"] <- 2017

# Bind all together into one file
gonorrhea = rbind(g2015, g2016, g2017)

# Clean workspace
rm(g2015,g2016,g2017)

# Renaming from the data dictionary
gonorrhea <- rename(gonorrhea, 
       "partner_male"="Sex_Partner_Gender_._Male",
       "partner_female"="Sex_Partner_Gender_._Female",
       "partner_mtf"="Sex_Partner_Gender_._Transgender_.M_to_F.",
       "partner_ftm"="Sex_Partner_Gender_._Transgender_.F_to_M.",
       "partner_unk"="Sex_Partner_Gender_._Unknown",
       "partner_ref"="Sex_Partner_Gender_._Refused",
       "num_partners"="CGSSPRFPRFSexPart3Mo",
       "rec_HIV_test" ="CGSSPRFPRFCliHIVTest",
       "orientation"  ="CGSSPRFBeh7",
       "num_male_part" ="CGSSPRFBeh8Male",
       "num_female_part"  ="CGSSPRFBeh8Fem",
       "met_at_bars" ="CGSSPRFBeh9Bars",
       "which_bars" ="CGSSPRFBeh9BarsVenue",
       "met_at_bathhouse" ="CGSSPRFBeh9Bath",
       "which_bathhouse" ="CGSSPRFBeh9BathVenue",
       "met_on_internet"  ="CGSSPRFBeh9Web",
       "where_internet"  ="CGSSPRFBeh9WebVenue",
       "met_other"  ="CGSSPRFBeh9Oth",
       "how_met"  ="CGSSPRFBeh9OthVenue",
       "last_intercourse"   ="CGSSPRFBeh18" ,
       "last_partner_again"   ="CGSSPRFBeh24",
       "last_partner_HIV"   ="CGSSPRFBeh23",
       "HC_symptomatic"  ="CGSSPRFHCExp31",
       "HC_notified_by_partner"="CGSSPRFHCExp32",
       "HC_received_info"="testinfo",
       "HC_told_partner"="toldpart",
       "HC_prescription_for_partner"="CGSSPRFHCExp40",
       "HC_prescription_who_offered"="CGSSPRFHCExp40a",
       "HC_prescription_received"="CGSSPRFHCExp40b",
       "HC_prescription_delivered"="CGSSPRFHCExp40c",
       "HC_partner_treated" ="CGSSPRFHCExp411",
       "HC_all_partners_treated"="CGSSPRFHCExp412",
       "HC_referral_services"="CGSSPRFHCExp42")

# Mapping responses using the CalREDIE data dictionary
# ( https://www.cdph.ca.gov/Programs/CID/DCDC/CDPH%20Document%20Library/CalREDIE-Data-Dictionary.pdf)

gonorrhea$orientation <- as.factor(mapvalues(gonorrhea$orientation, 
                                   from = c(1,2,3,4,9), 
                                   to = c("straight", "gay", "bisexual", "other", "refused")))
gonorrhea$HC_partner_treated <- as.factor(mapvalues(gonorrhea$HC_partner_treated, 
                                          from = c(1,2,3,4,9), 
                                          to = c("yes", "probably", "not sure", "probably not", "refused")))
gonorrhea$last_intercourse <- as.factor(mapvalues(gonorrhea$last_intercourse, 
                                                  from = c(1,2,3,4,5,9),
                                                  to = c("last week", "within last month", "within 2 months", "more than2 months ago", "don't know", "refused")))
gonorrhea$HC_prescription_who_offered <-as.factor(mapvalues(gonorrhea$HC_prescription_who_offered,
                                                            from = c(1), 
                                                            to = ("doctor")))

gonorrhea$HC_all_partners_treated <-as.factor(mapvalues(gonorrhea$HC_all_partners_treated,
                                                            from = c(1,2,3,4,5,9), 
                                                            to = c("all treated", "at least one definitely", "at least one probably", "not sure", "probably none", "refused")))
gonorrhea$last_partner_HIV <-as.factor(mapvalues(gonorrhea$last_partner_HIV,
                                                        from = c("Y","N","D","R"), 
                                                        to = c("HIV pos","HIV neg","Do not know", "Refused")))
gonorrhea$Year <- as.factor(gonorrhea$Year)

gonorrhea$AgeRange <- cut(gonorrhea$Age, 
                          breaks =c("14","19", "24", "29", "34", "39", "44", "49","54","94"), 
                          labels =c("Under 19","19-24","25-29","30-34","35-39","40-44","45-49","50-54","55+"),
                          include.lowest=TRUE)
# labels =c("Under 19","19-24","25-29","30-34","35-39","40-44","45-49","50-54","55+"),
# labels = c(1,2,3,4,5,6,7,8,9),


# "Analysis" ----
# Mostly just interested in generating a bunch of cross tabs and charts on this.
# TO DO: add size constraints, generate PDFs as well as images
# Split analyses into: age/gender, jail, drug use
# crosstabs so can look at % increases, etc
# Age Gender ====
# Total number of gonorrhea diagnoses by year
plyear <- ggplot(gonorrhea, aes(Year)) + geom_bar(aes(fill=Year))
plyear <- plyear + labs(x="Year", y= "# of patients", 
                        title = "Gonorrhea Diagnoses in Santa Cruz County by Year", 
                        caption = "Data for 2017 only goes until November - aka incomplete") 

# Looking at gonorrhea diagnosis trends by age and by year
age_year <- ggplot(gonorrhea, aes(AgeRange)) + geom_freqpoly(stat = "count")
age_year <- age_year + labs(x = "Age Range", y = "# of patients", 
                            title = "Gonorrhea Diagnoses in Santa Cruz County by Age and Year",
                            caption = "Data for 2017 only goes until November - aka incomplete")
age_year



# Excluding MTF and FTM for ease of display
age_year_gender <- ggplot(subset(gonorrhea, Sex %in% c("Female","Male")), aes(AgeRange)) + geom_bar(aes(fill = Year), position = "dodge") + facet_grid(~ Sex)
age_year_gender <- age_year_gender + labs(x = "Age Range", y = "# of patients", title = "Gonorrhea Diagnoses in Santa Cruz County by Age, Year, and Gender",
                                          caption = "Data for 2017 only goes until November - aka incomplete")

# gonorrhea by sexual orientations
age_year_orientation <- ggplot(subset(gonorrhea, orientation %in% c("bisexual","gay","straight","other")), aes(orientation)) + geom_bar(aes(fill = Year), position = "dodge")
age_year_orientation <- age_year_orientation + labs(x = "Sexual Orientation", y = "# of patients", title = "Gonorrhea Diagnoses in Santa Cruz County by Sexual Orientation, and Year",
                                                    caption = "Data for 2017 only goes until November - aka incomplete")

#gonorrhea by orientation - looking at gonorrhea rates in only the straight population
age_year_gender_straight <- ggplot(subset(gonorrhea, Sex %in% c("Female","Male") & orientation == "straight"), aes(AgeRange)) + geom_bar(aes(fill = Year), position = "dodge") + facet_grid(~ Sex)
age_year_gender_straight <- age_year_gender_straight + labs(x = "Age Range", y = "# of patients", title = "Gonorrhea Diagnoses in Santa Cruz County in persons who identify as Straight",
                                                            caption = "Data for 2017 only goes until November - aka incomplete")

age_year_gender_gay <- ggplot(subset(gonorrhea, Sex %in% c("Female","Male") & orientation == "gay"), aes(AgeRange)) + geom_bar(aes(fill = Year), position = "dodge") + facet_grid(~ Sex)
age_year_gender_gay <- age_year_gender_gay + labs(x = "Age Range", y = "# of patients", title = "Gonorrhea Diagnoses in Santa Cruz County in persons who identify as Gay",
                                                  caption = "Data for 2017 only goes until November - aka incomplete")

#plotlist <- list(plyear, age_year, age_year_gender)
#pdf("plots_age_gender.pdf", onefile=TRUE)
#invisible(lapply(plotlist, print))
#dev.off()


# Jail and Drugs ====


ageyear <- gonorrhea %>% count(Year, AgeRange)
ageyear$AgeRange <- as.factor(ageyear$AgeRange)
ay <- melt(ageyear, id.vars = c("AgeRange", "Year"))

# Reference for casting melted dfs in the future
#agy <- dcast(ay, AgeRange ~ Year, value.var = "value")
#ay <- melt(agy, id.vars = c("AgeRange"))

age_year <- ggplot(ay, aes(x = AgeRange, y = value, color = Year)) + geom_density()
age_year

age_year <- age_year + labs(x = "Age Range", y = "# of patients", 
                            title = "Gonorrhea Diagnoses in Santa Cruz County by Age and Year",
                            caption = "Data for 2017 only goes until November - aka incomplete")
age_year



#crack <- gonorrhea %>% group_by(crack) %>% tally()
druguse <- gonorrhea %>% count(Year, Sex, jail, prison, crack, heroin, meth, inj, AgeRange) %>% filter(crack == "Y" | heroin == "Y" | meth == "Y" | inj == "Y")
drugyearchange <- druguse %>% group_by(Year) %>% tally() 

druguse_year <- ggplot(drugyearchange, aes(nn, Year), stat = "identity") + geom_path()
druguse_year

#drug graphs ----
crack <- gonorrhea %>% count(Year, crack, AgeRange) %>% filter(crack == "Y")
crack <- rbind(crack,
               c(2015, 'Y', '40-44', 0),
               c(2015, 'Y', '45-49', 0),
               c(2016, 'Y', 'Under 19', 0),
               c(2016, 'Y', '30-34', 0),
               c(2016, 'Y', '35-39', 0),
               c(2016, 'Y', '40-44', 0),
               c(2016, 'Y', '45-49', 0),
               c(2017, 'Y', 'Under 19', 0))
crack$n <- as.numeric(crack$n)
crackuse <- ggplot(crack, aes(AgeRange,n, fill=Year)) + geom_col(position = "dodge") + ylim(0,6)
crackuse <- crackuse + labs(x = "Age", y = "# of patients", title = "Crack Use Among Gonorrhea Patients in SCC by Age",
                                                    caption = "Data for 2017 only goes until November - aka incomplete")

meth <- gonorrhea %>% count(Year, meth, AgeRange) %>% filter(meth == "Y")
meth <- rbind(meth,
              c(2015, 'Y', '19-24', 0),
              c(2015, 'Y', '25-29', 0),
              c(2016, 'Y', '40-44', 0),
              c(2016, 'Y', '45-49', 0),
              c(2016, 'Y', '50-54', 0))
meth$n <- as.numeric(meth$n)
methuse <- ggplot(meth, aes(AgeRange,n, fill=Year)) + geom_col(position = "dodge") + ylim(0,11)
methuse <- methuse + labs(x = "Age", y = "# of patients", title = "Meth Use Among Gonorrhea Patients in SCC by Age",
                            caption = "Data for 2017 only goes until November - aka incomplete")

