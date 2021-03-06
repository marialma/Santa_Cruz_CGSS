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
library(grid)
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
titletheme <- theme(plot.title = element_text(hjust=0, vjust=0, family= "Helvetica"), 
                    axis.text.x = element_text(colour="black", family = "Helvetica"),
                    axis.text.y = element_text(colour="black", family = "Helvetica"),
                    text = element_text(family="Helvetica"),
                    legend.title = element_text(colour="black", family = "Helvetica"),
                    theme(axis.text.x = element_text(angle = 45, hjust = 1)))

th <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            panel.background = element_blank(), axis.line = element_line(colour = "black"))
yearcolor <-  scale_fill_manual(values = c("cadetblue2", "cadetblue3", "cadetblue4"))
# "Analysis"
# Mostly just interested in generating a bunch of cross tabs and charts on this.
# TO DO: add size constraints, generate PDFs as well as images
# Split analyses into: age/gender, jail, drug use
# crosstabs so can look at % increases, etc
# Story I want to tell: ---- 
# DONE Gonorrhea incidence has increased since 2015 
# DONE This is what the age structure of gonorrhea looks like
# But this is what it looks like when split by gender - more men than women get gonorrhea
# If you look only at straight couples - younger women are getting gonorrhea, and older men
# If you look at gay couples, the age is also skewing older. 

# DONE Total number of gonorrhea diagnoses by year ----
plyear <- ggplot(gonorrhea, aes(Year)) + geom_point(stat="count") + geom_path(aes(group = 1), color ="turquoise4", stat = "count") +
  ylim(0,200) 
plyear <- plyear + labs(x="Year", y= "# of patients", 
                        title = "Gonorrhea Diagnoses in Santa Cruz County by Year", 
                        caption = "Fig 1 \n") 
plyear

# DONE Gonorrhea diagnosis trends by age and by year ----
ageyear <- gonorrhea %>% count(Year, AgeRange)
ageyear$AgeRange <- as.factor(ageyear$AgeRange)
ay <- melt(ageyear, id.vars = c("AgeRange", "Year"))
agy <- dcast(ay, AgeRange ~ Year, value.var = "value")
ay <- melt(agy, id.vars = c("AgeRange"))


agexyear <- ggplot(subset(gonorrhea, AgeRange != "NA"), aes(AgeRange)) + geom_bar(stat = "count", aes(fill=Year), position="dodge") + 
  yearcolor
agexyear <- agexyear + labs(x = "Age Range", y = "# of patients", 
                            title = "Gonorrhea Diagnoses in Santa Cruz County by Age and Year",
                            caption = "Fig 2") + theme_bw()
agexyear
# DONE line graph for age - maybe discard? ----
ay <- ay[complete.cases(ay),]
rownames(ay) <- 1:nrow(ay)

age_year <- ggplot(ay, aes(x = AgeRange, y = value, color = variable)) + geom_point() + geom_path(aes(group = variable))
age_year <- age_year + labs(x = "Age Range", y = "# of patients", 
                            title = "Gonorrhea Diagnoses in Santa Cruz County by Age and Year",
                            caption = "Data for 2017 only goes until November - aka incomplete")
age_year

# DONE Percent changes in age groups----
agy$pc2017 <- (agy$"2017" / agy$"2016") * 100
agy$pc2016 <- (agy$"2016" / agy$"2015") * 100
agyp <- melt(agy, id.vars = c("AgeRange", '2015', '2016','2017'))
agyp <- agyp[complete.cases(agyp),]
rownames(agyp) <- 1:nrow(agyp)

agyp$variable_f = factor(agyp$variable, levels = c("pc2016", "pc2017"))
agypct2017 <- ggplot(agyp, aes(x=AgeRange, y=value)) + 
  geom_line(aes(group = variable, color=variable)) + geom_point(aes(color = variable)) +
  geom_hline(aes(yintercept = 100)) + facet_grid(. ~ variable_f) +
  scale_color_discrete(name = "Change",
                       breaks = c("pc2016","pc2017"),
                       labels=c("% change from 2015-2016", "% change from 2016-2017"))
agypct2017
# DONE slope graph ----
agy <- agy[complete.cases(agy),]
rownames(agy) <- 1:nrow(agy)

l15 <- paste(agy$AgeRange, "(",round(agy$"2015"), ")")
l16 <- paste("(",round(agy$"2016"),")")
l17 <- paste(agy$AgeRange,"(",round(agy$"2017"), ")")

agesl <- ggplot(agy) + geom_segment(aes(x=0, xend= 25, y=agy$"2015", yend=agy$"2016", color=AgeRange), size=.75) + 
  geom_segment(aes(x=25, xend= 50, y=agy$"2016", yend=agy$"2017", color=AgeRange), size=.75) +
  scale_color_manual(values =c("lightsteelblue1", "lightsteelblue2", "lightsteelblue3","plum1", "plum2", "plum3", "plum4", "palegreen2", "palegreen3"))
agesl <- agesl + xlab("") + ylab("Age Range") +
  theme(panel.background = element_blank()) +
  theme(panel.grid=element_blank()) +
  theme(axis.ticks=element_blank()) +
  theme(axis.text=element_blank()) + 
  theme(panel.border=element_blank()) + xlim(-10,60) + ylim(0, 50)
agesl <- agesl + geom_text(label = l15, aes(y=agy$"2015", x=0), size = 2.5, hjust = 1.1) +
  geom_text(label = l16, aes(y=agy$"2016", x=25), size = 2.5) +
  geom_text(label = l17, aes(y=agy$"2017", x=50), size = 2.5, hjust = -.1) +
  geom_text(label = "2015", x = 0, y = (1.1*(max(agy$"2016"))), size = 4, hjust = 1.2, color = "cadetblue2") +
  geom_text(label = "2016", x = 25, y = (1.1*(max(agy$"2016"))), size = 4, color = "cadetblue3") +
  geom_text(label = "2017", x = 50, y = (1.1*(max(agy$"2016"))), size = 4, hjust = -.05, color = "cadetblue4") +
  labs(caption = "Data for 2017 only goes until November - aka incomplete")
agesl
rm(l15,l16,l17)

# T-test for gender-age differences by year ----
ageyg <- na.omit(transmute(gonorrhea, Sex, Age, Year, orientation))
AgeYearDiff <- list()
AgeYearDiff[[1]] <- t.test(subset(ageyg, Sex == "Female" & Year == "2015" & orientation == "straight")$Age, 
                           subset(ageyg, Sex == "Male" & Year == "2015" & orientation == "straight")$Age)
AgeYearDiff[[2]] <- t.test(subset(ageyg, Sex == "Female" & Year == "2016" & orientation == "straight")$Age, 
                           subset(ageyg, Sex == "Male" & Year == "2016" & orientation == "straight")$Age)
AgeYearDiff[[3]] <- t.test(subset(ageyg, Sex == "Female" & Year == "2017" & orientation == "straight")$Age, 
                           subset(ageyg, Sex == "Male" & Year == "2017" & orientation == "straight")$Age)
AgeYear_ttest <- sapply(AgeYearDiff, function(x) {
  c(x$estimate[1],
    x$estimate[2],
    p.value = x$p.value)
})
#outputting ttest results as a data frame for ease of display
ayt <- data.frame(AgeYear_ttest)
colnames(ayt) <- c("2015", "2016", "2017")
rownames(ayt) <- c("Mean Age for Women", "Mean Age for Men", "p-value")
ayt  <- as.data.frame(t(ayt))
ayt[1:2] <- round(ayt[1:2], 2)
ayt[3] <- round(ayt[3],4)


male_pt <- subset(ageyg, Sex == "Male")
male_pt_gay <- subset(male_pt, orientation == "gay")
male_pt_str <- subset(male_pt, orientation == "straight")
t.test(male_pt_gay$Age, male_pt_str$Age)

# DONE gender bar graph. Excluding MTF and FTM for ease of display ----
ageyg_mut <- transmute(ageyg, Sex, Age)
ageyg_mut <- melt(ageyg_mut, id.vars = c("Age"))
ageyg_mut <- dcast(ageyg_mut, Age ~ value)
ageyg_mut <- mutate(ageyg_mut, Total = Female + Male, "Percent Female" = round(Female / Total, digits = 2),
                    "Percent Male" = round(Male/Total, digits = 2))
ageyg_mut2 <- dcast(ageyg_mut2, Sex ~ Year, value.var = "Year")

year_gender <- ggplot(ageyg, aes(Year)) + geom_bar(aes(fill=Sex), position = "dodge") +
  scale_fill_manual(values = c("tomato3", "yellowgreen"))

ageyearg <- na.omit(transmute(gonorrhea, Sex, AgeRange, Year))
age_year_gender <- ggplot(ageyearg, aes(AgeRange)) + 
  geom_bar(aes(fill = Year), position = "dodge") + facet_grid(~ Sex) +
  scale_fill_manual(values = c("cadetblue2", "cadetblue3", "cadetblue4"))
age_year_gender <- age_year_gender + labs(x = "Age Range", y = "# of patients", title = "Gonorrhea Diagnoses in Santa Cruz County by Age, Year, and Gender",
                                          caption = "Data for 2017 only goes until November - aka incomplete")

ageyg <- subset(ageyg, Sex %in% c("Female", "Male"))



# DONE slope charts split by gender ----
sexyear <- gonorrhea %>% count(Year, AgeRange, Sex)
sexyear$AgeRange <- as.factor(sexyear$AgeRange)
sy_f <- subset(sexyear, Sex %in% c("Female"))
sy_f <- na.omit(sy_f, cols = sy_f$AgeRange)
sy_wf <- dcast(sy_f, AgeRange + Sex ~ Year, value.var = "n") # [s]ex[y]ear_[w]ideform[f]emale

#labels for slope chart
fl15 <- paste(sy_wf$AgeRange, "(",round(sy_wf$"2015"), ")")
fl16 <- paste("(",round(sy_wf$"2016"),")")
fl17 <- paste("(",round(sy_wf$"2017"), ")")

#slopechart
sy_female <- ggplot(sy_wf) + geom_segment(aes(x=0, xend= 25, y=sy_wf$"2015", yend=sy_wf$"2016", color=AgeRange), size=.75) + 
  geom_segment(aes(x=25, xend= 50, y=sy_wf$"2016", yend=sy_wf$"2017", color=AgeRange), size=.75) +
  scale_color_manual(values =c("lightsteelblue1", "lightsteelblue2", "lightsteelblue3","plum1", "plum2", "plum3", "plum4", "palegreen2", "palegreen3"))
sy_female <- sy_female + xlab("") + ylab("Age Range") +
  theme(panel.background = element_blank()) +
  theme(panel.grid=element_blank()) +
  theme(axis.ticks=element_blank()) +
  theme(axis.text=element_blank())+
  theme(panel.border=element_blank()) + xlim(-10,60) + ylim(0, 33) + 
  geom_text(label = fl15, aes(y=sy_wf$"2015", x=0), size = 3, hjust = 1.1) +
  geom_text(label = fl16, aes(y=sy_wf$"2016", x=25), size = 3) +
  geom_text(label = fl17, aes(y=sy_wf$"2017", x=50), size = 3, hjust = -.1) +
  geom_text(label = "2015", x = 0, y = 33, size = 4, hjust = 1.2, color = "cadetblue2") +
  geom_text(label = "2016", x = 25, y = 33, size = 4, color = "cadetblue3") +
  geom_text(label = "2017", x = 50, y = 33, size = 4, hjust = -.05, color = "cadetblue4") +
  labs(title = "Change in # of diagnoses for each age group for females", caption = "Data for 2017 only goes until November - aka incomplete")
sy_female

sy_m <- subset(sexyear, Sex %in% c("Male"))
sy_m <- na.omit(sy_m, cols = sy_m$AgeRange)
sy_wm <- dcast(sy_m, AgeRange + Sex ~ Year, value.var = "n") # [s]ex[y]ear_[w]ideform[m]ale

#labels for slope chart
ml15 <- paste(sy_wm$AgeRange, "(",round(sy_wm$"2015"), ")")
ml16 <- paste("(",round(sy_wm$"2016"),")")
ml17 <- paste("(",round(sy_wm$"2017"), ")")
#slopechart
sy_male <- ggplot(sy_wm) + geom_segment(aes(x=0, xend= 25, y=sy_wm$"2015", yend=sy_wm$"2016", color=AgeRange), size=.75) + 
  geom_segment(aes(x=25, xend= 50, y=sy_wm$"2016", yend=sy_wm$"2017", color=AgeRange), size=.75) +
  scale_color_manual(values =c("lightsteelblue1", "lightsteelblue2", "lightsteelblue3","plum1", "plum2", "plum3", "plum4", "palegreen2", "palegreen3"))
sy_male <- sy_male + xlab("") + ylab("Age Range") +
  theme(panel.background = element_blank()) +
  theme(panel.grid=element_blank()) +
  theme(axis.ticks=element_blank()) +
  theme(axis.text=element_blank())+
  theme(panel.border=element_blank()) + xlim(-10,60) + ylim(0, 33)
sy_male <- sy_male + geom_text(label = ml15, aes(y=sy_wm$"2015", x=0), size = 3, hjust = 1.1) +
  geom_text(label = ml16, aes(y=sy_wm$"2016", x=25), size = 3) +
  geom_text(label = ml17, aes(y=sy_wm$"2017", x=50), size = 3, hjust = -.1) +
  geom_text(label = "2015", x = 0, y = 33, size = 4, hjust = 1.2, color = "cadetblue2") +
  geom_text(label = "2016", x = 25, y = 33, size = 4, color = "cadetblue3") +
  geom_text(label = "2017", x = 50, y = 33, size = 4, hjust = -.05, color = "cadetblue4") +
  labs(title = "Change in # of diagnoses for each age group for males", caption = "Data for 2017 only goes until November - aka incomplete")
sy_male
#cleanup
rm(fl15, fl16, fl17, ml15, ml16, ml17)


# misc attempts to arrange things. saving it here for future reference. ----
#agetable <- agy[1:4]
#agetable <- grid.table(agetable)
#agelayout <- rbind(c(1,1,1),
#                   c(1,1,1),
#                   c(2,2,3),
#                   c(2,2,3),
#                   c(2,2,3))
#agel <- grid.arrange(arrangeGrob(agexyear, ncol=1, nrow = 1), 
#                     arrangeGrob(agesl, arrangeGrob(agetext, tableGrob(agetable), nrow =2, ncol = 1, heights = 3:2), 
#                                 ncol = 2, nrow = 1, widths = 3:2), 
#                     heights = c(5,7), widths = c(3))


# fill in missing orientation data? ---- 
# though this isn't necessarily accurate because their sexual orientation 
# and reported partners might not mesh up, so not sure how to deal with that

missing_ori <- gonorrhea[is.na(gonorrhea$orientation),]
missing_ori$orientation = ifelse((missing_ori$Sex == "Female" & missing_ori$partner_male == "Y" & missing_ori$partner_female == "N") | 
                                   (missing_ori$Sex == "Male" & missing_ori$partner_male == "N" & missing_ori$partner_female == "Y"),
                                 "straight",
                                 ifelse((missing_ori$Sex == "Female" & missing_ori$partner_male == "N" & missing_ori$partner_female == "Y") | 
                                          (missing_ori$Sex == "Male" & missing_ori$partner_male == "Y" & missing_ori$partner_female == "N"),
                                        "gay",
                                         ifelse((missing_ori$Sex == "Female" & missing_ori$partner_male == "Y" & missing_ori$partner_female == "Y") | 
                                                  (missing_ori$Sex == "Male" & missing_ori$partner_male == "Y" & missing_ori$partner_female == "Y"),
                                                "bisexual", NA)))


# gonorrhea by sexual orientations ----
ori_year <- na.omit(transmute(gonorrhea, orientation, AgeRange, Year))
ori_year <- transmute(ori_year, orientation, Year)
ori_year <- melt(ori_year, id.vars = c("Year", "orientation"))
ori_year <- dcast(ori_year, Year ~ orientation, value.var = "orientation")
ori_year <- transmute(ori_year, Year, straight, gay, bisexual, other, refused, Total = straight + gay + bisexual + other + refused, 
                      "% straight" = round(straight / Total, digits = 2)*100,
                      "% gay" = round(gay /Total, digits = 2)*100,
                      "% bisexual" = round(bisexual /Total, digits = 2)*100)

age_year_orientation <- ggplot(subset(gonorrhea, orientation %in% c("bisexual","gay","straight","other")), aes(orientation)) + 
  geom_bar(aes(fill = Year), position = "dodge") + scale_x_discrete(limits = c("straight","gay", "bisexual","other")) +  yearcolor + theme_bw()
age_year_orientation <- age_year_orientation + labs(x = "Sexual Orientation", y = "# of patients", title = "Gonorrhea Diagnoses in Santa Cruz County by Sexual Orientation, and Year",
                                                    caption = "Data for 2017 only goes until November - aka incomplete")
age_year_orientation

oriyear <- gonorrhea %>% count(Year, AgeRange, Sex, orientation)
oriyear <- oriyear[complete.cases(oriyear[, 2]),]
oriyear[is.na(oriyear)]<- "refused"
oy <- melt(oriyear, id.vars = c("AgeRange", "Year", "Sex","orientation"))
ogy <- dcast(oy, AgeRange + Sex + orientation ~ Year, value.var = "value", fun.aggregate = sum)
ory <- dcast(oy, orientation ~ Year, value.var = "value", fun.aggregate = sum)

# slope graph ----
l15 <- paste(ory$orientation, "(",round(ory$"2015"), ")")
l16 <- paste("(",round(ory$"2016"),")")
l17 <- paste(ory$orientation,"(",round(ory$"2017"), ")")

orisl <- ggplot(ory) + geom_segment(aes(x=0, xend= 25, y=ory$"2015", yend=ory$"2016", color=orientation), size=.75) + 
  geom_segment(aes(x=25, xend= 50, y=ory$"2016", yend=ory$"2017", color=orientation), size=.75) +
  scale_color_manual(limits = c("straight","gay", "bisexual","other","refused"), values =c("goldenrod3","violetred4", "darkseagreen4", "snow4","lightsteelblue3"))
orisl <- orisl + xlab("") + ylab("Orientation") +
  theme(panel.background = element_blank()) +
  theme(panel.grid=element_blank()) +
  theme(axis.ticks=element_blank()) +
  theme(axis.text=element_blank()) + 
  theme(panel.border=element_blank()) + xlim(-10,60) + ylim(0, 130)
orisl <- orisl + geom_text(label = l15, aes(y=ory$"2015", x=0), size = 2.5, hjust = 1.1) +
  geom_text(label = l16, aes(y=ory$"2016", x=25), size = 2.5) +
  geom_text(label = l17, aes(y=ory$"2017", x=50), size = 2.5, hjust = -.1) +
  geom_text(label = "2015", x = 0, y = (1.1*(max(ory$"2016"))), size = 4, hjust = 1.2, color = "cadetblue2") +
  geom_text(label = "2016", x = 25, y = (1.1*(max(ory$"2016"))), size = 4, color = "cadetblue3") +
  geom_text(label = "2017", x = 50, y = (1.1*(max(ory$"2016"))), size = 4, hjust = -.05, color = "cadetblue4") +
  labs(caption = "Data for 2017 only goes until November - aka incomplete") 
orisl
rm(l15,l16,l17)


# gonorrhea by orientation - looking at gonorrhea rates in only the straight population ----
age_year_gender_straight <- ggplot(subset(gonorrhea, Sex %in% c("Female","Male") & orientation == "straight"), aes(AgeRange)) + 
  geom_bar(aes(fill = Year), position = "dodge") +  facet_grid(~ Sex) + yearcolor + labs(x = "Age Range", y = "# of patients", title = "Gonorrhea Diagnoses in Santa Cruz County in persons who identify as Straight",
                                                            caption = "Data for 2017 only goes until November - aka incomplete")
age_year_gender_straight

age_year_gender_gay <- ggplot(subset(gonorrhea, Sex %in% c("Female","Male") & orientation == "gay"), aes(AgeRange)) + 
  geom_bar(aes(fill = Year), position = "dodge") + facet_grid(~ Sex) + yearcolor + labs(x = "Age Range", y = "# of patients", title = "Gonorrhea Diagnoses in Santa Cruz County in persons who identify as Gay",
                                                  caption = "Data for 2017 only goes until November - aka incomplete")

age_year_gender_bi <- ggplot(subset(gonorrhea, Sex %in% c("Female","Male") & orientation == "bisexual"), aes(AgeRange)) + 
  geom_bar(aes(fill = Year), position = "dodge") +   facet_grid(~ Sex) + yearcolor + labs(x = "Age Range", y = "# of patients", title = "Gonorrhea Diagnoses in Santa Cruz County in persons who identify as Gay",
                                                  caption = "Data for 2017 only goes until November - aka incomplete")

age_year_gender_gay
#plotlist <- list(plyear, age_year, age_year_gender)
#pdf("plots_age_gender.pdf", onefile=TRUE)
#invisible(lapply(plotlist, print))
#dev.off()






# Jail and Drugs ====


#crack <- gonorrhea %>% group_by(crack) %>% tally()
druguse <- gonorrhea %>% count(Year, Sex, jail, prison, crack, heroin, meth, inj, AgeRange) %>% filter(crack == "Y" | heroin == "Y" | meth == "Y" | inj == "Y")
drugyearchange <- druguse %>% group_by(Year) %>% tally() 

druguse_year <- ggplot(drugyearchange, aes(Year,nn), stat = "identity") + geom_path(aes(group = 1))
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

