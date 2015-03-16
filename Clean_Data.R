#####################################################################################################################
#####   Assignment: Replication Project (Part 1)                                                                #####
#####   Due Date:   3/15/2015                                                                                   #####
#####   Authors:    Megan Blanchard and Kalyani Jayasankar                                                      #####
#####   Input:      Raw data file(1626360926english_merge_2010_americasbarometer_v14v3.dta)                     ##### 
#####   Output:     Cleaned data file (clean2010data.dta)                                                       #####
#####   Notes:      This is the English merged 2010 data downloaded from http://www.americasbarometer.org       #####
#####               This file reads in the raw data and preps for analysis. In this file we:                    ##### 
#####                      1-subset to 23/26 total countries                                                    #####
#####                      2-create 3 new variables (tone, region, & parent_occ) needed for anaylsis            #####
#####################################################################################################################

#install.packages("foreign", "broom")

library(foreign)
library(dplyr)
library(ggplot2)
library(broom)

#getwd()
setwd("~/Desktop/R working directory/RepProj")

#read in orginal data
merged.2010<-read.dta("1626360926english_merge_2010_americasbarometer_v14v3.dta", convert.factors=TRUE, missing.type = TRUE, convert.underscore = TRUE, warn.missing.labels = TRUE) 
names(merged.2010)
#create cleaned data file

#1 subset: exclude Haiti, US, Canada, filter for vars of interest

#view countries- there should be 26
levels(merged.2010$pais)
      
#subset to 23 countires for analysis- exclude Canada, US, and Haiti
clean.2010 <- merged.2010 %>%
    filter(!pais=="Haiti", !pais=="Canada", !pais=="United States") %>%  
    select(year, pais, idnum, weight1500, ur, q1, ed, colorr, ocup1anc, q2, weight1500) 
      
#redefine factor
clean.2010$pais <- factor(clean.2010$pais)
      
#2 create new vars for analysis - tone and region
clean.2010 <- clean.2010 %>% 
  filter(!is.na(colorr)) %>%
  mutate(tone = ifelse(colorr == 1 | colorr == 2 | colorr == 3, "light", 
                             ifelse(colorr == 4 | colorr == 5, "medium", "dark"))) %>%
  mutate(region = ifelse(pais == "Panama" | pais == "Costa Rica"| pais == "Honduras"| pais == "Mexico"| pais =="Guatemala"| pais == "El Salvador"| pais == "Nicaragua", "Central America and Mexico",
                             ifelse(pais == "Bolivia"| pais == "Peru"| pais == "Venezuela"| pais == "Colombia"| pais == "Ecuador", "Andean",
                             ifelse(pais == "Argentina" | pais == "Chile" | pais == "Paraguay" | pais == "Uruguay"| pais == "Brazil", "Southern Cone and Brazil", "Caribbean")))) 


# create variable for social orgin
levels(clean.2010$ocup1anc)
clean.2010 <- clean.2010 %>%
  mutate(parent_occ = ifelse(ocup1anc == "Professional, intellectual and scientist", 81, 
                      ifelse(ocup1anc == "Director (manager, head of the department, supervisor)", 66,
                      ifelse(ocup1anc == "Technician or mid-level professional", 61, 
                      ifelse(ocup1anc == "Specialized worker", 53, 
                      ifelse(ocup1anc == "Office worker (secretary, offiice equipment operator, cashier, etc.)", 55,
                      ifelse(ocup1anc == "Merchant(streetvendor,ownerofcommercialestablishmentormarketstand,etc.)", 60,
                      ifelse(ocup1anc == "Warehous or market salesperson", 58,
                      ifelse(ocup1anc == "Employed,outsidanoffice,inthservicsector(hotelorrestaurantworker,taxdrivir,etc.)", 33,
                      ifelse(ocup1anc == "Farmlaborer,farmer,oagriculturandlivestockproduceragropecuar,andfshrmn(wnroflnd)", 26,
                      ifelse(ocup1anc == "Agricultural worker (works on land for others)", 16, 
                      ifelse(ocup1anc == "Artisan, craftsperson", 43,
                      ifelse(ocup1anc == "Domestic service", 24,
                      ifelse(ocup1anc == "Laborer", 23, 
                      ifelse(ocup1anc == "Memberoftharmedforcesorprotectionandsecurityservices(thpolice,firmn,wtchmn,etc.)", 48, NA))))))))))))))) 

#check
table(testrecode$ocup1anc, testrecode$parent_occ)

#check for 39,238 respondents in 23 of the 26 countries, as stated in paper
unique(clean.2010$pais)
unique(clean.2010$region)
unique(clean.2010$tone)
unique(clean.2010$colorr)
unique(clean.2010$parent_occ)
write.dta(clean.2010, "clean2010data.dta")
