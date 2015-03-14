#####################################################################################################################
#####   Assignment: Replication Project (Part 1)                                                                #####
#####   Due Date:   3/15/2015                                                                                   #####
#####   Authors:    Megan Blanchard and Kalyani Jayasankar                                                      #####
#####   Input:      Raw data file(1626360926english_merge_2010_americasbarometer_v14v3.dta)                     ##### 
#####   Output:     Cleaned data file (clean2010data.dta)                                                       #####
#####   Notes:      This is the English merged 2010 data downloaded from http://www.americasbarometer.org       #####
#####               This file reads in the raw data and preps for analysis. In this file we:                    ##### 
#####                      1-subset to 23/26 total countries                                                    #####
#####                      2-create 2 new variables (tone $ region) needed for anaylsis                         #####
#####################################################################################################################
test
#install.packages("foreign", "broom")

library(foreign)
library(dplyr)
library(plyr)
library(ggplot2)
library(broom)

#read in orginal data
merged.2010<-read.dta("1626360926english_merge_2010_americasbarometer_v14v3.dta", convert.factors=TRUE, missing.type = TRUE, convert.underscore = TRUE, warn.missing.labels = TRUE) 
View(merged.2010)

#create cleaned data file

#1 subset: exclude Haiti, US, Canada, filter for vars of interest

#first, view vars
names(merged.2010)
#view countries- there should be 26
levels(merged.2010$pais)

#grab 23 countires for analysis- exclude Canada, US, and Haiti
clean.2010 <- merged.2010 %>%
  filter(!pais=="Haiti", !pais=="Canada", !pais=="United States") %>%  
  select(year, pais, idnum, weight1500, ur, q1, ed, colorr, ocup1anc, q2) #%>%
#mutate(colorr=as.numeric(colorr))

#redefine factor
clean.2010$pais <- factor(clean.2010$pais)


#2 create new vars for analysis - tone and region
clean.2010 <- clean.2010 %>% 
  #filter(!is.na(colorr)) %>%
  mutate(tone = ifelse(colorr == 1 | colorr == 2 | colorr == 3, "light", 
                       ifelse(colorr == 4 | colorr == 5 | colorr == 6, "medium", "dark"))) %>%
  mutate(region = ifelse(pais == "Panama" | pais == "Costa Rica"| pais == "Honduras"| pais == "Mexico"| pais =="Guatemala"| pais == "El Salvador"| pais == "Nicaragua", "Central America and Mexico",
                         ifelse(pais == "Bolivia"| pais == "Peru"| pais == "Venezuela"| pais == "Colombia"| pais == "Ecuador", "Andean",
                                ifelse(pais == "Argentina" | pais == "Chile" | pais == "Paraguay" | pais == "Uruguay"| pais == "Brazil", "Southern Cone and Brazil", "Caribbean")))) %>%
  arrange(colorr, pais)

View(clean.2010)

#check for 39,238 respondents in 23 of the 26 countries, as stated in paper
unique(clean.2010$pais)

write.dta(clean.2010, "clean2010data.dta")
