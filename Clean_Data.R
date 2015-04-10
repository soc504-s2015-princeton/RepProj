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

unique(merged.2010$ocup1anc)
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
  mutate(parent_occ = ifelse(ocup1anc == "Professional, intellectual and scientist", 10, 
                      ifelse(ocup1anc == "Director (manager, head of the department, supervisor)", 9,
                      ifelse(ocup1anc == "Technician or mid-level professional", 8, 
                      ifelse(ocup1anc == "Specialized worker", 3, 
                      ifelse(ocup1anc == "Public official", 7, 
                      ifelse(ocup1anc == "Office worker (secretary, offiice equipment operator, cashier, etc.)", 7,
                      ifelse(ocup1anc == "Merchant (street vendor, owner of commercial establishment or market stand, etc.)", 4,
                      ifelse(ocup1anc == "Warehous or market salesperson", 6,
                      ifelse(ocup1anc == "Employed, outside an office, in the service sector (hotel or restaurant worker, taxi drivier, etc.)", 4,
                      ifelse(ocup1anc == "Farm laborer, farmer, o agriculture and livestock producer agropecuario, and fisherman (owner of land)", 1,
                      ifelse(ocup1anc == "Agricultural worker (works on land for others)", 1, 
                      ifelse(ocup1anc == "Artisan, craftsperson", 5,
                      ifelse(ocup1anc == "Domestic service", 1,
                      ifelse(ocup1anc == "Laborer", 2, 
                      ifelse(ocup1anc == "Member of the armed forces or protection and security services (the police, fireman, watchman, etc.)", 5, NA)))))))))))))))) 

View(clean.2010)


#check for 39,238 respondents in 23 of the 26 countries, as stated in paper
unique(clean.2010$pais)
unique(clean.2010$region)
unique(clean.2010$tone)
unique(clean.2010$colorr)
unique(clean.2010$parent_occ)
write.dta(clean.2010, "clean2010data.dta")


#output a dataset for graph 1

#get counts for colorr by country in temporary dataset
unique(clean.2010$colorr)
test<-xtabs(formula = ~colorr + pais , data=clean.2010, exclude= "97 Colud not be classified")
test<-as.data.frame(test)
test$colorr<- as.numeric(test$colorr)

test


#the loop that crashes freqs, first for low end values of colorr, then for high end to flag categories to collapse
for(country in unique(test$pais)){
  minInd = min(which(test$pais == country)) - 1
  for (colorr in 1:6) { 
    if (test[colorr +minInd, "Freq"] <= 30) {
      test[colorr +minInd + 1, "Freq"] <- test[colorr + minInd, "Freq"] + test[colorr + minInd + 1, "Freq"]
      test[colorr +minInd, "Freq"] <- 0 
    }
  }
}

for(country in unique(test$pais)){
  minInd = min(which(test$pais == country)) - 1
  for (colorr in 11:6) { 
    if (test[colorr +minInd, "Freq"] <= 30) {
      test[colorr +minInd - 1, "Freq"] <- test[colorr + minInd, "Freq"] + test[colorr + minInd - 1, "Freq"]
      test[colorr +minInd, "Freq"] <- 0 
    }
  }
}

test 

#check to make sure there are no vlaues between 1 and 29
filter(test, Freq < 30 & Freq > 0)


#Make a dataframe of what the minimum & maximum values are by country
min_max <- test %>%
  group_by(pais) %>%
  filter(Freq > 0) %>%
  select(colorr) %>%
  summarise(min = min(colorr), max = max(colorr))
min_max

####collapse

cty.vec <- as.character(unique(test$pais))

colorr_recode_subset<- clean.2010 %>%
  select(colorr, pais, region, ed) %>%
  mutate(colorr=as.numeric(colorr)) %>%
  arrange(pais, colorr) 

library(plyr)
colorr_recode_subset<- ldply(cty.vec, function(x){
  out <- colorr_recode_subset %>%
    filter(pais== x) %>%
    mutate(colorr_recode = ifelse(colorr <= filter(min_max, pais == x)$min, filter(min_max, pais == x)$min, ifelse(colorr >= filter(min_max, pais == x)$max, filter(min_max, pais == x)$max, colorr)))
} )

#check (manually recode nicaragua)
xtabs(formula = ~colorr_recode + pais , data=colorr_recode_subset)

write.dta(colorr_recode_subset, "colorr_recode_subset")
