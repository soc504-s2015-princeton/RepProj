#####################################################################################################################
#####   Assignment: Replication Project (Part 1)                                                                #####
#####   Due Date:   3/15/2015                                                                                   #####
#####   Authors:    Megan Blanchard and Kalyani Jayasankar                                                      #####
#####   Input:      Raw data file(1626360926english_merge_2010_americasbarometer_v14v3.dta)                     ##### 
#####   Output:     Cleaned data files for analysis (clean2010data.dta, colorr_recode_subset.dta)               #####
#####   Notes:      This is the English merged 2010 data downloaded from http://www.americasbarometer.org       #####
#####               This file reads in the raw data and preps for analysis. In this file we:                    ##### 
#####                      1-subset to 23/26 total countries                                                    #####
#####                      2-create 4 new variables (tone, region,colorr_recode & parent_occ) for anaylsis      #####
#####################################################################################################################

#install.packages("foreign", "broom")

library(foreign)
library(dplyr)
library(ggplot2)
library(broom)

#getwd()
setwd("~/Desktop/R working directory/RepProj")

#read in orginal data
merged.2010 <- read.dta("1626360926english_merge_2010_americasbarometer_v14v3.dta", convert.factors = TRUE, missing.type = TRUE, convert.underscore = TRUE, warn.missing.labels = TRUE) 

#create cleaned data file
#subset to 23 countires for analysis- exclude Canada, US, and Haiti
clean.2010 <- merged.2010 %>%
    filter(!pais == "Haiti", !pais == "Canada", !pais == "United States") %>%  
    select(year, pais, idnum, weight1500, estratopri, upm, ur, q1, ed, colorr, ocup1anc, q2, weight1500) 
      
#redefine factor
clean.2010$pais <- factor(clean.2010$pais)

#create new var for analysis region
clean.2010 <- clean.2010 %>% 
  mutate(region = ifelse(pais == "Panama" | pais == "Costa Rica"| pais == "Honduras"| pais == "Mexico"| pais =="Guatemala"| pais == "El Salvador"| pais == "Nicaragua", "Central America and Mexico",
                  ifelse(pais == "Bolivia"| pais == "Peru"| pais == "Venezuela"| pais == "Colombia"| pais == "Ecuador", "Andean",
                  ifelse(pais == "Argentina" | pais == "Chile" | pais == "Paraguay" | pais == "Uruguay"| pais == "Brazil", "Southern Cone and Brazil", "Caribbean")))) 

# create variable for social orgin
unique(clean.2010$ocup1anc)
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

clean.2010$parent_occ <- factor(clean.2010$parent_occ)

#check for 39,238 respondents in 23 of the 26 countries, as stated in paper
write.dta(clean.2010, "clean2010data.dta")

#output a dataset for graphs
#get counts for colorr by country in temporary dataset
test <- xtabs(formula = ~colorr + pais , data = clean.2010, exclude = "97 Colud not be classified")
test <- as.data.frame(test)
test$colorr <- as.numeric(test$colorr)

#write a loop to organize recode
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
    if (test[colorr + minInd, "Freq"] <= 30) {
        test[colorr + minInd - 1, "Freq"] <- test[colorr + minInd, "Freq"] + test[colorr + minInd - 1, "Freq"]
        test[colorr + minInd, "Freq"] <- 0 
    }
  }
}

#Make a dataframe of what the minimum & maximum values are by country
min_max <- test %>%
  group_by(pais) %>%
  filter(Freq > 0) %>%
  select(colorr) %>%
  summarise(min = min(colorr), max = max(colorr))

####collapse in single df with recoded values
cty.vec <- as.character(unique(test$pais))

colorr_recode_subset <- clean.2010 %>%
  filter(!is.na(colorr)) %>%
  select(colorr, pais, region, ed, weight1500, parent_occ, q1, q2, ur, estratopri, upm) %>%
  mutate(colorr = as.numeric(colorr)) %>%
  arrange(pais, colorr) 

library(plyr)
colorr_recode_subset <- ldply(cty.vec, function(x){
  out <- colorr_recode_subset %>%
    filter(pais == x) %>%
    mutate(colorr_recode = ifelse(colorr <= filter(min_max, pais == x)$min, filter(min_max, pais == x)$min, 
                           ifelse(colorr >= filter(min_max, pais == x)$max, filter(min_max, pais == x)$max, colorr))) %>%
    mutate(tone = ifelse(colorr_recode == 1 | colorr_recode == 2 | colorr_recode == 3, "light", 
                  ifelse(colorr_recode == 4 | colorr_recode == 5, "medium", "dark")))
} )

#recode Honduras and Nicaragua
colorr_recode_subset$colorr_recode <- ifelse(colorr_recode_subset$pais == "Honduras" & 
                                             colorr_recode_subset$colorr_recode == 10, 9, 
                                             colorr_recode_subset$colorr_recode)

colorr_recode_subset$colorr_recode <- ifelse(colorr_recode_subset$pais == "Nicaragua" & 
                                             colorr_recode_subset$colorr_recode == 9, 8, 
                                             colorr_recode_subset$colorr_recode)

write.dta(colorr_recode_subset, "colorr_recode_subset.dta")
