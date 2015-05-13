#####################################################################################################################
#####   Assignment: Replication Project (Part 1)                                                                #####
#####   Due Date:   3/15/2015                                                                                   #####
#####   Authors:    Megan Blanchard and Kalyani Jayasankar                                                      #####
#####   Input:      Raw data file(1626360926english_merge_2010_americasbarometer_v14v3.dta)                     ##### 
#####   Output:     Cleaned data files for analysis (clean2010data.dta, colorr_recode_subset.dta)               #####
#####   Notes:      This is the English merged 2010 data downloaded from http://www.americasbarometer.org       #####
#####               This file reads in the raw data and preps for analysis. In this file we:                    ##### 
#####                      1-subset to 23/26 total countries                                                    #####
#####                      2-create 4 new variables (tone, region, colorr_recode & parent_occ) for analysis     #####
#####################################################################################################################


suppressMessages(library(foreign))
suppressMessages(library(dplyr))
#we use plyr as well but it is loaded later because it interferes with dplyr
#suppressMessages(library(plyr))

#read in original data
merged.2010 <- read.dta("1626360926english_merge_2010_americasbarometer_v14v3.dta", convert.factors = TRUE, missing.type = TRUE, convert.underscore = TRUE, warn.missing.labels = TRUE) 

#create cleaned data file
#subset to 23 countries for analysis — exclude Canada, US, and Haiti

clean.2010 <- merged.2010 %>%
  filter(!pais == "Haiti", !pais == "Canada", !pais == "United States") %>%  
  select(year, pais, idnum, weight1500, estratopri, upm, ur, q1, ed, colorr, ocup1anc, q2, q10, weight1500) 

#redefine factor

clean.2010$pais <- factor(clean.2010$pais)

#rename variables
#we don't rename the variables related to survey design (estratropri, weight1500, and upm)

clean.2010 <- rename(clean.2010, country = pais, sex = q1, age = q2, income = q10)

#create new variable region

clean.2010 <- clean.2010 %>% 
  mutate(region = ifelse(country == "Panama" | country == "Costa Rica"| country == "Honduras"| country == "Mexico"| country =="Guatemala"| country == "El Salvador"| country == "Nicaragua", "Central America and Mexico",
                  ifelse(country == "Bolivia"| country == "Peru"| country == "Venezuela"| country == "Colombia"| country == "Ecuador", "Andean",
                  ifelse(country == "Argentina" | country == "Chile" | country == "Paraguay" | country == "Uruguay"| country == "Brazil", "Southern Cone and Brazil", "Caribbean")))) 

#create variable parent_occ based on parental occupational prestige

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

clean.2010 %>%
  summarise(n())

#save clean.2010

write.dta(clean.2010, "clean2010data.dta")

#output a dataset for graphs
#get counts for colorr by country in temporary dataset

colorr_index <- xtabs(formula = ~colorr + country , data = clean.2010, exclude = "97 Colud not be classified") #typo in the data, not our own
colorr_index <- as.data.frame(colorr_index)
colorr_index$colorr <- as.numeric(colorr_index$colorr)

#write a loop to organize recode of respondent skin color (colorr)
#the authors combine skin color categories (1-11) with fewer than 30 respondents with contiguous groups 
#this addition is towards the middle value (i.e. 6)
#for instance 1s are added to 2s to 3s and 11s to 10s to 9s and so on until that category has more than 30 people
#this loop crashes freqs, first for low end values of colorr, then for high end to flag categories to collapse

for(country in unique(colorr_index$country)){
  minInd = min(which(colorr_index$country == country)) - 1
  for (colorr in 1:6) { 
    if (colorr_index[colorr + minInd, "Freq"] <= 30) {
      colorr_index[colorr + minInd + 1, "Freq"] <- colorr_index[colorr + minInd, "Freq"] + colorr_index[colorr + minInd + 1, "Freq"]
      colorr_index[colorr + minInd, "Freq"] <- 0 
    }
  }
}

for(country in unique(colorr_index$country)){
  minInd = min(which(colorr_index$country == country)) - 1
  for (colorr in 11:6) { 
    if (colorr_index[colorr + minInd, "Freq"] <= 30) {
      colorr_index[colorr + minInd - 1, "Freq"] <- colorr_index[colorr + minInd, "Freq"] + colorr_index[colorr + minInd - 1, "Freq"]
      colorr_index[colorr + minInd, "Freq"] <- 0 
    }
  }
}


#make a dataframe of what the minimum and maximum skin color values are by country

min_max <- colorr_index %>%
  filter(Freq > 0) %>%
  select(country, colorr) %>%
  group_by(country) %>%
  summarise(min = min(colorr), max = max(colorr))

#collapse in single df with recoded values

cty.vec <- as.character(unique(colorr_index$country))

colorr_recode_subset <- clean.2010 %>%
  filter(!is.na(colorr)) %>%
  select(colorr, country, region, ed, weight1500, parent_occ, sex, age, income, ur, estratopri, upm) %>%
  mutate(colorr = as.numeric(colorr)) %>%
  arrange(country, colorr) 

#load plyr now because it interferes with dplyr if it is loaded earlier

suppressMessages(library(plyr)) 

#this loop is firstly mapping the recoded values back in to the data
#secondly it is creating a variable, tone, derived from skin color categorized in to light (1-3), medium (4-5), dark (6 <)

colorr_recode_subset <- ldply(cty.vec, function(x){
  out <- colorr_recode_subset %>%
    filter(country == x) %>%
    mutate(colorr_recode = ifelse(colorr <= filter(min_max, country == x)$min, filter(min_max, country == x)$min, 
                           ifelse(colorr >= filter(min_max, country == x)$max, filter(min_max, country == x)$max, colorr))) %>%
    mutate(tone = ifelse(colorr_recode == 1 | colorr_recode == 2 | colorr_recode == 3, "light", 
                         ifelse(colorr_recode == 4 | colorr_recode == 5, "medium", "dark")))
} )

#detach plyr

detach(package:plyr)

#recode skin color values for Honduras and Nicaragua
#we hardcode Honduras and Nicaragua because they are quirky and don't adhere to the recode rules above

colorr_recode_subset$colorr_recode <- ifelse(colorr_recode_subset$country == "Honduras" & 
                                             colorr_recode_subset$colorr_recode == 10, 9, 
                                             colorr_recode_subset$colorr_recode)

colorr_recode_subset$colorr_recode <- ifelse(colorr_recode_subset$country == "Nicaragua" & 
                                             colorr_recode_subset$colorr_recode == 9, 8, 
                                             colorr_recode_subset$colorr_recode)

#again check for 39,328 observations

colorr_index %>%
  summarise(n())

#save colorr_recode_subset

write.dta(colorr_recode_subset, "colorr_recode_subset.dta")

