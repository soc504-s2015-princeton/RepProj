#####################################################################################################################
#####   Assignment: Replication Project (Part 1)                                                                #####
#####   Due Date:   3/15/2015                                                                                   #####
#####   Authors:    Megan Blanchard and Kalyani Jayasankar                                                      #####
#####   Input:      clean2010data.dta                                                                           ##### 
#####   Output:     This code does anaylsis on the clean data file. We create 3 figures :                       #####
#####   Notes:                                                                                                  #####
#####                                                                                                           #####
#####################################################################################################################

#install.packages("stargazer")
#install.packages("gridExtra")
library(stargazer)
library(foreign)
library(dplyr)
library(plyr)
library(ggplot2)
library(broom)
library(gridExtra)

clean.2010<-read.dta("clean2010data.dta")

#for figures: in export , save as image, adjust width and height, and then save as


####################################################################################################################
#Creating Figure 3: Avg Ed Attainment for Persons with Darkest and Lightest Skin Colors in the Americas, 2010
#shows the mean levels of schooling for the residents with the lightest skin (1-3) 
#compared to those with darkest skin (6+) in all 23 countries, 
#ordered by the size of the average difference between the two. 
####################################################################################################################

#prep

#Create a new dataframe pais_ed
pais_ed <- clean.2010 %>%
  filter(!is.na(colorr), !is.na(ed), tone != "medium") %>%
  group_by(pais, tone) %>%
  select(pais, tone, ed) %>%
  mutate(se= sd(ed)/sqrt(length(ed)), 
         ed_n= length(ed), 
         ci.lower = (mean(ed) - 1 * qt(.975, (ed_n - 1)) * se), 
         ci.upper = (mean(ed) + 1 * qt(.975, (ed_n - 1)) * se)) %>%
  group_by(pais, tone, ci.lower, ci.upper) %>%
  summarise(mean_ed= mean(ed))


#formulas for finding the CI:
#se <- sd(x)/sqrt(n)
#mean(x) + c(-1,1) * qt(0.975, n-1) * se

####################################################################################################################
#Create the bargraph
####################################################################################################################

#need to get scale to extend past 10 on axis
#need to reorder the position or countries based on difference in mean between dark and light skin
#to change plot order of facet grid, change the order of variable levels with factor()
#ex code: levels = c("Ideal", "Very Good", "Fair", "Good", "Premium"))

fig3 <- ggplot(pais_ed, aes( x = factor(tone), y = mean_ed, fill = tone)) +
  ylim(0, 14) +
  ylab("Years of Schooling") + xlab("") +
  geom_bar(stat="identity") + 
  geom_errorbar(aes(ymin = ci.lower, ymax = ci.upper)) + 
  facet_wrap(~ pais, ncol = 2) +coord_flip() +
  geom_text(aes(label= round(mean_ed, 2)), size=3.5, hjust=-.7) +
  scale_fill_brewer(palette="Greens") +
  theme(legend.position = "none") +
  scale_x_discrete(labels = c("Light Skin", "Dark Skin")) +
  #scale_y_continuous(breaks= seq(0, 14, by=2)) +
  ggtitle("Average Eduacational Attainment for Persons with \n \nDarkest and Lightest Skin Colors in the Americas, 2010") +
  theme(plot.title = element_text(lineheight=.6, face="bold", size = 12),
        strip.text.x = element_text(face = "bold", hjust= 0), 
        strip.background = element_rect(colour="white", fill="white"), 
        panel.background = element_rect(fill = 'white', colour = 'white'),
        axis.text = element_text(colour = "black"), 
        axis.title = element_text(face= "bold")) 

fig3 

fig3.note <- arrangeGrob(fig3, 
                         sub = textGrob("95% Confidence Interval (Design-Effect Based) \nSource: AmericasBarometer by LAPOP",
                                        x = .1, hjust = 0, vjust= 0,
                                        gp = gpar(fontface = "italic", fontsize = 10)))
fig3.note


####################################################################################################################
#Creating:
#Appendix Table: Ordinary Least Squares Models Predicting Years of Schooling in Select Latin American Countries, 2010 
#Figure 4. Effects of Skin Color and Other Factors on Educational Attainment in Select Latin American Countries
####################################################################################################################

#regression models 1 and 2
#(2 problems to resolve: excluding parental occupation, also find out scaling for non numerics)

#prep
clean.2010$colorr[clean.2010$colorr=="97 Colud not be classified"] <- NA
table(clean.2010$colorr)
clean.2010$colorr <- factor(clean.2010$colorr)
table(clean.2010$colorr)
summary(clean.2010$colorr)
clean.2010$colorr <- as.numeric(clean.2010$colorr)

#select correct countries for analysis 
eight_pais <- clean.2010 %>%
  filter(pais %in% c("Brazil", "Mexico", "Guatemala", "Colombia", "Ecuador", "Bolivia", "Peru", "Dominican Republic"))

eight_pais$pais <- factor(eight_pais$pais)
#check
levels(eight_pais$pais)

#make Brazil the reference category
eight_pais$pais <- relevel(eight_pais$pais, ref = "Brazil")
#check
levels(eight_pais$pais)

#model 1
reg1 <- lm(scale(ed)~ scale(colorr) + q1 + scale(q2) + ur + pais, data=eight_pais)
model1<- summary(reg1)
model1_Rsquared <- model1$r.squared
model1_fstat <- model1$fstatistic["value"]

#check to make sure the scaling works
eight_pais$ed.std<- (eight_pais$ed - mean(eight_pais$ed, na.rm=TRUE))/ sd(eight_pais$ed, na.rm=TRUE)
test_model <- lm(ed.std ~ scale(colorr) + q1 + scale(q2) + ur + pais, data=eight_pais)
#(yes!)

#model 2
reg2<- lm(scale(ed)~ scale(colorr) + q1 + scale(q2) + ur + pais + pais:scale(colorr), data=eight_pais)
model2<- summary(reg2)
model2_R_squared <- model2$r.squared
model2_Fstat<- model2$fstatistic["value"]

#graph 1
model1_CI <- confint(model1)
model1_CIlower <-model1_CI[ , 1]
model1_CIupper <-model1_CI[ , 2]

#prep
labels = c("Skin color", "Female", "Age", "Urban", "Mexico", "Guatemala", "Colombia", "Ecuador", "Bolivia", "Peru", "Dominican Republic",
           "Interaction: Mexico X skin color", "Interaction: Guatemala X skin color", "Interaction: Colombia X skin color", "Interaction: Ecuador X skin color", "Interaction: Bolivia X skin color", "Interaction: Peru X skin color", "Interaction: Dominican Republic X skin color",
           "Constant")

####################################################################################################################
#Create Appendix Table
####################################################################################################################

stargazer(reg1, reg2, 
          type= "text", 
          covariate.labels = labels,
          column.labels = c("Model 1", "Model 2"),
          no.space = TRUE,
          dep.var.caption = "",
          dep.var.labels =  "",
          title = "Ordinary Least Squares Models Predicting Years of Schooling in Select Latin American Countries, 2010"
)

####################################################################################################################
#Create Graph 
####################################################################################################################


model1.graph <- lm(scale(ed) ~ scale(colorr) + ur + scale(q2) + q1 + pais, data=eight_pais)
model1.graph$coefficients

graph.coef <- summary(model1.graph)$coefficients[2:5, 1]
model1.CI <- confint(model1.graph)
model1.CIlower <- model1.CI[2:5 , 1]
model1.CIupper <- model1.CI[2:5 , 2]

labels = c("Skin Color", "Urban", "Age", "Female")


frame <- data.frame(variable = labels,
                    coefficient = graph.coef,
                    ci.lower = model1.CIlower,
                    ci.upper = model1.CIupper)

frame$variable.order <- factor(frame$variable, levels = c("Female", "Age", "Urban", "Skin Color"))


#Create Figure 4
fig4<- ggplot(frame, aes(y = coefficient, x = variable.order)) + geom_point() +
  geom_pointrange(aes(ymin = ci.lower,  ymax = ci.upper)) +
  geom_hline(yintercept = 0, color="dark green", size= 1) +
  coord_flip() +
  ylab("95 Percent C.I. (Design -Effects Based") + xlab("") +
  theme_classic() +
  geom_text(aes(x=4.3, y=-.12, label= "R-Squared:0.267 \nF=528.217 \nN=15923"), size= 4) +
  ggtitle("Effects of Skin Color and Other Factors on\n\n Educational Attainment in Select Latin American \n\nCountries") +
  theme(plot.title = element_text(lineheight=.6, face="bold", size = 12),
        axis.text = element_text(colour = "black", face= "bold"), 
        axis.title = element_text(face= "bold"))

fig4

fig4.note <- arrangeGrob(fig4, 
                         sub = textGrob("Source: Americas Barometer by LAPOP",
                                        x = .1, hjust = 0, vjust= 0,
                                        gp = gpar(fontface = "italic", fontsize = 10)))

fig4.note

