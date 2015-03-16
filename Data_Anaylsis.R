#####################################################################################################################
#####   Assignment: Replication Project (Part 1)                                                                #####
#####   Due Date:   3/15/2015                                                                                   #####
#####   Authors:    Megan Blanchard and Kalyani Jayasankar                                                      #####
#####   Input:      clean2010data.dta                                                                           ##### 
#####   Output:     This code does anaylsis on the clean data file. We create 3 figures :                       #####
#####               Figure 3: Bar Graph of Average Ed by Skin Color for each country                            #####
#####   
#####   Notes:                                                                                                  #####
#####                                                                                                           #####
#####################################################################################################################

#install.packages("stargazer")
#install.packages("gridExtra")
library(stargazer)
library(foreign)
library(dplyr)
library(ggplot2)
library(broom)
library(gridExtra)

setwd("~/Desktop/R working directory/RepProj")
clean.2010<-read.dta("clean2010data.dta")

####################################################################################################################
#Creating Figure 3: Avg Ed for Skin Colors in the Americas, 2010
#shows the mean levels of schooling for the residents with the lightest skin (1-3) 
#compared to those with darkest skin (6+) in all 23 countries, 
#ordered by the size of the average difference between the two. 
####################################################################################################################

#prep for Figure 3

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


#Note for reference- formulas for finding the CI:
# CI = mean(x) + c(-1,1) * qt(0.975, n-1) * se, where se = sd(x)/sqrt(n)


#Note: for Figure 3 below, we still need to: 
#1- specify the demensions in the code
#2- get scale to extend past 10 on axis
#3- reorder the position or countries based on difference in mean between dark and light skin
      #to change plot order of facet grid, change the order of variable levels with factor()
      #ex code: levels = c("Ideal", "Very Good", "Fair", "Good", "Premium"))

#Figure 3
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
#Creating Appendix Table: OLS Models Predicting Years of Schooling in Select Latin American Countries, 2010 
#And Figure 4. Effects of Skin Color and Other Factors on Educational Attainment 
####################################################################################################################

#run regression models 1 and 2
#Note: We have 2 problems to resolve: 
#1)finding how parental occupation is coded (currently excluded from anaylsis)
#2)finding out scaling for non numerics in the regression 

# data prep
clean.2010$colorr[clean.2010$colorr=="97 Colud not be classified"] <- NA
clean.2010$colorr <- factor(clean.2010$colorr)
clean.2010$colorr <- as.numeric(clean.2010$colorr)

#select correct countries for analysis in new data frame
eight_pais <- clean.2010 %>%
  filter(pais %in% c("Brazil", "Mexico", "Guatemala", "Colombia", "Ecuador", "Bolivia", "Peru", "Dominican Republic"))

#make pais a factor
eight_pais$pais <- factor(eight_pais$pais)
#make Brazil the reference category
eight_pais$pais <- relevel(eight_pais$pais, ref = "Brazil")
#check
levels(eight_pais$pais)

#model 1
reg1 <- lm(scale(ed)~ scale(colorr) + q1 + scale(q2) + ur + pais, data=eight_pais)
model1<- summary(reg1)
model1_Rsquared <- model1$r.squared
model1_fstat <- model1$fstatistic["value"]

#check to make sure the scaling works to standardize the coefficients- do by hand, then compare using "scale"
eight_pais$ed.std<- (eight_pais$ed - mean(eight_pais$ed, na.rm=TRUE))/ sd(eight_pais$ed, na.rm=TRUE)
test_model <- lm(ed.std ~ scale(colorr) + q1 + scale(q2) + ur + pais, data=eight_pais)
#(yes! it works)

#model 2
reg2<- lm(scale(ed)~ scale(colorr) + q1 + scale(q2) + ur + pais + pais:scale(colorr), data=eight_pais)
model2<- summary(reg2)
model2_R_squared <- model2$r.squared
model2_Fstat<- model2$fstatistic["value"]

#prep for table
labels = c("Skin color", "Female", "Age", "Urban", "Mexico", "Guatemala", "Colombia", "Ecuador", "Bolivia", "Peru", "Dominican Republic",
           "Interaction: Mexico X skin color", "Interaction: Guatemala X skin color", "Interaction: Colombia X skin color", "Interaction: Ecuador X skin color", "Interaction: Bolivia X skin color", "Interaction: Peru X skin color", "Interaction: Dominican Republic X skin color",
           "Constant")

#Appendix Table
stargazer(reg1, reg2, 
          type= "text", 
          covariate.labels = labels,
          column.labels = c("Model 1", "Model 2"),
          no.space = TRUE,
          dep.var.caption = "",
          dep.var.labels =  "",
          title = "Ordinary Least Squares Models Predicting Years of Schooling in Select Latin American Countries, 2010"
)

# Figure 4 

#prep for graph
#rerunning regreesion for correct order in graph
model1.graph <- lm(scale(ed) ~ scale(colorr) + ur + scale(q2) + q1 + pais, data=eight_pais)
model1.graph$coefficients

#creating data frame of coefficients and CI's
graph.coef <- summary(model1.graph)$coefficients[2:5, 1]
model1.CI <- confint(model1.graph)
model1.CIlower <- model1.CI[2:5 , 1]
model1.CIupper <- model1.CI[2:5 , 2]

labels = c("Skin Color", "Urban", "Age", "Female")


frame <- data.frame(variable = labels,
                    coefficient = graph.coef,
                    ci.lower = model1.CIlower,
                    ci.upper = model1.CIupper)

#reorder variables
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

