library(readr)
library(readxl)
library(dplyr)
library(stringr)
library(readxl)
#setwd("~/Desktop/DATA for BA/FINAL_116 states.csv")
#setwd
#FINAL_116_states <- read_csv("~/Desktop/FINAL_116 states.csv")
  #read_csv("Desktop/FINAL_116 states.csv")
#FINAL_116_states<-read.csv(file.choose(), sep=',')
library(car)
library(readr)
library(sandwich)
library(readxl)
library(table1)
library(ggplot2)
library(dplyr)
library(stringr)
library(tidyverse)
library(stargazer)
library(readr)
political_regime <- read_csv("Downloads/political-regime.csv")

dem=political_regime %>% filter(political_regime$Year==2021)
dem1=dem %>% filter(dem$regime_row_owid>=2)
names(dem1)[names(dem1) == "Entity"] <- "Country"
names(dem1)[names(dem1) == "regime_row_owid"] <- "regime_score"
dem1 = dem1 %>% dplyr::select(-"Code")
dem1$regime_score=mean(dem1$regime_score)
dem1$regime_score = round(dem1$regime_score, digits = 0)





dem2 <- cbind(FINAL_Update, dem1$regime_score)
dem2 <- left_join(FINAL_Update,
                  dem1 %>% dplyr::select(Country, regime_score),
                  by = "Country")
n_distinct(dem2$Country)

dem3 = dem2 %>% distinct() %>% filter(year==2020)
dem3=dem2 %>% filter(year==2021)
dem3$regime_score[is.na(dem3$regime_score)] <- 0
dem4=dem3 %>% filter(regime_score >=2) #58
#dem4=dem3 %>% filter(year==2020)
democr<-lm(pandem~`Total score`+`Judicial review`+`Executive dominance`+
                  `Minimal winning/ one-party c`+pluralism+federalism+Gini+
                  total_deaths+population+gdp_per_capita+`Rate - World Bank `+
                  `Rate - Int'l. Labour Org.`+`Rate - CIA`+happiness2020+stringency_index, data = Dem1)
summary(democr)
tab_model(democr)

democr1<-lm(pandem~`Total score`+`Judicial review`+`Executive dominance`+
             `Minimal winning/ one-party c`+pluralism+federalism+Gini+
             total_deaths+population+gdp_per_capita+
             `Rate - Int'l. Labour Org.`+happiness2021+stringency_index, data = Dem1)
summary(democr1)
tab_model(democr1)

cor(Dem1[, c('pandem','total_deaths','population', 
             'stringency_index')])
COR = Dem1%>%dplyr::select('pandem','total_deaths','population', 
                           'stringency_index')
library(corrplot)
corrplot(cor(COR),method = "pie" )
dem2=dem2 %>% filter(year==2020)
dem2=dem2 %>% filter(regime_score>=2)
write.csv(dem3,"Democracies_el_lib.csv", row.names = FALSE)


FINAL_Update <- read_csv("FINAL_Update.csv")
#HYPOTHESIS ON DEMOCRATIC BACKSLIDING
names(FINAL_Update)[names(FINAL_Update) == "2019"] <- "Gini"
#economy1 = economy <- merge(economy, e, by = c("Country"))
FINAL_Update$Gini<- as.numeric(str_replace_all(FINAL_Update$Gini, "%", ""))/100



Democracies = FINAL_Update %>% filter(FINAL_Update$`Total score` >= 51)
Dem = Democracies %>% filter(Democracies$year==2021)
Dem1 = Dem %>% filter(Dem, -Country=="Albania" | -Country =='Benin' | -Country=='India'| -Country=='Madagascar' | -Country=='Papua New Guinea'|
                    -Country=='Philippines' | -Country=='Tunisia'|
                        -Country =='Zambia' | -Country =='Serbia')
Dem1 <- Dem1[-c(1, 7, 21, 29, 30, 37, 48, 51, 56 ,65, 69), ]
newregr<-lm<-lm(pandem~`Total score`+`Judicial review`+`Executive dominance`+
                             `Minimal winning/ one-party c`+pluralism+federalism+Gini+
                             total_deaths+population+gdp_per_capita+`Rate - World Bank `+
                             `Rate - Int'l. Labour Org.`+`Rate - CIA`+happiness2020+stringency_index, data = Dem1)
summary(newregr)
tab_model(newregr)
#Dem1 = Dem 
#Dem1[1,]
#7,21,29,30,37,48,51,56,65,69,  ]
write.csv(Dem1,"Dem1.csv", row.names = FALSE)

regr_democr<-lm(pandem~`Total score`+`Judicial review`+`Executive dominance`+
                  `Minimal winning/ one-party c`+pluralism+federalism+Gini+
                  total_deaths+population+gdp_per_capita+`Rate - World Bank `+
                  `Rate - Int'l. Labour Org.`+`Rate - CIA`+happiness2020+stringency_index+regime_score, data = dem3)

summary(regr_democr)
tab_model(regr_democr)

regr_lib<-lm(pandem~libdem_2019, data = FINAL_Update)
summary(regr_lib)

regr_fh<-lm(pandem~`Total score`, data = FINAL_Update)
summary(regr_fh)

#multicollinearity

#interaction effect
mod.additive1 <- lm(pandem~libdem_2019+`Total score`,
                    data = FINAL_Update)
summary(mod.additive1)

mod.interaction1<- lm(pandem~libdem_2019*`Total score`,
                      data = FINAL_Update)
summary(mod.interaction1)
anova(regr_fh, mod.additive1, mod.interaction1)

vif(mod.additive1) #no multicol. all < 10
vif(mod.interaction1) #exists libdem_2019 & interaction

library(jtools)
export_summs(mod.additive1, mod.interaction1, scale = TRUE)


#w/o panback+`total score`+happiness2021+total cases+population_density
regr1<-lm(pandem~libdem_2019+`Judicial review`+`Executive dominance`+
            `Minimal winning/ one-party c`+pluralism+federalism+Gini+
            total_deaths+population+gdp_per_capita+`Rate - World Bank `+
            `Rate - Int'l. Labour Org.`+`Rate - CIA`+happiness2020+stringency_index, data = FINAL_Update)
            
summary(regr1)
#w/o libdem_2019+...
regr2<-lm(pandem~`Total score`+`Judicial review`+`Executive dominance`+
            `Minimal winning/ one-party c`+pluralism+federalism+Gini+
            total_deaths+population+gdp_per_capita+`Rate - World Bank `+
            `Rate - Int'l. Labour Org.`+`Rate - CIA`+happiness2020+stringency_index, data = FINAL_Update)

summary(regr2)

library(jtools)
export_summs(regr1, regr2, scale = TRUE)


tab_model(regr2)

tab_model(
  regr1, regr2, p.style="stars",
  pred.labels = c("Intercept", "panback","libdem_2019", "Total score",'Judicial review',
                  'Executive dominance','Minimal winning/ one-party c', 'pluralism','federalism','Gini','total deaths',
                  'total_deaths', 'population', 'GDP per capita', 'Rate - World Bank',
                  'Rate - Intl. Labour Org.', 'Rate - CIA', 'happiness index 2020', 'stringency index'),
  dv.labels = c("LDI", "FH"),
  string.pred = "Coeffcient",
  string.ci = "Conf. Int (95%)",
  string.p = "P-Value"
)



regr3<-lm(pandem~`Total score`+`Executive dominance`+
            federalism+`2019`+
            total_deaths+population+gdp_per_capita+`Rate - World Bank `+
            `Rate - Int'l. Labour Org.`+`Rate - CIA`+happiness2020+stringency_index, data = FINAL_Update)

summary(regr3)


r1<-lm(pandem~`Executive dominance`, data=FINAL_Update)
summary(r1)

#correlation
library(xtable)

cor(FINAL_Update[, c('Executive dominance', 'pluralism', 'federalism', 'Judicial review',
                     'Minimal winning/ one-party c')])

COR = FINAL_Update%>%dplyr::select(9:14)
COR = COR %>% dplyr::select(-4)
library(ggcorrplot)

ggcorrplot(cor(COR))
#library(ggcorrplot)
#plot(cor)
library(corrplot)
corrplot(cor(COR))
cor(FINAL_Update[, c('Executive dominance',  'federalism')])

regr_old<-lm(pandem~panback+libdem_2019+`Total score`+`Judicial review`+`Executive dominance`+
          `Minimal winning/ one-party c`+pluralism+federalism+bicameralism+Gini+
            total_deaths+population+gdp_per_capita+`Rate - World Bank `+
            `Rate - Int'l. Labour Org.`+`Rate - CIA`+happiness2020+stringency_index, data = FINAL_Update)

summary(regr2)
library(stargazer)


stargazer(regr_old,
          type="html",
          model.numbers = F,
          title="Table 1.First Regression Results",
          dep.var.labels=c("pandem"), 
          covariate.labels=c("panback","libdem_2019", "Total score",'Judicial review',
                             'Executive dominance','Minimal winning/ one-party c', 'pluralism','federalism','Gini','total deaths',
            'total_deaths', 'population', 'GDP per capita', 'Rate - World Bank',
            'Rate - Intl. Labour Org.', 'Rate - CIA', 'happiness index 2020', 'stringency_index'),
          out="Regression.htm") 


stargazer(FINAL_Update, type="html",
          title="Descriptive statistics for Regression 1", 
          digits=3, 
          summary.stat=c("n", "mean", "sd", "min", "max","median"),
          out="table.htm",
          covariate.labels=c("panback","libdem_2019", "Total score",'Judicial review',
                             'Executive dominance','Minimal winning/ one-party c', 'pluralism','federalism','Gini','total deaths',
                             'total_deaths', 'population', 'GDP per capita', 'Rate - World Bank',
                             'Rate - Intl. Labour Org.', 'Rate - CIA', 'happiness index 2020', 'stringency_index'))
getwd()




#regression1<-lm(pandem~panback+libdem_2019, data - FINAL_116_states)
#summary(regression1)




#without control variable
regr1.1<-lm(pandem~panback, data = FINAL_116_states)
summary(regr1.1) #***
regr1.2<-lm(pandem~libdem_2019, data = FINAL_116_states)
summary(regr1.2) #***
regr1.3<-lm(pandem~`Total score`, data = FINAL_116_states)
summary(regr1.3) #***

#with 'Total score' as control variable
regr1<-lm(pandem~panback+`Total score`, data = FINAL_116_states)
summary(regr1) #***
regr2<-lm(pandem~libdem_2019+`Total score`, data = FINAL_116_states)
summary(regr2)
#regr3<-lm(pandem~`Total score`, data = FINAL_116_states)
#summary(regr3) ***


plot(pandem~panback, data = FINAL_116_states, xlim=c(0,1), ylim=c(0,1), xlab="Backsliding", ylab="PanDem")
title(main = list("Dependence of pandem on backsliding"))
abline(regr1.1, lty=1, col="green") 

plot(pandem~libdem_2019, data = FINAL_116_states, xlim=c(0,1), ylim=c(0,1), xlab="Liberal democracy", ylab="PanDem")
title(main = list(""))
abline(regr1.2, lty=1, col="red") 

plot(pandem~panback+`Total score`, data = FINAL_116_states)
abline(regr1, data = FINAL_116_states)

plot(pandem~libdem_2019+`Total score`, data = FINAL_116_states)
abline(regr2, data = FINAL_116_states)

outlierTest(regr1) #30,31,111
outlierTest(regr2) #31

#plot(FINAL_116_states$pandem, FINAL_116_states$panback)
#abline(lm(pandem~panback, data = FINAL_116_states))


#HYPOTHESIS ON INSTITUTIONS
#without control variable
regr2.1<-lm(pandem~bicameralism, data = FINAL_116_states)
summary(regr2.1) #*
regr2.2<-lm(pandem~`Judicial review`, data = FINAL_116_states)
summary(regr2.2)
regr2.3<-lm(pandem~`Executive dominance`, data = FINAL_116_states)
summary(regr2.3) #***
regr2.4<-lm(pandem~`Minimal winning/ one-party c`, data = FINAL_116_states)
summary(regr2.4)
regr2.5<-lm(pandem~pluralism, data = FINAL_116_states)
summary(regr2.5) #***
regr2.6<-lm(pandem~federalism, data = FINAL_116_states)
summary(regr2.6) #***

outlierTest(regr2.1)
outlierTest(regr2.3)
outlierTest(regr2.5)
outlierTest(regr2.6)
#Странно, что при прогоне строчек 73-76, выдаётся 1 аутлайер
plot(regr2.1, which=4, cook.levels=1,
     abline(h=0.6, lty=1, col="#FF00FF")) #30,31,79
plot(regr2.3, which=4, cook.levels=1,
     abline(h=0.6, lty=1, col="blue")) #30,31,111
plot(regr2.5, which=4, cook.levels=1,
     abline(h=0.6, lty=1, col="red")) #30,31,104
plot(regr2.6, which=4, cook.levels=1,
     abline(h=0.6, lty=1, col="yellow")) #4,5, 31

#with control - freedom house (Total score)
regr3<-lm(pandem~bicameralism+`Total score`, data = FINAL_116_states)
summary(regr3) #*** у total score

regr4<-lm(pandem~`Executive dominance`+`Total score`, data = FINAL_116_states)
summary(regr4)

regr5<-lm(pandem~pluralism+`Total score`, data = FINAL_116_states)
summary(regr5)

regr6<-lm(pandem~federalism+`Total score`, data = FINAL_116_states)
summary(regr6)

#У всех независимых переменных уровень значимости упал*

#HYPOTHESIS ON ECONOMY AND WELFARE
#without control  
#regr3.1<-lm(total_cases~population_density+population, data = FINAL_116_states)
#summary(regr3.1) *** population
regr3.1<-lm(pandem~happiness2020, data = FINAL_116_states)
summary(regr3.1) #***
regr3.2<-lm(pandem~gdp_per_capita, data = FINAL_116_states)
summary(regr3.2) #***
regr3.3<-lm(pandem~total_deaths, data = FINAL_116_states)
summary(regr3.3)
#with happiness2020 as CV
regr3.4<-lm(pandem~gdp_per_capita*happiness2020, data = FINAL_116_states)
summary(regr3.4)
  
#plot(regr3.4)  
 # abline(regr3.4)
  
regr3.5<-lm(pandem~gdp_per_capita*`2019`*happiness2020, data = FINAL_116_states)
summary(regr3.5)

regr3.6<-lm(pandem~gdp_per_capita*`2019`*happiness2020*`Rate - World Bank `, data = FINAL_116_states)
summary(regr3.6)

regr.3.7<-lm(pandem~total_vaccinations, data = FINAL_116_states)
summary(regr.3.7)
plot(regr.3.7)

#interaction effect
#mod.additive1 <- lm(pandem ~ `Executive dominance`+pluralism+federalism,
#                    data = FINAL_116_states)
#summary(mod.additive1)

#mod.interaction1<- lm(pandem ~ `Executive dominance`*pluralism*federalism,
#                      data = FINAL_116_states)
#summary(mod.interaction1)
#anova(regr5, mod.additive1, mod.interaction1)

#vif(mod.additive1) #no multicol. all < 10
#vif(mod.interaction1) #exists
#cd<-cooks.distance(mod.additive1)
#plot(mod.additive1, which=4, cook.levels=1,
#     abline(h=0.6, lty=1, col="blue")) #some influential cases - CHINA (2020,2021 and SAUDI ARABIA)


#qqPlot(mod.additive1, simulate=TRUE, main="Q-Q Plot") #there're problems in the model as there're values out of the interval
#library(tidyr)
#crPlots(mod.additive1)



