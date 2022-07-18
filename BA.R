



#library(esttab)

library(sjlabelled)
library(sjmisc)
library(sjPlot)

names(FINAL_Update)[names(FINAL_Update) == "2019"] <- "Gini"

tab_model(regr_old, p.style="stars")

tab_model(
  regr_old, 
  pred.labels = c("Intercept", "panback","libdem_2019", "Total score",'Judicial review',
                  'Executive dominance','Minimal winning/ one-party c', 'pluralism','federalism','Gini','total deaths',
                  'total_deaths', 'population', 'GDP per capita', 'Rate - World Bank',
                  'Rate - Intl. Labour Org.', 'Rate - CIA', 'happiness index 2020', 'stringency index', 'bicameralism'),
  dv.labels = c("First Model"),
  string.pred = "Coeffcient",
  string.ci = "Conf. Int (95%)",
  string.p = "P-Value"
)







FINAL <- left_join(FINAL_116_states,
                        health1 %>% dplyr::select(Country, stringency_index),
                        by = "Country")
FINAL1 = FINAL %>% distinct(Country, .keep_all= TRUE)
FINAL2 = FINAL %>% distinct()
FINAL3 = FINAL %>% distinct(Country,year, .keep_all=TRUE)
n_distinct(FINAL3$Country)

write.csv(FINAL3,"FINAL_Update.csv", row.names = FALSE)
library(readr)
library(readxl)
library(dplyr)
library(stringr)
library(readxl)
ТАБЛИЦА_КОВИД <- read_excel("ТАБЛИЦА КОВИД.xlsx")
table = ТАБЛИЦА_КОВИД
Happiness <- read_csv("Downloads/Happiness.csv")
#n_distinct(table$country) #203, 145, 211
#vdem<-read.csv(file.choose(), sep=',')
V_Dem_CD_v12 <- read_csv("Downloads/Country_Date_V-Dem_CSV_v12/V-Dem-CD-v12.csv")
API_SI_POV_GINI_DS2_en_excel_v2_3930624 <- read_excel("Desktop/API_SI.POV.GINI_DS2_en_excel_v2_3930624.xls")
#VAD1 <- read_excel("Desktop/VAD1.xlsx")
#Gini + Health
Plus_GINI <- read_csv("Plus GINI.csv")
getwd()


#Gini = API_SI_POV_GINI_DS2_en_excel_v2_3930624
#Gini$`2018`[is.na(Gini$'2018')] <- 0
#Gini$`2019`[is.na(Gini$'2019')] <- 0
#Gini = Gini %>% dplyr::select(-"2020", -"Indicator Code")
#names(Gini)[names(Gini) == "Country Name"] <- "Country"
#n_distinct(Plus_GINI$Country) #123
#Gini_vad1 <- merge(vad1, Gini, by = c("Country"))
#Gini = API_SI_POV_GINI_DS2_en_excel_v2_3930624 %>% dplyr::select("Country Name", "Indicator Name", "")
health = table %>% dplyr::select("Country 4", "stringency_index", "total_cases", "YEAR",
                                 "total_deaths", "total_vaccinations", "population", "population_density",
                                 "gdp_per_capita")
names(health)[names(health) == "Country 4"] <- "Country"
names(health)[names(health) == "YEAR"] <- "year"
health <- health[complete.cases(health$total_deaths), ]
health <- health[complete.cases(health$total_cases), ]
health <- health[complete.cases(health$total_vaccinations), ]
n_distinct(health$Country) #217
# Omit NAs by columns
health1 = health
health1$stringency_index[is.na(health1$stringency_index)] <- 0
health1$population[is.na(health1$population)] <- 0
health1$population_density[is.na(health1$population_density)] <- 0

health1 = health %>% group_by(Country, year) %>% summarise(
  total_cases = mean(total_cases), 
  total_deaths = mean(total_deaths),
  total_vaccinations = mean(total_vaccinations), 
  population = mean(population), 
  population_density = mean(population_density), 
  gdp_per_capita = mean(gdp_per_capita),
  stringency_index = mean(stringency_index)
)
health1$gdp_per_capita[is.na(health1$gdp_per_capita)] <- 0
health1$population[is.na(health1$population)] <- 0
health1$population_density[is.na(health1$population_density)] <- 0
health1$stringency_index[is.na(health1$stringency_index)] <- 0
#n_distinct(health1$Country) #217
write.csv(health1,"HEALTH.csv", row.names = FALSE)
health

#FINAL_116<-merge(x = FINAL_116_states, y = health1[ , c("Country", "stringency_index")], by = "Country", all.x=TRUE)
#FINAL_116_final <- FINAL_116[complete.cases(FINAL_116$pandem), ]
#duplicated(FINAL_116_final)
#FINAL_116_final = FINAL_116_final %>% group_by(Country, year) %>% summarise(
#  pandem = mean(pandem), 
 # libdem_2019 = mean(libdem_2019), 
 # panback = mean(panback),
 # `Bicameralism legislature` = mean(`Bicameralism legislature`),
 # `Total score` = mean(`Total score`),
 # Status = mean(Status),
 # `Bicameralism legislature` = mean(`Bicameralism legislature`),
  
  
)
FINAL_116_final
health1$stringency_index=mean(health1$stringency_index)
F116 <- left_join(FINAL_116_states,
            health1 %>% dplyr::select(Country, stringency_index),
            by = "Country")

#FINAL_116_countries<-merge(FINAL_116_states, health1, by=c("Country"))
#health1 = health1 %>% dplyr::select("stringency_index", "Country", "year")
#FINAL_116_countries <- left_join(FINAL_116_states, health1, by = "Country")
#FINAL_116_countries = FINAL_116_countries %>% dplyr::select(-)
#Health <- merge(health1, Plus_GINI, by = c("Country", "year"))
n_distinct(FINAL_116$Country) #116
PLUS = Plus_GINI %>%
  dplyr::full_join(health1) 
PLUS1 <- PLUS[complete.cases(PLUS$pandem), ]
n_distinct(PLUS1$Country) #123
write.csv(PLUS1,"PLUS1_246 obs.csv", row.names = FALSE)
PlUS2 <- PLUS1[complete.cases(PLUS1$total_cases), ]
write.csv(PlUS2,"PlUS2_116 states.csv", row.names = FALSE)
n_distinct(PlUS2$Country) #116
#Health = health1%>%
#dplyr::full_join(Plus_GINI) #%>%
#replace_na(list(v3elncbpr=0)) %>% arrange(Country)



#health1 <- health1[complete.cases(health1$bicameralism), ]
#health1 <- health1[complete.cases(health1$RGI), ]
#health1 <- health1[complete.cases(health1$LGI), ]

#health2 <- merge(health1, e, by = c("Country"))

economy = table %>% dplyr::select(33:39)
names(economy)[names(economy) == "Country 5"] <- "Country"
#economy1 = economy <- merge(economy, e, by = c("Country"))
economy$`Rate - World Bank `<- as.numeric(str_replace_all(economy$'Rate - World Bank ', "%", ""))/100
economy$`Rate - CIA`<- as.numeric(str_replace_all(economy$`Rate - CIA`, "%", ""))/100
economy$`Rate - Int'l. Labour Org.`<- as.numeric(str_replace_all(economy$`Rate - Int'l. Labour Org.`, "%", ""))/100

economy$`Rate - World Bank `[is.na(economy$`Rate - World Bank )] <- 0
economy$`Rate - CIA`[is.na(economy$`Rate - CIA`)] <- 0
economy$`Rate - Int'l. Labour Org.`[is.na(economy$`Rate - Int'l. Labour Org.`)] <- 0
economy1 <- filter(economy, economy$`Year (WB)` >= 2019 & economy$`Year (ILO)`>=2019 &
                     economy$`Year (CIA)`>=2019)

PlUS_a = PlUS2 %>%
  dplyr::full_join(economy1) 
n_distinct(PlUS_a$Country) #132
PLUS_b <- PlUS_a[complete.cases(PlUS_a$pandem), ]
n_distinct(PLUS_b$Country)#116

PLUS_b$`Rate - World Bank `[is.na(PLUS_b$`Rate - World Bank `)] <- 0
PLUS_b$`Year (WB)`[is.na(PLUS_b$`Year (WB)`)] <- 0
PLUS_b$`Rate - Int'l. Labour Org.`[is.na(PLUS_b$`Rate - Int'l. Labour Org.`)] <- 0
PLUS_b$`Year (ILO)`[is.na(PLUS_b$`Year (ILO)`)] <- 0
PLUS_b$`Rate - CIA`[is.na(PLUS_b$`Rate - CIA`)] <- 0
PLUS_b$`Year (CIA)`[is.na(PLUS_b$`Year (CIA)`)] <- 0
PLUS_b = PLUS_b %>% dplyr::select(-"Country Code")
PLUS_b$`2018`[is.na(PLUS_b$`2018`)] <- 0

write.csv(PLUS_b,"PlUS_b 116 states.csv", row.names = FALSE)

names(Happiness)[names(Happiness) == "country"] <- "Country"
PlUS_c = PLUS_b %>%
  dplyr::full_join(Happiness) 

#n_distinct(PlUS_c$Country) #153

PLUS_d <- PlUS_c[complete.cases(PlUS_c$pandem), ]


n_distinct(PLUS_d$Country)#116
PLUS_d$rank[is.na(PLUS_d$rank)] <- 0
PLUS_d$happiness2020[is.na(PLUS_d$happiness2020)] <- 0
PLUS_d$happiness2021[is.na(PLUS_d$happiness2021)] <- 0
PLUS_d$pop2022[is.na(PLUS_d$pop2022)] <- 0
PLUS_d$`Indicator Name`[is.na(PLUS_d$`Indicator Name`)] <- F
PLUS_d$`2019`[is.na(PLUS_d$`2019`)] <- 0


PLUS_d=PLUS_d %>% dplyr::select(-"v2lgbicam.y")
names(PLUS_d)[names(PLUS_d) == "v2lgbicam.x"] <- "Bicameralism legislature"
names(PLUS_d)[names(PLUS_d) == "v2jureview"] <- "Judicial review"
names(PLUS_d)[names(PLUS_d) == "v2xlg_legcon"] <- "Executive dominance"
names(PLUS_d)[names(PLUS_d) == "v2x_divparctrl"] <- "Minimal winning/ one-party c"
n_distinct(PLUS_d$Country)#116


write.csv(PLUS_d,"FINAL_116 states.csv", row.names = FALSE)




#n_distinct(PLUS_b$Country)#116
#health1$vdisprop[is.na(health1$vdisprop)] <- 0
#health2 = health1 %>% dplyr::select(-"vdisprop")
#too many observations, looking into duplicates

#duplicated(health2)
#health3 = health2 %>% distinct()
#health2 <- filter(health1, health1$vdisprop>0)
#health4 = health3 %>% dplyr::select(-"v2csstruc_2", -"v2csstruc_3", -"v2csstruc_1", -"v2cscnsult")
#health5 = health4 %>% dplyr::select(-25,-26,-27)






a = table %>% dplyr::select(1:6)
b = table %>% dplyr::select(8:12)

#b$Status = str_replace_all(b$Status, "\\&quot\\;", " ")
#b$Status = str_replace_all(b$Status, "\\&apos\\;", " ")
#b$Status = str_replace_all(b$Status, "[[:punct:]]", "")

c = table %>% dplyr::select(7,13:15)
c <- filter(c, c$year2>=2019)

names(a)[names(a) == "year1"] <- "year"
names(c)[names(c) == "year2"] <- "year"
#names(a)[names(a) == "libdem_2019"] <- "libdem"
#names(c)[names(c) == "v2x_libdem 1789-2021"] <- "libdem"
names(a)[names(a) == "Country 1"] <- "Country"
names(c)[names(c) == "Country 3"] <- "Country"
names(b)[names(b) == "Country 2"] <- "Country"

n_distinct(a$Country) #145
n_distinct(b$Country) #208
n_distinct(c$Country) #179

#mean(gapminder[gapminder$continent == "Africa", "gdpPercap"])
a = a %>% dplyr::select(-"quarter")
n_distinct(a$Country)
#plot(a$Country, a$pandem)
#str(a$Country)
#unique(a[c("Country")])
library(ggplot2)
#plot(a$Country, a$year)
#!!!!sort(unique(a$Country))
#a = a %>% dplyr::select(-2)
#a1 = a %>% filter(Country == "Algeria" & year == 2020)
#a3 = a %>% group_by(Country, pandem) 
a3 = a %>% group_by(Country, year) %>% summarise(
  pandem = mean(pandem), 
  libdem_2019 = mean(libdem_2019), 
  panback = mean(panback)
)
a3
d <- merge(a3, c, by=c("year", "Country"))
n_distinct(d$Country) #144
d = d %>% dplyr::select(-"v2x_libdem 1789-2021")
b = b %>% dplyr::select("Country", "Total score", "Status")
ad <- merge(d, b, by = c("Country"))
n_distinct(ad$Country) #140

#PieChart(a, hole=0, quiet=TRUE)
#by_cyl %>% summarise(
#disp = mean(disp),
#hp = mean(hp)
#)
#gapminder %>%
  filter(year == 2002) %>%
  count(continent, sort = TRUE)

#a1 <- filter(a, a$Country == "")
library(stargazer)
#descriptive_statistics<-data.frame(a$year, a$quarter, a$Country,
                                   a$libdem_2019, a$pandem, a$panback)



#stargazer(descriptive_statistics, type="html",
          title="Descriptive statistics for A", 
          digits=3, 
          summary.stat=c("n", "mean", "sd", "min", "max","median"),
          out="table.htm",
          covariate.labels=c("Year", "Quarter", "Country", "LibDem 2019", "PanDem", "PanBack"))

#d <- merge(a, c, by=c("year", "Country"))
#d = d %>% dplyr::select(-"v2x_libdem 1789-2021")
#b = b %>% dplyr::select("Country", "Total score", "Status")
#ad <- merge(d, b, by = c("Country"))

vdem = V_Dem_CD_v12
#na.omit(vdem)
#na.vdem <- vdem[!is.na(vdem), ] 
va = vdem %>% dplyr::select("v2jureview", "v2elparlel", "v2elloelsy", "country_name", "v2xlg_legcon", 
                            "v2ellocgov", "v2elreggov", "v2ellocelc", "v2elsrgel", "v2ellocpwr",
                            "v2elrgpwr", "v2lgello", "v2lgdomchm", "v2lgelecup", "v2csstruc_2",
                            "v2csstruc_3", "v2csstruc_1", "v2cscnsult", "v2x_divparctrl", "v3elncbpr")
va1=va
#va1[complete.cases(va1[ , 1:3]),]
library(tidyr)
va1 = va %>% dplyr::filter(!is.na(v2xlg_legcon))

names(va1)[names(va1) == "country_name"] <- "Country"
n_distinct(va1$Country) #198
vad <- merge(ad, va1, by = c("Country"))
n_distinct(vad$Country) #140
vad$bicameralism <- c((vad$v2lgello*(4 - vad$v2lgdomchm)/4) + (vad$v2lgelecup* (vad$v2lgdomchm)/4))
vad$bicameralism<- as.numeric(str_replace_all(vad$bicameralism, "%", ""))/100
vad$pluralism <-c((vad$v2csstruc_2 + vad$v2csstruc_3- vad$v2csstruc_1-vad$v2cscnsult)/4)
vad$LGI<-c((vad$v2ellocgov*vad$v2elreggov*vad$v2ellocelc)/3)

vad$RGI<-c((vad$v2elsrgel*vad$v2ellocpwr*vad$v2elrgpwr)/3)
vad$federalism<-c(vad$RGI*vad$LGI)
vad$vdisprop = (vad$v2elparlel + vad$v2elloelsy) / 2
#vad = vad %>% dplyr::select(1:9, 12, 26:29, 32)
vad = vad %>% dplyr::select("Country", "year","pandem", "libdem_2019", 
                            "panback", 'v2lgbicam', "Total score",
                            "Status", "v2jureview", "v2xlg_legcon", 
                            "v2x_divparctrl", "bicameralism", "pluralism",
                            "federalism", 'v3elncbpr')
#(1:10,24:26,29)
vad = vad %>% dplyr::filter(!is.na(federalism))
n_distinct(vad$Country) #123

vad$bicameralism[is.na(vad$bicameralism)] <- 0
vad$v2jureview[is.na(vad$v2jureview)] <- 0
vad$v2x_divparctrl[is.na(vad$v2x_divparctrl)] <- 0
vad$pluralism[is.na(vad$pluralism)] <- 0
vad$v3elncbpr[is.na(vad$v3elncbpr)] <- 0

vad1 = vad %>% group_by(Country, year) %>% summarise(
  v2jureview = mean(v2jureview), 
  v2xlg_legcon = mean(v2xlg_legcon), 
  v2x_divparctrl = mean(v2x_divparctrl), 
  v2lgbicam = mean(v2lgbicam),
  bicameralism = mean(bicameralism),
  pluralism = mean(pluralism),
  federalism = mean(federalism),
  v3elncbpr = mean(v3elncbpr)
)
n_distinct(vad1$Country) #123
vad2 <- merge(ad, vad1, by=c("Country", "year"))
n_distinct(vad2$Country) #123

write.csv(vad2,"Malysheva_23.04.csv", row.names = FALSE)
#vad2$bicameralism[is.na(vad2$bicameralism)] <- 0
#vad2$v2jureview[is.na(vad2$v2jureview)] <- 0
#vad2$v2x_divparctrl[is.na(vad2$v2x_divparctrl)] <- 0
#vad2$pluralism[is.na(vad2$pluralism)] <- 0



#<- rowMeans(vad[,c('v2elparlel', 'v2elloelsy')], na.rm=TRUE)
#vad <- vad[complete.cases(vad$vdisprop), ]
#federalism - v2ellocgov v2elreggov v2ellocelc v2elsrgel v2ellocpwr v2elrgpwr
#bicameralism - v2lgello*(4 - v2lgdomchm)/4 + v2lgelecup* (v2lgdomchm)/4
#i-g pluralism - v2csstruc_2 + v2csstruc_3- v2csstruc_1-v2cscnsult/4

#effective number of parties should be dropped from vad2 as values are lost
#vad3 = vad2 %>% dplyr::select(-"v3elncbpr", -"v2x_libdem 1789-2021")
#n_distinct(vad3$Country)
names(vdem)[names(vdem) == "country_name"] <- "Country"
e = vdem %>% dplyr::select("v3elncbpr", "Country") #effective number of parties
e <- e[complete.cases(e$v3elncbpr), ] # Omit NAs by columns
e
n_distinct(e$Country) #45
vad4=vad3
#vad4 %>%
  #dplyr::full_join(e) %>%
  #replace_na(list(v3elncbpr=0)) %>% arrange(Country)
#vad4 %>% mutate("v3elncbpr") #drop it
#n_distinct(e$Country)

#vad1 = vad %>% group_by(Country, year) %>% summarise(
  #v2jureview = mean(v2jureview), 
  #v2xlg_legcon = mean(v2xlg_legcon), 
  #v2x_divparctrl = mean(v2x_divparctrl), 
  #v2lgbicam = mean(v2lgbicam),
  #bicameralism = mean(bicameralism),
  #pluralism = mean(pluralism),
  #federalism = mean(federalism)
#)
#e$v3elncbpr[is.na(e$v3elncbpr)] <- 0


#vad3 = e %>% group_by(Country) %>% summarise(
 # v3elncbpr = mean(v3elncbpr)
#)
#vad3 <- merge(e, vad2, by = c("Country"))
#vad4 = vad3 %>% group_by(Country, year) %>% summarise(
  #v3elncbpr = mean(v3elncbpr)
#)
#n_distinct(vad4) #86
#vad5 <- merge(vad4, vad2, by = c("Country"))
#vad$vdisprop <- rowMeans(vad[,c('v2elparlel', 'v2elloelsy')], na.rm=TRUE) #many NAs
#vad <- vad[complete.cases(vad$vdisprop), ] # Omit NAs by columns
#vad
# or table1$new_cases_smoothed[is.na(table1$new_cases_smoothed)] <- 0

#merge <- merge(e, vad, by = c("Country"))
#na.omit(merge)
#!is.na(merge)
#as.numeric(na.omit(merge$bicameralism))     
#vad1 = vad %>% dplyr::select(-"v2elparlel", -"v2elloelsy")
#vad1$v2jureview[is.na(vad1$v2jureview)] <- 0
#write.csv(data=vad1, "VAD1")
#write.csv(vad1,"C:\\Users\\dasha19malysheva\\Desktop\\Test\\VAD1.csv", row.names = FALSE)


#vad2 <- merge(e, vad1, by = c("Country"))

API_SI_POV_GINI_DS2_en_excel_v2_3930624 <- read_excel("Desktop/API_SI.POV.GINI_DS2_en_excel_v2_3930624.xls")
View(API_SI_POV_GINI_DS2_en_excel_v2_3930624)
Gini = API_SI_POV_GINI_DS2_en_excel_v2_3930624
Gini$`2018`[is.na(Gini$'2018')] <- 0
Gini$`2019`[is.na(Gini$'2019')] <- 0
Gini = Gini %>% dplyr::select(-"2020", -"Indicator Code")
names(Gini)[names(Gini) == "Country Name"] <- "Country"
Gini_vad1 <- merge(vad4, Gini, by = c("Country"))
n_distinct(Gini_vad1$Country) #110

Vad = vad3 %>% dplyr::full_join(Gini) 
Vad1 <- Vad[-c(247:402), ]
n_distinct(Vad1$Country) #123
write.csv(Vad1,"Plus GINI.csv", row.names = FALSE)

#Gini = API_SI_POV_GINI_DS2_en_excel_v2_3930624 %>% dplyr::select("Country Name", "Indicator Name", "")
health = table %>% dplyr::select("Country 4", "stringency_index", "total_cases", "YEAR",
                                  "total_deaths", "total_vaccinations", "population", "population_density",
                                  "gdp_per_capita")
names(health)[names(health) == "Country 4"] <- "Country"
names(health)[names(health) == "YEAR"] <- "year"
health <- health[complete.cases(health$total_deaths), ]
health <- health[complete.cases(health$total_cases), ]
health <- health[complete.cases(health$total_vaccinations), ]
# Omit NAs by columns
health
Health = health %>% group_by(Country, year) %>% summarise(
stringency_index = mean(stringency_index), 
total_cases = mean(total_cases), 
total_deaths = mean(total_deaths), 
total_vaccinations = mean(total_vaccinations),
gdp_per_capita = mean(gdp_per_capita),
population = mean(population),
population_density = mean(population_density)
)
Health$stringency_index[is.na(Health$stringency_index)] <- 0
Health$gdp_per_capita[is.na(Health$gdp_per_capita)] <- 0
Health$population_density[is.na(Health$population_density)] <- 0
n_distinct(Health$Country) #217
#Health1 <- merge(Health, Vad1, by = c("Country", "year"))

n_distinct(Health1$Country)

#Vad2 = Vad1 %>% dplyr::full_join(Health) 
#Vad1 <- Vad[-c(247:402), ]

#health1 <- health1[complete.cases(health1$bicameralism), ]
#health1 <- health1[complete.cases(health1$RGI), ]
#health1 <- health1[complete.cases(health1$LGI), ]

#health2 <- merge(health1, e, by = c("Country"))

economy = table %>% dplyr::select(33:39)
names(economy)[names(economy) == "Country 5"] <- "Country"
economy1 = economy <- merge(economy, e, by = c("Country"))
economy1$`Rate - World Bank `<- as.numeric(str_replace_all(economy1$'Rate - World Bank ', "%", ""))/100
economy1$`Rate - CIA`<- as.numeric(str_replace_all(economy1$`Rate - CIA`, "%", ""))/100
economy1$`Rate - Int'l. Labour Org.`<- as.numeric(str_replace_all(economy1$`Rate - Int'l. Labour Org.`, "%", ""))/100

economy1$`Rate - World Bank `[is.na(economy1$`Rate - World Bank )] <- 0
economy1$`Rate - CIA`[is.na(economy1$`Rate - CIA`)] <- 0
economy1$`Rate - Int'l. Labour Org.`[is.na(economy1$`Rate - Int'l. Labour Org.`)] <- 0
economy2 <- filter(economy1, economy1$`Year (WB)` >= 2019 & economy1$`Year (ILO)`>=2019 &
                     economy1$`Year (CIA)`>=2019)

write.csv(economy2, "Economy2.csv", row.names = F)
library(ggplot2)

health1$vdisprop[is.na(health1$vdisprop)] <- 0
health2 = health1 %>% dplyr::select(-"vdisprop")
#too many observations, looking into duplicates

duplicated(health2)
health3 = health2 %>% distinct()
#health2 <- filter(health1, health1$vdisprop>0)
health4 = health3 %>% dplyr::select(-"v2csstruc_2", -"v2csstruc_3", -"v2csstruc_1", -"v2cscnsult")
health5 = health4 %>% dplyr::select(-25,-26,-27)
all <- merge(health5, economy2, by = c("Country"))
write.csv(health5, "Health5.csv", row.names = F)
all1 = all %>% distinct()
all2 = all1 %>% dplyr::select(-31,-32)
all3 = all2 %>% distinct()
all4 <- all3[complete.cases(all3$stringency_index), ]
write.csv(all4, "Final.csv", row.names = F)
#health6 = health5 %>% distinct()
#vdisprop -нельзя
new1 <- merge(health, economy1, by = c("Country"))
new <- write.csv(x=new, file='BA')
write.csv(new1,"Malysheva_17.04.csv", row.names = FALSE)
#write.csv(Gini_vad1, "COVID", row.names = F)


na.merging <- new1[!is.na(new1), ] 



library(lubridate)
library(tidyverse)
library(lubridate)
library(zoo)
require(zoo)
library(tidyr)
install.packages(vdemdata)
library(vdemdata)

vdem1 = vdem 
vdem1 <- filter(vdem1, vdem1$year >= 2019)
vdem1 = vdem1 %>% dplyr::select("country_name", "year", "v2x_libdem", "v2jureview", "v2ddlexor",
                        "v2elparlel", "v2elloelsy", "v2csstruc_2", "v2csstruc_1", "v2csstruc_3",
                        "v2cscnsult", "v2xlg_legcon", "v2x_divparctrl", "v2lgello",
                        "v2lgdomchm", "v2lgelecup", "v2ellocgov", "v2elreggov", "v2ellocelc", "v2elsrgel", "v2ellocpwr",
                        "v2elrgpwr", )
vdem1 <- vdem1[complete.cases(vdem1$country_name), ] # Omit NAs by columns
vdem1

vdem1 <- vdem1[complete.cases(vdem1$year), ] # Omit NAs by columns
vdem1

vdem1 <- vdem1[complete.cases(vdem1$v2x_libdem), ] # Omit NAs by columns
vdem1

vdem1 <- vdem1[complete.cases(vdem1$v2jureview), ] # Omit NAs by columns
vdem1

vdem1 <- vdem1[complete.cases(vdem1$v2ddlexor), ] # Omit NAs by columns
vdem1







n_distinct(vdem1$country_name)









#Are there elected local and regional governments, and – if so – to what extent can they operate without interference from unelected bodies at the local level?
#  This index is an equally weighted average of a local government index and a regional government index. 
The local government index is the product of a dummy variable 
for the existence of local government (v2ellocgov), a recoded version of Local government elected (v2ellocelc),
and a CDF of Local offices relative power (v2ellocpwr). 
#Local governments are recoded as unelected (0) if they did not exist or if data is missing. They are coded 0.5 if an executive is elected but no assembly, 
#and 1 if an assembly is elected, with or without an executive. 
#the regional government index is calculated the same way but using the existence ofregional government (v2elreggov),
#Regional government elected (v2elsrgel), and Regional offices relative power (v2elrgpwr).

vdem1 %>% 
  mutate(dummy = case_when(v2ellocgov  == "NA" ~ 0,
                           v2ellocelc  ~ 0.5,
                           year >= 2017 & state == "CA" ~ 0,
                           TRUE ~ 1))





vdem1$LGI <- ifelse(vdem1$v2ellocgov == 'NA', 0, 1) & ifelse(vdem1$v2ellocelc == 'elected but no assembly', 0.5, 1)

df$Male <- ifelse(df$sex == 'male', 1, 0) df$Female <- ifelse(df$sex == 'female', 1, 0)
vdem1$LGI <- ifelse(vdem1f$v2ellocgov == 'elected', 1, 0.5, 0)
dataf$Disc_B <- ifelse(dataf$discipline == 'B', 1, 0)
1. Average lgindex and regional g index
2. LGI = dummy v2ellocgov, v2ellocelc, v2ellocpwr
3. RGI = dummy v2elreggov, v2elsrgel, v2elrgpwr

v <- vdem1[complete.cases(vdem1$v2x_divparctrl), ] # Omit NAs by columns
v     

V_dem = V_Dem_CY_Core_v12 %>% dplyr::select("country_name", "year", "v2x_libdem", "v2ddlexor",
                                       "v2xlg_legcon", "v2lgello",
                                      "v2lgelecup")



"v2lgamend",

"v2x_feduni", 




table2 = table %>% dplyr::select("year2", "Country 3", "v2x_libdem 1789-2021")
table2 <- filter(table2, table2$year2 >= 2019)

#table1 = table %>% dplyr::filter(table1, year2 >= 2019 & )
#table1$combined <- str_c(table1$`Country 1`, '', table1$`Country 2`)
names(table2)[names(table2) == "year2"] <- "year1"
names(table2)[names(table2) == "Country 3"] <- "Country 1"
#names(table2)[names(table2) == "v2x_libdem 1789-2021"] <- "year1"

merging <- merge(table1, table2, by="Country 1")
merging = select(merging, -year2)
merge(x, y, by=c("k1","k2"))
#year(date)

table1$`Gini index`<- as.numeric(str_replace_all(table1$`Gini index`, "%", ""))/100
table1$`Rate - World Bank `<- as.numeric(str_replace_all(table1$`Rate - World Bank `, "%", ""))/100

freedomhouse = table %>% dplyr::select("Country", "Total Score and status", "Political rights", "Civil liberties")

libdem_new = table %>% dplyr::select("country_name...13", "v2x_libdem 1789-2021", "year...8")
libdem_new1 = libdem_new %>% filter(year...8 > 2018)

names(libdem_new1)[names(libdem_new1) == "country_name...13"] <- "country_name"
names(libdem_new1)[names(libdem_new1) == "year...8"] <- "year"
table1 = table
#convert NAs into 0s
table1$new_cases_smoothed[is.na(table1$new_cases_smoothed)] <- 0
table1$new_cases[is.na(table1$new_cases)] <- 0
table1$total_cases[is.na(table1$total_cases)] <- 0
table1$total_deaths[is.na(table1$total_deaths)] <- 0


table1[, 21:27][is.na(table1[, 21:27])] <- 0
table1$extreme_poverty[is.na(table1$extreme_poverty)] <- 0
table1$weekly_hosp_admissions[is.na(table1$weekly_hosp_admissions)] <- 0
table1$year...1[is.na(table1$year...1)] <- 0
table1$quarter[is.na(table1$quarter)] <- 0
table1$time[is.na(table1$time)] <- 0
table1$country_name...4[is.na(table1$country_name...4)] <- 0
table1$v2x_libdem_2019[is.na(table1$v2x_libdem_2019)] <- 0
table1$pandem[is.na(table1$pandem)] <- 0
table1$panback[is.na(table1$panback)] <- 0

table1$Country[is.na(table1$Country)] <- 0
table1$`Total Score and status`[is.na(table1$`Total Score and status`)] <- 0
table1$`Political rights`[is.na(table1$`Political rights`)] <- 0
table1$`Civil liberties`[is.na(table1$`Civil liberties`)] <- 0
table1$`v2x_libdem 1789-2021`[is.na(table1$`v2x_libdem 1789-2021`)] <- 0





owid_covid_data <- read_excel("Desktop/owid-covid-data.xlsx")
View(owid_covid_data)
mortality = owid_covid_data
library(readxl)
pandem_TS_v6 <- read_excel("Desktop/pandem_TS_v6.xlsx")
View(pandem_TS_v6)
v_dem = pandem_TS_v6
names(v_dem)[names(v_dem) == "country_name"] <- "location"
library(readxl)
V_Dem_CY_Core_v12 <- read_excel("Desktop/V-Dem-CY-Core-v12.xlsx")
View(V_Dem_CY_Core_v12)
names(V_Dem_CY_Core_v12)[names(V_Dem_CY_Core_v12) == "country_name"] <- "location"

detailed = V_Dem_CY_Core_v12
#detailed1 = detailed %>% select(location, country_id, detailed$project, detailed$historical, 
 #                                      v2x_polyarchy, detailed$v2x_libdem, detailed$v2x_freexp_altinf, detailed$v2x_partip, detailed$v2xel_frefair, detailed$v2x_liberal, 
 #                                    v2x_cspart, detailed$v2xcl_rol, detailed$v2xel_regelec, detailed$v2xeg_eqaccess, detailed$v2xeg_eqprotec, 
 #                                     v2xeg_eqdr, detailed$v2elfrfair, detailed$v2psbars, 
  #                                     detailed$v2psprlnks, detailed$v2pscohesv)

#detailed1 = detailed<-data.frame(detailed$location, detailed$country_id, detailed$project, detailed$historical,
#                                 detailed$v2x_polyarchy,detailed$v2x_libdem,detailed$v2x_freexp_altinf, detailed$v2x_partip, detailed$v2xel_frefair,detailed$v2x_liberal, 
#                                 detailed$v2x_cspart, detailed$v2xcl_rol,detailed$v2xel_regelec, detailed$v2xeg_eqaccess, detailed$v2xeg_eqprotec, 
 #                                detailed$v2xeg_eqdr, detailed$v2elfrfair, detailed$v2psbars, 
#                                 detailed$v2psprlnks,detailed$v2pscohesv)
names(detailed1)[names(detailed1) == "detailed.location"] <- "location"                                


          #detailed$v2eltype_0,detailed$v2eltype_1, detailed$v2eltype_2, detailed$v2eltype_3, detailed$v2eltype_4, detailed$v2eltype_5),
#detailed$v2eltype_6,detailed$v2eltype_7

#cleaning
#detailed1 = str_replace_all(detailed1, "\\&quot\\;", " ")
#detailed1 = str_replace_all(detailed1, "\\&apos\\;", " ")
#detailed1 = str_replace_all(detailed1, "[[:punct:]]", "")


na.omit(merging)
na.omit(detailed1)



merging <- merge(v_dem, mortality, by="location")

na.merging <- merging[!is.na(merging$x1), ]                 # Omit NA by column via is.na
data_is.na                    
data_by_column <- data[complete.cases(data_subset), ] # Omit NAs by columns
data_by_column                                    
merging1 <- merge(merging, detailed1, by="location")
n_distinct(mortality$location)
