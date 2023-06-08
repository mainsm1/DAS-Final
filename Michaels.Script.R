library(tidyverse)
library(openintro)
library(plotly)
library("rnaturalearth")
library("rnaturalearthdata")
library(readxl)
library(readr)
library(gitcreds)
library(usethis)
library(psych)
library(Hmisc)
library('corrr')
library(ggcorrplot)
library("FactoMineR")
library("factoextra")
library(scales)
library(ggpattern)
DrugDataSetFull <- read_rds("DrugDatasetFull.rds")
world <- ne_countries(scale = "medium", returnclass = "sf")

DrugDataSetFullMap <- left_join(world, DrugDataSetFull, by=c('iso_a3' = 'ISO3'))

DrugDataSetFullMap <- DrugDataSetFullMap %>%
  select(measure, location, year, sex, cause, iso_a3, val, Religion, AgeToDrink.2016, regime_row_owid, civlibs_fh, GDPPerCapita, PovertyProportion, Unemployment, AvgBeerConsump.L, AvgSpiritConsump.L, AvgWineConsump.L, PropAUDTreatment.2008 , geometry) %>%
  arrange(desc(measure), location, iso_a3, year, sex, cause, val)

my_colors <- c("yellow","blue", "green","white","orange", "red")
my_colors2 <-c("red", "blue")

my_map_theme <- function(){
  theme(panel.background=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank())
}

Alc.Inc.DrugData <- DrugDataSetFull %>%
  filter(cause == "Alcohol use disorders", measure == "Incidence")

Alc.Dea.DrugData <- DrugDataSetFull %>%
  filter(cause == "Alcohol use disorders", measure == "Deaths")

Can.Inc.DrugData <- DrugDataSetFull %>%
  filter(cause == "Cannabis use disorders", measure == "Incidence")

Alc.Inc1999Map <- DrugDataSetFullMap %>%
  filter(cause == "Alcohol use disorders", measure == "Incidence", sex == "Both", year == "1999")

Alc.Inc2009Map <- DrugDataSetFullMap %>%
  filter(cause == "Alcohol use disorders", measure == "Incidence", sex == "Both", year == "2009")

Alc.Inc2019Map <- DrugDataSetFullMap %>%
  filter(cause == "Alcohol use disorders", measure == "Incidence", sex == "Both", year == "2019")

Alc.Dea1999Map <- Alc.Dea.DrugData %>%
  filter(sex == "Both", year == "1999")

Alc.Dea2009Map <- Alc.Dea.DrugData %>%
  filter(sex == "Both", year == "2009")

Alc.Dea2019Map <- Alc.Dea.DrugData %>%
  filter(sex == "Both", year == "2019")

Can.Inc1999Map <- DrugDataSetFullMap %>%
  filter(cause == "Cannabis use disorders", measure == "Incidence", sex == "Both", year == "1999")

Can.Inc2009Map <- DrugDataSetFullMap %>%
  filter(cause == "Cannabis use disorders", measure == "Incidence", sex == "Both", year == "2009")

Can.Inc2019Map <- DrugDataSetFullMap %>%
  filter(cause == "Cannabis use disorders", measure == "Incidence", sex == "Both", year == "2019")

#####
AlcyWalky <- DrugDataSetFullMap

AlcyWalky <- subset(AlcyWalky, year %in% c(1999, 2009, 2019))

AlcyWalky <- AlcyWalky %>%
  filter(sex == "Both", cause == "Alcohol use disorders", measure == "Incidence")

AlcyWalky$highest_alcohol_type <- NA

for (i in 1:nrow(AlcyWalky)) {
  
  if (is.na(AlcyWalky$AvgSpiritConsump.L[i]) & is.na(AlcyWalky$AvgBeerConsump.L[i]) & is.na(AlcyWalky$AvgWineConsump.L[i])){
    next
  }
  if (AlcyWalky$AvgSpiritConsump.L[i] > AlcyWalky$AvgBeerConsump.L[i] & AlcyWalky$AvgSpiritConsump.L[i] > AlcyWalky$AvgWineConsump.L[i]) {
    AlcyWalky$highest_alcohol_type[i] <- "Spirits"
  }
  else if (AlcyWalky$AvgBeerConsump.L[i] > AlcyWalky$AvgSpiritConsump.L[i] & AlcyWalky$AvgBeerConsump.L[i] > AlcyWalky$AvgWineConsump.L[i]){
    AlcyWalky$highest_alcohol_type[i] <- "Beer"
  }
  else{
    AlcyWalky$highest_alcohol_type[i] <- "Wine"
  }
}

AlcyWalky1999 <- AlcyWalky %>%
  filter(year == 1999)

ggplot(Alc.Inc2019Map) +
  geom_sf(aes(fill = highest_alcohol_type)) +
  scale_fill_manual(values = c("goldenrod", "lightcyan", "firebrick"))+
  my_map_theme() + 
  labs(title = "Highest Average Consumed Alcohol Type by Country in 1999", 
       subtitle = "There are 90 countries who primarily drink beer, 60 who primarily drink\nspirits, and 32 who primarily drink wine.") +
  guides(fill = guide_legend(title = "Type of Alcohol"))

table(AlcyWalky1999$highest_alcohol_type)

AlcyWalky2009 <- AlcyWalky %>%
  filter(year == 2009)

AlcyWalky2009 %>%
  ggplot() +
  geom_sf(aes(fill = highest_alcohol_type, group = 1)) +
  scale_fill_manual(values = c("goldenrod", "lightcyan", "firebrick"))+
  my_map_theme() + 
  labs(title = "Highest Average Consumed Alcohol Type by Country in 2009", 
       subtitle = "There are 102 countries who primarily drink beer, 62 who primarily drink\nspirits, and 20 who primarily drink wine.") +
  guides(fill = guide_legend(title = "Type of Alcohol"))

table(AlcyWalky2009$highest_alcohol_type)

aAlcyWalky2019 <- AlcyWalky %>%
  filter(year == 2019)

ggplot(AlcyWalky2019) +
  geom_sf(aes(fill = highest_alcohol_type)) +
  scale_fill_manual(values = c("goldenrod", "lightcyan", "firebrick"))+
  my_map_theme() + 
  labs(title = "Highest Average Consumed Alcohol Type by Country in 2019", 
       subtitle = "There are 104 countries who primarily drink beer, 52 who primarily drink\nspirits, and 29 who primarily drink wine.") +
  guides(fill = guide_legend(title = "Type of Alcohol"))

table(AlcyWalky2019$highest_alcohol_type)


ugg <- aov(val ~ highest_alcohol_type, data = AlcyWalky1999)
summary(ugg)
TukeyHSD(ugg)

cor(AlcyWalky1999$val, AlcyWalky1999$AvgBeerConsump.L, method = "pearson", use = "complete.obs")
cor.test(AlcyWalky1999$val, AlcyWalky1999$AvgBeerConsump.L)

cor(AlcyWalky1999$val, AlcyWalky1999$AvgSpiritConsump.L, method = "pearson", use = "complete.obs")
cor.test(AlcyWalky1999$val, AlcyWalky1999$AvgSpiritConsump.L)

cor(AlcyWalky1999$val, AlcyWalky1999$AvgWineConsump.L, method = "pearson", use = "complete.obs")
cor.test(AlcyWalky1999$val, AlcyWalky1999$AvgWineConsump.L)




ugg2 <- aov(val ~ highest_alcohol_type, data = AlcyWalky2009)
summary(ugg2)
TukeyHSD(ugg2)

cor(AlcyWalky2009$val, AlcyWalky2009$AvgBeerConsump.L, method = "pearson", use = "complete.obs")
cor.test(AlcyWalky2009$val, AlcyWalky2009$AvgBeerConsump.L)

cor(AlcyWalky2009$val, AlcyWalky2009$AvgSpiritConsump.L, method = "pearson", use = "complete.obs")
cor.test(AlcyWalky2009$val, AlcyWalky2009$AvgSpiritConsump.L)

cor(AlcyWalky2009$val, AlcyWalky2009$AvgWineConsump.L, method = "pearson", use = "complete.obs")
cor.test(AlcyWalky2009$val, AlcyWalky2009$AvgWineConsump.L)




ug3 <- aov(val ~ highest_alcohol_type, data = AlcyWalky2019)
summary(ugg3)
TukeyHSD(ugg3)

cor(AlcyWalky2019$val, AlcyWalky2019$AvgBeerConsump.L, method = "pearson", use = "complete.obs")
cor.test(AlcyWalky2019$val, AlcyWalky2019$AvgBeerConsump.L)

cor(AlcyWalky2019$val, AlcyWalky2019$AvgSpiritConsump.L, method = "pearson", use = "complete.obs")
cor.test(AlcyWalky2019$val, AlcyWalky2019$AvgSpiritConsump.L)

cor(AlcyWalky2019$val, AlcyWalky2019$AvgWineConsump.L, method = "pearson", use = "complete.obs")
cor.test(AlcyWalky2019$val, AlcyWalky2019$AvgWineConsump.L)
######################################################################################################################################################

##### AUD and AgeToDrink
Alc.Inc2016 <- Alc.Inc.DrugData %>%
  filter(year == "2016", sex == "Both")

Alc.Age.2016.aov <- aov(val ~ AgeToDrink.2016, data = Alc.Inc2016)
summary(Alc.Age.2016.aov)
TukeyHSD(Alc.Age.2016.aov)

unique(Alc.Inc2016$AgeToDrink.2016)

AgeData2008 <- DrugDataSetFull %>%
  filter(year == 2008, sex == "Both", cause == "Alcohol use disorders", measure == "Incidence")

cor(AgeData2008$val, AgeData2008$PropAUDTreatment.2008, method = "pearson", use = "complete.obs")
cor.test(Alc.Inc1999Map$val, Alc.Inc1999Map$PovertyProportion)

ggplot()

#####
CanBanRelgion1999 <- aov(val ~ regime_row_owid, data = Can.Inc1999Map)
summary(CanBanRelgion1999)
TukeyHSD(CanBanRelgion1999)

CanBanRelgion2009 <- aov(val ~  regime_row_owid, data = Can.Inc2009Map)
summary(CanBanRelgion2009)
TukeyHSD(CanBanRelgion2009)

CanBanRelgion2019 <- aov(val ~  regime_row_owid, data = Can.Inc2019Map)
summary(CanBanRelgion2019)
TukeyHSD(CanBanRelgion2019)
#####


colSums(is.na(DrugDataSetFull))

Alc.Inc2019PCA <- DrugDataSetFull %>%
  filter(cause == "Alcohol use disorders", measure == "Incidence", sex == "Both", year == "2019")

PCA.AUD.GDP.Beer.Unemployment.2019 <- subset(Alc.Inc2019PCA, select = c(GDPPerCapita, AvgBeerConsump.L, AvgSpiritConsump.L, AvgWineConsump.L, Unemployment))
  
PCA.AUD.GDP.Beer.Unemployment.2019 <- scale(PCA.AUD.GDP.Beer.Unemployment.2019)

PCA.matrix <- cor(PCA.AUD.GDP.Beer.Unemployment.2019, use = "pairwise.complete.obs")
ggcorrplot(PCA.matrix)

Data.pca <- princomp(PCA.matrix)
summary(Data.pca)

Data.pca$loadings[, 1:2]

fviz_eig(Data.pca, addlabels = TRUE)

# Graph of the variables
fviz_pca_var(Data.pca, col.var = "black")

fviz_pca_var(Data.pca, col.var = "cos2",
             gradient.cols = c("black", "orange", "green"),
             repel = TRUE)

pc <- prcomp(na.omit(PCA.AUD.GDP.Beer.Unemployment.2019), center=T, scale.=T)


##### AUD/CUD Poverty

cor(Alc.Inc1999Map$val, Alc.Inc1999Map$PovertyProportion, method = "pearson", use = "complete.obs")
cor.test(Alc.Inc1999Map$val, Alc.Inc1999Map$PovertyProportion)

cor(Alc.Inc2009Map$val, Alc.Inc2009Map$PovertyProportion, method = "pearson", use = "complete.obs")
cor.test(Alc.Inc2009Map$val, Alc.Inc2009Map$PovertyProportion)

cor(Alc.Inc2019Map$val, Alc.Inc2019Map$PovertyProportion, method = "pearson", use = "complete.obs")
cor.test(Alc.Inc2019Map$val, Alc.Inc2019Map$PovertyProportion)



cor(Can.Inc1999Map$val, Can.Inc1999Map$PovertyProportion, method = "pearson", use = "complete.obs")
cor.test(Can.Inc1999Map$val, Can.Inc1999Map$PovertyProportion)

cor(Can.Inc2009Map$val, Can.Inc2009Map$PovertyProportion, method = "pearson", use = "complete.obs")
cor.test(Can.Inc2009Map$val, Can.Inc2009Map$PovertyProportion)

cor(Can.Inc2019Map$val, Can.Inc2019Map$PovertyProportion, method = "pearson", use = "complete.obs")
cor.test(Can.Inc2019Map$val, Can.Inc2019Map$PovertyProportion)


#####AUD/CUD Unemployment

cor(Alc.Inc1999Map$val, Alc.Inc1999Map$Unemployment, method = "pearson", use = "complete.obs")
cor.test(Alc.Inc1999Map$val, Alc.Inc1999Map$Unemployment)

cor(Alc.Inc2009Map$val, Alc.Inc2009Map$Unemployment, method = "pearson", use = "complete.obs")
cor.test(Alc.Inc2009Map$val, Alc.Inc2009Map$Unemployment)

cor(Alc.Inc2019Map$val, Alc.Inc2019Map$Unemployment, method = "pearson", use = "complete.obs")
cor.test(Alc.Inc2019Map$val, Alc.Inc2019Map$Unemployment)



cor(Can.Inc1999Map$val, Can.Inc1999Map$Unemployment, method = "pearson", use = "complete.obs")
cor.test(Can.Inc1999Map$val, Can.Inc1999Map$Unemployment)

cor(Can.Inc2009Map$val, Can.Inc2009Map$Unemployment, method = "pearson", use = "complete.obs")
cor.test(Can.Inc2009Map$val, Can.Inc2009Map$Unemployment)

cor(Can.Inc2019Map$val, Can.Inc2019Map$Unemployment, method = "pearson", use = "complete.obs")
cor.test(Can.Inc2019Map$val, Can.Inc2019Map$Unemployment)


#####Treatment and AUD 

Alc.Inc2008 <- Alc.Inc.DrugData %>%
  filter(year == "2008", sex == "Both")

cor(Alc.Inc2008$val, Alc.Inc2008$PropAUDTreatment.2008, method = "pearson", use = "complete.obs")
cor.test(Alc.Inc2008$val, Alc.Inc2008$PropAUDTreatment.2008)

countries_to_label <- c("United States of America", "Saudi Arabia", "Mauritius", "Croatia", "Iceland", "Austria", "Japan", "United Kingdom", "Germany", "Brazil", "Mongolia", "El Salvador")
gdp_breaks <- c(5000, 10000, 15000, 20000, Inf)
gdp_labels <- c("<5000", "5000-9999", "10000-14999", "15000-20000")


ggplot(Alc.Inc2008, aes(PropAUDTreatment.2008, val, color = cut(GDPPerCapita, breaks = gdp_breaks, labels = gdp_labels))) +
  geom_point() +
  geom_text(data = subset(Alc.Inc2008, location %in% countries_to_label),
            aes(label = location), size = 3, vjust = -0.5, color = "black") +
  scale_color_manual(values = c("<5000" = "blue", "5000-9999" = "green", "10000-14999" = "yellow", "15000-20000" = "red")) +
  labs(title = "Proportion of population getting Treatment for AUD\nby AUD rate, colored by GDP Per Capita",
       subtitle = "If country is grey, GDP per Capita is less than $5000",
       color = "GDP Range ($)",
       x = "Proportion of Population getting treatment for AUD",
       y = "AUD Rate (Per 100k)") +
  theme_minimal() +
  scale_x_continuous(labels = function(x) sprintf("%1.0f%%", x))

#####GDP Tests 

###AUD and CUD val and GDP analysis

cor(Alc.Inc1999Map$val, Alc.Inc1999Map$GDPPerCapita, method = "pearson", use = "complete.obs")
cor.test(Alc.Inc1999Map$val, Alc.Inc1999Map$GDPPerCapita)

cor(Alc.Inc2009Map$val, Alc.Inc1999Map$GDPPerCapita, method = "pearson", use = "complete.obs")
cor.test(Alc.Inc2009Map$val, Can.Inc2009Map$Unemployment)

cor(Alc.Inc2019Map$val, Can.Inc2019Map$Unemployment, method = "pearson", use = "complete.obs")
cor.test(Alc.Inc2019Map$val, Can.Inc2019Map$Unemployment)




cor(Can.Inc1999Map$val, Can.Inc1999Map$GDPPerCapita, method = "pearson", use = "complete.obs")
cor.test(Can.Inc1999Map$val, Can.Inc1999Map$GDPPerCapita, method = "pearson")

cor(Can.Inc2009Map$val, Can.Inc2009Map$GDPPerCapita, method = "pearson", use = "complete.obs")
cor.test(Can.Inc2009Map$val, Can.Inc2009Map$GDPPerCapita, method = "pearson")

cor(Can.Inc2019Map$val, Can.Inc2019Map$GDPPerCapita, method = "pearson", use = "complete.obs")
cor.test(Can.Inc2019Map$val, Can.Inc2019Map$GDPPerCapita, method = "pearson")


###AUD and CUD val and Poverty Proportion 

cor(Alc.Inc1999Map$val, Alc.Inc1999Map$PovertyProportion, method = "pearson", use = "complete.obs")
cor.test(Alc.Inc1999Map$val, Alc.Inc1999Map$PovertyProportion)


cor(Alc.Inc2009Map$val, Alc.Inc2009Map$PovertyProportion, method = "pearson", use = "complete.obs")
cor.test(Alc.Inc2009Map$val, Alc.Inc2009Map$PovertyProportion)

cor(Alc.Inc2019Map$val, Alc.Inc2019Map$PovertyProportion, method = "pearson", use = "complete.obs")
cor.test(Alc.Inc2019Map$val, Alc.Inc2019Map$PovertyProportion)




cor(Can.Inc1999Map$val, Can.Inc1999Map$PovertyProportion, method = "pearson", use = "complete.obs")
cor.test(Can.Inc1999Map$val, Can.Inc1999Map$PovertyProportion, method = "pearson")

cor(Can.Inc2009Map$val, Can.Inc2009Map$PovertyProportion, method = "pearson", use = "complete.obs")
cor.test(Can.Inc2009Map$val, Can.Inc2009Map$PovertyProportion, method = "pearson")

cor(Can.Inc2019Map$val, Can.Inc2019Map$PovertyProportion, method = "pearson", use = "complete.obs")
cor.test(Can.Inc2019Map$val, Can.Inc2019Map$PovertyProportion, method = "pearson")

###AUD and CUD val and Unemployment

cor(Alc.Inc1999Map$val, Alc.Inc1999Map$`Unemployment, total (% of total labor force) (modeled ILO estimate)`, method = "pearson", use = "complete.obs")
cor.test(Alc.Inc1999Map$val, Alc.Inc1999Map$`Unemployment, total (% of total labor force) (modeled ILO estimate)`)


cor(Alc.Inc2009Map$val, Alc.Inc2009Map$`Unemployment, total (% of total labor force) (modeled ILO estimate)`, method = "pearson", use = "complete.obs")
cor.test(Alc.Inc2009Map$val, Alc.Inc2009Map$`Unemployment, total (% of total labor force) (modeled ILO estimate)`)

cor(Alc.Inc2019Map$val, Alc.Inc2019Map$`Unemployment, total (% of total labor force) (modeled ILO estimate)`, method = "pearson", use = "complete.obs")
cor.test(Alc.Inc2019Map$val, Alc.Inc2019Map$`Unemployment, total (% of total labor force) (modeled ILO estimate)`)




cor(Can.Inc1999Map$val, Can.Inc1999Map$`Unemployment, total (% of total labor force) (modeled ILO estimate)`, method = "pearson", use = "complete.obs")
cor.test(Can.Inc1999Map$val, Can.Inc1999Map$`Unemployment, total (% of total labor force) (modeled ILO estimate)`, method = "pearson")

cor(Can.Inc2009Map$val, Can.Inc2009Map$`Unemployment, total (% of total labor force) (modeled ILO estimate)`, method = "pearson", use = "complete.obs")
cor.test(Can.Inc2009Map$val, Can.Inc2009Map$`Unemployment, total (% of total labor force) (modeled ILO estimate)`, method = "pearson")

cor(Can.Inc2019Map$val, Can.Inc2019Map$`Unemployment, total (% of total labor force) (modeled ILO estimate)`, method = "pearson", use = "complete.obs")
cor.test(Can.Inc2019Map$val, Can.Inc2019Map$`Unemployment, total (% of total labor force) (modeled ILO estimate)`, method = "pearson")


####Civil Liberties maps

ggplot(Alc.Inc1999Map) +
  geom_sf(aes(fill = civlibs_fh)) +
  scale_fill_manual(values = c("blue", "red", "yellow"))+
  my_map_theme() + 
  labs(title = "Relative Score of Civil Liberties Per country in 1999", 
       subtitle = "Based on the assessment and rating by Freedom House (2022). It captures the extent of freedom of \nexpression and association, the rule of law, and personal autonomy. Lower ratings indicate more liberties.") +
  guides(fill = guide_legend(title = "Civil Liberties"))

ggplot(Alc.Inc2009Map) +
  geom_sf(aes(fill = civlibs_fh)) +
  scale_fill_manual(values = c("blue", "red", "yellow"))+
  my_map_theme() + 
  labs(title = "Relative Score of Civil Liberties Per country in 2009", 
       subtitle = "Based on the assessment and rating by Freedom House (2022). It captures the extent of freedom of \nexpression and association, the rule of law, and personal autonomy. Lower ratings indicate more liberties.") +
  guides(fill = guide_legend(title = "Civil Liberties"))

ggplot(Alc.Inc2019Map) +
  geom_sf(aes(fill = civlibs_fh)) +
  scale_fill_manual(values = c("blue", "red", "yellow"))+
  my_map_theme() + 
  labs(title = "Relative Score of Civil Liberties By country in 2019", 
       subtitle = "Based on the assessment and rating by Freedom House (2022). It captures the\nextent of freedom of expression and association, the rule of law, and personal\nautonomy. Lower ratings indicate more liberties.") +
  guides(fill = guide_legend(title = "Civil Liberties"))


##### Religion map 2019
ggplot(AlcyWalky2019) +
  geom_sf(aes(fill = Religion)) +
  scale_fill_manual(values = c("red2", "green2", "tan")) +
  geom_sf_pattern(data = AlcyWalky2019, aes(fill = highest_alcohol_type), pattern_fill = "black", pattern = "line_cross_tr45", pattern_color = "transparent") +
  my_map_theme() + 
  labs(title = "Primary Religion of Each Country in 2019", 
       subtitle = "Shows the Primary Religion of each Country.") +
  guides(fill = guide_legend(title = "Religion")) +
  theme(legend.key.width = unit(1, "cm"))

ggplot(AlcyWalky2019) +
  geom_sf(aes(fill = Religion)) +
  scale_fill_manual(values = c("red2", "green2", "tan", "")) +
  geom_sf_pattern(data = AlcyWalky2019, aes(fill = highest_alcohol_type), pattern_fill = "black", pattern = "line_cross_tr45", pattern_color = "transparent") +
  my_map_theme() +
  labs(title = "Primary Religion and Highest Booze of Each Country in 2019",
       subtitle = "Shows the Primary Religion and Highest Booze of each Country.") +
  guides(fill = guide_legend(title = "Religion")) +
  theme(legend.key.width = unit(1, "cm"))

#####Alc incidence and deaths by Civil liberties

Alc.Inc.CL.1999 <- aov(val ~ civlibs_fh, data = Alc.Inc1999Map)
summary(Alc.Inc.CL.1999)
TukeyHSD(Alc.Inc.CL.1999)

Alc.Dea.CL.1999 <- aov(val ~ civlibs_fh, data = Alc.Dea1999Map)
summary(Alc.Dea.CL.1999)
TukeyHSD(Alc.Dea.CL.1999)



Alc.Inc.CL.2009 <- aov(val ~ civlibs_fh, data = Alc.Inc2009Map)
summary(Alc.Inc.CL.2009)
TukeyHSD(Alc.Inc.CL.2009)

Alc.Dea.CL.2009 <- aov(val ~ civlibs_fh, data = Alc.Dea2009Map)
summary(Alc.Dea.CL.2009)
TukeyHSD(Alc.Dea.CL.2009)



Alc.Inc.CL.2019 <- aov(val ~ civlibs_fh, data = Alc.Inc2019Map)
summary(Alc.Dea.CL.2019)
TukeyHSD(Alc.Inc.CL.2019)

Alc.Dea.CL.2019 <- aov(val ~ civlibs_fh, data = Alc.Dea2019Map)
summary(Alc.Dea.CL.2019)
TukeyHSD(Alc.Dea.CL.2019)


#####Can incidence by Civil liberties

Can.Inc.CL.1999 <- aov(val ~ civlibs_fh, data = Can.Inc1999Map)
summary(Can.Inc.CL.1999)
TukeyHSD(Can.Inc.CL.1999)

Can.Inc.CL.2009 <- aov(val ~ civlibs_fh, data = Can.Inc2009Map)
summary(Can.Inc.CL.2009)
TukeyHSD(Can.Inc.CL.2009)

Can.Inc.CL.2019 <- aov(val ~ civlibs_fh, data = Can.Inc2019Map)
summary(Can.Inc.CL.2019)
TukeyHSD(Can.Inc.CL.2019)


####Bar chart of civil liberties for cannabis and Alc
Alc.Can.CL.Bar<- DrugDataSetFull %>%
  filter(measure == "Incidence", sex == "Both", cause == "Cannabis use disorders")

Alc.Can.CL.Bar <- subset(Alc.Can.CL.Bar, year %in% c(1999, 2009, 2019))

ggplot(Alc.Can.CL.Bar, aes(cause, val, fill = civlibs_fh)) +
  geom_bar(stat = "identity", position = "dodge", width = 1) +
  my_map_theme() +
  guides(fill = guide_legend(title = "Civil Liberties")) +
  facet_grid(. ~year) +
  labs(title = "Rate of CUD among countries of\ndiffering Civil Liberty levels by year") +
  xlab("Substance Use Disorder") +
  ylab("Number of Incidence (Per 100k)") + 
  theme(axis.text.x = element_text(face = "bold", size  = 8, angle = 25),
        axis.text.y = element_text(face = "bold", color = "black", size = 8),
        axis.title.x = element_text(face = "bold", color = "black", size = 12),
        axis.title.y = element_text(face = "bold", color = "black", size = 12)) +
  scale_fill_manual(values = c("lightsalmon1", "plum1", "maroon2", "grey"))
  
  

ggplot(Alc.Can.CL.Bar, aes(cause, val, fill = civlibs_fh)) +
  geom_bar(stat = "identity", position = "dodge", width = 1) +
  my_map_theme() +
  guides(fill = guide_legend(title = "Civil Liberties")) +
  facet_grid(. ~year) +
  labs(title = "Differences in AUD and CUD among countries of differing Civil Liberty\nlevels by year") +
  xlab("Substance Use Disorder") +
  ylab("Number of Incidence (Per 100k)") + 
  theme(axis.text.x = element_text(face = "bold", size  = 8, angle = 25),
        axis.text.y = element_text(face = "bold", color = "black", size = 8),
        axis.title.x = element_text(face = "bold", color = "black", size = 12),
        axis.title.y = element_text(face = "bold", color = "black", size = 12)) +
  scale_fill_manual(values = c("lightsalmon1", "plum1", "maroon2", "grey"))

#####Alc incidence and death by Religion

Alc.Inc.Religion.1999 <- aov(val ~ Religion, data = Alc.Inc1999Map)
summary(Alc.Inc.Religion.1999)
TukeyHSD(Alc.Inc.Religion.1999)

Alc.Dea.Religion.1999 <- aov(val ~ Religion, data = Alc.Dea1999Map)
summary(Alc.Dea.Religion.1999)
TukeyHSD(Alc.Dea.Religion.1999)

Alc.Inc.Islam.1999 <- Alc.Inc1999Map %>%
  filter(Religion == "Islam")

Alc.Inc.Christ.1999 <- Alc.Inc1999Map %>%
  filter(Religion == "Christianity")


Alc.Inc.Religion.2009 <- aov(val ~ Religion, data = Alc.Inc2009Map)
summary(Alc.Inc.Religion.2009)
TukeyHSD(Alc.Inc.Religion.2009)

Alc.Dea.Religion.2009 <- aov(val ~ Religion, data = Alc.Dea2009Map)
summary(Alc.Dea.Religion.2009)
TukeyHSD(Alc.Dea.Religion.2009)

Alc.Inc.Islam.2009 <- Alc.Inc2009Map %>%
  filter(Religion == "Islam")

Alc.Inc.Christ.2009 <- Alc.Inc2009Map %>%
  filter(Religion == "Christianity")



Alc.Inc.Religion.2019 <- aov(val ~ Religion, data = Alc.Inc2019Map)
summary(Alc.Inc.Religion.2019)
TukeyHSD(Alc.Inc.Religion.2019)

Alc.Dea.Religion.2019 <- aov(val ~ Religion, data = Alc.Dea2019Map)
summary(Alc.Dea.Religion.2019)
TukeyHSD(Alc.Dea.Religion.2019)

Alc.Inc.Islam <- Alc.Inc.DrugData %>%
  filter(Religion == "Islam", sex == "Both")

Alc.Inc.Islam <- subset(Alc.Inc.Islam, year %in% c(1999, 2009, 2019))

Piechart.Islam.Alc <- Alc.Inc.Islam %>%
  group_by(year, civlibs_fh) %>%
  count(civlibs_fh)



ggplot(Piechart.Islam.Alc, aes(x = "", y = n, fill = civlibs_fh)) +
  geom_bar(stat = "identity", width = 1) + 
  coord_polar(theta = "y") +
  theme_void() +
  facet_grid(. ~year) +
  scale_fill_manual(values = c("red", "yellow", "grey")) +
  labs(title = "  Distribution of Civil Liberties Ratings among Islamic Nations\n  in 1999, 2009, and 2019") +
  guides(fill = guide_legend(title = "Civil Liberties")) +
  geom_text(aes(label = n), position = position_stack(vjust = 0.5), size = 4)


Alc.Inc.Christ <- Alc.Inc.DrugData %>%
  filter(Religion == "Christianity", sex == "Both")

Alc.Inc.Christ <- subset(Alc.Inc.Christ, year %in% c(1999, 2009, 2019))

Piechart.Christ.Alc <- Alc.Inc.Christ %>%
  group_by(year, civlibs_fh) %>%
  count(civlibs_fh)

ggplot(Piechart.Christ.Alc, aes(x = "", y = n, fill = civlibs_fh)) +
  geom_bar(stat = "identity", width = 1) + 
  coord_polar(theta = "y") +
  theme_void() +
  facet_grid(. ~year) +
  scale_fill_manual(values = c("cyan", "yellow", "red", "grey")) +
  labs(title = "  Distribution of Civil Liberties Rating among Christian Nations\n  in 1999, 2009, and 2019") +
  guides(fill = guide_legend(title = "Civil Liberties")) +
  geom_text(aes(label = n), position = position_stack(vjust = 0.5), size = 4)




#####Can incidence by Religion

Can.Inc.Religion.1999 <- aov(val ~ Religion, data = Can.Inc1999Map)
summary(Can.Inc.Religion.1999)
TukeyHSD(Can.Inc.Religion.1999)



Can.Inc.Religion.2009 <- aov(val ~ Religion, data = Can.Inc2009Map)
summary(Can.Inc.Religion.2009)
TukeyHSD(Can.Inc.Religion.2009)

Can.Inc.Religion.2019 <- aov(val ~ Religion, data = Can.Inc2019Map)
summary(Can.Inc.Religion.2019)
TukeyHSD(Can.Inc.Religion.2019)




#####Alc incidence by Regime Type 

Alc.Inc.Regime.1999 <- aov(val ~ regime_row_owid, data = Alc.Inc1999Map)
summary(Alc.Inc.Regime.1999)
TukeyHSD(Alc.Inc.Regime.1999)

Alc.Dea.Regime.1999 <- aov(val ~ regime_row_owid, data = Alc.Dea1999Map)
summary(Alc.Dea.Regime.1999)
TukeyHSD(Alc.Dea.Regime.1999)



Alc.Inc.Regime.2009 <- aov(val ~ regime_row_owid, data = Alc.Inc2009Map)
summary(Alc.Inc.Regime.2009)
TukeyHSD(Alc.Inc.Regime.2009)

Alc.Dea.Regime.2009 <- aov(val ~ regime_row_owid, data = Alc.Dea2009Map)
summary(Alc.Dea.Regime.2009)
TukeyHSD(Alc.Dea.Regime.2009)



Alc.Inc.Regime.2019 <- aov(val ~ regime_row_owid, data = Alc.Inc2019Map)
summary(Alc.Inc.Regime.2019)
TukeyHSD(Alc.Inc.Regime.2019)

Alc.Dea.Regime.2019 <- aov(val ~ regime_row_owid, data = Alc.Dea2019Map)
summary(Alc.Dea.Regime.2019)
TukeyHSD(Alc.Dea.Regime.2019)

#####Can incidence by Regime Type 

Can.Inc.Regime.1999 <- aov(val ~ regime_row_owid, data = Can.Inc1999Map)
summary(Can.Inc.Regime.1999)
TukeyHSD(Can.Inc.Regime.1999)

Can.Inc.Regime.2009 <- aov(val ~ regime_row_owid, data = Can.Inc2009Map)
summary(Can.Inc.Regime.2009)
TukeyHSD(Can.Inc.Regime.2009)

Can.Inc.Regime.2019 <- aov(val ~ regime_row_owid, data = Can.Inc2019Map)
summary(Can.Inc.Regime.2019)
TukeyHSD(Can.Inc.Regime.2019)
  
#####CL and Religion
CL.Religion.1999 <- Alc.Inc.DrugData %>%
  filter(year == 1999)

AL.Religion.1999 <- aov(civlibs_fh ~ Religion, data = CL.Religion.1999)
summary(AL.Religion.1999)
TukeyHSD(AL.Religion.1999)



#####Regime Over time Alc inc

Regime.Alc.Overtime <- Can.Inc.DrugData %>%
  filter(measure == "Incidence", sex == "Both") %>%
  group_by(year, regime_row_owid) %>%
  summarise('Mean Estimated Number of People with a CUD Per 100k' = mean(val))




ggplot(Regime.Alc.Overtime) +
  geom_line(aes(year, `Mean Estimated Number of People with a CUD Per 100k`, color = regime_row_owid), size = 2) +
  labs(title = "Mean Rate of Cannabis-Use Disorder Incidences by Regime type\novertime, regardless of sex, per 100k people", 
       subtitle = "Based on estimates from the Global Burden of Data (GBD)",
       color = "Regime Type ") +
  my_map_theme() +
  theme(axis.text.x = element_text(face = "bold", size  = 12),
        axis.text.y = element_text(face = "bold", color = "black", size = 12)) +
  scale_color_manual(values = c("red", "salmon3", "pink", "purple", "grey"))


Regime.Can.Overtime <- Can.Inc.DrugData %>%
  filter(measure == "Incidence", sex == "Both") %>%
  group_by(year, regime_row_owid) %>%
  summarise('Mean Estimated Number of People with a CUD Per 100k' = mean(val))

ggplot(Regime.Can.Overtime) +
  geom_line(aes(year, `Mean Estimated Number of People with a CUD Per 100k`, color = regime_row_owid), size = 2) +
  my_map_theme() +
  labs(title = "Mean Rate of Cannabis-Use Disorder Incidences by Regime type\novertime, regardless of Age or Sex, per 100k people", 
       subtitle = "Based on estimates from the Global Burden of Data (GBD)",
       ylab("Incidences of CUD (per 100k)"),
       color = "Regime Type ") +
  theme(axis.text.x = element_text(face = "bold", size  = 12),
        axis.text.y = element_text(face = "bold", color = "black", size = 12)) +
  scale_color_manual(values = c("red", "salmon3", "pink", "purple", "grey"))

#####Regime and GDP 

GDP.Regime.1999 <- aov(`GDP Measure` ~ regime_row_owid, data = Alc.Inc1999Map)
summary(GDP.Regime.1999)
TukeyHSD(GDP.Regime.1999)

GDP.Regime.2009 <- aov(`GDP Measure` ~ regime_row_owid, data = Alc.Inc2009Map)
summary(GDP.Regime.2009)  c
TukeyHSD(GDP.Regime.2009)

GDP.Regime.2019 <- aov(`GDP Measure` ~ regime_row_owid, data = Alc.Inc2019Map)
summary(GDP.Regime.2019)
TukeyHSD(GDP.Regime.2019)



  
#####regime by year
ggplot(Alc.Inc1999Map) +
  geom_sf(aes(fill = regime_row_owid)) + 
  my_map_theme() + 
  labs(title = "Regime Type Per Country in 2019") +
  guides(fill = guide_legend(title = "Type of Regime"))

ggplot(Alc.Inc2009Map) +
  geom_sf(aes(fill = regime_row_owid)) + 
  my_map_theme() + 
  labs(title = "Regime Type Per Country in 2009") +
  guides(fill = guide_legend(title = "Type of Regime"))

ggplot(Alc.Inc2019Map) +
  geom_sf(aes(fill = regime_row_owid)) + 
  my_map_theme() + 
  labs(title = "Regime Type Per Country in 2019") +
  guides(fill = guide_legend(title = "Type of Regime"))  
  
#################
#################
#################
#################
#################
#################

Alc.Policy.Anova <- aov(val ~ regime_row_owid, data = Alc.Inc2019)
summary(Alc.Policy.Anova)

TukeyHSD(Alc.Policy.Anova)

Alc.Religion.Anova <- aov(val ~ Religion, Alc.Inc2019)
summary(Alc.Religion.Anova)

ggplot(Alc.Inc2019Map) +
  geom_sf(aes(fill = Religion)) +
  my_map_theme() +
  labs(title = "Primary Religions of each Country in 2019") +
  scale_fill_manual(values = c("lightsalmon1", "aquamarine1", "grey", "gold1"))


TukeyHSD(Alc.Religion.Anova)

ggplot(Alc.Inc2019Map) +
  geom_sf(aes(fill = civlibs_fh)) +
  scale_fill_continuous(low = "yellow", high = "red") +
  my_map_theme() + 
    labs(title = "Relative Score of Civil Liberties Per country in 2019", 
         subtitle = "Based on the assessment and rating by Freedom House (2022). It captures the extent of freedom of \nexpression and association, the rule of law, and personal autonomy. Lower ratings indicate more liberties.") +
  guides(fill = guide_legend(title = "Civil Liberties\nScore"))

SexDiff.Time.Line <- Alc.Inc.DrugData %>%
  filter(measure == "Incidence") %>%
  group_by(year, civlibs_fh) %>%
  summarise('Mean Rate of Alcohol-Use Disorder Per 100k' = mean(val))

ggplot(SexDiff.Time.Line) +
  geom_line(aes(year, `Mean Rate of Alcohol-Use Disorder Per 100k`, color = civlibs_fh)) +
  labs(title = "Global Mean Rate of Alcohol-Use Disorder Incidences by Sex, \nRegardless of Age, per 100k people", 
       subtitle = "Based on estimates from the GBD") +
  theme(axis.text.x = element_text(face = "bold", size  = 12),
        axis.text.y = element_text(face = "bold", color = "black", size = 10))
######################################################################################################

MeanNoAgeSex <-  AlcCanWithFactors %>%
  filter(measure == "Incidence") %>%
  group_by(cause) %>%
  summarise('Mean Drug Usage Per 100k' = mean(estimate))

MeanNoAgeSexDeath <- AlcCanWithFactors %>%
  filter(measure == "Deaths") %>%
  group_by(cause) %>%
  summarise('Mean Drug Deaths Per 100k' = mean(estimate))


###Plot of Drug incidence rate mean, Regardless of Sex, Age, or Year, per 100k people
ggplot(MeanNoAgeSex, aes(cause, `Mean Drug Usage Per 100k`, fill = cause)) + 
  geom_bar(stat = "identity") +
  labs(title = "Mean Drug Usage, Regardless of Sex, Age, or Year, per 100k people") +
  theme(axis.text.x = element_text(face = "bold", size  = 6, angle = 30 ),
        axis.text.y = element_text(face = "bold", color = "black", size = 10, angle = 45))


###Plot of Drug-related death mean, Regardless of Sex, Age, or Year, per 100k people
ggplot(MeanNoAgeSexDeath, aes(cause, `Mean Drug Deaths Per 100k`, fill = cause)) + 
  geom_bar(stat = "identity") +
  labs(title = "Mean Drug Death Rate, Regardless of Sex, Age, or Year, per 100k people") +
  theme(axis.text.x = element_text(face = "bold", size  = 6, angle = 30),
        axis.text.y = element_text(face = "bold", color = "black", size = 10, angle = 30))



###All drug line graph over time

All.yearly.line <- AlcCanWithFactors %>%
  filter(measure == "Incidence") %>%
  group_by(year, cause) %>%
  summarise('Mean Estimated Number of People with a SUD Per 100k' = mean(estimate))

ggplot(All.yearly.line) +
  geom_line(aes(year, `Mean Estimated Number of People with a SUD Per 100k`, color = cause)) +
  labs(title = "Global Mean Rate of Substance-Use Disorder Incidences by Year, \nRegardless of Age or Sex, per 100k people", 
       subtitle = "Based on estimates from the GBD") +
  theme(axis.text.x = element_text(face = "bold", size  = 12),
        axis.text.y = element_text(face = "bold", color = "black", size = 10))

All.yearlyD.line <- AlcCanWithFactors %>%
  filter(measure == "Deaths") %>%
  group_by(year, cause) %>%
  summarise('Mean Estimated Number of People with a SUD Per 100k' = mean(estimate))

ggplot(All.yearlyD.line) +
  geom_line(aes(year, `Mean Estimated Number of People with a SUD Per 100k`, color = cause)) 

Specific.Alc.Line <- AlcDrugData %>%
  filter(measure == "Incidence") %>%
  group_by(year, location) %>%
  filter(location == "Mongolia" | location == "Russian Federation") %>%
  summarise('Mean AUD Per 100k People' = mean(estimate))

ggplot(Specific.Alc.Line) +
  geom_line(size = 2, aes(year, `Mean AUD Per 100k People`, color = location)) +
  labs(title = "Mean Rate of Alcohol-Use Disorder Incidences by Year, \nRegardless of Age or Sex, per 100k people in Russia and Mongolia", 
       subtitle = "Based on estimates from the GBD") +
  theme(axis.text.x = element_text(face = "bold", size  = 12),
        axis.text.y = element_text(face = "bold", color = "black", size = 10)) +
  scale_color_manual(values = my_colors2)

######################################################################################################
###Age differences in Alcohol-use disorders

AgeDiff.Inc.Alc <- AlcDrugData %>%
  filter(measure == "Incidence") %>%
  group_by(age) %>%
  summarise('Mean Alcohol-Use Disorder Incidence Rates Per 100k' = mean(estimate))

ggplot(AgeDiff.Inc.Alc, aes(age, `Mean Alcohol-Use Disorder Incidence Rates Per 100k`, fill = age)) + 
  geom_bar(stat = "identity") +
  labs(title = "Global Mean Rate of Alcohol-Use Disorders Incidences by Age Group, \nRegardless of Year or Sex, per 100k people", 
       subtitle = "Based on estimates from the GBD") +
  theme(axis.text.x = element_text(face = "bold", size  = 8, angle = 30),
        axis.text.y = element_text(face = "bold", color = "black", size = 12, angle = 30)) +
  scale_fill_manual(values = c("tan", "lightsalmon1", "plum1", "maroon2", "midnightblue", "dodgerblue2", "aquamarine1", "chartreuse1", "gold1"))

AgeDiff.Dea.Alc <- AlcDrugData %>%
  filter(measure == "Deaths") %>%
  group_by(age) %>%
  summarise('Mean Alcohol-Use Disorder Deaths Rate Per 100k' = mean(estimate))

ggplot(AgeDiff.Dea.Alc, aes(age, `Mean Alcohol-Use Disorder Deaths Rate Per 100k` , fill = age)) + 
  geom_bar(stat = "identity") +
  labs(title = "Global Mean Rate of Alcohol-Use Disorders Deaths by Age Group, \nRegardless of Year or Sex, per 100k people", 
       subtitle = "Based on estimates from the GBD") +
  theme(axis.text.x = element_text(face = "bold", size  = 8, angle = 30),
        axis.text.y = element_text(face = "bold", color = "black", size = 12, angle = 30)) +
  scale_fill_manual(values = c("tan", "lightsalmon1", "plum1", "maroon2", "midnightblue", "dodgerblue2", "aquamarine1", "chartreuse1", "gold1"))

######################################################################################################
###Maps of Alcohol Use

AlcDrugDataMap <- AlcCanWithFactors %>%
  filter(cause == "Alcohol use disorders")

AlcDrugDataMap$estimate <- round(AlcDrugDataMap$estimate)

AlcDrugDataMap <- left_join(AlcDrugDataMap, Iso_code_countries, by=c('location' = 'location_name'))

AlcDrugDataMap <- left_join(world, AlcDrugDataMap, by = c("iso_a3" = "ISO3")) %>%
  select(measure, admin, iso_a3, year, cause, estimate, geometry) %>%
  arrange(measure, admin, year, cause)

AlcDrugDataMap <- AlcDrugDataMap %>%
  filter(measure == "Incidence")


###1999

AlcDrugDataMap.1999 <- AlcDrugDataMap %>%
  filter(year == "1999")

AlcDrugDataMap.1999 %>%
  mutate(text = paste("<b>", admin, "</b>\n", year, "</b>\n", "AUD Incidence Rate (Per 100k): ", estimate)) %>%
  ggplot() +
  geom_sf(aes(fill = estimate, text = text), color = "black") +
  scale_fill_continuous(low = "blue", high = "red") +
  my_map_theme() +
  labs(title = "Mean Rate of People with Alcohol-Use Disorders,\nRegardless of Sex and Age, Across the World in 1999\n Per 100k",
       subtitle = "Based on estimates from the GBD")

###2009

AlcDrugDataMap.2009 <- AlcDrugDataMap %>%
  filter(year == "2009")
  
AlcDrugDataMap.2009 %>%
  mutate(text = paste("<b>", admin, "</b>\n", year, "</b>\n", "AUD Incidence Rate (Per 100k): ", estimate)) %>%
  ggplot() +
  geom_sf(aes(fill = estimate, text = text), color = "black") +
  scale_fill_continuous(low="blue", high="red") +
  my_map_theme() +
  labs(title = "Mean Rate of People with Alcohol-Use Disorders,\nRegardless of Sex and Age, Across the World in 2009\nPer 100k People",
       subtitle = "Based on estimates from the GBD")

###2019
AlcDrugDataMap.2019 <- AlcDrugDataMap %>%
  filter(year == "2019")

AlcDrugDataMap.2019 %>%
  mutate(text = paste("<b>", admin, "</b>\n", year, "</b>\n", "AUD Incidence Rate (Per 100k): ", estimate)) %>%
  ggplot() +
  geom_sf(aes(fill = estimate, text = text), color = "black") +
  scale_fill_continuous(low = "blue", high = "red") +
  my_map_theme() + 
  labs(title = "Mean Rate of People with Alcohol-Use Disorders,\nRegardless of Sex and Age, Across the World in 2019\nPer 100k People",
       subtitle = "Based on estimates from the GBD")


######################################################################################################
DrugDataSetFull <- DrugDataSetFull %>%
  arrange(desc(measure), location, year, age, cause)

DrugDataSetFull <- left_join(DrugDataSetFull, Iso_code_countries, by=c('location' = 'location_name'))

DrugDataSetFullMapData <- left_join(world, DrugDataSetFull, by = c("iso_a3" = "ISO3")) %>%
  select(measure, admin, iso_a3, year, sex, age, cause, val, geometry)

DrugDataSetFull <- DrugDataSetFull %>%
  arrange(measure, admin, year, desc(age), cause)

DrugDataSetFull %>%
  group_by(cause) %>%
  filter(sex == "Male") %>%
  summarise(mean(estimate))

############

AlcCanWithFactors <- read_excel("AgeStandardized_AlcoholCannabis.xlsx")
political_regime <- read_csv("political-regime.csv")
civil_liberties <- read_csv("civil-liberties-fh.csv")

world <- ne_countries(scale = "medium", returnclass = "sf") 

AlcCanWithFactors <- AlcCanWithFactors[,-1]
AlcCanWithFactors <- AlcCanWithFactors[,-13]

AlcCanWithFactors <- AlcCanWithFactors %>%
  distinct()

political_regime <- political_regime %>%
  filter(Year >= 1999)

civil_liberties <- civil_liberties %>%
  filter(Year >= 1999) %>%
  select(Code, Year, civlibs_fh)

AlcCanWithFactors <- left_join(AlcCanWithFactors, political_regime, by=c('ISO3' = 'Code', 'year' = 'Year'))

AlcCanWithFactors <- AlcCanWithFactors %>%
  mutate(regime_row_owid = factor(regime_row_owid, levels = c(0, 1, 2, 3), 
                                  labels = c("Closed Autocracy",
                                             "Electoral Autocracy",
                                             "Electoral Democracy", 
                                             "Liberal Democracy")))

AlcCanWithFactors$Religion <-
  recode(AlcCanWithFactors$Religion, Muslim = "Islam")

AlcCanWithFactors$Religion <- 
  recode(AlcCanWithFactors$Religion, Hinuism = "Hinduism")

AlcCanWithFactors$Religion <- 
  recode(AlcCanWithFactors$Religion, Hindu = "Hinduism")

AlcCanWithFactors$Religion <- 
  recode(AlcCanWithFactors$Religion, 'Roman Catholic' = "Christianity")

AlcCanWithFactors$Religion <- 
  recode(AlcCanWithFactors$Religion, None = "NA")

AlcCanWithFactors <- left_join(AlcCanWithFactors, civil_liberties, by=c('ISO3' = 'Code', 'year' = 'Year'))

AlcCanWithFactors <- AlcCanWithFactors %>%
  select(measure, location, year, sex, cause, ISO3, val, Religion, DrinkingAge, `GDP Measure`, regime_row_owid, civlibs_fh) %>%
  arrange(desc(measure), location, ISO3, year, sex, cause, val)

#######

########

DrugDataSetFull <- DrugDataSetFull %>%
  mutate(civlibs_fh = ifelse(civlibs_fh %in% 1:2, "High Liberties", civlibs_fh),
         civlibs_fh = ifelse(civlibs_fh %in% 3:5, "Moderate Liberties", civlibs_fh),
         civlibs_fh = ifelse(civlibs_fh %in% 6:7, "Low Liberties", civlibs_fh))

DrugDataSetFull <- DrugDataSetFull %>%
  mutate(`GDP Measure` = ifelse(`GDP Measure` %in% "NA", NA, `GDP Measure`))

DrugDataSetFull$`GDP Measure` <- as.numeric(DrugDataSetFull$`GDP Measure`)

DrugDataSetFull <- DrugDataSetFull %>%
  mutate(Religion = ifelse(Religion %in% c("Buddhism", "Hinduism", "Judaism", "Shintoism", "Folk", "NA"), "Other", Religion))


saveRDS(DrugDataSetFull, "DrugDataSetFull.rds")


#####

GDPPerCapita <- read_csv("API_NY.GDP.PCAP.CD_DS2_en_csv_v2_5454904.csv", skip = 3)
PovertyData <- read_csv("PovertyData.csv", skip = 3)
UnemploymentRate <- read_csv("unemployment-rate.csv")


GDPPerCapita <- GDPPerCapita %>%
  pivot_longer(cols = c("1960", "1961", "1962", "1963", "1964", "1965", "1966", "1967", "1968", "1969", "1970", "1971", "1972", "1973", "1974", "1975", "1976", "1977", "1978", "1979", "1980", "1981", "1982", "1983", "1984", "1985", "1986", "1987", "1988", "1989", "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022"),
               names_to = "Year",
               values_to = "GDPPerCapita")


GDPPerCapita$Year <- as.numeric(GDPPerCapita$Year)

str(GDPPerCapita)

GDPPerCapita <- GDPPerCapita %>%
  filter(Year >= 1999)

GDPPerCapita <- GDPPerCapita[, -c(1, 3, 4, 5)]

DrugDataSetFull <- DrugDataSetFull[, -c(10)]

DrugDataSetFull <- left_join(DrugDataSetFull, GDPPerCapita, by=c('ISO3' = 'Country Code', 'year' = 'Year'))

PovertyData <- PovertyData %>%
  pivot_longer(cols = c("1960", "1961", "1962", "1963", "1964", "1965", "1966", "1967", "1968", "1969", "1970", "1971", "1972", "1973", "1974", "1975", "1976", "1977", "1978", "1979", "1980", "1981", "1982", "1983", "1984", "1985", "1986", "1987", "1988", "1989", "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022"),
               names_to = "Year",
               values_to = "PovertyProportion")

PovertyData$Year <- as.numeric(PovertyData$Year)

PovertyData <- PovertyData %>%
  filter(Year >= 1999)

PovertyData <- PovertyData[, -c(1, 3, 4, 5)]

DrugDataSetFull <- left_join(DrugDataSetFull, PovertyData, by=c('ISO3' = 'Country Code', 'year' = 'Year'))

UnemploymentRate <- UnemploymentRate[, -c(1)]


DrugDataSetFull <- left_join(DrugDataSetFull, UnemploymentRate, by=c('ISO3' = 'Code', 'year' = 'Year'))


saveRDS(DrugDataSetFull, "DrugDataSetFull.rds")

#####

BeerConsumptionRate<- read_csv("beer-as-share-alcohol-consumption.csv")
SpiritConsumptionRate <- read_csv("spirits-as-share-total-alcohol-consumption.csv")
WineConsumptionRate <- read_csv("wine-as-share-alcohol-consumption.csv")


BeerConsumptionRate <- BeerConsumptionRate[, -c(1)]
BeerConsumptionRate <- rename(BeerConsumptionRate, BeerRate = `Indicator:Alcohol, consumption of pure alcohol by type of beverage (%) - Beverage Types:Beer`)

SpiritConsumptionRate <- SpiritConsumptionRate[, -c(1)]
SpiritConsumptionRate <- rename(SpiritConsumptionRate, SpiritRate = `Indicator:Alcohol, consumption of pure alcohol by type of beverage (%) - Beverage Types:Spirits`)

WineConsumptionRate <- WineConsumptionRate[, -c(1)]
WineConsumptionRate <- rename(WineConsumptionRate, WineRate = `Indicator:Alcohol, consumption of pure alcohol by type of beverage (%) - Beverage Types:Wine`)

DrugDataSetFull <- left_join(DrugDataSetFull, BeerConsumptionRate, by=c('ISO3' = 'Code', 'year' = 'Year'))

DrugDataSetFull <- left_join(DrugDataSetFull, SpiritConsumptionRate, by=c('ISO3' = 'Code', 'year' = 'Year'))

DrugDataSetFull <- left_join(DrugDataSetFull, WineConsumptionRate, by=c('ISO3' = 'Code', 'year' = 'Year'))

saveRDS(DrugDataSetFull, "DrugDataSetFull.rds")


#####
beer_consumption_per_person <- read_csv("beer-consumption-per-person.csv")
spirits_consumption_per_person <- read_csv("spirits-consumption-per-person.csv")
wine_consumption_per_person <- read_csv("wine-consumption-per-person.csv")
share_with_alcohol_use_disorders_receiving_treatment <- read_csv("share-with-alcohol-use-disorders-receiving-treatment.csv")


beer_consumption_per_person$AvgBeerConsump.L <- beer_consumption_per_person$`Indicator:Alcohol, recorded per capita (15+) consumption (in litres of pure alcohol) - Beverage Types:Beer`
beer_consumption_per_person <- beer_consumption_per_person[, -c(1, 4)]
beer_consumption_per_person <- beer_consumption_per_person %>%
  filter(Year >= "1999")

spirits_consumption_per_person$AvgSpiritConsump.L <- spirits_consumption_per_person$`Indicator:Alcohol, recorded per capita (15+) consumption (in litres of pure alcohol) - Beverage Types:Spirits`
spirits_consumption_per_person <- spirits_consumption_per_person[, -c(1, 4)]
spirits_consumption_per_person <- spirits_consumption_per_person %>%
  filter(Year >= "1999")

wine_consumption_per_person$AvgWineConsump.L <- wine_consumption_per_person$`Indicator:Alcohol, recorded per capita (15+) consumption (in litres of pure alcohol) - Beverage Types:Wine`
wine_consumption_per_person <- wine_consumption_per_person[, -c(1, 4)]
wine_consumption_per_person <- wine_consumption_per_person %>%
  filter(Year >= "1999")

share_with_alcohol_use_disorders_receiving_treatment <- rename(share_with_alcohol_use_disorders_receiving_treatment, PropAUDTreatment.2008 = `Indicator:Persons with alcohol use disorders receiving treatment (%)`)
share_with_alcohol_use_disorders_receiving_treatment <- share_with_alcohol_use_disorders_receiving_treatment[, -c(1)]


DrugDataSetFull <- left_join(DrugDataSetFull, beer_consumption_per_person, by=c('ISO3' = 'Code', 'year' = 'Year'))
DrugDataSetFull <- left_join(DrugDataSetFull, spirits_consumption_per_person, by=c('ISO3' = 'Code', 'year' = 'Year'))
DrugDataSetFull <- left_join(DrugDataSetFull, wine_consumption_per_person, by=c('ISO3' = 'Code', 'year' = 'Year'))
DrugDataSetFull <- left_join(DrugDataSetFull, share_with_alcohol_use_disorders_receiving_treatment, by=c('ISO3' = 'Code', 'year' = 'Year'))



DrugDataSetFull <- DrugDataSetFull[, -c(15, 16, 17)]

DrugDataSetFull <- rename(DrugDataSetFull, DrinkingAge.2016 = DrinkingAge)


saveRDS(DrugDataSetFull, "DrugDataSetFull.rds")


#####

DrugDataSetFull <- DrugDataSetFull[, -c(19)]
DrugDataSetFull <- rename(DrugDataSetFull, AgeToDrink.2016 = Group)


#####

colnames(DrugDataSetFull)[13] = "Unemployment"

Unemployment.Data <- read_csv("API_SL.UEM.TOTL.ZS_DS2_en_csv_v2_5454833.csv", skip = 3)

Unemployment.Data <- Unemployment.Data %>%
  pivot_longer(cols = c("1960", "1961", "1962", "1963", "1964", "1965", "1966", "1967", "1968", "1969", "1970", "1971", "1972", "1973", "1974", "1975", "1976", "1977", "1978", "1979", "1980", "1981", "1982", "1983", "1984", "1985", "1986", "1987", "1988", "1989", "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022"),
               names_to = "Year",
               values_to = "Unemployment")

Unemployment.Data$Year <- as.numeric(Unemployment.Data$Year)

Unemployment.Data <- Unemployment.Data %>%
  filter(Year >= 1999)

Unemployment.Data <- Unemployment.Data %>%
  filter(Year <= 2019) %>%
  select(`Country Code`, Year, Unemployment)

DrugDataSetFull <- DrugDataSetFull[, -13]

DrugDataSetFull <- left_join(DrugDataSetFull, Unemployment.Data, by=c('ISO3' = 'Country Code', 'year' = 'Year'))

saveRDS(DrugDataSetFull, "DrugDataSetFull.rds")
