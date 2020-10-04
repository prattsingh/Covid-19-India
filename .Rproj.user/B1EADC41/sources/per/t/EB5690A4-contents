library(tidyverse)
library(readxl)
library(dslabs)
library(lubridate)
library(cowplot)

#cases_per_million data
dat <- read_csv("data/covid_india_aug.csv")
head(dat)

confirmed <- dat %>% select(Date , `State/UnionTerritory`, Confirmed ) %>% mutate(Date = dmy(Date) ) %>% 
  filter(Date == "2020-09-15") %>%
  group_by( `State/UnionTerritory` )%>% 
            summarise(confirmed = sum(Confirmed))
head(confirmed)
tail(ymd(dat$Date))

pop_raw <- read_csv("data/population_india_census2011.csv")
pop <- pop %>% select(`State / Union Territory` , Population)
names(confirmed)[1] <- "id"
names(pop)[1] <- "id"

confirmed <- confirmed %>% left_join(pop , by = "id")
confirmed <- confirmed %>% mutate(confirmed_per_million = (confirmed/Population)*10^6)
new_row <- data.frame("id" = c("Dadara & Nagar Havelli" , "Daman & Diu") ,
                      "confirmed" = c(confirmed$confirmed[8],confirmed$confirmed[8]),
                      "Population" = c(confirmed$Population[8] , confirmed$Population[8]),
                      "confirmed_per_million" = c(confirmed$confirmed_per_million[8] , confirmed$confirmed_per_million[8]))
confirmed <- confirmed %>% rbind(new_row) 
#-------------------------------------------------------------------------------------------------------------------------------
#map 
library(ggplot2)
library(maptools)
library(rgeos)
library(ggmap)
library(scales)
library(RColorBrewer)
library(rgdal)
shp <- readShapeSpatial('/home/pratyush/projects/map_india/data/Admin2.shp')

plot_conf <- confirmed %>% select(id , confirmed_per_million) %>%filter(!id == "Dadra and Nagar Haveli and Daman and Diu")
plot_conf <- plot_conf %>% mutate(id = str_replace(id ,"\\s+and\\s+" ," & ") ,
                                    id = str_replace(id ,"Delhi" , "NCT of Delhi") ,
                                  id = str_replace(id ,"Andaman & Nicobar Islands" , "Andaman & Nicobar Island"),
                                  id = str_replace(id ,"Telengana" , "Telangana"),
                                  id = str_replace(id ,"Arunachal Pradesh" , "Arunanchal Pradesh"))
shp.f <- fortify(shp, region = "ST_NM")

merge.shp.coef<-merge(shp.f,plot_conf, by="id", all.x=TRUE)
final.plot<-merge.shp.coef[order(merge.shp.coef$order), ]

save(confirmed , file = "rda/confirmed.rda")
save(plot_conf , file = "rda/plot_conf.rda")
save(final.plot , file = "rda/final.plot.rda")

#----------------------------------------------------------------------------------------------------------------------------
#Testing Data
dat2 <- read_csv("data/StatewiseTestingDetails.csv")
samples <- dat2 %>% select(Date ,State ,TotalSamples )%>% 
  filter(Date == "2020-09-14") %>%
  group_by( State )%>% 
  summarise(totalsamples = sum(TotalSamples))

names(samples)[1] <- "id"
samples.pop <- samples %>% mutate( id = str_replace(id, "Telangana" ,"Telengana")) %>%left_join(pop , by = "id")
new_row2 <- data.frame("id" = c("Andaman and Nicobar Islands" ,"Dadara & Nagar Havelli" ,"Daman & Diu") ,
                       "totalsamples" = c(dat2$TotalSamples[123] ,dat2$TotalSamples[1169] , dat2$TotalSamples[1169]),
                       "Population" = c(pop$Population[34] ,pop$Population[33] , pop$Population[33] ))
samples.pop <- samples.pop %>% rbind(new_row2)

samples.pop <- samples.pop %>% mutate(samples_per_million = (totalsamples/Population)*10^6)

samples.final <- samples.pop %>% mutate(id = str_replace(id ,"\\s+and\\s+" ," & ") ,
                                  id = str_replace(id ,"Delhi" , "NCT of Delhi") ,
                                  id = str_replace(id ,"Andaman & Nicobar Islands" , "Andaman & Nicobar Island"),
                                  id = str_replace(id ,"Telengana" , "Telangana"),
                                  id = str_replace(id ,"Arunachal Pradesh" , "Arunanchal Pradesh")) %>% select(id , samples_per_million)
merge.shp.coef<-merge(shp.f,samples.final, by="id", all.x=TRUE)
final.plot2<-merge.shp.coef[order(merge.shp.coef$order), ]

save(samples.final , file = "rda/samples.final.rda")
save(final.plot2 , file = "rda/final.plot2.rda")
#-----------------------------------------------------------------------------------------------------------------------------------------------------
#plot#3

combined <- samples.pop %>% select(id ,samples_per_million , Population  ) %>%  mutate(id = str_replace(id ,"\\s+and\\s+" ," & ") ,
                                                                                       id = str_replace(id ,"Delhi" , "NCT of Delhi") ,
                                                                                       id = str_replace(id ,"Andaman & Nicobar Islands" , "Andaman & Nicobar Island"),
                                                                                       id = str_replace(id ,"Telengana" , "Telangana"),
                                                                                       id = str_replace(id ,"Arunachal Pradesh" , "Arunanchal Pradesh")) %>%left_join(
                                                                                         plot_conf , by = "id")
pop.density <- pop_raw %>% select(`State / Union Territory` , Density)
pattern <- "^(\\d+),?(\\d+)*.?(\\d)*/km2.+$"
pop.den <- pop.density %>% mutate(Density = str_replace_all(Density , pattern , "\\1\\2.\\3"))
pop.den <- pop.den %>% mutate(Density = as.numeric(Density))
names(pop.den)[1] <- "id"
new_row3 <- data.frame("id" = c("Dadara & Nagar Havelli" , "Daman & Diu") ,
                       "Density" = c(pop.den$Density[8],pop.den$Density[8]))
pop.den <- pop.den %>% rbind(new_row3) %>% filter(!id == "Dadra and Nagar Haveli and Daman and Diu")
combined.final <- pop.den %>%   mutate(id = str_replace(id ,"\\s+and\\s+" ," & ") ,
                                       id = str_replace(id ,"Delhi" , "NCT of Delhi") ,
                                       id = str_replace(id ,"Andaman & Nicobar Islands" , "Andaman & Nicobar Island"),
                                       id = str_replace(id ,"Telengana" , "Telangana"),
                                       id = str_replace(id ,"Arunachal Pradesh" , "Arunanchal Pradesh")) %>% right_join(combined , by = "id")

save(combined.final ,file ="rda/combined.final.rda")
#----------------------------------------------------------------------------------------------------------------------------------------------------------------
#plot4

dat3_raw <- read_excel("data/india_rural_urban.xls")

dat3 <- dat3_raw[6:40,]
colnames(dat3) <- c(columns[1:3],columns2[4:7])
columns2 <- as.character(dat3_raw[3,])
names(dat3)[1:2] <- c("S.no" , "id")
names(dat3)[6:7] <- c("Rural%" , "Urban%")
rural_urban <- dat3 %>% select(id , `Rural%` , `Urban%` ) %>% mutate(`Rural%` = as.numeric(`Rural%`) ,
                                                                     `Urban%` = as.numeric(`Urban%`) ,
                                                                     rural_prop = `Rural%`/`Urban%`)

rural_urban <- rural_urban %>% mutate(id = str_replace_all(id , "#" , ""),
                                      id = str_to_title(id))
combined.rural <- rural_urban %>% select(id ,rural_prop)   %>% mutate(id = str_replace(id ,"\\s+and\\s+" ," & ") , 
                                                                      id = str_trim(id),
                                       id = str_replace(id ,"Nct Of Delhi" , "NCT of Delhi") ,
                                       id = str_replace(id ,"A & N Islands" , "Andaman & Nicobar Island"), 
                                       id = str_replace(id ,"Dadra & Nagar Haveli" ,"Dadara & Nagar Havelli"), 
                                      id = str_replace(id ,"Orissa" ,"Odisha"),
                                       id = str_replace(
                                         id ,"Arunachal Pradesh" , "Arunanchal Pradesh")) %>% right_join(
                                           combined , by = "id")
save(combined.rural , file = "rda/combined.rural.rda")
