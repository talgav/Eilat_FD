#________________________Eilat data wrangling including Oct 2020_________________________________________

#load library:
library(tidyverse)
library(ggmap)
#________________________________import (2020) data________________________________________________

#import new (2020) data:
oct_2020_data = read.csv("C:\\Users\\TalG\\Documents\\PhD\\Dark diversity\\Script\\attachments\\Eilat Transects October 2020 RAW.csv")
unique(oct_2020_data$Notes)

oct_2020_data <- Eilat_Transects_October_2020_RAW
#import old (2018-june2020) data :
#NOTE - this data has been through some processing before - lat + lon coordinatees have been manually added,
#and depth category calculated. Further down the code, you'll see that depth category variable is removed,
#so that the calculation will be identical for all data - 2018-2020


previous_data = read.csv("C:\\Users\\TalG\\Documents\\PhD\\Dark diversity\\Script\\attachments\\transects_data_for_analysis_including_June_2020.csv")
previous_data<- transects_data_for_analysis_including_June_2020

#create species list
oct_2020_sp = c(unique(oct_2020_data$Species))
sp_2018_2020 = c(unique(previous_data$Species))
list_of_sp_for_wrangling = union(oct_2020_sp,sp_2018_2020)

#export this list to create a file with all categories needed for future analyses:

#write.csv(list_of_sp_for_wrangling,"C:/Users/Maila/Google Drive/PhD/data/adults/Red_sea/uncategorized_sp_list.csv")

#within this file, I MANUALLY categorized the species. I also changed the following sp. names - 
#1. Zebrasoma veliferum -> zebrasoma desjardini 
#2. Coryphopterus neophytus -> Fusigobios neophytus
#3. Amblygobius hectori -> Koumansetta hectori
#4. Pomacentrus trichourus -> Pomacentrus trichrourus
#Since there's a lot of species-related wrangling, I am changing these names in both data 
#already at this point:


oct_2020_data$Species[oct_2020_data$Species == "Zebrasoma veliferum"] = "Zebrasoma desjardinii"
oct_2020_data$Species[oct_2020_data$Species == "Coryphopterus neophytus"] = "Fusigobios neophytus"
oct_2020_data$Species[oct_2020_data$Species == "Amblygobius hectori"] = "Koumansetta hectori"
oct_2020_data$Species[oct_2020_data$Species == "Pomacentrus trichourus"] = "Pomacentrus trichrourus"   

previous_data$Species[previous_data$Species == "Zebrasoma veliferum"] = "Zebrasoma desjardinii"
previous_data$Species[previous_data$Species == "Coryphopterus neophytus"] = "Fusigobios neophytus"
previous_data$Species[previous_data$Species == "Amblygobius hectori"] = "Koumansetta hectori"
previous_data$Species[previous_data$Species == "Pomacentrus trichourus"] = "Pomacentrus trichrourus"   

#there are still "sp" or "spp" observations within this data. for my (Mai) purposes, I will remove them
#further ahead, but maybe they'll be useful for Noy or Tal. 

#Import the file:
sp_traits = read.csv("C:\\Users\\TalG\\Documents\\PhD\\Dark diversity\\Script\\attachments\\categorized_sp_list.csv")

#___________________Data manipulations - wrangling of new data (oct 2020)___________________________

#Species names:

#change first letter to uppercase
oct_2020_data$Species = as.character(oct_2020_data$Species)
oct_2020_data$Species = paste0(toupper(substr(oct_2020_data$Species, 1, 1)), substr(oct_2020_data$Species, 2, nchar(oct_2020_data$Species)))

#correcting cryptic & transient species:
#apparently, some transient species were recorded in cryptic transects. Fixing that:
cryptic = oct_2020_data %>% filter(Letter == "C")
transient_sp = c(sp_traits[sp_traits$transient == 1,1])#only use transient species names 
cryptic = cryptic[!cryptic$Species %in% transient_sp,]

#Now, transient sp. 
transient = oct_2020_data %>% filter(Letter == "T")
cryptic_sp = c(sp_traits[sp_traits$cryptic == 1,1])#only use transient species names 
transient = transient[!transient$Species %in% cryptic_sp,]


#bind transient and cryptic data back
oct_2020_data = rbind.data.frame(cryptic,transient)


#Check for differences in variables before creating an ID for each transect
#the variables that will be used to create an ID are:
#1. SiteNo,Site,Transect,First.Observer,Second.Observer,month,year
unique(oct_2020_data$Location)
unique(oct_2020_data$Site)
unique(oct_2020_data$First.Observer.x)
unique(oct_2020_data$Second.Observer.x)

#Need to change Hava's name since we have 2 versions of Hava :
oct_2020_data$Second.Observer.x[oct_2020_data$Second.Observer.x == "Have Wandel"] = "Hava Wandel" 
unique(oct_2020_data$Date)
unique(oct_2020_data$Transect)

#some transects values are missing. I examined the sheets to see why, this is the correction
oct_2020_data$Transect[oct_2020_data$Transect == "" & 
                         oct_2020_data$meta_to_deployment_id == "Tal Perevolotsky and Yamit Romano - 1"] = "B"

oct_2020_data$Transect[oct_2020_data$Transect == "" & 
                         oct_2020_data$meta_to_deployment_id == "Shevy Rothman and Yamit Romano - 2"] = "B"

oct_2020_data$Transect[oct_2020_data$Transect == "" & 
                         oct_2020_data$meta_to_deployment_id == "Tal Perevolotsky and Sarah Ohayon - 1"] = "B"

oct_2020_data$Transect[oct_2020_data$Transect == "" & 
                         oct_2020_data$meta_to_deployment_id == "Shevy Rothman and Hava Wandel - 1"] = "B"

#check the artificial structures values:
unique(oct_2020_data$A1_length)
unique(oct_2020_data$A2_length)
unique(oct_2020_data$A3_length)
#there's only A1 data (only one artificial structure per transect)
# I will remove only A2+A3
#check the knoll columns:
unique(oct_2020_data$K1_height)
unique(oct_2020_data$K2_height)
unique(oct_2020_data$K3_height)
#I have only 2 knolls - i will remove the other columns ahead


#_________________________________organize columns in the new data (oct 2020)_________________________________________# :

#Good to go
oct_2020_data$First.Observer.x = as.character(oct_2020_data$First.Observer.x)
oct_2020_data$Second.Observer.x = as.character(oct_2020_data$Second.Observer.x)

#It seems that there is 2 identical columns in october DF: Site, SiteID/
#let's check it out (True = identical):
identical(oct_2020_data[['SiteID']],oct_2020_data[['Site']])
#So the 2 columns are identical. To fit the previous data i will delete - SiteID

#examine the "Notes" variable - are there any problematic transects?
unique(oct_2020_data$Notes)
#It seems that all notes describe either:
#1. a joint transect for transient and cryptic (this was conducted when there was a relatively 
#"empty" transect, usually on sandy/gravel habitats)
#2. the habitat (I am leaving all habitats at this point, maybe will be of use for someone)

oct_2020 = oct_2020_data %>% 
  #Remove unnecessary columns
  select(-c("Country","Project","Expedition","meta_to_deployment_id",
            "First.Observer.y","Second.Observer.y","Photos","Logger",
            "Notes","SiteID","A2_height","A2_width",
            "A2_length","A3_height","A3_width","A3_length","K3_height","K3_width",
            "K3_length"
            ))%>% 
  #Rename columns
  rename(SiteNo = Location,
         Trip.date = Date,
         Number_Ind = Amount,
         lat = Latitude,
         lon = Longitude,
         )%>%

#Create new, needed variables
  mutate(month = "october",
         year = "2020",
         Trans_No = paste(Transect,Letter),
         observer = ifelse(Observer == "first", 
                           First.Observer.x, 
                           Second.Observer.x),
         trans_ID = paste(SiteNo,Site,Transect,First.Observer.x,Second.Observer.x,month,year),
         ID = paste(SiteNo,Site,observer,month,year)
         ) %>%
  #remove the observer column (not its name, but whether first/second) and create a column with the number of observers
  select(-c("Observer")) %>%
  group_by(trans_ID) %>%
  mutate(observer_num = ifelse(n_distinct(observer) > 1,
                               "two_observers",
                               "one_observer"),
         Depth.m. = mean(c(Depth.End,Depth.Start),na.rm = T)) %>% # Calculate the mean depth between start and end points of the transect
  ungroup() %>%
  select(-c("Depth.End","Depth.Start")) %>% #Remove original depths columns, after mean depth calculations
  filter(Confidence < 2)#keep observations with confidence < 2 (identification is very problematic in these cases)

#create new column for SW & A,K size:
oct_2020$A1_size = oct_2020$A1_height *oct_2020$A1_length *oct_2020$A1_width
oct_2020$K1_size = oct_2020$K1_height*oct_2020$K1_width*oct_2020$K1_length
oct_2020$K2_size = oct_2020$K2_height*oct_2020$K2_width*oct_2020$K2_length
#check out the new columns:
unique(oct_2020$A1_size)
unique(oct_2020$K1_size)
unique(oct_2020$K2_size)
unique(oct_2020$Artificial_No)

#in 2020 there are no data on which observations belong to artificial structures, but in the previous 
#data there are several rows that do, so I will keep this column 

#percent cover calculation:
#select relevant columns
per_cover = oct_2020[,c(58,12:36)]
#there are 2 columns for s( S,s), unite:
per_cover[per_cover == "s"] = "S"  

#count how many times a letter appears in the selected columns:
per_cover$S <- apply(per_cover, 1, function(x) length(which(x=="S")))
per_cover$SG <- apply(per_cover, 1, function(x) length(which(x=="SG")))
per_cover$C <- apply(per_cover, 1, function(x) length(which(x=="C")))
per_cover$G <- apply(per_cover, 1, function(x) length(which(x=="G")))
per_cover$B <- apply(per_cover, 1, function(x) length(which(x=="B")))
per_cover$R <- apply(per_cover, 1, function(x) length(which(x=="R")))
per_cover$A <- apply(per_cover, 1, function(x) length(which(x=="A")))
per_cover$SW = NA

per_cover = per_cover %>% mutate(S = (S/25) * 100,
                                 SG = (SG/25) * 100,
                                 C = (C/25) * 100,
                                 G = (G/25) * 100,
                                 B = (B/25) * 100,
                                 R = (R/25) * 100,
                                 A = (A/25) *100)


per_cover = per_cover %>% rowwise() %>% mutate(sum_check = sum(S,SG,C,G,B,R,A))

per_cover = per_cover %>% select(c(S,SG,C,G,B,R,A,SW))

#bind with data
oct_2020_with_cov = cbind.data.frame(oct_2020,per_cover)

#remove the last unnecessary columns from the new data:
oct_2020_with_cov = oct_2020_with_cov %>% select(-c("A1_height","A1_width","A1_length",
                     "K1_height","K1_width","K1_length","K2_height",
                     "K2_width","K2_length","First.Observer.x","Second.Observer.x",
                     "Dolphin.crossing."))

#remove all columns from which per. cover was calculated: 
colnames(oct_2020_with_cov)
oct_2020_with_cov = oct_2020_with_cov %>% select(-contains("X"))

#______________________________Orgenaize the previous data____________________________________:

#remove unnecessary column from previous data 
previous_data = previous_data %>%
  select(-c("X")) 
  
#----Previous data additions----#
previous_data$K1_size = NA
previous_data$K2_size = NA
previous_data$Description = NA 

#Select final columns and reorder
#Remember to select also artificial no from the correct data that Mai will sent me(01/12/2020)
previous = previous_data %>% select(SiteNo,Trip.date,year,month,Site,Transect,Letter,Trans_No,trans_ID,ID,observer,observer_num,lat,lon,Depth.m.,Species,Confidence,Number_Ind,Length,C,R,G,S,SG,SW,A,B,A1_size,K1_size,K2_size,Description)
new = oct_2020_with_cov %>% select(SiteNo,Trip.date,year,month,Site,Transect,Letter,Trans_No,trans_ID,ID,observer,observer_num,lat,lon,Depth.m.,Species,Confidence,Number_Ind,Length,C,R,G,S,SG,SW,A,B,A1_size,K1_size,K2_size,Description)

#some more validation of the previous data (which may have been missed in the previous cleaning code)
#Species names:
 
#change first letter to uppercase
previous$Species = as.character(previous$Species)
previous$Species = paste0(toupper(substr(previous$Species, 1, 1)), substr(previous$Species, 2, nchar(previous)))
 
#correcting cryptic & transient species:
#apparently, some transient species were recorded in cryptic transects. Fixing that:
cryptic = previous %>% filter(Letter == "C")
transient_sp = c(sp_traits[sp_traits$transient == 1,1])#only use transient species names 
cryptic = cryptic[!cryptic$Species %in% transient_sp,]
 
#Now, transient sp. 
transient = previous %>% filter(Letter == "T")
cryptic_sp = c(sp_traits[sp_traits$cryptic == 1,1])#only use transient species names 
transient = transient[!transient$Species %in% cryptic_sp,]
 
 
#bind transient and cryptic data back
previous = rbind.data.frame(cryptic,transient)
 
 
#_______________________________Bind old and new data_____________________________________

eilat_data = rbind(previous,new)

#validate species names

#Using Tal's code, update sp. names according to fishbase:
library(rfishbase)
valid_name <-validate_names(unique(eilat_data$Species))
species = data.frame(Species = unique(eilat_data$Species))
species$valid<-ifelse(species$Species %in% valid_name,"valid","not valid")

species_v<-species %>% filter(species$valid == "not valid")
species_v<-species_v %>% filter(str_detect(species_v$Species,"spp")==F)
species_v<-species_v %>% filter(str_detect(species_v$Species,"sp.")==F)
species_v<-species_v %>% filter(str_detect(species_v$Species,"No Fish")==F)

new_name <- species_v$Species %>% synonyms() %>%
  select(provided_name = synonym, valid_name = Species, Comment = Status) %>%
  filter(Comment == "synonym") %>% rename(Species = provided_name)

eilat_data_new<-left_join(eilat_data,new_name,by="Species")

eilat_data_new$Species<-ifelse(!is.na(eilat_data_new$valid_name),
                               eilat_data_new$valid_name,
                               eilat_data_new$Species)

eilat_data <- eilat_data_new %>% select(-c(valid_name))

#sites names validation
sort(unique(as.character(eilat_data$SiteNo)))
eilat_data$SiteNo = as.character(eilat_data$SiteNo)
eilat_data$SiteNo[eilat_data$SiteNo == "dekel"] = "Dekel"
eilat_data$SiteNo[eilat_data$SiteNo == "iui"] = "IUI"
eilat_data$SiteNo[eilat_data$SiteNo == "princess"] = "Princess"
eilat_data$SiteNo[eilat_data$SiteNo == "north_beach"] = "North_beach"
eilat_data$SiteNo[eilat_data$SiteNo == "katza"] = "Katza"
eilat_data$SiteNo[eilat_data$SiteNo == "japanease_gardens"] = "Japanease_gardens"
eilat_data$SiteNo[eilat_data$SiteNo == "reserve"] = "Reserve"


#create depth category variable
eilat_data$depth_category = NA
eilat_data$depth_category<-ifelse(eilat_data$Depth.m.<=9,
                            "shallow",
                            ifelse(eilat_data$Depth.m.>=9.001 & eilat_data$Depth.m.<=17.4,
                                   "middle",
                                   "deep"))

#just to see the distribution of depths across sites:
depths_division = eilat_data %>% group_by(SiteNo, depth_category) %>% summarize(count = n_distinct(trans_ID))

#remove inexperienced observers:
unique(eilat_data$observer)
observers_to_remove = c("Rei Diga", "Yamit Romano", "Noy Shapira")
eilat_data = eilat_data %>% filter(!observer %in% observers_to_remove)
#remove Sarah's observations from June 2020
sara_june_2020 = eilat_data[eilat_data$observer == "Sarah Ohayon" & eilat_data$month == "june",]                  
sara_t_to_remove = unique(sara_june_2020$ID)
eilat_data = eilat_data %>% filter(!ID %in% sara_t_to_remove)

#fix (both) tal's family name:
eilat_data$observer[eilat_data$observer == "Tal Gavrieli"] = "Tal Gavriel"
eilat_data$observer[eilat_data$observer == "Tal Gavriely"] = "Tal Gavriel"

eilat_data$observer[eilat_data$observer == "Tal Perevoloski"] = "Tal Perevolotski"
eilat_data$observer[eilat_data$observer == "Tal Perevolotsky"] = "Tal Perevolotski"

#--Biomass--##
#Add biomass values for each observation:

#import LW ratios data, and fixing code from Hezi
uppercasefirstletter <- function(x){
  a <- toupper(substr(as.character(x),1,1))
  b <- substr(as.character(x),2,nchar(as.character(x)))
  return(paste(a,b,sep=""))
}

eliminatelastdot <- function(x){
  if (substr(x,nchar(x)-3,nchar(x)) == "spp.")
    return(x)
  else{
    if (substr(x,nchar(x),nchar(x)) == "."){
      nodot <- eliminatelastdot(substr(x,1,nchar(x)-1))
      return(nodot)
    }
    else
      return(x)
  }
}

eliminatelastspace <- function(x){
  if (substr(x,nchar(x),nchar(x)) == " "){
    nodot <- eliminatelastspace(substr(x,1,nchar(x)-1))
    return(nodot)
  }
  else
    return(x)
}

ab_data <- read.csv("C:\\Users\\TalG\\Documents\\PhD\\Dark diversity\\Script\\attachments\\ab_eilat_complete.csv",stringsAsFactors = F)
colnames(ab_data) <- c("Species","a","b","method","c_t")
data$Species <- sapply(data$Species, uppercasefirstletter)
data$Species <-  sapply(data$Species, eliminatelastspace)
ab_data$Species <- sapply(ab_data[,1], function(x) ifelse((substr(x,nchar(x)-3,nchar(x)) == "spp."),sub("[.]"," ",x),
                                                          gsub("\\.", " ", x)  ))
ab_data$Species <- sapply(ab_data$Species, eliminatelastspace)

#join LW data with observation data
eilat_data_with_biomass <-  left_join(eilat_data,ab_data,by = "Species")

#weight calculation according to l-w ratios
eilat_data_with_biomass$weight=(eilat_data_with_biomass$Number_Ind)*
  (eilat_data_with_biomass$a*(eilat_data_with_biomass$Length^eilat_data_with_biomass$b))

#for short names purposes:
data = eilat_data_with_biomass
#for unity, change Transect characters to numbers
data$Trans_No = str_squish(data$Trans_No)
data$Trans_No = ifelse(data$Trans_No == "C C", "3C", data$Trans_No)
data$Trans_No = ifelse(data$Trans_No == "C T", "3T", data$Trans_No)
data$Trans_No = ifelse(data$Trans_No == "A C", "1C", data$Trans_No)
data$Trans_No = ifelse(data$Trans_No == "B C", "2C", data$Trans_No)
data$Trans_No = ifelse(data$Trans_No == "D C", "4C", data$Trans_No)
data$Trans_No = ifelse(data$Trans_No == "A T", "1T", data$Trans_No)
data$Trans_No = ifelse(data$Trans_No == "B T", "2T", data$Trans_No)
data$Trans_No = ifelse(data$Trans_No == "D T", "4T", data$Trans_No)
data$Trans_No = ifelse(data$Trans_No == "1 T", "1T", data$Trans_No)

#summary
print(paste(n_distinct(data$trans_ID),"transects"))
print(paste(n_distinct(data$observer),"observers"))
print(paste(n_distinct(data$Species),"species"))
print(paste(n_distinct(data$SiteNo),"sites"))
print(paste(unique(data$SiteNo)))
data %>% group_by(depth_category) %>% summarize(n_distinct(trans_ID))




#--------SAVE FINAL DATA----------#:

write.csv(data,"transects_data_for_analyses_2018-2020.csv")

#_______________________________________Transect map__________________________________________

library(PKPDmisc)
library(stringr)
library(biogeo)
library(ggplot2)
library(ggmap)
library(geosphere)
library(plotly)
library(TSP)
library(leaflet)
library(tidyverse)

data<-read.csv("C:\\Users\\TalG\\Documents\\PhD\\Dark diversity\\Script\\transects_data_for_analyses_2018-2020.csv")

transects_map_oct2020 = get_stamenmap(bbox = c(left = 34.88, bottom = 29.42,right = 35.02, top = 29.56),zoom = 12,
                                     maptype ="terrain-background")

transects_map_oct2020<-ggmap(transects_map_oct2020)
#see map
transects_map_oct2020

#data for map was the data I used, which consisted of coordinates and other meta data information. you should use your data here :)
data_for_map = data[!is.na(data$lat),]
data_for_map = data[!is.na(data$lon),]

data_for_map<-data_for_map %>% select(2:16,34) %>% distinct(trans_ID,.keep_all = T)

#By year:
transects_map_oct2020 +
  geom_point(data = data_for_map, aes(x = lon, y = lat, color = year))+
  theme(legend.position = "bottom")

#By Depth:
transects_map_oct2020 +
  geom_point(data = data_for_map, aes(x = lon, y = lat, color = depth_category))+
  theme(legend.position = "bottom")



data_for_map$depth_category_color <- ifelse(data_for_map$depth_category=="deep",
                                            "blue",
                                            ifelse(data_for_map$depth_category == "middle",
                                                   "green","red"))
  
data_for_map<-data_for_map %>% mutate("month_color"= case_when(month == "july" ~ "green",
                                                               month == "may" ~ "blue",
                                                               month == "june" ~ "red",
                                                               month == "december" ~ "black",
                                                               month == "october" ~ "white"))



icons_depth <- awesomeIcons(icon = "whatever",
                      iconColor = "black",
                      library = "ion",
                      markerColor = data_for_map$depth_category_color)


icons_month <- awesomeIcons(icon = "whatever",
                            iconColor = "black",
                            library = "ion",
                            markerColor = data_for_map$month_color)

markerLegendHTML <- function(IconSet) {
  # container div:
  legendHtml <- "<div style='padding: 10px; padding-bottom: 10px;'><h4 style='padding-top:0; padding-bottom:10px; margin: 0;'> Marker Legend </h4>"
  
  n <- 1
  # add each icon for font-awesome icons icons:
  for (Icon in IconSet) {
    if (Icon[["library"]] == "fa") {
      legendHtml<- paste0(legendHtml, "<div style='width: auto; height: 45px'>",
                          "<div style='position: relative; display: inline-block; width: 36px; height: 45px' class='awesome-marker-icon-",Icon[["markerColor"]]," awesome-marker'>",
                          "<i style='margin-left: 8px; margin-top: 11px; 'class= 'fa fa-",Icon[["icon"]]," fa-inverse'></i>",
                          "</div>",
                          "<p style='position: relative; top: -20px; display: inline-block; ' >", names(IconSet)[n] ,"</p>",
                          "</div>")    
    }
    n<- n + 1
  }
  paste0(legendHtml, "</div>")
}


leaflet(data_for_map) %>% 
  addTiles() %>% 
  addAwesomeMarkers(lng = ~lon, lat = ~lat,icon = icons_depth,label  = ~as.character(month)) %>% 
  addControl(html = markerLegendHTML(IconSet = IconSet), position = "bottomleft")


leaflet(data_for_map) %>% 
  addTiles() %>% 
  addAwesomeMarkers(lng = ~lon, lat = ~lat,icon = icons_month,label  = ~as.character(depth_category)) 
