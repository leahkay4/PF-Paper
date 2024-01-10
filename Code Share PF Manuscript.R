#################################
### Load Libraries & Packages ###
#################################
library_list <- c("readxl", "hydroTSM", "Metrics", "caret", "dplyr", "tidyr", "randomForest",
                  "tidyverse", "ellipse", "ggstatsplot", "rpart", "ROSE", "e1071", "ggsci",
                  "ggplot2", "gridExtra", "cluster", "ggridges", "ggplot2", "ggpubr",
                  "factoextra", "ggthemes", "ggfortify", "cluster", "patchwork", "cowplot",
                  "scales", "ggstance", "ggsignif", "car")

for (package_name in library_list) {
  library(package_name, character.only = TRUE)}

######################################
### Reading and Preparing the Data ###
######################################
Machine_Learning_Data <- na.omit(read_excel("G:/My Drive/1. Notibility/1. Lets Get This PhD/1. [Paper 1] Preferential Flow/Second Round Data Collection/Data/Completed/Machine Learning Data2.xlsx",
                                            sheet = "Master", col_types = c("text", "numeric", "text", "skip", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "numeric", "numeric", "numeric")))
# Rename values in the "Season" column
Machine_Learning_Data$Season[Machine_Learning_Data$Season == "Fall"] <- "autumn"
Machine_Learning_Data$Season[Machine_Learning_Data$Season == "Spring"] <- "spring"
Machine_Learning_Data$Season[Machine_Learning_Data$Season == "Summer"] <- "summer"
Machine_Learning_Data$Season[Machine_Learning_Data$Season == "Winter"] <- "winter"

Site <- c("Walker", "Chatham", "Jones", "Pasquotank", "St. Lawrence", "Tazewell", "Blaine", "Big Horn", 
          "Red Lake", "St. Croix", "Kalamazoo","Monmouth", "Hooker", "Clinton", "Duplin", "Garden",
          "Tioga", "Laurens", "Saline", "Catawba")

state <- c(", Texas", ", Georgia", ", North Carolina", ", North Carolina", ", New York", ", Illinois", ", Nebraska",
           ", Montana", ", Minnesota", ", Wisconsin", ", Michigan", ", New Jersey", ", Nebraska", ", Illinois", ", North Carolina", "Nebraska", "New York", "Georgia", "Missouri", "North Carolina")

long <- c(-95.716731, -81.27, -77.503333, -76.275278, -74.764444, -89.4726111, -100.100867, -107.601111, -96.276125,
          -92.491603, -85.477222, -74.48333, -101.337722, -89.441194, -78.020972, -102.444692, -76.2652, -83.0456, -93.3153, -81.1473)

lat <- c(30.575361, 31.997222, 34.97, 36.308333, 44.817778, 40.3226111, 41.831103, 45.837778, 47.7889, 45.180106, 42.321667,
         40.208056, 42.032728, 38.514389, 34.847778, 41.769283, 42.1376, 32.5075, 39.2060, 35.7711)

farmland <- c("No", "Yes", "Yes", "Yes", "No", "Yes", "No", "No", "No", "Yes", "No", "Yes", "Yes")

precip <- c(44, 48, 47.5, 50, 36.5, 56.5, 20, 13, 23, 32, 36, 37, 43.5, 53.5)
clay <- c(7.5, 7, 5, 18, 2, 22, 5, 21, 6, 15, 11, 12.5, 4, 22.5, 12.4, 4, 12, 5.3, 23.7, 12.5)
sand <- c(83.1, 86.4, 78.6, 27.3, 96, 4, 87, 41.6, 78, 14, 48, 67.9, 94, 24.8, 70.9, 93, 20.7, 85.7, 4.2, 67.9)
silt <- as.numeric(c(9.4,6.6,16.4,54.7,2,74,8,37.4,16,71,41,19.6,2,52.7,16.7,3,67.3,9,72.1,19.6))
slope <- c(3,1,4,1,5,3.5,1,1,4,1.5,0.5,7.5,6,1,1,6,1.5,1,1,8)
elevation <- c(300,230,90,15,1250,770,2955,7900,1115,1375,970,160,2985,680,180,2955,1395,250,880,800)

map <- data.frame(Site,long,lat,state,clay,sand,silt,slope,elevation)
map = map[map$Site != "Somerset" & map$Site != "Jackson" & map$Site != "Taos" & map$Site != "Eureka" & map$Site != "St. Joseph" & map$Site != "Scott" & map$Site != "Hampden" & map$Site != "Catawba",]
all <- merge(map,Machine_Learning_Data,by="Site")
all$claypercent <- ifelse(all$clay>0&all$clay<=5,all$claypercent <- "0-5% Clay",
                          ifelse(all$clay>5&all$clay<=10,all$claypercent <- "5-10% Clay",
                                 ifelse(all$clay>10&all$clay<=15,all$claypercent <- "10-15% Clay",
                                        ifelse(all$clay>15&all$clay<=30,all$claypercent <- "15-30% Clay", " "))))
all$claypercent <- factor(all$claypercent,levels=c("0-5% Clay","5-10% Clay","10-15% Clay","15-30% Clay"))


