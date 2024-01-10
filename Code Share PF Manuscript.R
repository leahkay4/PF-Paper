#################################
### Load Libraries & Packages ###
#################################
library_list <- c("readxl", "hydroTSM", "Metrics", "caret", "dplyr", "tidyr", 
                  "randomForest","tidyverse", "ellipse", "ggstatsplot", "rpart",
                  "ROSE", "e1071","ggsci","ggplot2", "gridExtra", "cluster", 
                  "ggridges", "ggplot2", "ggpubr","factoextra", "ggthemes", 
                  "ggfortify", "cluster", "patchwork", "cowplot","scales", 
                  "ggstance", "ggsignif", "car")

for (package_name in library_list) {
  library(package_name, character.only = TRUE)}

#################################
###### Datasets to Import #######
#################################

# Study site city, state, longitude, latitude, clay%, sand%, silt%, soil texture,
# slope, elevation, clay percentage range (ex: 0-5, 5-10, 10-15, or 15-30%).

# SMAP data with time (Keep only data flagged for successful retrievals). Linear
# interpolation was used to fill in missing values. 

# Upload HYDRUS-1D Obs_Node.OUT files (Hydrus-1D runs explained in publication)
# containing observed and estimated groundwater table levels.

# Record optimized soil hydraulic parameter values from HYDRUS-1D Fit.OUT files.

#################################
##### Dataset Manipulation ######
#################################

# Hydrus-1D outputs will have observed and estimated groundwater table level 
# with time and the optimized soil hydraulic properties for each season at each 
# site that is run.

# Calculating the mean and standard deviation for the optimized soil hydraulic
# parameters for each soil texture gives the output we reached and presented in
# table 1 of the publication. The mean values for each optimized soil hydraulic 
# parameter by soil texture were added to the dataset used hereafter.

# Merge the site characteristic dataset, SMAP dataset, and resulting HYDRUS 
# observed and estimated groundwater data with time into one dataset. 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Resulting dataset at this point is output_file_1.xlsx
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##########################################
###### Classify Events as PF or Not ######
##########################################

# merged_data <- Imported data file: "output_file_1.xlsx"

# Sort the data by site and date
merged_data <- merged_data[order(merged_data$Site, merged_data$Date),]

# Calculate the difference between consecutive Obs values for each site
merged_data$GWdiff <- unlist(tapply(merged_data$Obs, merged_data$Site, 
                                    function(x) c(NA, diff(x))))

# Calculate the difference between consecutive SMAPsm values for each site
merged_data$SMAPsmdiff <- unlist(tapply(merged_data$SMAPsm, merged_data$Site, 
                                        function(x) c(NA, diff(x))))

# Add new column indicating direction (increase, decrease, or no change) of Obs
merged_data$direc <- ifelse(merged_data$GWdiff > 0, "up", 
                            ifelse(merged_data$GWdiff < 0, "down", "no change"))

# Remove first row for each site based on the date column and remove rows with NA
total.3 <- merged_data[!duplicated(merged_data$Site) | duplicated(merged_data$Site, 
                                                                  fromLast = TRUE), ]
total.3 <- total.3[complete.cases(total.3), ]

# Reset row names if needed
row.names(total.3) <- NULL

# Print the updated dataframe
total.3 <- na.omit(total.3)

# Function to check if precipitation is greater than or equal to average
is_high_precipitation <- function(precipitation, site, data) {
  average_precipitation <- mean(data[data$Site == site, "Precip.cm.day"], na.rm = TRUE)
  return(ifelse(precipitation >= average_precipitation, "high", "low"))
}

# Create a new column "label" indicating whether precipitation is high
total.3$height <- mapply(is_high_precipitation, total.3$Precip.cm.day, 
                         total.3$Site,MoreArgs = list(data = total.3))

# Run through all 3 criteria and determine PF event or not
test.site <- function(site) {
  rmse.test <- (rmse.fun(paste(site))) * 0.5
  test <- subset(total.3, Site == paste(site))
  test$error <- test$Obs - test$Fitted
  
  # Label "PF" if direc = "up", height = "high", and error < rmse
  test$Class <- ifelse(test$direc == "up" & test$height == "high" & test$error < rmse.test, "PF", "no PF")
  
  remainderpos <- subset(test, error > rmse.test)
  remainderneg <- subset(test, error < 0)
  remainder <- rbind(remainderpos, remainderneg)
  
  remainder$Class <- "no PF"
  
  together <- rbind(test, remainder) %>%
    distinct(Site, Date, .keep_all = TRUE) %>%
    group_by(Site, Date) %>%
    filter(!(Class == "PF" & n() > 1))
  
  # Return any desired results
  return(together)
}

blaine.test <- test.site("Blaine")
jones.test <- test.site("Jones")
stlaw.test <- test.site("St. Lawrence")
chat.test <- test.site("Chatham")
redl.test <- test.site("Red Lake")
walk.test <- test.site("Walker")
kalam.test <- test.site("Kalamazoo")
mon.test <- test.site("Monmouth")
stcro.test <- test.site("St. Croix")
bighorn.test <- test.site("Big Horn")
pasq.test <- test.site("Pasquotank")
taze.test <- test.site("Tazewell")

total.4 <- rbind(blaine.test,jones.test,stlaw.test,chat.test,redl.test,walk.test,
                 kalam.test,mon.test,stcro.test,bighorn.test,pasq.test,taze.test)

total.4$Site <- factor(total.4$Site,
                       levels = c("St. Lawrence","Jones","Blaine",
                                  "Red Lake","Chatham","Walker",
                                  "Kalamazoo","Monmouth","St. Croix",
                                  "Pasquotank","Big Horn","Tazewell"))

total.4$Season <- factor(total.4$Season,
                         levels = c("spring","summer","autumn","winter"))

total.4$claypercent <- factor(total.4$claypercent,
                              levels=c("0-5% Clay","5-10% Clay",
                                       "10-15% Clay","15-30% Clay"))
total.4$Class <- factor(total.4$Class)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Resulting dataset at this point is output_file_2.xlsx
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##########################################
########### Random Forest Model ##########
##########################################
set.seed(123)

# Split the data into training (80%) and testing (20%) sets
index <- createDataPartition(total.4$Class, p = 0.8, list = FALSE)
train_data <- total.4[index, ]
test_data <- total.4[-index, ]

fitcontroll <- trainControl(
  method = "cv",
  number = 10,
  search = "random",
  savePredictions = TRUE)

modelFitrf <- train(
  Class ~ Precip.cm.day + GWdiff + SMAPsm + Ks + Alpha +
    Omega + I + n + theta.r + theta.s + Season +
    elevation + BD + slope + theta.r.im + theta.s.im +
    soil.texture,
  data = train_data,  # Use the training set here
  method = "rf",
  trControl = fitcontroll,
  tuneLength = 10,
  ntree = 1000)

variable_importance <- caret::varImp(modelFitrf)

# Extract information
importance_values <- variable_importance$importance
variable_names <- rownames(importance_values)

# Create a data frame
variable_importance_df <- data.frame(Variable = variable_names, 
                                     Overall = importance_values)

# Order the data frame by importance in descending order
variable_importance_df <- variable_importance_df[order(-variable_importance_df$Overall), ]

# Create a bar plot using ggplot2
library(ggplot2)

ggplot(variable_importance_df, aes(x = reorder(Variable, Overall), y = Overall)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Var Imp: RF 10 fold CV") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.title.y = element_blank()) +
  coord_flip() +
  scale_y_continuous(expand=c(0,0)) +
  my_theme() +
  theme(axis.text.x = element_text(angle=0,size = 12, colour = "black"))

# Make predictions on the test set
test_predictions <- predict(modelFitrf, newdata = test_data)

# Evaluate the model on the test set
confusion_matrix <- caret::confusionMatrix(table(test_predictions, test_data$Class))
print(confusion_matrix)
