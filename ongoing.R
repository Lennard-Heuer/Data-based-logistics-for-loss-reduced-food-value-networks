library("readxl")
library("tidyverse")
Project_data <- read_excel("Project data 3.xlsx", 
                           sheet = "Merge FLI and LPI Aggr. '12-'18") 

Food_categorization <- read_excel("Project data.xlsx", 
                           sheet = "Food categorization") 

Project_data$index <- 1:nrow(Project_data)

add_column(Project_data)

Project_data <- cbind(Project_data, food_category=NA)

Project_data <- Project_data %>%
  add_column(FC = NA)
head(Project_data)

summary(Project_data$food_category)
  
x1 <- Project_data$index %in% Food_categorization$`Index Cereals`
x2 <- Project_data$index %in% Food_categorization$`Index Roots an tumbers`
x3 <- Project_data$index %in% Food_categorization$`Oilseeds and pulses`
x4 <- Project_data$index %in% Food_categorization$`Milk products`
x5 <- Project_data$index %in% Food_categorization$`Index Eggs`
x6 <- Project_data$index %in% Food_categorization$`Index Fish`
x7 <- Project_data$index %in% Food_categorization$`Index Meat`
x8 <- Project_data$index %in% Food_categorization$`Index Vegetables`
x9 <- Project_data$index %in% Food_categorization$`Index Fruits`


summary(x1)


length(Project_data$FC)


for (i in 1:length(Project_data$FC)) {
if (x1[i] == TRUE) { 
  Project_data$FC[i] =  "Cereals"
      } else if (x2[i] == TRUE) {
      Project_data$FC[i] =  "Roots and tumbers"
        } else if (x3[i] == TRUE) {
        Project_data$FC[i] =  "Oilseeds and pulses"
          } else if (x4[i] == TRUE) {
          Project_data$FC[i] =  "Milk products"
            } else if (x4[i] == TRUE) {
            Project_data$FC[i] =  "Fish products"
            } else if (x3[i] == TRUE) {
               Project_data$FC[i] =  "Meat products"
              } else if (x3[i] == TRUE) {
                Project_data$FC[i] =  "Vegetables"
                  } else if (x3[i] == TRUE) {
                    Project_data$FC[i] =  "Fruits"
          } else {
          Project_data$FC[i] =  "None"
          }
}

Project_data$FC <- as.factor(Project_data$FC)

summary(Project_data$FC)


# Replace values
x# Print new vector



# Only consider food loss data from 2007 onwards

Project_data <- subset(Project_data, year >= 2001)

# Following the syntax of the code, only data of year 2001 or later is included.
# NAs will therefore not be included from now be dropped out

# Since loss_percentage ideally is only only the value of loss_percentage_ori-
# nal multiplied by 100, keeping both columns would be redundant. Therefore, 
# loss_percentage is eliminated.

Project_data <- Project_data %>% select(-c(loss_percentage))

# Heat map Darstellungen in R:

# notwendig: Group by 


Project_data$food_supply_stage <- as.factor(Project_data$food_supply_stage)

summary(Project_data$food_supply_stage)

agg_mean= aggregate(Project_data, by=list(Project_data$food_supply_stage), FUN = mean, na.rm = TRUE)

summary(agg_mean)

library(dplyr)
test <- Project_data %>%
  group_by(food_supply_stage, FC) %>% 
  summarise_each(funs(mean))


library(ggplot2)


p <- ggplot(test, aes(x=test$food_supply_stage, y=test$FC, fill=test$loss_percentage)) +
  geom_tile() +
  scale_fill_gradient(low="white", high="darkgreen", name="Your Legend")

p + scale_fill_continuous(trans = 'log10')





