# This script builds the managers dataset
# and populates it with data
# Load data from previous session
column_names <- c("Date", "Country", "Gender", "Age", "Q1", "Q2", "Q3", "Q4", "Q5")
# Enter data into vectors before constructing the data frame
date_col <- c("2018-15-10", "2018-01-11", "2018-21-10", "2018-28-10", "2018-01-05")
country_col <- c("US", "US", "IRL", "IRL", "IRL")
gender_col <- c("M", "F", "F", "M", "F")
age_col <- c(32, 45, 25, 39, 99) # 99 is one of the values in the age attribute - will require recoding
q1_col <- c(5, 3, 3, 3, 2)
q2_col <- c(4, 5, 5, 3, 2)
q3_col <- c(5, 2, 5, 4, 1)
q4_col <- c(5, 5, 5, NA, 2) # NA is inserted in place of the missing data for this attribute
q5_col <- c(5, 5, 2, NA, 1)
# Construct a data frame using the data from all vectors above
managers <- data.frame(date_col, country_col, gender_col, age_col, q1_col, q2_col, q3_col, q4_col, q5_col)
# Add column names to data frame using column_names vector
colnames(managers) <- column_names
# Recode the incorrect 'age' data to NA
managers$Age[managers$Age == 99] <- NA
# Create a new attribute called AgeCat and set valuess
# in AgeCat to the following if true:
# <= 25 = Young
# >= 26 & <= 44 = Middle Aged
# >= 45 = Elderly
# We will also recode age 'NA' to Elder
managers$AgeCat[managers$Age >= 45] <- "Elder"
managers$AgeCat[managers$Age >= 26 & managers$Age <= 44] <- "Middle Aged"
managers$AgeCat[managers$Age <= 25] <- "Young"
managers$AgeCat[is.na(managers$Age)] <- "Elder"

# Recode AgeCat so that is ordinal and factored with the
# order Young, Middle aged, Elder
# We'll srore the ordinal factored data in variable 'AgeCat'
AgeCat <- factor(managers$AgeCat, order = TRUE, levels = c("Young", "Middle Aged", "Elder"))

# Replace managers's AgeCat attribute with newly ordinal foctored data
managers$AgeCat <- AgeCat

# Create a new column called 'summary_col' that
# contains a summary of each row
summary_col <- managers$Q1 + managers$Q2 + managers$Q3 + managers$Q4 + managers$Q5
summary_col

# Add summary_col to the end of the data frame
managers <- data.frame(managers, summary_col)

# Calculate mean value for each row
mean_value <- rowMeans(managers[5:9])

# Add mean_value to end of managers data frame
managers <- data.frame(managers, mean_value)

# Show data frame contents
managers

# Change the name of this column to "mean value"
names(managers)[12] <- "mean value"

# Change name of summary_col to "Answer total"
names(managers)[11] <- "Answer total"

# Show 
str(managers)

# Change the date structure from the factor
# to the required date structure

# We cannot convert a factor variable to date
# without first converting to a character variable
# from the default factor variable
date_field   <- as.character(managers$Date)
new_date <- as.Date(date_field, "%Y-%d-%m")
str(new_date)

# Now overwrite the contents of the date field with new date structure
managers$Date = new_date
str(managers)

# Load manager data set 
managers_data <- read.csv("managers.csv", na = "")
str(managers_data)

# Dealing with missing data


# Listwise deletion
new_data <- na.omit(managers_data)
new_data

# Doesnt work with char needs to be converted

managers_data$Q4 <- as.integer(managers_data$Q4)
managers_data$Q5 <- as.integer(managers_data$Q5)
str(managers_data)

new_data <- na.omit(managers_data)
new_data

# Use complete.case to show where data 
# Rows are complete 
complete_data <- complete.cases(managers_data)
complete_data
sum(complete_data)

# List the rows where data isnt missing 
complete_data <- managers_data[complete.cases(managers_data),]
complete_data
str(managers_data)

# Age column is still an issue
# so need to convert it to int 
managers_data$Age <- as.integer(managers_data$Age)

# We can use '!' as to negate the action of any function 
complete_data <- managers_data[!complete.cases(managers_data),]
complete_data

# Find the sum of missing data
sum(!is.na(managers_data$Age))

mean(is.na(managers_data))

# Installing a package
install.packages("mice")
library(mice)
md.pattern(managers_data) 

# Use VIM package to show missing value

install.packages("VIM")
library(VIM)
missing_value <- aggr(managers_data, prop = TRUE,numbers = FALSE)
summary(missing_value)

# Pra merging data 
# Loading data

new_manager_data <- read.csv("MoreData.csv", na = "")

# Structure of loaded data

str(new_manager_data)

# Exporting data of interest 

new_manager_data_cleaned <- subset(new_manager_data, select = c(Age,Country,Date,Gender,Q1,Q2,Q3,Q4,Q5))

# Creating Agecat
new_manager_data_cleaned$AgeCat[new_manager_data_cleaned$Age >= 45] <- "Elder"
new_manager_data_cleaned$AgeCat[new_manager_data_cleaned$Age >= 26 & new_manager_data_cleaned$Age <= 44] <- "Middle Aged"
new_manager_data_cleaned$AgeCat[new_manager_data_cleaned$Age <= 25] <- "Young"
new_manager_data_cleaned$AgeCat[is.na(new_manager_data_cleaned$Age)] <- "Elder"
AgeCat <- factor(new_manager_data_cleaned$AgeCat, order = TRUE, levels = c("Young", "Middle Aged", "Elder"))
new_manager_data_cleaned$AgeCat <- AgeCat

#creating x column

new_manager_data_cleaned$X <- NA
# Merging cleaned to manager data
#final_manager_data <- merge.data.frame(new_manager_data_cleaned, managers_data, all.x = FALSE,)

final_manager_data <- rbind(new_manager_data_cleaned,managers_data)
