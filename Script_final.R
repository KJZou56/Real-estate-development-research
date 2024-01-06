# case 1
###############################################################
######################### housing.csv
setwd("C:/Users/kj_zo/Desktop/Data science course work/Exploratory data analysis and visulization/the final")
library(readr)
library(ggplot2)
library(scales)
library(dplyr)
housing <- read_csv("housing.csv")
schools <- read_csv("schools.csv")
summary(housing)
summary(housing)

############ Let's deal with nas
table(is.na(housing))

# Count NAs for each variable
(na_counts <- colSums(is.na(housing)))

# take a look at where those NAs are
# Columns of interest
columns_of_nas <- c("sqft", "lotsize", "cooling", "heating", "fireplace")

# Print rows with NA values for each column
for (col in columns_of_nas) {
  print(housing[which(is.na(housing[[col]])), ])
} 

########### where are 42 nas in total. Not significant.drop them for now.
housing_clean <- housing[complete.cases(housing[, columns_of_nas]), ] 


##################### categorical variables: convert to factors
## let's fix some typos/errors first
housing_clean$type[housing_clean$type=="town house"]<-"townhouse" 
housing_clean$type[housing_clean$type=="condominium"]<-"condo"
housing_clean <- housing_clean[housing_clean$levels != "?", ] 

# these are the columns I want to convert to factors
columns_to_convert <- c("neighborhood", "type","levels","cooling", "heating", "fireplace", "elementary", "middle", "high")

# Loop through the columns and convert to factor
for (col in columns_to_convert) {
  housing_clean[[col]] <- as.factor(housing_clean[[col]])
}

# check factor levels of all factor variables to make sure it all makes sense
factor_columns <- sapply(housing_clean, is.factor) # Identify factor columns

# Extract factor levels for each factor column 
(factor_levels <- sapply(housing_clean[, factor_columns, drop = FALSE], levels))

## let's make sure all categorical variables are taken care of.
str(housing_clean)
summary(housing_clean)

###################### numerical variables:outliers and feature engineering
# Get the current year
current_year <- as.numeric(format(Sys.Date(), "%Y"))

# Calculate age and create a new column 'age'
housing_clean$age <- current_year - housing_clean$year
housing_clean$year<-NULL

# create a new column for soldprice in thousands
housing_clean$soldprice_000<-housing_clean$soldprice/1000
housing_clean$soldprice<-NULL

################# let's see how numeric variables are distributed

# Identify numeric columns
numeric_columns <- sapply(housing_clean, is.numeric)

# Set up a 2x3 grid for plots
par(mfrow = c(2, 3))

# Plot histograms for numeric variables
for (col in names(housing_clean)[numeric_columns]) {
  hist(housing_clean[[col]], main = paste("Histogram of", col), xlab = col, col = "lightblue", border = "black")
}

# Plot boxplots for numeric variables
for (col in names(housing_clean)[numeric_columns]) {
  boxplot(housing_clean[[col]], main = paste("Boxplot of", col), col = "lightblue", border = "black")
}

# Reset the plotting layout
par(mfrow = c(1, 1))

# remove outliers that don't make sense
# Remove rows where age is -88
housing_clean <- housing_clean[housing_clean$age != -88, ] 
# Remove rows where soldprice_000 is 0.664
housing_clean <- housing_clean[housing_clean$soldprice_000 != 0.664, ]
# Remove rows where baths is 25, which does not make sense
housing_clean <- housing_clean[housing_clean$baths != 25, ]
# Remove rows where age is 528, which does not make sense. Houses this old are usually preserved.
housing_clean <- housing_clean[housing_clean$age != 528, ]

# let's check box plots again
par(mfrow = c(2, 3))

# Plot boxplots for numeric variables
for (col in names(housing_clean)[numeric_columns]) {
  boxplot(housing_clean[[col]], main = paste("Boxplot of", col), col = "lightgreen", border = "black")
}

# Reset the plotting layout
par(mfrow = c(1, 1))

# Identify categorical variables
str(housing_clean)
categorical_vars <- sapply(housing_clean, is.factor)

# Count occurrences of each category in categorical variables
category_counts <- sapply(housing_clean[categorical_vars], table)

# Print the counts
print(category_counts)

# Create a bar chart to take a closer look at neighborhood. 
# only 3 purple. Gonna be misleading
ggplot(housing_clean, aes(x = neighborhood)) +
  geom_bar(fill = "lightblue", color = "black") +
  geom_text(stat = "count", aes(label = after_stat(count)),
            vjust = -0.5, size = 3, color = "black") +
  labs(title = "Bar Chart of Neighborhood Counts",
       x = "Neighborhood",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

# Remove rows where neighborhood is 'Purple'
housing_clean <- housing_clean[housing_clean$neighborhood != 'Purple', ]

# generate a bar chart without neighborhood purple
neighborhood_counts <- housing_clean %>%
  group_by(neighborhood) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

# Use the sorted data frame for plotting
ggplot(neighborhood_counts, aes(x = reorder(neighborhood, -count), y = count)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  geom_text(aes(label = count), vjust = -0.5, size = 3, color = "black") +
  labs(title = "Bar Chart of Neighborhood Counts",
       x = "Neighborhood",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## the reminding outliers were not removed since they are not obviously off.

######################## schools.csv
summary(schools)
schools$school<-as.factor(schools$school)

############### merge data sets
# Merge by elementary and school
merged_data <- merge(housing_clean, schools, by.x = "elementary", by.y = "school", all.x = TRUE)
# Rename elementary size and rating
names(merged_data)[names(merged_data) %in% c("size", "rating")] <- c("elementary_size", "elementary_rating")

# Merge by middle and school
merged_data <- merge(merged_data, schools, by.x = "middle", by.y = "school", all.x = TRUE)
# Rename middle size and rating
names(merged_data)[names(merged_data) %in% c("size", "rating")] <- c("middle_size", "middle_rating")

# Merge by high and school
merged_data <- merge(merged_data, schools, by.x = "high", by.y = "school", all.x = TRUE)
# Rename high size and rating
names(merged_data)[names(merged_data) %in% c("size", "rating")] <- c("high_size", "high_rating")

###### engineer 2 new variables
merged_data$mean_size <- rowMeans(merged_data[, c("elementary_size", "middle_size", "high_size")], na.rm = TRUE)
merged_data$mean_rating <- rowMeans(merged_data[, c("elementary_rating", "middle_rating", "high_rating")], na.rm = TRUE)

names(merged_data)

# Specify the columns to keep
columns_to_keep <- c("neighborhood", "beds", "baths", "sqft", "lotsize", "type", "levels", 
                     "cooling", "heating", "fireplace", "age", "soldprice_000", "mean_size", "mean_rating")

# Subset the data frame
data_clean <- merged_data[columns_to_keep]

############# Now, we are done with data cleaning. Let's explore and
############# create some visuals.

# how do neighborhoods affect median housing prices?
# Calculate median of soldprice_000 grouped by neighborhood using aggregate
(median_by_neighborhood <- aggregate(soldprice_000 ~ neighborhood, 
                                     data = data_clean, median))

# Create a histogram with ggplot2 and add high-density line
ggplot(data_clean, aes(x = soldprice_000)) +
  geom_histogram(binwidth = 100, fill = "lightblue", color = "black", aes(y = ..density..)) +
  geom_density(fill = "red", alpha = 0.5) +  # Add high-density line
  labs(title = "Histogram of Sold Prices with High-Density Line",
       x = "Sold Price (in '000s)", y = "Density") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for better readability
  scale_y_continuous(labels = comma)  # Format y-axis labels without scientific notation

summary(data_clean)

# Create a bar chart with ggplot2
ggplot(median_by_neighborhood, aes(x = reorder(neighborhood, soldprice_000), y = soldprice_000)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  geom_text(aes(label = soldprice_000), vjust = -0.5, size = 3, color = "black") +
  labs(title = "Median Sold Prices by Neighborhood",
       x = "Neighborhood",
       y = "Median Sold Price (in thousands)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Calculate median of soldprice_000 grouped by type
(median_by_type <- aggregate(soldprice_000 ~ type, data = data_clean, median))

# Create a bar chart with ggplot2
ggplot(median_by_type, aes(x = type, y = soldprice_000)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  geom_text(aes(label = soldprice_000), vjust = -0.5, size = 3, color = "black") +
  labs(title = "Median Sold Prices by Type",
       x = "Type",
       y = "Median Sold Price (in thousands)") +
  theme_minimal()



# Calculate median of soldprice_000 grouped by type
(age_by_neighborhood <- aggregate(age ~ neighborhood, data = data_clean, median))

# Create a bar chart: age_by_neighborhood
ggplot(age_by_neighborhood, aes(x = reorder(neighborhood, age), y = age)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  geom_text(aes(label = age), vjust = -0.5, size = 3, color = "black") +
  labs(title = "Median Age by Neighborhood",
       x = "Neighborhood",
       y = "Median Age") +
  theme_minimal()

# Create a scatter plot with regression lines for each type 
# average school size vs average school rating
ggplot(data_clean, aes(x = mean_rating, y = mean_size, color=neighborhood,size=1)) +
  geom_point() +
  labs(title = "Scatter Plot of average school size vs. average school rating by neighborhood", x = "Average school rating", y = "Average school size") +
  theme_minimal() +
  scale_color_discrete(name = "neighborhood") + 
  guides(size = FALSE)  # Remove the legend for size


ggplot(data_clean, aes(x = sqft, y = soldprice_000, color=neighborhood,size=1)) +
  geom_point() +
  labs(title = "Scatter Plot of housing price vs. sqft by neighborhood", x = "Sqft", y = "Housing price in thousands") +
  theme_minimal() +
  scale_color_discrete(name = "neighborhood") + 
  guides(size = FALSE)  # Remove the legend for size

ggplot(data_clean, aes(x = sqft, y = soldprice_000, color = neighborhood, size = 1)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, aes(group = neighborhood), size = 1.5) +  # Adjust the size here
  labs(title = "Scatter Plot of housing price vs. sqft by neighborhood",
       x = "Sqft",
       y = "Housing price in thousands") +
  theme_minimal() +
  scale_color_discrete(name = "neighborhood") +
  guides(size = FALSE)

############### regression
# Specify the selected features as a character vector
selected_features <- c("neighborhood", "beds", "sqft", "type", "heating", "fireplace", "age", "mean_size", "mean_rating")

# Construct the formula dynamically
formula_string <- paste("soldprice_000 ~", paste(selected_features, collapse = " + "))

# Create a regression model
reg_model <- lm(as.formula(formula_string), data = data_clean)

# Print the summary of the regression model
summary(reg_model)

# Assess model assumptions and diagnostics
par(mfrow = c(2, 2))  # Set up a 2x2 grid for diagnostic plots
plot(reg_model)

# Calculate and print residuals
residuals <- residuals(reg_model)

# Calculate and print the Root Mean Squared Error (RMSE)
rmse <- sqrt(mean(residuals^2))
print(paste("Root Mean Squared Error (RMSE):", rmse))

###################### clustering
## Get the data ready for clustering
# Identify categorical variables
categorical_vars <- c("neighborhood", "type", "cooling", "heating", "fireplace")

# Convert the specified columns to factors
data_clean[, categorical_vars] <- lapply(data_clean[, categorical_vars], as.factor)

# Create one-hot encoding using model.matrix
data_clean_encoded <- cbind(data_clean, model.matrix(~ 0 + ., data = data_clean[, categorical_vars]))

# Remove original categorical columns
data_clean_encoded <- data_clean_encoded[, !(names(data_clean_encoded) %in% categorical_vars)]

# Now, 'data_clean_encoded' contains the dataset with one-hot encoded categorical variables
# Load required library for clustering
library(cluster)

# Specify the number of clusters (you may need to choose this based on your problem)
num_clusters <- 4

# Perform k-means clustering
kmeans_result <- kmeans(data_clean_encoded, centers = num_clusters, nstart = 10)

# Add cluster assignment to the original dataset
data_clean_clustered <- cbind(data_clean, Cluster = kmeans_result$cluster)
kmeans_result

# Reset plot parameters to default
par(mfrow = c(1, 1))
hc <- hclust(dist(data_clean_encoded))
plot(hc, hang = -1, main = "Dendrogram")

## it looks 4 is a reasonable number of clusters.

## let's reconfirm with the elbow method
par(mfrow = c(1, 1))
wcss_values <- numeric(10)

for (k in 1:10) {
  kmeans_result <- kmeans(data_clean_encoded, centers = k)
  wcss_values[k] <- kmeans_result$tot.withinss
}

plot(1:10, wcss_values, type = "b", pch = 19, frame = FALSE, xlab = "Number of Clusters (k)", ylab = "WCSS")

# Assuming you have data_clean_clustered with a "Cluster" column
ggplot(data_clean_clustered, aes(x = sqft, y = lotsize, color = factor(Cluster))) +
  geom_point() +
  labs(title = "Scatter Plot of Sqft vs. lot size with Clusters",
       x = "Square Footage",
       y = "lot size") +
  theme_minimal() +
  scale_color_discrete(name = "Cluster") +
  guides(color = guide_legend(title = "Cluster"))



# case 2
################################################################ 
setwd("C:/Users/kj_zo/Desktop/Data science course work/Exploratory data analysis and visulization/the final")
library(readr)
library(ggplot2)
library(scales)
housing <- read_csv("housing.csv")
schools <- read_csv("schools.csv")
summary(housing)

############ Let's deal with nas
table(is.na(housing))

# Count NAs for each variable
(na_counts <- colSums(is.na(housing)))

# take a look at where those NAs are
# Columns of interest
columns_of_nas <- c("sqft", "lotsize", "cooling", "heating", "fireplace")

# Print rows with NA values for each column
for (col in columns_of_nas) {
  print(housing[which(is.na(housing[[col]])), ])
} 

# 1. Impute NAs in sqft

filtered_data <- subset(housing, neighborhood == "Red" & beds == 1 & baths == 1)
mean_sqft <- mean(filtered_data$sqft, na.rm = TRUE)
housing$sqft[which(is.na(housing$sqft))] <- mean_sqft

# 2. impute NAs in lotsize
# Create a linear regression model
linear_model <- lm(lotsize ~ sqft, data = housing)

# Print the summary of the regression model
summary(linear_model)

# Plot the regression line
plot(housing$sqft, housing$lotsize, main = "Regression Line: sqft vs. lotsize", xlab = "sqft", ylab = "lotsize")
abline(linear_model, col = "red")

# Identify rows with missing 'lotsize'
rows_with_missing_lotsize <- which(is.na(housing$lotsize))

# Predict 'lotsize' for rows with missing values
predicted_lotsize <- predict(linear_model, newdata = data.frame(sqft = housing$sqft[rows_with_missing_lotsize]))

# Replace missing 'lotsize' with the predicted values
housing$lotsize[rows_with_missing_lotsize] <- predicted_lotsize

# 3. impute NAs in cooling
(table(housing$cooling,housing$type))
housing$cooling[is.na(housing$cooling)]<-"No" ## most houses are "No"
# 4. impute NAs in heating
(table(housing$heating,housing$type))
housing$heating[is.na(housing$heating)]<-"No" ## most house are "No"

# 5. impute NAs in fireplace
(table(housing$fireplace,housing$neighborhood))
housing$fireplace[is.na(housing$fireplace)]<-"No" ## most house are "No"

##################### categorical variables: convert to factors
## let's fix some typos/errors first
housing$type[housing$type=="town house"]<-"townhouse" 
housing$type[housing$type=="condominium"]<-"condo"

table(housing$levels,housing$beds) # levels are more likely to be 1
housing$levels[housing$levels== "?"]<-"1" 


# these are the columns I want to convert to factors
columns_to_convert <- c("neighborhood", "type","levels","cooling", "heating", "fireplace", "elementary", "middle", "high")

# Loop through the columns and convert to factor
for (col in columns_to_convert) {
  housing[[col]] <- as.factor(housing[[col]])
}

# check factor levels of all factor variables to make sure it all makes sense
factor_columns <- sapply(housing, is.factor) # Identify factor columns

# Extract factor levels for each factor column
(factor_levels <- sapply(housing[, factor_columns, drop = FALSE], levels))

## let's make sure all categorical variables are taken care of.
str(housing)
summary(housing)

###################### numerical variables:outliers and feature engineering
# Get the current year
current_year <- as.numeric(format(Sys.Date(), "%Y"))

# Calculate age and create a new column 'age'
housing$age <- current_year - housing$year
housing$year<-NULL

# create a new column for soldprice in thousands
housing$soldprice_000<-housing$soldprice/1000
housing$soldprice<-NULL

################# let's see how numeric variables are distributed

# Identify numeric columns
numeric_columns <- sapply(housing, is.numeric)

# Set up a 2x3 grid for plots
par(mfrow = c(2, 3))

# Plot histograms for numeric variables
for (col in names(housing)[numeric_columns]) {
  hist(housing[[col]], main = paste("Histogram of", col), xlab = col, col = "lightblue", border = "black")
}

# Plot boxplots for numeric variables
for (col in names(housing)[numeric_columns]) {
  boxplot(housing[[col]], main = paste("Boxplot of", col), col = "lightgreen", border = "black")
}

# Reset the plotting layout
par(mfrow = c(1, 1))

# remove outliers that don't make sense
# Remove rows where age is -88
housing_clean <- housing[housing$age != -88, ] 
# Remove rows where soldprice_000 is 0.664
housing_clean <- housing_clean[housing_clean$soldprice_000 != 0.664, ]
# Remove rows where baths is 25, which does not make sense
housing_clean <- housing_clean[housing_clean$baths != 25, ]
# Remove rows where age is 528, which does not make sense. Houses this old are usually preserved.
housing_clean <- housing_clean[housing_clean$age != 528, ]
# Remove row there beds is 999,high influential point
housing_clean <- housing_clean[housing_clean$beds != 999, ]

# let's check box plots again
par(mfrow = c(2, 3))

# Plot boxplots for numeric variables
for (col in names(housing_clean)[numeric_columns]) {
  boxplot(housing_clean[[col]], main = paste("Boxplot of", col), col = "lightgreen", border = "black")
}

# Reset the plotting layout
par(mfrow = c(1, 1))

# Identify categorical variables
str(housing_clean)
categorical_vars <- sapply(housing_clean, is.factor)

# Count occurrences of each category in categorical variables
category_counts <- sapply(housing_clean[categorical_vars], table)

# Print the counts
print(category_counts)

# Create a bar chart to take a closer look at neighborhood. 
# only 3 purple. Gonna be misleading
ggplot(housing_clean, aes(x = neighborhood)) +
  geom_bar(fill = "lightblue", color = "black") +
  geom_text(stat = "count", aes(label = after_stat(count)),
            vjust = -0.5, size = 3, color = "black") +
  labs(title = "Bar Chart of Neighborhood Counts",
       x = "Neighborhood",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

# Remove rows where neighborhood is 'Purple'
housing_clean <- housing_clean[housing_clean$neighborhood != 'Purple', ]

# generate a bar chart without neighborhood purple
neighborhood_counts <- housing_clean %>%
  group_by(neighborhood) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

# Use the sorted data frame for plotting
ggplot(neighborhood_counts, aes(x = reorder(neighborhood, -count), y = count)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  geom_text(aes(label = count), vjust = -0.5, size = 3, color = "black") +
  labs(title = "Bar Chart of Neighborhood Counts",
       x = "Neighborhood",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## the reminding outliers were not removed since they are not obviously off.

######################## schools.csv
summary(schools)
schools$school<-as.factor(schools$school)

############### merge data sets
# Merge by elementary and school
merged_data <- merge(housing_clean, schools, by.x = "elementary", by.y = "school", all.x = TRUE)
# Rename elementary size and rating
names(merged_data)[names(merged_data) %in% c("size", "rating")] <- c("elementary_size", "elementary_rating")

# Merge by middle and school
merged_data <- merge(merged_data, schools, by.x = "middle", by.y = "school", all.x = TRUE)
# Rename middle size and rating
names(merged_data)[names(merged_data) %in% c("size", "rating")] <- c("middle_size", "middle_rating")

# Merge by high and school
merged_data <- merge(merged_data, schools, by.x = "high", by.y = "school", all.x = TRUE)
# Rename high size and rating
names(merged_data)[names(merged_data) %in% c("size", "rating")] <- c("high_size", "high_rating")

###### engineer 2 new variables
merged_data$mean_size <- rowMeans(merged_data[, c("elementary_size", "middle_size", "high_size")], na.rm = TRUE)
merged_data$mean_rating <- rowMeans(merged_data[, c("elementary_rating", "middle_rating", "high_rating")], na.rm = TRUE)

names(merged_data)

# Specify the columns to keep
columns_to_keep <- c("neighborhood", "beds", "baths", "sqft", "lotsize", "type", "levels", 
                     "cooling", "heating", "fireplace", "age", "soldprice_000", "mean_size", "mean_rating")

# Subset the data frame
data_clean <- merged_data[columns_to_keep]

############# Now, we are done with data cleaning. Let's explore and
############# create some visuals.

# how do neighborhoods affect median housing prices?
# Calculate median of soldprice_000 grouped by neighborhood using aggregate
(median_by_neighborhood <- aggregate(soldprice_000 ~ neighborhood, 
                                     data = data_clean, median))

# Create a histogram with ggplot2 and add high-density line
ggplot(data_clean, aes(x = soldprice_000)) +
  geom_histogram(binwidth = 100, fill = "lightblue", color = "black", aes(y = ..density..)) +
  geom_density(fill = "red", alpha = 0.5) +  # Add high-density line
  labs(title = "Histogram of Sold Prices with High-Density Line",
       x = "Sold Price (in '000s)", y = "Density") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for better readability
  scale_y_continuous(labels = comma)  # Format y-axis labels without scientific notation

summary(data_clean)

# Create a bar chart with ggplot2
ggplot(median_by_neighborhood, aes(x = reorder(neighborhood, soldprice_000), y = soldprice_000)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  geom_text(aes(label = soldprice_000), vjust = -0.5, size = 3, color = "black") +
  labs(title = "Median Sold Prices by Neighborhood",
       x = "Neighborhood",
       y = "Median Sold Price (in thousands)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Calculate median of soldprice_000 grouped by type
(median_by_type <- aggregate(soldprice_000 ~ type, data = data_clean, median))

# Create a bar chart with ggplot2
ggplot(median_by_type, aes(x = type, y = soldprice_000)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  geom_text(aes(label = soldprice_000), vjust = -0.5, size = 3, color = "black") +
  labs(title = "Median Sold Prices by Type",
       x = "Type",
       y = "Median Sold Price (in thousands)") +
  theme_minimal()



# Calculate median of soldprice_000 grouped by type
(age_by_neighborhood <- aggregate(age ~ neighborhood, data = data_clean, median))

# Create a bar chart: age_by_neighborhood
ggplot(age_by_neighborhood, aes(x = reorder(neighborhood, age), y = age)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  geom_text(aes(label = age), vjust = -0.5, size = 3, color = "black") +
  labs(title = "Median Age by Neighborhood",
       x = "Neighborhood",
       y = "Median Age") +
  theme_minimal()

# Create a scatter plot with regression lines for each type 
# average school size vs average school rating
ggplot(data_clean, aes(x = mean_rating, y = mean_size, color=neighborhood,size=1)) +
  geom_point() +
  labs(title = "Scatter Plot of average school size vs. average school rating by neighborhood", x = "Average school rating", y = "Average school size") +
  theme_minimal() +
  scale_color_discrete(name = "neighborhood") + 
  guides(size = FALSE)  # Remove the legend for size


ggplot(data_clean, aes(x = sqft, y = soldprice_000, color=neighborhood,size=1)) +
  geom_point() +
  labs(title = "Scatter Plot of housing price vs. sqft by neighborhood", x = "Sqft", y = "Housing price in thousands") +
  theme_minimal() +
  scale_color_discrete(name = "neighborhood") + 
  guides(size = FALSE)  # Remove the legend for size

ggplot(data_clean, aes(x = sqft, y = soldprice_000, color = neighborhood, size = 1)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, aes(group = neighborhood), size = 1.5) +  # Adjust the size here
  labs(title = "Scatter Plot of housing price vs. sqft by neighborhood",
       x = "Sqft",
       y = "Housing price in thousands") +
  theme_minimal() +
  scale_color_discrete(name = "neighborhood") +
  guides(size = FALSE)

############### regression
# Specify the selected features as a character vector
selected_features <- c("neighborhood", "beds", "sqft", "type", "heating", "fireplace", "age", "mean_size", "mean_rating")

# Construct the formula dynamically
formula_string <- paste("soldprice_000 ~", paste(selected_features, collapse = " + "))

# Create a regression model
reg_model <- lm(as.formula(formula_string), data = data_clean)

# Print the summary of the regression model
summary(reg_model)

# Assess model assumptions and diagnostics
par(mfrow = c(2, 2))  # Set up a 2x2 grid for diagnostic plots
plot(reg_model)

# Calculate and print residuals
residuals <- residuals(reg_model)

# Calculate and print the Root Mean Squared Error (RMSE)
rmse <- sqrt(mean(residuals^2))
print(paste("Root Mean Squared Error (RMSE):", rmse))

###################### clustering
## Get the data ready for clustering
# Identify categorical variables
categorical_vars <- c("neighborhood", "type", "cooling", "heating", "fireplace")

# Convert the specified columns to factors
data_clean[, categorical_vars] <- lapply(data_clean[, categorical_vars], as.factor)

# Create one-hot encoding using model.matrix
data_clean_encoded <- cbind(data_clean, model.matrix(~ 0 + ., data = data_clean[, categorical_vars]))

# Remove original categorical columns
data_clean_encoded <- data_clean_encoded[, !(names(data_clean_encoded) %in% categorical_vars)]

# Now, 'data_clean_encoded' contains the dataset with one-hot encoded categorical variables
# Load required library for clustering
library(cluster)

# Specify the number of clusters (you may need to choose this based on your problem)
num_clusters <- 4

# Perform k-means clustering
kmeans_result <- kmeans(data_clean_encoded, centers = num_clusters, nstart = 10)

# Add cluster assignment to the original dataset
data_clean_clustered <- cbind(data_clean, Cluster = kmeans_result$cluster)
kmeans_result

# Reset plot parameters to default
par(mfrow = c(1, 1))
hc <- hclust(dist(data_clean_encoded))
plot(hc, hang = -1, main = "Dendrogram")

## it looks 4 is a reasonable number of clusters.

## let's reconfirm with the elbow method
par(mfrow = c(1, 1))
wcss_values <- numeric(10)

for (k in 1:10) {
  kmeans_result <- kmeans(data_clean_encoded, centers = k)
  wcss_values[k] <- kmeans_result$tot.withinss
}

plot(1:10, wcss_values, type = "b", pch = 19, frame = FALSE, xlab = "Number of Clusters (k)", ylab = "WCSS")

# Assuming you have data_clean_clustered with a "Cluster" column
ggplot(data_clean_clustered, aes(x = sqft, y = lotsize, color = factor(Cluster))) +
  geom_point() +
  labs(title = "Scatter Plot of Sqft vs. lot size with Clusters",
       x = "Square Footage",
       y = "lot size") +
  theme_minimal() +
  scale_color_discrete(name = "Cluster") +
  guides(color = guide_legend(title = "Cluster"))
