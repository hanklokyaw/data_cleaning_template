### Session 1 - Loading Data
###
###############################################################################
# Load data from a CSV file
data <- read.csv("./file.csv")

# Load data from a TSV file
data <- read.table("./file.tsv", sep="\t", header=TRUE)

# Load data from an Excel file (requires the 'readxl' package)
library(readxl)
data <- read_excel("path/to/file.xlsx", sheet = "Sheet1", col_names = TRUE)

# Load data from a JSON file (requires the 'jsonlite' package)
library(jsonlite)
data <- fromJSON("path/to/file.json")

# Load data from an XML file (requires the 'XML' package)
library(XML)
data <- xmlToDataFrame("path/to/file.xml")

# Load data from a SQL database (requires the 'RMySQL' package)
library(RMySQL)
conn <- dbConnect(MySQL(), user="username", password="password", dbname="database")
query <- "SELECT * FROM table"
data <- dbGetQuery(conn, query)

# Load data from a NoSQL database (requires the appropriate package for your database)
library(mongolite)
mongo_conn <- mongo("mongodb://localhost:27017")
data <- mongo_conn$database$collection$find()

# Load data from a URL (requires the 'httr' package)
library(httr)
url <- "https://example.com/data.csv"
data <- read.csv(text = content(GET(url), "text"))



### Session 2 - Overview Data Frame
###
###############################################################################
# Display the first few rows of a data frame
head(df)

# Display the last few rows of a data frame
tail(df)

# Display summary statistics of a data frame
summary(df)

# Display the structure of a data frame
str(df)

# Return the dimensions of a data frame
dim(df)

# Return the number of rows in a data frame
nrow(df)

# Return the number of columns in a data frame
ncol(df)

# Open a data frame in a separate window for interactive exploration
View(df)

# check the summary statistic for x_client
summary(my_germ[x_client,] )

# check column names
colnames(my_germ)

### Session 3 - Check Data type
###
###############################################################################

# Check data type using class()
class(x) # returns "numeric"

# Check data type using typeof()
typeof(x) # returns "double"

mode(z) # returns "logical"

# Check data type using is.*
is.numeric(x) # returns TRUE
is.character(y) # returns TRUE
is.logical(z) # returns TRUE

# returns a character vector with the class of each column: "numeric", "character", "logical"
sapply(df, class) 

# returns a character vector with the data type of each column: "double", "character", "logical"
sapply(df, typeof) 

# returns a logical vector indicating which columns are numeric: TRUE, FALSE, FALSE
sapply(df, is.numeric) 




### Session 4 - Overview Missing Data Frame
###
###############################################################################
# Check for missing values in a data frame
# returns a logical value indicating if any missing values are present in the data frame
any(is.na(df)) 
# returns the total number of missing values in the data frame
sum(is.na(df)) 

# Check for missing values in a specific column
# returns a logical value indicating if any missing values are present in the column
any(is.na(df$column)) 
# returns the total number of missing values in the column
sum(is.na(df$column)) 

# List rows with missing values in a data frame
# lists all rows with missing values in the specified column
df[is.na(df$column),] 

# List rows with complete cases (no missing values) in a data frame
# lists all rows with complete cases in the data frame
df[complete.cases(df),] 

# Remove rows with missing values from a data frame
# creates a new data frame with all rows containing missing values removed
clean_df <- na.omit(df) 

# Replace missing values in a data frame
clean_df <- df # create a new data frame
# replaces missing values in the specified column with the replacement value
clean_df$column[is.na(clean_df$column)] <- replacement_value 

# Identify duplicated rows in a data frame
# creates a new data frame with all duplicated rows
duplicated_rows <- df[duplicated(df),] 

# Identify unique rows in a data frame
# creates a new data frame with only the unique rows
unique_rows <- unique(df) 



### Session 5 - Treating Missing Data Frame
###
###############################################################################
# Remove rows with missing values from a data frame
# creates a new data frame with all rows containing missing values removed
clean_df <- na.omit(df) 

# Replace missing values in a data frame with a specified value
# create a new data frame
clean_df <- df 
# replaces missing values in the specified column with the replacement value
clean_df$column[is.na(clean_df$column)] <- replacement_value 

# Replace missing values in a data frame with the mean value of the column
clean_df <- df # create a new data frame
# replaces missing values in the specified column with the mean value of the column
clean_df$column[is.na(clean_df$column)] <- mean(clean_df$column, na.rm = TRUE) 

# Interpolate missing values in a data frame using linear interpolation
clean_df <- df # create a new data frame
# interpolates missing values in the specified column using linear interpolation
clean_df$column <- approx(x = which(!is.na(clean_df$column)), 
                          y = clean_df$column[!is.na(clean_df$column)], 
                          xout = 1:nrow(clean_df), method = "linear")$y 

# Impute missing values in a data frame using the k-nearest neighbors algorithm
library(DMwR)
# creates a new data frame with missing values imputed using the k-nearest neighbors algorithm
clean_df <- knnImputation(df, k = 3) 

# Impute missing values in a data frame using the random forest algorithm
library(missForest)
# creates a new data frame with missing values imputed using the random forest algorithm

clean_df <- missForest(df) 
### Replace with Mean, Median, Mode
# Replace missing values in a column with the mean value of the column
df$y <- ifelse(is.na(df$y), mean(df$y, na.rm = TRUE), df$y)

# Replace missing values in a column with the median value of the column
df$x <- ifelse(is.na(df$x), median(df$x, na.rm = TRUE), df$x)

# Replace missing values in a column with the mode value of the column
library(modeest)
df$z <- ifelse(is.na(df$z), mlv(df$z), df$z)


### Session 6 - Sub-setting Data Frame
###
###############################################################################
# Subset data based on conditions
# creates a new data frame containing all rows where x > 2
subset_df <- df[df$x > 2, ] 
# creates a new data frame containing all rows where z = "B" and y < 8
subset_df <- df[df$z == "B" & df$y < 8, ] 

# Subset data by column
# creates a new data frame containing only the x column
subset_df <- df[, "x"] 
# creates a new data frame containing only the x and y columns
subset_df <- df[, c("x", "y")] 

# Subset data by row and column
# creates a new data frame containing rows 2-4 and only the x and z columns
subset_df <- df[2:4, c("x", "z")] 

# Subset data using logical conditions
# creates a logical vector indicating whether each row meets the condition
logical_df <- df$x > 3 
# creates a new data frame containing only the rows where the logical vector is TRUE
subset_df <- df[logical_df, ] 

# Subset data using the `dplyr` package
library(dplyr)
# creates a new data frame containing all rows where x > 2
subset_df <- df %>% filter(x > 2) 
# creates a new data frame containing all rows where z = "B" and y < 8
subset_df <- df %>% filter(z == "B", y < 8) 
# creates a new data frame containing only the x column
subset_df <- df %>% select(x) 
# creates a new data frame containing only the x and y columns
subset_df <- df %>% select(x, y) 

# Subset data using the `sqldf` package
library(sqldf)
# creates a new data frame containing only the x column
subset_df <- sqldf("SELECT x FROM df") 
# creates a new data frame containing only the x and y columns where z = "B"
subset_df <- sqldf("SELECT x, y FROM df WHERE z = 'B'") 
# creates a new data frame containing all rows where x > 2
subset_df <- sqldf("SELECT * FROM df WHERE x > 2") 

# assign purpose column from the data frame into a vector
var1 <- my_germ$purpose

# assign good_bad column from the data frame into a vector
var2 <- my_germ$good_bad

# check first 10 observations
var1[1:10]             # same as head(var1,10)

# check last 10 observations
var2[990:1000]         # same as tail(var2,11)

# choose only the first 5 rows and the first 3 columns
my_germ[1:5, 1:3]

# returns indices of NA values
which(is.na(vec)) 

# returns indices of rows where the "value" column is greater than 3
which(df$value > 3) 

# returns indices of rows where the "value" column is greater than 3 and the "category" column is "B"
which(df$value > 3 & df$category == "B") 

# returns indices of rows where the "fruit" column contains the string "an"
which(grepl("an", df$fruit)) 

# returns the index of the first value equal to 3
which.max(vec == 3) 

# assign client with good condition to good_client
good_client <- my_germ[which(my_germ$good_bad == "good"),]

# show data frame with chosen columns and matching purpose_fixed with 10.
# without subset R will collapse the views
my_germ[which(my_germ$purpose_fixed == 10),c("duration",
                                             "good_bad", 
                                             "purpose", 
                                             "amount",
                                             "age",
                                             "purpose_fixed")]

### Session 7 - Combine Data Frames
###
###############################################################################
# combines df1 and df2 by rows
df_combined <- rbind(df1, df2) 

# combines df1 and df3 by columns
df_combined <- cbind(df1, df3) 

# merges df1 and df2 based on the "id" column
df_merged <- merge(df1, df2, by = "id") 

library(dplyr)
# joins df1 and df2 based on the "id" column, keeping all rows from df1
df_joined <- left_join(df1, df2, by = "id") 

# combines df1 and df2 by rows
df_combined <- bind_rows(df1, df2) 

# combines df1 and df3 by columns
df_combined <- bind_cols(df1, df3) 

# left_join() - keeps all rows from df1, and only matching rows from df2
df_joined <- left_join(df1, df2, by = "id") 

# inner_join() - keeps only matching rows from both df1 and df2
df_joined <- inner_join(df1, df2, by = "id") 

# right_join() - keeps all rows from df2, and only matching rows from df1
df_joined <- right_join(df1, df2, by = "id") 

# full_join() - keeps all rows from both df1 and df2
df_joined <- full_join(df1, df2, by = "id") 

# merge() - performs an inner join by default
df_merged <- merge(df1, df2, by = "id")

# adds an ID column to indicate the source of each row
df_combined <- bind_rows(df1, df2, .id = "data_frame") 


### Session 8 - Convert Data Types
###
###############################################################################
# converts numeric vector to character vector
as.character(vec1) 

# converts character column to numeric
df$value <- as.numeric(df$value) 

# converts numeric column to character
df$value <- as.character(df$value) 

library(dplyr)
# converts character column to logical using mutate()
df <- mutate(df, value = as.logical(value)) 

# converts character column to numeric
df <- mutate_at(df, vars(value1), as.numeric) 

# converts character column to logical
df <- mutate_at(df, vars(value2), as.logical) 

# converts date string to Date object
as.Date(date_str) 

# Convert a list to a data frame using data.frame()
df <- data.frame(lst, stringsAsFactors = FALSE) # converts list to data frame

# Convert a data frame to a matrix using as.matrix()
mat <- as.matrix(df) # converts data frame to matrix

# Convert a matrix to a data frame using as.data.frame()
df <- as.data.frame(mat, stringsAsFactors = FALSE) # converts matrix to data frame

# Convert a character vector to a factor using factor()
vec <- c("A", "B", "C", "A", "B", "C")
fct <- factor(vec) # converts character vector to factor
fct <- factor(vec, levels = c("A", "B", "C")) # sets the levels of the factor

# Convert a data frame column from one factor level to another using levels()
df <- data.frame(id = 1:6, value = c("A", "B", "C", "A", "B", "C"), stringsAsFactors = FALSE)
levels(df$value)[levels(df$value) == "A"] <- "D" # converts "A" level to "D"



### Session 9 - Replace Data Types
###
###############################################################################
# Replace a pattern in a string with another pattern
# replace "quick" with "slow"
new_text <- gsub("quick", "slow", text) 

# Replace a pattern using regular expressions
text <- "The price is $10.50"
# replace the dollar amount with "X dollars"
new_text <- gsub("\\$([0-9]+)\\.[0-9]+", "\\1 dollars", text) 
# Output: "The price is 10 dollars"

# Replace multiple patterns using a named vector
text <- "The quick brown fox jumps over the lazy dog"
patterns <- c("quick" = "slow", "fox" = "cat")
# replace "quick" with "slow" and "fox" with "cat"
new_text <- gsub(names(patterns), patterns, text) 
# Output: "The slow brown cat jumps over the lazy dog"

# Replace values in a vector
vec <- c(1, 2, 3, NA, 5)
# replace NA values with 0
vec[is.na(vec)] <- 0 
# replace 2 with 4
vec[vec == 2] <- 4 
# replace 1, 3, 5 with NA
vec[vec %in% c(1, 3, 5)] <- NA 

# Replace values in a data frame
df <- data.frame(id = 1:5, value = c(1, 2, 3, NA, 5))
# replace NA values with 0
df$value[is.na(df$value)] <- 0 
# replace 2 with 4
df$value[df$value == 2] <- 4 
# replace 1, 3, 5 with NA
df$value[df$value %in% c(1, 3, 5)] <- NA 

# Replace values based on a condition using ifelse()
vec <- c(1, 2, 3, 4, 5)
# replace values > 3 with 10
vec <- ifelse(vec > 3, 10, vec) 

# Replace values using recode() from the dplyr package
library(dplyr)
vec <- c(1, 2, 3, 4, 5)
# replace 4 with 10 and 5 with 11
vec <- recode(vec, "4" = "10", "5" = "11") 

# Replace values using the replace() function
vec <- c(1, 2, 3, NA, 5)
# replace NA values with 0
vec <- replace(vec, is.na(vec), 0) 

# Replace values in a data frame using the replace() function
df <- data.frame(id = 1:5, value = c(1, 2, 3, NA, 5))
# replace NA values with 0
df$value <- replace(df$value, is.na(df$value), 0) 

# Replace values using the ifelse() function with multiple conditions
vec <- c(1, 2, 3, 4, 5)
# replace values based on multiple conditions
vec <- ifelse(vec < 2, "low", ifelse(vec > 4, "high", "medium")) 

# Replace values using the mapvalues() function from the plyr package
library(plyr)
vec <- c(1, 2, 3, 4, 5)
# replace 2 with 10 and 3 with 11
vec <- mapvalues(vec, from = c(2, 3), to = c(10, 11)) 

# Replace values using the mutate() function from the dplyr package
library(dplyr)
df <- data.frame(id = 1:5, value = c(1, 2, 3, NA, 5))
# replace NA values with 0 using if_else()
df <- mutate(df, value = if_else(is.na(value), 0, value)) 

# Replace values in a vector
vec <- c(1, 2, 3, NA, 5)
# replace NA values with 0
vec[is.na(vec)] <- 0 
# replace 2 with 4
vec[vec == 2] <- 4 
# replace 1, 3, 5 with NA
vec[vec %in% c(1, 3, 5)] <- NA 

# Replace values in a data frame
df <- data.frame(id = 1:5, value = c(1, 2, 3, NA, 5))
# replace NA values with 0
df$value[is.na(df$value)] <- 0 
# replace 2 with 4
df$value[df$value == 2] <- 4 
# replace 1, 3, 5 with NA
df$value[df$value %in% c(1, 3, 5)] <- NA 

# Replace values based on a condition using ifelse()
vec <- c(1, 2, 3, 4, 5)
# replace values > 3 with 10
vec <- ifelse(vec > 3, 10, vec) 

# Replace values using recode() from the dplyr package
library(dplyr)
vec <- c(1, 2, 3, 4, 5)
# replace 4 with 10 and 5 with 11
vec <- recode(vec, "4" = "10", "5" = "11") 


### Session 9 - Using for loop
###
###############################################################################
# Iterate over a sequence of numbers
for (i in 1:5) {
  print(i)
}

# Iterate over elements of a vector
vec <- c("apple", "banana", "pear")
for (fruit in vec) {
  print(fruit)
}

# Iterate over elements of a list
lst <- list(a = 1, b = 2, c = 3)
for (key in names(lst)) {
  print(paste(key, "=", lst[[key]]))
}

# Nested loops
for (i in 1:3) {
  for (j in 1:3) {
    print(paste(i, "*", j, "=", i * j))
  }
}

# Using break to exit loop
for (i in 1:10) {
  if (i == 5) {
    break
  }
  print(i)
}

# Using next to skip iteration
for (i in 1:10) {
  if (i %% 2 == 0) {
    next
  }
  print(i)
}

# Using a counter variable
count <- 0
for (i in 1:10) {
  if (i %% 2 == 0) {
    count <- count + 1
  }
}
print(count)

# Using a conditional statement to control loop
for (i in 1:10) {
  if (i %% 2 == 0) {
    next
  }
  if (i == 7) {
    break
  }
  print(i)
}

# Using a for loop to create a new vector
vec <- 1:5
new_vec <- numeric(length(vec))
for (i in 1:length(vec)) {
  new_vec[i] <- vec[i] * 2
}
print(new_vec)

### with try
# Basic tryCatch statement within a loop
for (i in 1:5) {
  tryCatch({
    result <- 10/i
    print(result)
  }, error = function(e) {
    print(paste("Error:", e$message))
  })
}

# Using a counter to limit number of tries
count <- 0
for (i in 1:10) {
  result <- tryCatch({
    count <- count + 1
    10 / (i - 5)
  }, error = function(e) {
    print(paste("Error:", e$message))
    NA
  })
  if (!is.na(result)) {
    print(result)
  }
  if (count >= 3) {
    break
  }
}

# Using try() function within a loop
for (i in 1:5) {
  result <- try(10/i)
  if (class(result) == "try-error") {
    print(paste("Error:", result$message))
  } else {
    print(result)
  }
}

# Using a nested loop with tryCatch
for (i in 1:3) {
  for (j in 1:3) {
    result <- tryCatch({
      10 / (i - j)
    }, error = function(e) {
      NA
    })
    if (!is.na(result)) {
      print(result)
    }
  }
}

# Create a sample data frame
df <- data.frame(x = 1:10, y = c(5, 7, NA, 8, 4, NA, 2, 9, NA, 3))

# Use a for loop to perform multiple tasks
for (i in 1:ncol(df)) {
  
  # Subset data based on missing values
  missing <- is.na(df[, i])
  print(paste("Column", i, "has", sum(missing), "missing values."))
  
  # Replace missing values with median
  if (sum(missing) > 0) {
    median_val <- median(df[!missing, i])
    df[missing, i] <- median_val
    print(paste("Missing values in column", i, "replaced with median", median_val))
  }
  
  # Convert data type to numeric
  if (is.factor(df[, i])) {
    df[, i] <- as.numeric(as.character(df[, i]))
    print(paste("Column", i, "converted to numeric."))
  }
  
  # Calculate mean, standard deviation, and variance
  mean_val <- mean(df[, i], na.rm = TRUE)
  sd_val <- sd(df[, i], na.rm = TRUE)
  var_val <- var(df[, i], na.rm = TRUE)
  print(paste("Mean:", mean_val, "SD:", sd_val, "Variance:", var_val))
  
  # Calculate covariance with column 1
  if (i != 1) {
    cov_val <- cov(df[, 1], df[, i], use = "pairwise.complete.obs")
    print(paste("Covariance between column 1 and column", i, "is", cov_val))
  }
  
  # Normalize and standardize data
  if (is.numeric(df[, i])) {
    norm_val <- (df[, i] - min(df[, i], na.rm = TRUE)) / (max(df[, i], na.rm = TRUE) - min(df[, i], na.rm = TRUE))
    stand_val <- (df[, i] - mean(df[, i], na.rm = TRUE)) / sd(df[, i], na.rm = TRUE)
    df[, paste0("norm_", i)] <- norm_val
    df[, paste0("stand_", i)] <- stand_val
    print(paste("Normalized column", i, "and added new column norm_", i))
    print(paste("Standardized column", i, "and added new column stand_", i))
  }
  
  # Perform a calculation
  if (i == 2) {
    new_col <- df[, 1] * df[, 2]
    df <- cbind(df, new_col)
    print("New column added to data frame.")
  }
}



### Session 9 - Using if statement
###
###############################################################################
# Basic if statement
x <- 10
if (x > 5) {
  print("x is greater than 5")
}

# If-else statement
y <- 2
if (y > 5) {
  print("y is greater than 5")
} else {
  print("y is less than or equal to 5")
}

# Multiple conditions with else-if statements
z <- 3
if (z > 5) {
  print("z is greater than 5")
} else if (z > 2) {
  print("z is greater than 2 but less than or equal to 5")
} else {
  print("z is less than or equal to 2")
}

# Using the logical operators AND and OR
a <- 5
b <- 7
if (a > 2 & b > 6) {
  print("Both conditions are true")
}
if (a > 2 | b < 5) {
  print("At least one condition is true")
}

# Using the NOT operator
c <- 2
if (!c > 5) {
  print("c is less than or equal to 5")
}

# Nested if statements
d <- 10
if (d > 5) {
  if (d < 15) {
    print("d is between 5 and 15")
  } else {
    print("d is greater than or equal to 15")
  }
} else {
  print("d is less than or equal to 5")
}



### Session 10 - Correlation
###
###############################################################################
# Calculating correlation matrix for all variables
cor_matrix <- cor(mydata)

# Calculating correlation between two variables
cor(mydata$var1, mydata$var2)

# for heatmap
library(corrplot)
corrplot(cor(test_df), is.corr = FALSE, method="square")

# Calculating partial correlation between two variables controlling for other variables
library(ppcor)
pcor(mydata$var1, mydata$var2, mydata[,c("var3","var4")])

# Calculating pairwise correlation between all pairs of variables
library(psych)
pairs.panels(mydata)

# Calculating correlation between two variables with missing data
library(Hmisc)
rcorr(cbind(mydata$var1, mydata$var2), na.action=na.omit)

# Calculating Spearman rank correlation coefficient between two variables
cor(mydata$var1, mydata$var2, method = "spearman")

# Calculating Kendall's tau correlation coefficient between two variables
cor(mydata$var1, mydata$var2, method = "kendall")

# Calculating point-biserial correlation coefficient between two variables
library(psych)
pointbiserial(mydata$var1, mydata$var2)

# Calculating biserial correlation coefficient between two variables
library(psych)
biserial(mydata$var1, mydata$var2)

# Calculating Phi coefficient of association between two nominal variables
library(polycor)
phi(matrix(c(mydata$var1, mydata$var2), ncol=2))

# Calculating Cramer's V coefficient of association between two nominal variables
library(vcd)
assocstats(table(mydata$var1, mydata$var2))



### Session 11 - Normalize dataframe
###
###############################################################################
# Select only numeric columns
num_cols <- sapply(df, is.numeric)

# Normalize numeric columns and add new columns with "_norm" suffix
for (i in names(df)[num_cols]) {
  norm_col_name <- paste0(i, "_norm")
  df[[norm_col_name]] <- scale(df[[i]])
}

# Normalize and standardize data
if (is.numeric(df[, i])) {
  norm_val <- (df[, i] - min(df[, i], na.rm = TRUE)) / (max(df[, i], na.rm = TRUE) - min(df[, i], na.rm = TRUE))
  stand_val <- (df[, i] - mean(df[, i], na.rm = TRUE)) / sd(df[, i], na.rm = TRUE)
  df[, paste0("norm_", i)] <- norm_val
  df[, paste0("stand_", i)] <- stand_val
  print(paste("Normalized column", i, "and added new column norm_", i))
  print(paste("Standardized column", i, "and added new column stand_", i))
}


# define function first then apply
normalize <- function(x, min, max){ # x is the variable vector
  min_max <- ((x- min)/(max- min))
  return(min_max)
}# closing normalize function

# normalize every observation in the df using the for loop
group_by_pub_cam$cpc_norm <- c()
group_by_pub_cam$ctr_norm <-  c()
group_by_pub_cam$tbpc_norm <- c()
group_by_pub_cam$cac_norm <- c()
group_by_pub_cam$roa_norm <- c()

# Use for loop to normalize all observations to normalize values
for (i in 1:nrow(group_by_pub_cam)){
  group_by_pub_cam$cpc_norm[i]  <-  normalize(group_by_pub_cam$cost_per_click[i], min(group_by_pub_cam$cost_per_click), max(group_by_pub_cam$cost_per_click))
  group_by_pub_cam$ctr_norm[i]  <-  normalize(group_by_pub_cam$click_thru_rate[i], min(group_by_pub_cam$click_thru_rate), max(group_by_pub_cam$click_thru_rate))
  group_by_pub_cam$tbpc_norm[i] <-  normalize(group_by_pub_cam$total_booking_per_click[i], min(group_by_pub_cam$total_booking_per_click), max(group_by_pub_cam$total_booking_per_click))
  group_by_pub_cam$cac_norm[i]  <-  normalize(group_by_pub_cam$cus_acq_cost[i], min(group_by_pub_cam$cus_acq_cost), max(group_by_pub_cam$cus_acq_cost))
  group_by_pub_cam$roa_norm[i]  <-  normalize(group_by_pub_cam$ROA[i], min(group_by_pub_cam$ROA), max(group_by_pub_cam$ROA))
}



### Session 12 - Identify all numeric columns in the data frame
###
###############################################################################
# Get a list of variable names in the global environment
variable_names <- ls(df_numeric)

# Loop through the variable names and print them out with quotation marks
for (name in variable_names) {
  cat(paste0('"', name, '",'))
}# end for loop

# i need to copy the output of the for loop and save it in this vector
heading <- c("age","amount","binary","checking","coapp","depends","duration","employed",
             "existcr","foreign","good_bad_factor","history","housing","installp","job",
             "marital","other","property","purpose_fixed","resident","savings","telephon")



### Session 13 - Handling outliers
###
###############################################################################
# Read in data and create a numeric vector for demonstration purposes
data <- read.csv("my_data.csv")
numeric_var <- data$numeric_var

# Identify outliers using a boxplot
boxplot(numeric_var, main = "Boxplot of Numeric Variable")

# Option 1: Remove outliers
qnt <- quantile(numeric_var, probs=c(.25, .75), na.rm = TRUE)
H <- 1.5 * IQR(numeric_var, na.rm = TRUE)
data_clean <- subset(data, numeric_var >= (qnt[1] - H) & numeric_var <= (qnt[2] + H))

# Option 2: Winsorize outliers
library(wqp)
data_winsorized <- winsorize(numeric_var, probs = c(0.05, 0.95))

# Option 3: Transform variable to reduce impact of outliers
data_transformed <- log(numeric_var)

# Option 4: Flag outliers as missing and impute
threshold <- 3 # Adjust as needed
data_imputed <- ifelse(abs(scale(numeric_var)) > threshold, NA, numeric_var)
data_imputed <- na.mean(data_imputed) # Replace NA with mean value

# Option 5 : Winsorization
library(DescTools)
df_norm <- df
df_norm[] <- lapply(df, function(x) {
  if (is.numeric(x)) {
    x <- winsorize(x, probs = c(0.05, 0.95))
    x_norm <- (x - min(x)) / (max(x) - min(x))
    return(x_norm)
  } else {
    return(x)
  }
})

# Option  6: Robust methods
library(robustbase)
df_norm <- df
df_norm[] <- lapply(df, function(x) {
  if (is.numeric(x)) {
    x_median <- median(x)
    x_mad <- mad(x)
    x_norm <- (x - x_median) / x_mad
    return(x_norm)
  } else {
    return(x)
  }
})

# Option 7 : Clustering
library(dbscan)
df_norm <- df
df_norm[] <- lapply(df, function(x) {
  if (is.numeric(x)) {
    x_dbscan <- dbscan(x, eps = 0.5, minPts = 5)
    x_norm <- x[x_dbscan$cluster != -1]
    x_norm <- (x_norm - min(x_norm)) / (max(x_norm) - min(x_norm))
    return(x_norm)
  } else {
    return(x)
  }
})

# Option 8 : Machine learning algorithms
library(randomForest)
df_norm <- df
df_norm[] <- lapply(df, function(x) {
  if (is.numeric(x)) {
    x_rf <- randomForest(x ~ ., data = df, proximity = TRUE)
    x_outliers <- which(x_rf$proximity[, "mean"] < mean(x_rf$proximity[, "mean"]))
    x_norm <- x[-x_outliers]
    x_norm <- (x_norm - min(x_norm)) / (max(x_norm) - min(x_norm))
    return(x_norm)
  } else {
    return(x)
  }
})




### Session 14 - Descriptive Analysis
###
###############################################################################
# Load required libraries
library(dplyr)
library(plyr)
library(psych)

# Load data
data <- read.csv("path/to/data/file.csv")

# Summary statistics
summary(data)
describe(data)

# Correlation matrix
cor(data)

# Scatter plot matrix
pairs(data)

# Box plot
boxplot(data)

# Histogram
hist(data$variable)

# Density plot
plot(density(data$variable))

# Bar plot
ggplot(data, aes(x=variable)) +
  geom_bar()

# Heatmap
library(gplots)
heatmap.2(cor(data))

# Factor plot
ggplot(data, aes(x=variable, fill=factor(grouping_variable))) +
  geom_bar(position="dodge")

# Line plot
ggplot(data, aes(x=time_variable, y=value_variable, group=grouping_variable, color=grouping_variable)) +
  geom_line()

# Stacked bar plot
ggplot(data, aes(x=variable, fill=factor(grouping_variable))) +
  geom_bar(position="stack")

# Violin plot
ggplot(data, aes(x=variable, y=value_variable)) +
  geom_violin()

# Density plot with rug plot
ggplot(data, aes(x=variable)) +
  geom_density() +
  geom_rug()

# Scatter plot with regression line
ggplot(data, aes(x=variable_1, y=variable_2)) +
  geom_point() +
  geom_smooth(method="lm")





### Session 15 - Train Test Split
###
###############################################################################
# Using caret package
library(caret)

# Splitting data into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(df$target_variable, p = 0.7, list = FALSE)
train <- df[trainIndex, ]
test <- df[-trainIndex, ]

# or in class example
# use sample for indexes
training_idx <- sample(1:1000, size=0.8*nrow(my_germ))
my_germ_train <- my_germ[training_idx,]
my_germ_test <- my_germ[-training_idx,]

# Using caTools package
library(caTools)

# Splitting data into training and testing sets
set.seed(123)
split <- sample.split(df$target_variable, SplitRatio = 0.7)
train <- subset(df, split == TRUE)
test <- subset(df, split == FALSE)

# Using rsample package
library(rsample)

# Splitting data into training and testing sets
set.seed(123)
split <- initial_split(df, prop = 0.7, strata = 'target_variable')
train <- training(split)
test <- testing(split)

# Load necessary libraries
library(dplyr)
library(tidyr)

# Load data
data <- read.csv("data.csv")

# Stratified sampling using the `dplyr` package
strat_sample <- data %>% 
  group_by(stratum) %>% 
  sample_n(size = 50)

# Stratified sampling using the `sampling` package
library(sampling)
strat_sample <- strata(data, stratanames = c("stratum"), size = c(50), method = "srswor")

# Stratified sampling using the `survey` package
library(survey)
data_design <- svydesign(ids = ~1, strata = ~stratum, data = data)
strat_sample <- svydesign(ids = ~1, strata = ~stratum, data = data_design)

# Stratified sampling using the `rsample` package
library(rsample)
data_split <- initial_split(data, prop = 0.8, strata = "stratum")
train_data <- training(data_split)
test_data <- testing(data_split)

# Stratified Sampling - in class example
install.packages("splitstackshape")
library(splitstackshape)
# sample syntax
strat_train <- stratified(as.data.frame(test_df), group=4, size=0.6)
# use actual data frame
training_testing <- stratified(as.data.frame(my_data_frame), group=4, size=0.6, bothSets = T )
# seperate data into train and test df
training_testing$SAMP1 #this is my training data
training_testing$SAMP2 #this is my testing data





### Session 16 - Train Model
###
###############################################################################
# Logistic Model
my_logit <- glm( binary ~ age + 
                   duration + 
                   checking + 
                   coapp + 
                   savings + 
                   amount,
                 data = my_germ_train, family = "binomial")

summary(my_logit)

# amount coefficient of amount is very tiny because 1 euro does not make
# any difference, if we increase the amount to 1000 of euro then it would
# increase the higher coefficient 

my_logit_norm <- glm( binary ~ age_norm + 
                        duration_norm + 
                        checking_norm + 
                        coapp_norm + 
                        savings_norm + 
                        amount_norm,
                      data = my_germ_train, 
                      family = "binomial")

summary(my_logit_norm)

#install.packages("caret")
library(caret)

my_predict <- predict(my_logit, my_germ_test, type="response")
my_predict

confusionMatrix(data=as.factor(as.numeric(my_predict > 0.35)),
                reference = as.factor(as.numeric(my_germ_test$binary)))

my_germ$binary[15]

# Gini Decision Tree Model
library(rpart)
library(rpart.plot)

my_tree <- rpart(binary ~ age + 
                   duration + 
                   checking + 
                   coapp + 
                   savings + 
                   amount, 
                 data= my_germ, method = "class",
                 control = rpart.control(minsplit = 20,
                                         minbucket = 15,
                                         cp=0.01))

rpart.plot(my_tree, type=1, extra=1)

my_tree_pred <- predict(my_tree, my_germ_test, type= "prob")

confusionMatrix(data= as.factor(as.numeric(my_tree_pred[,2]>0.5)),
                reference = as.factor(as.numeric(my_germ_test$binary)))