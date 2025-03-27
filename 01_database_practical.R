#=================================================================================================
# Step 1: Install and Load required packages
required_packages <- c("DBI", "RSQLite", "tidyverse")

install_if_missing <- function(packages) {
  missing_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  if(length(missing_packages)) {
    install.packages(missing_packages)
  }
}

install_if_missing(required_packages)

lapply(required_packages, library, character.only = TRUE)

set.seed(123)



#=================================================================================================
# Step 2: Create mock Data and Database

# Define countries and number of observations per country
countries <- c("Senegal", "India", "Ethiopia")
n_per_country <- 300

# Target table with apparent intakes
targets <- do.call(rbind, lapply(countries, function(ctry) {
  data.frame(
    country = rep(ctry, n_per_country),
    hhid = 1:n_per_country,
    zinc = rnorm(n_per_country, mean = 90, sd = 10),
    iron = rnorm(n_per_country, mean = 100, sd = 15),
    folate = rnorm(n_per_country, mean = 12, sd = 3)
  )
}))

# hh info table with household information
hh_info <- do.call(rbind, lapply(countries, function(ctry) {
  data.frame(
    country = rep(ctry, n_per_country),
    hhid = 1:n_per_country,
    sex = sample(c("Male", "Female"), n_per_country, replace = TRUE),
    age = sample(30:80, n_per_country, replace = TRUE),
    education = sample(c(0, 1), n_per_country, replace = TRUE)
  )
}))

# Connect to an in-memory SQLite database
MIMI_DB <- dbConnect(RSQLite::SQLite(), ":memory:")

# Write the data frames to the SQLite database as tables
dbWriteTable(MIMI_DB, "targets", targets, overwrite = TRUE)
dbWriteTable(MIMI_DB, "hh_info", hh_info, overwrite = TRUE)



#===============================================================================================
# -------------------------------------------- Sample Queries --------------------------------------
# Query 1: Join targets and hh_info on country and hhid
query <- "
SELECT 
    t.*,
    h.sex,
    h.age,
    h.education
FROM 
    targets AS t
INNER JOIN 
    hh_info AS h
ON 
    t.country = h.country 
    AND t.hhid = h.hhid;
"

# Execute the query
result <- dbGetQuery(MIMI_DB, query)

# Display the first few rows of the result
print(head(result))

#---------------------------------------------------------------------------------------------
# Query 2
# Query for Senegal and zinc values from the targets table
query_senegal <- "SELECT hhid, zinc FROM targets WHERE country = 'Senegal';"

senegal_zinc <- dbGetQuery(MIMI_DB, query_senegal)

# Plot the histogram
ggplot(senegal_zinc, aes(x = zinc)) +
  geom_histogram(binwidth = 2, fill = "steelblue", color = "black") +
  labs(
    title = "Zinc Distribution in Senegal",
    x = "Zinc Level",
    y = "Frequency"
  ) +
  theme(
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    plot.title = element_text(hjust = 0.5) 
  )

#---------------------------------------------------------------------------------------------
# Query 2
# Query: Join the targets and hh_info tables for India and select iron, age, sex, and education
query_india <- "
SELECT 
    t.iron,
    h.age,
    h.sex,
    h.education
FROM 
    targets AS t
INNER JOIN 
    hh_info AS h
ON 
    t.country = h.country 
    AND t.hhid = h.hhid
WHERE 
    t.country = 'India';
"

india_data <- dbGetQuery(MIMI_DB, query_india)

# Plot: Scatter plot of iron levels vs. age, with points colored by sex
ggplot(india_data, aes(x = age, y = iron, color = sex)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "black") +
  labs(
    title = "Iron Levels vs Age in India",
    x = "Age",
    y = "Iron Level"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)  # Center the title
  )


## Other forms of queries for calculations
# ---------------------------------------------------------------------------------------------
# Query 1: Simple Count
# How many households are in Senegal?
# -----------------------------
query1 <- "
SELECT COUNT(*) AS total_households
FROM targets
WHERE country = 'Senegal';
"
result1 <- dbGetQuery(MIMI_DB, query1)
print("Query 1: Total households in Senegal")
print(result1)

# -----------------------------
# Query 2: Filtering with a Calculation
# What percent of households in India have iron below 30?
# -----------------------------
query2 <- "
SELECT 100.0 * SUM(CASE WHEN iron < 80 THEN 1 ELSE 0 END) / COUNT(*) AS percent_iron_below_80
FROM targets
WHERE country = 'India';
"
result2 <- dbGetQuery(MIMI_DB, query2)
print("Query 2: Percentage of households in India with iron below 80")
print(result2)

# -----------------------------
# Query 3: Joining with a MIMI_DBdition
# What is the average zinc level for male households in Ethiopia?
# -----------------------------
query3 <- "
SELECT AVG(t.zinc) AS avg_zinc
FROM targets AS t
JOIN hh_info AS h
  ON t.country = h.country AND t.hhid = h.hhid
WHERE t.country = 'Ethiopia' 
  AND h.sex = 'Male';
"
result3 <- dbGetQuery(MIMI_DB, query3)
print("Query 3: Average zinc level for male households in Ethiopia")
print(result3)

# -----------------------------
# Query 4: Grouping by a Categorical Variable
# What is the average folate level in India by education group?
# -----------------------------
query4 <- "
SELECT h.education, AVG(t.folate) AS avg_folate
FROM targets AS t
JOIN hh_info AS h
  ON t.country = h.country AND t.hhid = h.hhid
WHERE t.country = 'India'
GROUP BY h.education;
"
result4 <- dbGetQuery(MIMI_DB, query4)
print("Query 4: Average folate level in India by education group")
print(result4)

# -----------------------------
# Query 5: More Complex Aggregation Across Groups
# For each country, what is the average age and the average iron level for households with education = 1?
# -----------------------------
query5 <- "
SELECT t.country,
       AVG(h.age) AS avg_age,
       AVG(t.iron) AS avg_iron
FROM targets AS t
JOIN hh_info AS h
  ON t.country = h.country AND t.hhid = h.hhid
WHERE h.education = 1
GROUP BY t.country;
"
result5 <- dbGetQuery(MIMI_DB, query5)
print("Query 5: Average age and average iron level for households with education = 1, by country")
print(result5)



#==============================================================================================================
# Q1: Extract dataframe "Ethiopia_folate", should contain hhid, folate and zinc levels levels. Then create a scatter plot
# Bonus point: What can you possibly tell from the scatter plot
# Q2: Extract dataframe "Senegal_data, should contain hhid, folate, education and age variables for senegal. Create any plot of choice


## Always remember to disconnect from a database after use, so you don't have admin looking for you. Find the command so that you remember better !!!!