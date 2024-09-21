#install these packages ####
install.packages("dplyr")
install.packages("knitr")
install.packages("stargazer")
install.packages("gt")
install.packages("kableExtra")

# Load necessary libraries
library(dplyr)      # for data cleaning
library(knitr)      # For cleaning up table output
library(gt)         # For regression 
library(kableExtra) # For exporting to .html

#################################### clean up data ####

# Load the CSV file and process the data
cd <- read.csv("Census 2016.csv")%>%
  select(TotInc, Sex, AGEIMM, HDGREE, YRIMM) %>%   # Select specified variables
  filter(AGEIMM >= 5 & AGEIMM <= 13,               # Filter AGEIMM range 5 to 13
         YRIMM >= 2000 & YRIMM <= 2015,            # Filter YRIMM range 2000 to 2015
         !(TotInc %in% c(33, 88888888, 99999999)), # Remove TotInc = 33, 88888888, 99999999
         TotInc >= 1000,                           # Remove cases with TOTINC less than 1000
         !(HDGREE %in% c(88, 99)))                 # Remove cases with HDGREE = 88 or 99
         
glimpse(cd)
print(cd)

################################ re-code sex and education ####

# re-code variables
cd[] <- lapply(cd, as.numeric)     #convert all variables to numeric

cd <- cd %>%
  mutate(Sex = case_when(          # Recoding Sex variable
    Sex == 1 ~ "Female",
    Sex == 2 ~ "Male"),
    Sex = factor(Sex, levels = c("Female", "Male")))

cd <- cd %>%
  mutate(HDGREE.R = case_when(                              # Creating HDGREE.R variable
           HDGREE >= 1 & HDGREE <= 8 ~ 1,                   # 1 to 8 = below bachelor
           HDGREE == 9 ~ 2,                                 # 9 = bachelor
           HDGREE >= 10 & HDGREE <= 13 ~ 3),                # 10 to 13 = above bachelor
         HDGREE.R = factor(HDGREE.R,                        # Adding labels to HDGREE.R
                           levels = c(1, 2, 3),             # Defining levels for the factor
                           labels = c("Below Bachelor",     # Adding labels for each level
                                      "Bachelor",
                                      "Above Bachelor")))
print(cd)

################################## Discriptive statistics ####

# Summary statistics for Sex
sex_summary <- cd %>%
  group_by(Sex) %>%
  summarise(
    Frequency = n()
  ) %>%
  mutate(Percentage = round((Frequency / sum(Frequency)) * 100, 2))

# Adding a row for total
sex_summary <- sex_summary %>%
  bind_rows(summarise(., Sex = "Total", Frequency = sum(Frequency), Percentage = sum(Percentage)))

# Summary statistics for HDGREE.R
hdgree_summary <- cd %>%
  group_by(HDGREE.R) %>%
  summarise(
    Frequency = n()
  ) %>%
  mutate(Percentage = round((Frequency / sum(Frequency)) * 100, 2))

# Adding a row for total
hdgree_summary <- hdgree_summary %>%
  bind_rows(summarise(., HDGREE.R = "Total", Frequency = sum(Frequency), Percentage = sum(Percentage)))

# Print the summary tables
kable(sex_summary, caption = "Summary Statistics for Sex (Categorical Variable)")
kable(hdgree_summary, caption = "Summary Statistics for Highest Degree (Categorical Variable)")

########################## Multivariate regression analysis  ####

# Fit the linear model
regression_table_gt <- regression_table %>%
  gt() %>%
  tab_header(
    title = "Regression Results",
    subtitle = "Total Income as Dependent Variable"
  ) %>%
  fmt_number(
    columns = c(estimate, std.error, statistic, p.value), # Use c() instead of vars()
    decimals = 3
  ) %>%
  cols_label(
    term = "Term",
    estimate = "Estimate",
    std.error = "Std. Error",
    statistic = "t-value",
    p.value = "p-value"
  ) %>%
  tab_footnote(
    footnote = "Regression coefficients with standard errors.",
    locations = cells_column_labels(columns = c(estimate)) # Use c() instead of vars()
  )

# Display the formatted table
regression_table_gt


################## Save all tables into one HTML file  ####

# Create HTML content with kable and gt
html_content <- "
<html>
<head>
  <title>Combined Summary Tables</title>
</head>
<body>
  <h1>Summary Statistics for Sex (Categorical Variable)</h1>
  {sex_table}

  <h1>Summary Statistics for Highest Degree (Categorical Variable)</h1>
  {degree_table}

  <h1>Regression Table</h1>
  {regression_table}
</body>
</html>
"

# Render the kable tables as HTML
sex_table <- sex_summary %>%
  kable("html", caption = "Summary Statistics for Sex (Categorical Variable)") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  as.character()

hdgree_table <- hdgree_summary %>%
  kable("html", caption = "Summary Statistics for Highest Degree (Categorical Variable)") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  as.character()

# Convert gt table to HTML
regression_table_html <- regression_table_gt %>% 
  as_raw_html()

# Replace placeholders in HTML content
html_content <- gsub("\\{sex_table\\}", sex_table, html_content)
html_content <- gsub("\\{degree_table\\}", hdgree_table, html_content)
html_content <- gsub("\\{regression_table\\}", regression_table_html, html_content)

# Write the combined content to a file
writeLines(html_content, "combined_summary.html")

# Open the HTML file in the Viewer
rstudioapi::viewer("combined_summary.html")

#################################### End ##################



