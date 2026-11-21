# install.packages("reshape2")
# install.packages("factoextra")

library(reshape2)
library(factoextra)

# Load the credit card dataset
credit_card <- read.csv("data/original/default of credit card clients.csv", skip = 1)


# ======== inspect the dataset ========

# I start by checking the structure of the dataset
str(credit_card)

# Take a quick look at the first six rows
head(credit_card)

# Check how many rows and columns the dataset has
dim(credit_card)

# Check the names of all columns
names(credit_card)


# ======== Check missing values ========

# Check whether any columns have missing values
colSums(is.na(credit_card))

# Count all missing values in the dataset
sum(is.na(credit_card))


# ======== Check duplicated records ========

# Check if any full rows are repeated
sum(duplicated(credit_card))

# Make sure each customer ID appears only once
sum(duplicated(credit_card$ID))


# ======== Inspect payment status values ========

unique(credit_card$PAY_0)
unique(credit_card$PAY_2)
unique(credit_card$PAY_3)
unique(credit_card$PAY_4)
unique(credit_card$PAY_5)
unique(credit_card$PAY_6)


# ======== Identify potential outliers ========

# Choose the numerical columns I want to check for outliers
numeric_vars <- c(
  "LIMIT_BAL", "AGE",
  "BILL_AMT1", "BILL_AMT2", "BILL_AMT3",
  "BILL_AMT4", "BILL_AMT5", "BILL_AMT6",
  "PAY_AMT1", "PAY_AMT2", "PAY_AMT3",
  "PAY_AMT4", "PAY_AMT5", "PAY_AMT6"
)

# Use boxplots to get a first look at possible outliers
par(mfrow = c(3, 5))

for (variable in numeric_vars) {
  boxplot(
    credit_card[[variable]],
    main = variable,
    col = "lightblue"
  )
}

par(mfrow = c(1, 1))


# ======== Detect outliers and calculate IQR boundaries ========

iqr_summary <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR_value <- IQR(x, na.rm = TRUE)
  
  lower_limit <- Q1 - 1.5 * IQR_value
  upper_limit <- Q3 + 1.5 * IQR_value
  
  c(
    outliers = sum(x < lower_limit | x > upper_limit, na.rm = TRUE),
    lower_limit = lower_limit,
    upper_limit = upper_limit
  )
}

# Apply the IQR check to each numerical variable
iqr_results <- t(sapply(credit_card[numeric_vars], iqr_summary))
iqr_results

# ======== Inspect potential outliers ========

# Have a look at the customers with the highest credit limits
head(
  credit_card[order(-credit_card$LIMIT_BAL),
              c("ID", "LIMIT_BAL")],
  10
)

# Check the largest bill amounts
head(
  credit_card[order(-credit_card$BILL_AMT1),
              c("ID", "BILL_AMT1")],
  10
)

# Check the largest payment amounts
head(
  credit_card[order(-credit_card$PAY_AMT1),
              c("ID", "PAY_AMT1")],
  10
)


# ======== Categorical Data Validation ========

# List the columns that contain categories or codes
categorical_vars <- c(
  "SEX",
  "EDUCATION",
  "MARRIAGE",
  "PAY_0",
  "PAY_2",
  "PAY_3",
  "PAY_4",
  "PAY_5",
  "PAY_6",
  "default.payment.next.month"
)

# Check which values appear in each categorical column
lapply(
  credit_card[categorical_vars],
  function(x) sort(unique(x))
)


# ======== Check categorical value frequencies ========

lapply(
  credit_card[categorical_vars],
  table
)


# ======== Check numerical data ranges ========

# Get a summary of the numerical columns
summary(credit_card[numeric_vars])

# Check if there are any impossible age values
sum(credit_card$AGE <= 0)

# Check whether any credit limits are negative
sum(credit_card$LIMIT_BAL < 0)


# ======== Investigate negative bill amounts ========

# Count negative bill amounts for each of the six months
negative_bill_counts <- sapply(
  credit_card[paste0("BILL_AMT", 1:6)],
  function(x) sum(x < 0)
)

negative_bill_counts

# Look at a few customers with a negative bill amount
negative_bill_records <- credit_card[
  credit_card$BILL_AMT1 < 0,
  c("ID", "BILL_AMT1", "BILL_AMT2", "PAY_AMT1")
]

head(negative_bill_records, 10)


# ======== Count customers with negative bill amounts ========

# Find how many customers had a negative bill in at least one month
negative_bill_customers <- rowSums(
  credit_card[paste0("BILL_AMT", 1:6)] < 0
)

sum(negative_bill_customers > 0)

# See how many months had a negative bill for each customer
table(negative_bill_customers)


# ======== Investigate negative payment amounts ========

negative_payment_counts <- sapply(
  credit_card[paste0("PAY_AMT", 1:6)],
  function(x) sum(x < 0)
)

negative_payment_counts


# ======== Investigate unusual categorical values ========

# Count education codes that are not in the documented categories
sum(credit_card$EDUCATION %in% c(0, 5, 6))

# Count records with an unusual marriage code
sum(credit_card$MARRIAGE == 0)

# Take a look at some records with unusual education codes
education_unusual <- credit_card[
  credit_card$EDUCATION %in% c(0, 5, 6),
]

head(education_unusual, 10)

# Take a look at some records with an unusual marriage code
marriage_unusual <- credit_card[
  credit_card$MARRIAGE == 0,
]

head(marriage_unusual, 10)


# ======== Data Cleaning ========

# Keep the original data unchanged in case I need to compare it later
credit_raw <- credit_card

# EDUCATION: values 0, 5 and 6 are not documented
# (1 = graduate school, 2 = university, 3 = high school, 4 = others)
# Group them into category 4 (others)
credit_card$EDUCATION[credit_card$EDUCATION %in% c(0, 5, 6)] <- 4

# MARRIAGE: value 0 is not documented
# (1 = married, 2 = single, 3 = others)
# Group it into category 3 (others)
credit_card$MARRIAGE[credit_card$MARRIAGE == 0] <- 3

# Check that the categories look right after the changes
table(credit_card$EDUCATION)
table(credit_card$MARRIAGE)

# I decided to keep the outliers instead of removing them.
# Large credit limits, bills and payments can happen in real financial data,
# so I did not treat them as data entry errors.

# I kept negative bill amounts because they can represent a credit balance,
# for example, when a customer has paid more than the amount billed.

# Check the size of the cleaned dataset against the assignment requirements
dim(credit_card)

# Save a copy of the cleaned data
dir.create("data/clean", showWarnings = FALSE, recursive = TRUE)
write.csv(credit_card, "data/clean/credit_card_clean.csv", row.names = FALSE)


# ============================================================
# EXPLORATORY DATA ANALYSIS (EDA)
# ============================================================
# Main question: What customer characteristics and financial
# behaviours are associated with credit card default?
# I use smaller questions below to explore different parts of the main question.


# ======== Q1: Education, marriage, sex and default ========
# I want to see whether default rates differ between customer groups.
# First I check the size of each group, then compare
# the default rate within each group.

# Check what proportion of customers belong to each category
prop.table(table(credit_card$EDUCATION))
prop.table(table(credit_card$MARRIAGE))
prop.table(table(credit_card$SEX))

# Calculate the overall default rate as a point of comparison
prop.table(table(credit_card$default.payment.next.month))

# Count default and non-default customers in each category
table(credit_card$EDUCATION, credit_card$default.payment.next.month)
table(credit_card$MARRIAGE, credit_card$default.payment.next.month)
table(credit_card$SEX, credit_card$default.payment.next.month)

# Calculate the default rate within each category (each row adds up to 1)
prop.table(
  table(credit_card$EDUCATION, credit_card$default.payment.next.month),
  margin = 1
)

prop.table(
  table(credit_card$MARRIAGE, credit_card$default.payment.next.month),
  margin = 1
)

prop.table(
  table(credit_card$SEX, credit_card$default.payment.next.month),
  margin = 1
)

# Use a mosaic plot to compare education and default
# The width shows how many customers are in each group,
# and the height shows the proportion in each default category.
mosaicplot(
  table(credit_card$EDUCATION, credit_card$default.payment.next.month),
  main = "Education and Default Status",
  xlab = "Education (1 = Grad School, 2 = University, 3 = High School, 4 = Other)",
  ylab = "Default (0 = No, 1 = Yes)",
  col = c("steelblue", "darkorange")
)

# Compare the group results with the overall default rate
overall_default_rate <- mean(credit_card$default.payment.next.month)
overall_default_rate

# My notes: use the results above to explain any differences between the groups.


# ======== Q2: Credit limit and default ========
# I compare credit limits for customers who did and did not default.

boxplot(
  LIMIT_BAL ~ default.payment.next.month,
  data = credit_card,
  main = "Credit Limit by Default Status",
  xlab = "Default Payment (0 = No, 1 = Yes)",
  ylab = "Credit Limit",
  col = "lightblue"
)

# Compare the median credit limit for each group
aggregate(
  LIMIT_BAL ~ default.payment.next.month,
  data = credit_card,
  FUN = median
)

# My notes: describe what the boxplot and median values show.


# ======== Q3: Age distribution ========
# First, I want to understand the age range of the customers.

hist(
  credit_card$AGE,
  main = "Age Distribution of Credit Card Clients",
  xlab = "Age",
  ylab = "Number of Customers",
  col = "lightblue",
  breaks = 15
)

# Check the main age statistics
summary(credit_card$AGE)

# My notes: describe the typical ages and the shape of the histogram.


# ======== Q4: Previous payment status and default ========
# I want to check whether a late payment last month is linked to
# a higher chance of default in the following month.
# PAY_0 codes: -2 = no consumption, -1 = paid in full,
# 0 = revolving credit, 1 to 8 = months of delay

# Count customers for each previous payment status
table(credit_card$PAY_0, credit_card$default.payment.next.month)

# Work out the default rate for each payment status
default_by_pay <- prop.table(
  table(credit_card$PAY_0, credit_card$default.payment.next.month),
  margin = 1
)

default_by_pay

# Plot the default rate for each payment status
barplot(
  default_by_pay[, "1"],
  main = "Default Rate by Previous Payment Status",
  xlab = "Previous Payment Status",
  ylab = "Default Rate",
  col = "lightblue",
  ylim = c(0, 1)
)

# My notes: compare the default rates across the different payment statuses.
# A higher rate suggests an association, but it does not prove that one causes the other.


# ======== Q5: Bill amount and default ========
# Compare the bill amounts of customers who did and did not default.

boxplot(
  BILL_AMT1 ~ default.payment.next.month,
  data = credit_card,
  main = "Bill Amount by Default Status",
  xlab = "Default Payment (0 = No, 1 = Yes)",
  ylab = "Bill Amount",
  col = "lightblue"
)

# The outliers make the boxplot harder to read,
# so I also look at the median bill for each of the six months.
aggregate(
  credit_card[paste0("BILL_AMT", 1:6)],
  by = list(Default = credit_card$default.payment.next.month),
  FUN = median
)

# My notes: compare the bill amounts for the two groups across the six months.


# ======== Q6: Payment amount and default ========
# I want to see whether payment amounts differ between the two groups.
# I use the median because payment amounts are quite skewed.

aggregate(
  credit_card[paste0("PAY_AMT", 1:6)],
  by = list(Default = credit_card$default.payment.next.month),
  FUN = median
)

# My notes: compare the median payments for customers who did and did not default.


# ======== Q7: Correlation between months ========
# I check whether bill and payment amounts move together across months.
# This helps me look at how the variables are related.

# Check the relationships between monthly payments
payment_vars <- credit_card[paste0("PAY_AMT", 1:6)]
payment_correlation <- cor(payment_vars)
payment_correlation

# Check the relationships between monthly bills
bill_vars <- credit_card[paste0("BILL_AMT", 1:6)]
bill_correlation <- cor(bill_vars)
bill_correlation

# Create a heatmap to make the bill correlations easier to compare
bill_colors <- colorRampPalette(
  c("white", "lightblue", "steelblue")
)(100)

image(
  x = 1:6,
  y = 1:6,
  z = bill_correlation,
  col = bill_colors,
  axes = FALSE,
  xlab = "",
  ylab = "",
  main = "Correlation Between Monthly Bill Amounts"
)

# Add the month names to both axes
axis(1, at = 1:6, labels = colnames(bill_correlation))
axis(2, at = 1:6, labels = rownames(bill_correlation))

# Show the correlation value in each square
text(
  x = rep(1:6, each = 6),
  y = rep(1:6, times = 6),
  labels = sprintf("%.2f", as.vector(bill_correlation)),
  cex = 0.9
)

title(
  xlab = "Bill Amount Variables",
  ylab = "Bill Amount Variables"
)

# Create the same type of heatmap for monthly payments
image(
  x = 1:6,
  y = 1:6,
  z = payment_correlation,
  col = bill_colors,
  axes = FALSE,
  xlab = "",
  ylab = "",
  main = "Correlation Between Monthly Payment Amounts"
)

axis(1, at = 1:6, labels = colnames(payment_correlation))
axis(2, at = 1:6, labels = rownames(payment_correlation))

text(
  x = rep(1:6, each = 6),
  y = rep(1:6, times = 6),
  labels = sprintf("%.2f", as.vector(payment_correlation)),
  cex = 0.9
)

title(
  xlab = "Payment Amount Variables",
  ylab = "Payment Amount Variables"
)

# My notes: values closer to 1 show a stronger positive relationship.
# This shows a relationship, not that one month causes another.


# ======== Q8: Payments over the six months ========
# I compare payment patterns over the six months and check whether they are
# different for customers who defaulted.

# Reshape the data so each customer has one row for each payment month
payment_long <- melt(
  credit_card,
  id.vars = c("ID", "default.payment.next.month"),
  measure.vars = paste0("PAY_AMT", 1:6),
  variable.name = "Payment_Month",
  value.name = "Payment_Amount"
)

# Check that the reshaped data looks as expected
head(payment_long)
dim(payment_long)
str(payment_long)

# Calculate the median payment for each month and default group
payment_medians <- aggregate(
  Payment_Amount ~ Payment_Month + default.payment.next.month,
  data = payment_long,
  FUN = median
)

payment_medians

# Put the results back into a format with one column for each group
payment_medians_wide <- dcast(
  payment_medians,
  Payment_Month ~ default.payment.next.month,
  value.var = "Payment_Amount"
)

payment_medians_wide

# Prepare the two groups so I can plot them on the same chart
payment_matrix <- as.matrix(payment_medians_wide[, -1])

matplot(
  1:6,
  payment_matrix,
  type = "b",
  pch = c(16, 17),
  lty = 1,
  col = c("steelblue", "darkorange"),
  xaxt = "n",
  main = "Monthly Payment Behaviour by Default Status",
  xlab = "Payment Month",
  ylab = "Median Payment Amount"
)

axis(1, at = 1:6, labels = payment_medians_wide$Payment_Month)

legend(
  "topright",
  legend = c("No Default", "Default"),
  col = c("steelblue", "darkorange"),
  pch = c(16, 17),
  lty = 1
)

# My notes: describe how the median payment changes over the six months
# and whether the two groups follow different patterns.


# ======== Q9: Age and credit limit ========
# I check whether credit limits appear to change with age, and whether the pattern
# looks different for customers who defaulted.

plot(
  credit_card$AGE,
  credit_card$LIMIT_BAL,
  col = ifelse(
    credit_card$default.payment.next.month == 1,
    "darkorange",
    "steelblue"
  ),
  pch = 16,
  main = "Age and Credit Limit by Default Status",
  xlab = "Customer Age",
  ylab = "Credit Limit"
)

legend(
  "topright",
  legend = c("No Default", "Default"),
  col = c("steelblue", "darkorange"),
  pch = 16
)

# My notes: describe what you can see, without saying that age causes a higher limit.


# ======== Q10: Default rate by age ========
# I check how the default rate varies across customer ages.
# Because default is coded as 0 or 1, its mean gives the default rate.

default_by_age <- aggregate(
  credit_card$default.payment.next.month,
  by = list(Age = credit_card$AGE),
  FUN = mean
)

# Count customers at each age, as some rates may be based on very small groups
customers_by_age <- aggregate(
  ID ~ AGE,
  data = credit_card,
  FUN = length
)
names(customers_by_age)[2] <- "Customer_Count"
customers_by_age

plot(
  default_by_age$Age,
  default_by_age$x,
  type = "l",
  lwd = 2,
  col = "steelblue",
  main = "Default Rate by Customer Age",
  xlab = "Customer Age",
  ylab = "Default Rate",
  ylim = c(0, 1)
)

# There are fewer customers at the oldest ages, so the rate can jump around
# more in that part of the chart. Those spikes should be treated with caution.

# My notes:


# ============================================================
#============== ENCODING CATEGORICAL VARIABLES ===============
# ============================================================


# Make a separate copy for encoding, leaving the cleaned EDA data as it is
credit_encoded <- credit_card

# SEX has two categories, so I represent it with a single binary column
# Here, 1 means female and 0 means male
credit_encoded$SEX_FEMALE <- ifelse(credit_encoded$SEX == 2, 1, 0)

# EDUCATION and MARRIAGE are categories without a natural order, so I use one-hot encoding
credit_encoded$EDUCATION <- factor(
  credit_encoded$EDUCATION,
  levels = 1:4,
  labels = c("Edu_GradSchool", "Edu_University", "Edu_HighSchool", "Edu_Other")
)

credit_encoded$MARRIAGE <- factor(
  credit_encoded$MARRIAGE,
  levels = 1:3,
  labels = c("Mar_Married", "Mar_Single", "Mar_Other")
)

# Create a separate 0/1 column for each category
education_dummies <- dcast(data = credit_encoded[, c("ID", "EDUCATION")], ID ~ EDUCATION, length)
marriage_dummies  <- dcast(data = credit_encoded[, c("ID", "MARRIAGE")], ID ~ MARRIAGE, length)

credit_encoded <- merge(credit_encoded, education_dummies, by = "ID")
credit_encoded <- merge(credit_encoded, marriage_dummies, by = "ID")

# Remove the original category columns now that the encoded columns are in place
credit_encoded$SEX       <- NULL
credit_encoded$EDUCATION <- NULL
credit_encoded$MARRIAGE  <- NULL

# Payment status has an order because the values represent payment delays,
# so I leave these columns in their original numeric form.

# Take a look at the encoded dataset
head(credit_encoded)
dim(credit_encoded)

# Make sure the new dummy columns contain only 0s and 1s
dummy_vars <- c(
  "SEX_FEMALE",
  "Edu_GradSchool", "Edu_University", "Edu_HighSchool", "Edu_Other",
  "Mar_Married", "Mar_Single", "Mar_Other"
)
sapply(credit_encoded[dummy_vars], function(x) sort(unique(x)))

# Check how many customers fall into each encoded category
colSums(credit_encoded[dummy_vars])

# Save the encoded data for later use
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
write.csv(credit_encoded, "data/processed/credit_card_encoded.csv", row.names = FALSE)


# ============ PRINCIPAL COMPONENT ANALYSIS (PCA) ============

# Leave out the customer ID and the default outcome before running PCA
# ID is just an identifier, and the default outcome is the variable I am studying
# I also leave out one reference category from each one-hot group
# The reference categories here are Edu_Other and Mar_Other.
pca_data <- credit_encoded[
  , !(names(credit_encoded) %in% c(
    "ID", "default.payment.next.month", "Edu_Other", "Mar_Other"
  ))
]

# Run PCA on the selected variables
# I scale the variables because they are measured in very different units
pca_result <- prcomp(pca_data, center = TRUE, scale. = TRUE)

# Check how much variance each component explains
summary(pca_result)


# ======== Variance Explained by Components ========

# Calculate the proportion of variance explained by each component
pca_variance <- pca_result$sdev^2 / sum(pca_result$sdev^2)
cumulative_variance <- cumsum(pca_variance)

# Plot the variance explained by the first ten components
barplot(
  pca_variance[1:10],
  names.arg = paste0("PC", 1:10),
  main = "Scree Plot - Variance Explained by Each Component",
  xlab = "Principal Component",
  ylab = "Proportion of Variance",
  col = "lightblue"
)

# Plot the cumulative variance explained
plot(
  cumulative_variance,
  type = "b",
  pch = 16,
  col = "steelblue",
  main = "Cumulative Variance Explained",
  xlab = "Number of Components",
  ylab = "Cumulative Proportion of Variance",
  ylim = c(0, 1)
)

# Add a line to mark the 80% level
abline(h = 0.80, col = "darkorange", lty = 2)

# Check the cumulative variance as components are added
summary(pca_result)$importance[3, ]

# Find how many components are needed to explain at least 80% of the variance
components_80 <- which(cumulative_variance >= 0.80)[1]
components_80


# ======== Interpret PCA Loadings ========

# Look at the loadings for the first three components
round(pca_result$rotation[, 1:3], 2)

# See which variables contribute most to the first component
fviz_contrib(pca_result, choice = "var", axes = 1, top = 10)

# See which variables contribute most to the second component
fviz_contrib(pca_result, choice = "var", axes = 2, top = 10)


# ======== Customers on the First Two Components ========

plot(
  pca_result$x[, 1],
  pca_result$x[, 2],
  col = ifelse(
    credit_encoded$default.payment.next.month == 1,
    "darkorange",
    "steelblue"
  ),
  pch = 16,
  cex = 0.5,
  main = "Customers on PC1 and PC2 by Default Status",
  xlab = "PC1",
  ylab = "PC2"
)

legend(
  "topright",
  legend = c("No Default", "Default"),
  col = c("steelblue", "darkorange"),
  pch = 16
)

# Final EDA note:
# Use the results above to support the findings in my report.
# Remember that an association does not mean one variable causes the other.

