install.packages("mosaic")
install.packages("reshape2")

library(reshape2)

#import the dataset and data cleaning
credit_card <- read.csv("data/original/default of credit card clients.csv", skip = 1)


# ======== inspect the dataset ========

# Display the structure of the dataset
str(credit_card)

# Display the first six rows
head(credit_card)

# Display the dimensions
dim(credit_card)

# Display column names
names(credit_card)


# ======== Check missing values ========

# Count missing values in each column
colSums(is.na(credit_card))

# Count the total missing values
sum(is.na(credit_card))


# ======== Check duplicated records ========

# Count duplicated rows
sum(duplicated(credit_card))

# Check duplicated customer IDs
sum(duplicated(credit_card$ID))


# ======== Inspect payment status values ========

unique(credit_card$PAY_0)
unique(credit_card$PAY_2)
unique(credit_card$PAY_3)
unique(credit_card$PAY_4)
unique(credit_card$PAY_5)
unique(credit_card$PAY_6)


# ======== Identify potential outliers ========

# Select numerical variables for outlier analysis
numeric_vars <- c(
  "LIMIT_BAL", "AGE",
  "BILL_AMT1", "BILL_AMT2", "BILL_AMT3",
  "BILL_AMT4", "BILL_AMT5", "BILL_AMT6",
  "PAY_AMT1", "PAY_AMT2", "PAY_AMT3",
  "PAY_AMT4", "PAY_AMT5", "PAY_AMT6"
)

# Display boxplots
par(mfrow = c(3, 5))

for (variable in numeric_vars) {
  boxplot(
    credit_card[[variable]],
    main = variable,
    col = "lightblue"
  )
}

par(mfrow = c(1, 1))


# ======== Detect outliers using IQR ========

count_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  
  IQR_value <- IQR(x, na.rm = TRUE)
  
  lower_limit <- Q1 - 1.5 * IQR_value
  upper_limit <- Q3 + 1.5 * IQR_value
  
  sum(x < lower_limit | x > upper_limit, na.rm = TRUE)
}

# Count potential outliers in each variable
outlier_counts <- sapply(
  credit_card[numeric_vars],
  count_outliers
)

outlier_counts


# ======== Calculate IQR boundaries ========

calculate_iqr <- function(x) {
  
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  
  IQR_value <- IQR(x, na.rm = TRUE)
  
  lower_limit <- Q1 - 1.5 * IQR_value
  upper_limit <- Q3 + 1.5 * IQR_value
  
  c(
    lower_limit = lower_limit,
    upper_limit = upper_limit
  )
}

# Calculate boundaries for each numeric variable
iqr_boundaries <- t(sapply(
  credit_card[numeric_vars],
  calculate_iqr
))

iqr_boundaries


# ======== Inspect potential outliers ========

# Display the highest credit limits
head(
  credit_card[order(-credit_card$LIMIT_BAL),
              c("ID", "LIMIT_BAL")],
  10
)

# Display the highest bill amounts
head(
  credit_card[order(-credit_card$BILL_AMT1),
              c("ID", "BILL_AMT1")],
  10
)

# Display the highest payment amounts
head(
  credit_card[order(-credit_card$PAY_AMT1),
              c("ID", "PAY_AMT1")],
  10
)


# ======== Categorical Data Validation ========

# Identify categorical variables
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

# Display unique values in categorical variables
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

# Display summary statistics
summary(credit_card[numeric_vars])

# Check invalid or unusual ages
sum(credit_card$AGE <= 0)

# Check negative credit limits
sum(credit_card$LIMIT_BAL < 0)


# ======== Investigate negative bill amounts ========

# Count negative values in each bill variable
negative_bill_counts <- sapply(
  credit_card[paste0("BILL_AMT", 1:6)],
  function(x) sum(x < 0)
)

negative_bill_counts

# Display records with negative bill amounts
negative_bill_records <- credit_card[
  credit_card$BILL_AMT1 < 0,
  c("ID", "BILL_AMT1", "BILL_AMT2", "PAY_AMT1")
]

head(negative_bill_records, 10)


# ======== Count customers with negative bill amounts ========

# Check how many customers have at least one negative bill
negative_bill_customers <- rowSums(
  credit_card[paste0("BILL_AMT", 1:6)] < 0
)

sum(negative_bill_customers > 0)

# Check how many negative bill months each customer has
table(negative_bill_customers)


# ======== Investigate negative payment amounts ========

negative_payment_counts <- sapply(
  credit_card[paste0("PAY_AMT", 1:6)],
  function(x) sum(x < 0)
)

negative_payment_counts


# ======== Investigate unusual categorical values ========

# Count unusual education values
sum(credit_card$EDUCATION %in% c(0, 5, 6))

# Count unusual marriage values
sum(credit_card$MARRIAGE == 0)

# Display records with unusual education values
education_unusual <- credit_card[
  credit_card$EDUCATION %in% c(0, 5, 6),
]

head(education_unusual, 10)

# Display records with unusual marriage values
marriage_unusual <- credit_card[
  credit_card$MARRIAGE == 0,
]

head(marriage_unusual, 10)


# ======== Data Cleaning ========

# Keep a copy of the original data before cleaning
credit_raw <- credit_card

# EDUCATION: values 0, 5 and 6 are not documented
# (1 = graduate school, 2 = university, 3 = high school, 4 = others)
# Group them into category 4 (others)
credit_card$EDUCATION[credit_card$EDUCATION %in% c(0, 5, 6)] <- 4

# MARRIAGE: value 0 is not documented
# (1 = married, 2 = single, 3 = others)
# Group it into category 3 (others)
credit_card$MARRIAGE[credit_card$MARRIAGE == 0] <- 3

# Check the categories after cleaning
table(credit_card$EDUCATION)
table(credit_card$MARRIAGE)

# Outliers: the values identified by the IQR method were kept.
# High credit limits, bills and payments are realistic for financial data
# (right-skewed distributions), so they are not data entry errors.

# Negative bill amounts were kept: they represent a credit balance
# (the customer paid more than the bill amount).

# Confirm the size of the cleaned dataset (requirement: >= 3000 rows, >= 7 columns)
dim(credit_card)

# Save the cleaned dataset
dir.create("data/clean", showWarnings = FALSE, recursive = TRUE)
write.csv(credit_card, "data/clean/credit_card_clean.csv", row.names = FALSE)


# ============================================================
# EXPLORATORY DATA ANALYSIS (EDA)
# ============================================================
# Main question: what customer characteristics and payment
# behaviours are linked to default next month?
# I split it into smaller questions below, one per section.


# ======== Q1: Education, marriage, sex and default ========
# Do some groups of customers default more than others?
# First I look at how big each group is, then I compare
# the default rate inside each group.

# How many customers are in each category (proportions)
prop.table(table(credit_card$EDUCATION))
prop.table(table(credit_card$MARRIAGE))
prop.table(table(credit_card$SEX))

# Overall default rate, to have a reference value
prop.table(table(credit_card$default.payment.next.month))

# Counts of default (0/1) in each category
table(credit_card$EDUCATION, credit_card$default.payment.next.month)
table(credit_card$MARRIAGE, credit_card$default.payment.next.month)
table(credit_card$SEX, credit_card$default.payment.next.month)

# Default rate per category (margin = 1 so each row adds up to 1)
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

# Finding:


# ======== Q2: Credit limit and default ========
# Do customers who default have lower credit limits?

boxplot(
  LIMIT_BAL ~ default.payment.next.month,
  data = credit_card,
  main = "Credit Limit by Default Status",
  xlab = "Default Payment (0 = No, 1 = Yes)",
  ylab = "Credit Limit",
  col = "lightblue"
)

# Finding:


# ======== Q3: Age distribution ========
# What is the age profile of the customers?

hist(
  credit_card$AGE,
  main = "Age Distribution of Credit Card Clients",
  xlab = "Age",
  ylab = "Number of Customers",
  col = "lightblue",
  breaks = 15
)

# Finding:


# ======== Q4: Previous payment status and default ========
# If a customer was late paying last month, are they more
# likely to default next month?
# PAY_0 codes: -2 = no consumption, -1 = paid in full,
# 0 = revolving credit, 1 to 8 = months of delay

# Counts per payment status
table(credit_card$PAY_0, credit_card$default.payment.next.month)

# Default rate per payment status
default_by_pay <- prop.table(
  table(credit_card$PAY_0, credit_card$default.payment.next.month),
  margin = 1
)

default_by_pay

# Bar chart with only the default rate (column "1")
barplot(
  default_by_pay[, "1"],
  main = "Default Rate by Previous Payment Status",
  xlab = "Previous Payment Status",
  ylab = "Default Rate",
  col = "lightblue",
  ylim = c(0, 1)
)

# Finding:


# ======== Q5: Bill amount and default ========
# Do customers who default have higher or lower bills?

boxplot(
  BILL_AMT1 ~ default.payment.next.month,
  data = credit_card,
  main = "Bill Amount by Default Status",
  xlab = "Default Payment (0 = No, 1 = Yes)",
  ylab = "Bill Amount",
  col = "lightblue"
)

# The boxplot is hard to read because of the outliers,
# so I also compare the medians for the six months
aggregate(
  credit_card[paste0("BILL_AMT", 1:6)],
  by = list(Default = credit_card$default.payment.next.month),
  FUN = median
)

# Finding:


# ======== Q6: Payment amount and default ========
# Do customers who default pay smaller amounts?
# I used the median because payments are very skewed

aggregate(
  credit_card[paste0("PAY_AMT", 1:6)],
  by = list(Default = credit_card$default.payment.next.month),
  FUN = median
)

# Finding:


# ======== Q7: Correlation between months ========
# Are the amounts of one month related to the next months?
# (covariation between features)

# Payments
payment_vars <- credit_card[paste0("PAY_AMT", 1:6)]
payment_correlation <- cor(payment_vars)
payment_correlation

# Bills
bill_vars <- credit_card[paste0("BILL_AMT", 1:6)]
bill_correlation <- cor(bill_vars)
bill_correlation

# Heatmap for the bill correlation
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

# Variable names on both axes (same order as the matrix)
axis(1, at = 1:6, labels = colnames(bill_correlation))
axis(2, at = 1:6, labels = rownames(bill_correlation))

# Correlation value inside each square
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

# Finding:


# ======== Q8: Payments over the six months ========
# Does the payment behaviour change over time, and is it
# different for customers who default?

# Wide to long format: one row per customer per month
payment_long <- melt(
  credit_card,
  id.vars = c("ID", "default.payment.next.month"),
  measure.vars = paste0("PAY_AMT", 1:6),
  variable.name = "Payment_Month",
  value.name = "Payment_Amount"
)

# Quick check of the new format
head(payment_long)
dim(payment_long)
str(payment_long)

# Median payment for each month and each default group
payment_medians <- aggregate(
  Payment_Amount ~ Payment_Month + default.payment.next.month,
  data = payment_long,
  FUN = median
)

payment_medians

# Back to wide format: one column per default group
payment_medians_wide <- dcast(
  payment_medians,
  Payment_Month ~ default.payment.next.month,
  value.var = "Payment_Amount"
)

payment_medians_wide

# Matrix with the two groups to plot both lines together
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

# Finding:


# ======== Q9: Age and credit limit ========
# Do older customers get higher limits? Does it look
# different for the customers who default?

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

# Finding:


# ======== Q10: Default rate by age ========
# Does the chance of default change with age?
# The mean of a 0/1 variable is the default rate

default_by_age <- aggregate(
  credit_card$default.payment.next.month,
  by = list(Age = credit_card$AGE),
  FUN = mean
)

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

# Note: very old ages have few customers, so the line jumps
# a lot there - those spikes are not reliable

# Finding:


# ============================================================
#============== ENCODING CATEGORICAL VARIABLES ===============
# ============================================================


# Copy of the cleaned data, so the EDA data stays the same
credit_encoded <- credit_card

# SEX has only two values -> binary (label) encoding
# 1 = female, 0 = male
credit_encoded$SEX_FEMALE <- ifelse(credit_encoded$SEX == 2, 1, 0)

# EDUCATION and MARRIAGE have no natural order -> one-hot encoding
credit_encoded$EDUCATION <- factor(
  credit_encoded$EDUCATION,
  levels = 1:4,
  labels = c("GradSchool", "University", "HighSchool", "Other")
)

credit_encoded$MARRIAGE <- factor(
  credit_encoded$MARRIAGE,
  levels = 1:3,
  labels = c("Married", "Single", "Other")
)

# One 0/1 column for each category
education_dummies <- model.matrix(~ EDUCATION - 1, data = credit_encoded)
marriage_dummies  <- model.matrix(~ MARRIAGE - 1, data = credit_encoded)

credit_encoded <- cbind(credit_encoded, education_dummies, marriage_dummies)

# Remove the original columns (they are encoded now)
credit_encoded$SEX       <- NULL
credit_encoded$EDUCATION <- NULL
credit_encoded$MARRIAGE  <- NULL

# PAY_0 to PAY_6 are ordinal (months of delay),
# so they keep their numeric values

# Check the result
head(credit_encoded)
dim(credit_encoded)

