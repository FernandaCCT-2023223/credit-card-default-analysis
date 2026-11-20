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


# ======== Analyse categorical variables ========

# Check education category proportions
prop.table(table(credit_card$EDUCATION))

# Check marriage category proportions
prop.table(table(credit_card$MARRIAGE))

# Check gender category proportions
prop.table(table(credit_card$SEX))

# Check default payment proportions
prop.table(table(credit_card$default.payment.next.month))

# ======== Compare categorical variables with default ========

# Education and default payment
table(
  credit_card$EDUCATION,
  credit_card$default.payment.next.month
)

# Marriage and default payment
table(
  credit_card$MARRIAGE,
  credit_card$default.payment.next.month
)

# Sex and default payment
table(
  credit_card$SEX,
  credit_card$default.payment.next.month
)


# ======== Calculate default proportions by category ========

# Education and default proportions
prop.table(
  table(
    credit_card$EDUCATION,
    credit_card$default.payment.next.month
  ),
  margin = 1
)

# Marriage and default proportions
prop.table(
  table(
    credit_card$MARRIAGE,
    credit_card$default.payment.next.month
  ),
  margin = 1
)

# Sex and default proportions
prop.table(
  table(
    credit_card$SEX,
    credit_card$default.payment.next.month
  ),
  margin = 1
)


# ======== Credit Limit and Default Analysis ========

# Compare credit limits between customers with and without default
boxplot(
  LIMIT_BAL ~ default.payment.next.month,
  data = credit_card,
  main = "Credit Limit by Default Status",
  xlab = "Default Payment (0 = No, 1 = Yes)",
  ylab = "Credit Limit",
  col = "lightblue"
)


# ======== Age Distribution Analysis ========

# Display the distribution of customer ages
hist(
  credit_card$AGE,
  main = "Age Distribution of Credit Card Clients",
  xlab = "Age",
  ylab = "Number of Customers",
  col = "lightblue",
  breaks = 15
)


# ======== Payment Status and Default Analysis ========

# Compare payment status with default payment
table(
  credit_card$PAY_0,
  credit_card$default.payment.next.month
)

# ======== Calculate Default Proportions by Payment Status ========

# Calculate default proportions for each payment status
prop.table(
  table(
    credit_card$PAY_0,
    credit_card$default.payment.next.month
  ),
  margin = 1
)
# ======== Default Rate by Payment Status ========

# Calculate default proportions for each payment status
default_by_pay <- prop.table(
  table(
    credit_card$PAY_0,
    credit_card$default.payment.next.month
  ),
  margin = 1
)

# Display default rates by payment status
barplot(
  default_by_pay[, "1"],
  main = "Default Rate by Previous Payment Status",
  xlab = "Previous Payment Status",
  ylab = "Default Rate",
  col = "lightblue",
  ylim = c(0, 1)
)

# ======== Bill Amount and Default Analysis ========

# Compare bill amounts between customers with and without default
boxplot(
  BILL_AMT1 ~ default.payment.next.month,
  data = credit_card,
  main = "Bill Amount by Default Status",
  xlab = "Default Payment (0 = No, 1 = Yes)",
  ylab = "Bill Amount",
  col = "lightblue"
)
# ======== Payment Amount and Default Analysis ========

# Compare median payment amounts between default groups
aggregate(
  credit_card[paste0("PAY_AMT", 1:6)],
  by = list(Default = credit_card$default.payment.next.month),
  FUN = median
)
# ======== Bill Amount and Default Analysis ========

# Compare median bill amounts between default groups
aggregate(
  credit_card[paste0("BILL_AMT", 1:6)],
  by = list(Default = credit_card$default.payment.next.month),
  FUN = median
)
# ======== Payment Amount Correlation Analysis ========

# Select payment amount variables
payment_vars <- credit_card[paste0("PAY_AMT", 1:6)]

# Calculate correlation between payment variables
payment_correlation <- cor(payment_vars)

# Display correlation matrix
payment_correlation

# ======== Reshape Payment Amount Data ========

# Transform payment variables from wide to long format
payment_long <- melt(
  credit_card,
  id.vars = c("ID", "default.payment.next.month"),
  measure.vars = paste0("PAY_AMT", 1:6),
  variable.name = "Payment_Month",
  value.name = "Payment_Amount"
)

# ======== Bill Amount Correlation Analysis ========

#Select bill amount variables
bill_vars <- credit_card[paste0("BILL_AMT", 1:6)]

# Calculate correlation between bill amount variables
bill_correlation <- cor(bill_vars)

# Display correlation matrix
bill_correlation


# ======== Bill Amount Correlation Heatmap ========

# Create colour palette
bill_colors <- colorRampPalette(
  c("white", "lightblue", "steelblue")
)(100)

# Display correlation heatmap
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

# Display variable names
axis(1, at = 1:6, labels = colnames(bill_correlation))
axis(2, at = 1:6, labels = rev(rownames(bill_correlation)))

# Add correlation values inside each square
text(
  x = rep(1:6, each = 6),
  y = rep(1:6, times = 6),
  labels = sprintf("%.2f", as.vector(bill_correlation)),
  cex = 0.9
)

# Add axis titles
title(
  xlab = "Bill Amount Variables",
  ylab = "Bill Amount Variables"
)

