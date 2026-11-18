install.packages("mosaic")

#import the dataset and data cleaning
credit_card <- read.csv("data/original/default of credit card clients.csv", skip = 1)


# ======== inspect the dataset ========

#display the structure of the dataset
str(credit_card)

#display the first siz rows
head(credit_card)
  
#display the dimensions
dim(credit_card)

#Display column names
names(credit_card)

# ======= Check missing values ======

#Count missing values in each column
colSums(is.na(credit_card))

# Count the total missing values
sum(is.na(credit_card))

# ======== check duplicated records ========

# Count duplicated rows
sum(duplicated(credit_card))

# Check duplicated customer IDs
sum(duplicated(credit_card$ID))

# ======== inspect payment status values ========

unique(credit_card$PAY_0)
unique(credit_card$PAY_2)
unique(credit_card$PAY_3)
unique(credit_card$PAY_4)
unique(credit_card$PAY_5)
unique(credit_card$PAY_6)


# ======== identify potential outliers ========

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

# ======== detect outliers using IQR ========

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
table(credit_card$EDUCATION,
      credit_card$default.payment.next.month)

# Marriage and default payment
table(credit_card$MARRIAGE,
      credit_card$default.payment.next.month)

# Sex and default payment
table(credit_card$SEX,
      credit_card$default.payment.next.month)


# ======== Calculate default proportions by category ========

# Education and default proportions
prop.table(
  table(credit_card$EDUCATION,
        credit_card$default.payment.next.month),
  margin = 1
)

# Marriage and default proportions
prop.table(
  table(credit_card$MARRIAGE,
        credit_card$default.payment.next.month),
  margin = 1
)

# Sex and default proportions
prop.table(
  table(credit_card$SEX,
        credit_card$default.payment.next.month),
  margin = 1
)



