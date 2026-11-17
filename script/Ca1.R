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

