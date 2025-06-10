# Import libraries
library(tidyverse)
library(mice)
library(VIM)
library(randomForest)

# Load data
dataset <- read_csv("C:\\Games\\R PROGRAMMING\\project\\data.csv")
View(dataset)

# Inspect dataset
glimpse(dataset)
ncol(dataset)
nrow(dataset)

# Summary statistics
summary(dataset)

# Define a function to display unique values in columns
show_unique_values <- function(col_name, col_data) {
  cat("Unique values in", col_name, ":\n")
  print(unique(col_data))
}

# Columns for checking
columns <- c("checking_status", "credit_history", "purpose", "savings_status", 
             "employment", "installment_commitment", "personal_status", 
             "other_parties", "residence_since", "property_magnitude", "age", 
             "other_payment_plans", "housing", "existing_credits", "job", 
             "num_dependents", "own_telephone", "foreign_worker", "class")

# Check unique values in each column
walk(columns, ~ show_unique_values(.x, dataset[[.x]]))

#------------------------------------------------------------------------------
# Data Cleaning
#------------------------------------------------------------------------------
dataset <- dataset %>%
  mutate(
    # Round duration to the nearest integer
    duration = round(duration),
    # Merge "all paid" and "no credits/all paid" into "no credits/all paid"
    credit_history = if_else(credit_history == "all paid", "no credits/all paid", credit_history),
    # Create new purpose categories combining similar categories
    purpose = case_when(
      purpose %in% c("radio/tv", "furniture/equipment", "domestic appliance") ~ "Consumer goods",
      purpose %in% c("new car", "used car") ~ "Car Purchase",
      TRUE ~ purpose
    ),
    # Fix typo in savings status
    savings_status = if_else(savings_status == "500<=X<10000", "500<=X<1000", savings_status),
    # Round installment commitment to 2 decimal places
    installment_commitment = round(installment_commitment, 2),
    # Change personal status into a gender column
    gender = case_when(
      str_detect(personal_status, "female") ~ "female",
      str_detect(personal_status, "male") ~ "male",
      TRUE ~ personal_status
    ),
    # Round residence since to the nearest integer
    residence_since = round(residence_since),
    # Round Age to the nearest integer
    age = round(age),
    # Replace "" with NA's that are to be imputed
    other_payment_plans = if_else(other_payment_plans == "", NA_character_, other_payment_plans),
    # Round existing credits to the nearest integer
    existing_credits = round(existing_credits),
    # Rename high qualif/self emp/mgmt to high qualified
    job = if_else(job == "high qualif/self emp/mgmt", "high qualified", job),
    # Round number of dependents to the nearest integer
    num_dependents = round(num_dependents),
    # Replace "none" with "no" in own telephone
    own_telephone = if_else(own_telephone == "none", "no", own_telephone)
  )

# Rename the personal status column to gender
# names(dataset)[names(dataset) == "personal_status"] <- "gender"

#------------------------------------------------------------------------------
# Categorization
#------------------------------------------------------------------------------
# Apply categorical transformation to the dataset
# Make some of the columns binary for later imputation
dataset <- dataset %>%
  mutate(
    checking_status = factor(checking_status, levels = c("no checking", "<0", "0<=X<200", ">=200"), ordered = TRUE),
    savings_status = factor(savings_status, levels = c("<100", "100<=X<500", "500<=X<1000", ">=1000"), ordered = TRUE),
    employment = factor(employment, levels = c("unemployed", "<1", "1<=X<4", "4<=X<7", ">=7"), ordered = TRUE),
    job = factor(job, levels = c("unemp/unskilled non res", "unskilled resident", "skilled", "high qualified"), ordered = TRUE),
    # Convert gender to numeric: male = 0, female = 1
    gender = as.numeric(factor(gender, levels = c("male", "female"))) - 1,
    # Convert num_dependents to numeric: 1 = 0, 2 = 1
    num_dependents = as.numeric(factor(num_dependents, levels = c("1", "2"))) - 1,
    # Convert own_telephone to numeric: no = 0, yes = 1
    own_telephone = as.numeric(factor(own_telephone, levels = c("no", "yes"))) - 1,
    # Convert foreign_worker to numeric: no = 0, yes = 1
    foreign_worker = as.numeric(factor(foreign_worker, levels = c("no", "yes"))) - 1
  )

#------------------------------------------------------------------------------
# Imputation
#------------------------------------------------------------------------------
# MICE for Ordered Categorical Data
set.seed(123)
mice_vars <- c("checking_status", "savings_status", "employment", "job")
mice_imputed <- mice(dataset[mice_vars], method = 'cart', m = 5, maxit = 20)
dataset[mice_vars] <- complete(mice_imputed)

# KNN for Unordered Categorical Data
knn_vars <- c("credit_history", "purpose", "other_parties", "property_magnitude", 
              "other_payment_plans", "housing", "job")
dataset <- kNN(dataset, variable = knn_vars, k = 5, imp_var = FALSE)

lm_impute <- function(data, column) {
  # Exclude columns with NA values, except for the target column
  predictors <- names(data)[!names(data) %in% column & colSums(is.na(data)) == 0]
  if (length(predictors) == 0) {
    stop(paste("No columns without NA's to use as predictors for", column))
  }
  
  # Create formula with the remaining predictors
  formula <- reformulate(predictors, response = column)
  
  # Filter rows with complete cases for the predictors and target column
  complete_data <- data[complete.cases(data[c(column, predictors)]), ]
  if (nrow(complete_data) == 0) {
    stop(paste("No complete cases to build the model for", column))
  }
  
  # Build the linear regression model
  lm_model <- lm(formula, data = complete_data)
  
  # Identify rows with missing values in the target column
  missing <- is.na(data[[column]])
  if (any(missing)) {
    # Predict using the model for rows with complete predictors
    data[missing, column] <- predict(lm_model, newdata = data[missing, predictors, drop = FALSE])
  }
  
  data
}

# Apply linear regression imputation to numerical columns
num_columns <- c("duration", "credit_amount", "installment_commitment", "residence_since", "age", "existing_credits")
for (col in num_columns) {
  dataset <- lm_impute(dataset, col)
}

logistic_impute <- function(data, column) {
  # Exclude columns with NA values, except for the target column
  predictors <- names(data)[!names(data) %in% column & colSums(is.na(data)) == 0]
  if (length(predictors) == 0) {
    stop(paste("No columns without NA's to use as predictors for", column))
  }
  
  # Create formula with the remaining predictors
  formula <- reformulate(predictors, response = column)
  
  # Filter rows with complete cases for the predictors and target column
  complete_data <- data[complete.cases(data[c(column, predictors)]), ]
  if (nrow(complete_data) == 0) {
    stop(paste("No complete cases to build the model for", column))
  }
  
  # Build the logistic regression model
  glm_model <- glm(formula, data = complete_data, family = binomial)
  
  # Identify rows with missing values in the target column
  missing <- is.na(data[[column]])
  if (any(missing)) {
    # Predict probabilities for missing rows
    predictions <- predict(glm_model, newdata = data[missing, predictors, drop = FALSE], type = "response")
    data[missing, column] <- as.numeric(predictions > 0.5) # Convert probabilities to binary values
  }
  
  data
}

# Apply logistic regression imputation to binary columns
binary_columns <- c("gender", "num_dependents", "own_telephone", "foreign_worker")
for (col in binary_columns) {
  dataset <- logistic_impute(dataset, col)
}

#------------------------------------------------------------------------------
# Post-Imputation Adjustments
#------------------------------------------------------------------------------
# Post Imputation some of the columns may no longer conform to the
# transformation done previously. We will reapply the transformation
dataset <- dataset %>%
  # Rename some columns
  rename(
    credit_class = class,
    loan_purpose = purpose
  ) %>%
  mutate(
    duration = round(duration),
    credit_amount = round(credit_amount),
    installment_commitment = round(pmax(pmin(installment_commitment, 4), 1), 2),
    residence_since = round(pmax(pmin(residence_since, 4), 1)),
    existing_credits = round(pmax(pmin(existing_credits, 4), 1)),
    age = round(age),
    across(c(gender, num_dependents, own_telephone, foreign_worker), ~ round(.)),
    credit_class_binary = ifelse(credit_class == "good", 1, 0)
  ) %>%
  select(-personal_status)


write.csv(dataset, "data_cleaned.csv", row.names = FALSE)
ncol(dataset)
nrow(dataset)
# Dataset ready for further analysis
#----------------------------------------------------------------------------------------------------------------





#----------------------------------------------------------------------------------------------------------------
# Hypothesis Testing
# Load cleaned data
data <- read_csv("data_cleaned.csv")
View(data)
# Hypothesis: Customer saving status, age, credit amount, and loan duration can significantly impact the credit class.
# Objective 1: To investigate the relationship between the loan duration and credit class.

# Analysis 1-1: Is there any relationship between loan duration and credit class?

# unclean
ggplot(data, aes(x = credit_class, y = duration, fill = credit_class)) +
  geom_boxplot() +
  labs(title = "Loan Duration vs Credit Class", x = "Credit Class", y = "Loan Duration") +
  theme_minimal()

ggsave("duration_vs_credit_class.png")

# Choose values that are not NA from duration
duration_clean <- dataset[!is.na(dataset$duration), ]

# Remove outliers separately for each credit_class
duration_cleaned_data <- duration_clean %>%
  group_by(credit_class) %>%
  filter(
    duration >= quantile(duration, 0.25) - 1.5 * IQR(duration),
    duration <= quantile(duration, 0.75) + 1.5 * IQR(duration)
  ) %>%
  ungroup()

# cleaned
ggplot(duration_cleaned_data, aes(x = credit_class, y = duration, fill = credit_class)) +
  geom_boxplot() +
  labs(title = "Loan Duration vs Credit Class after removing outliers", 
       x = "Credit Class", y = "Loan Duration") +
  theme_minimal()

# Analysis 1-2: Is loan duration a strong predictor for credit class?
anova_duration <- aov(duration ~ credit_class, data = duration_cleaned_data)
summary(anova_duration)

#this was in analysis 1-2, but makes more sense here
logistic_model <- glm(credit_class_binary ~ duration + age + savings_status,
                      data = duration_cleaned_data, family = binomial)
summary(logistic_model)

# Objective 2: To examine the impact of age on credit class.
# Analysis 2-1: Is there a significant difference in age distribution between credit classes?
ggplot(data, aes(x = credit_class, y = age, fill = credit_class)) +
  geom_violin(trim = FALSE) +
  labs(title = "Age Distribution vs Credit Class", x = "Credit Class", y = "Age") +
  theme_minimal()

ggsave("age_vs_credit_class.png")

anova_age <- aov(age ~ credit_class, data = data)
summary(anova_age)

# Analysis 2-2: Are the differences in age distributions statistically significant?
ks_test_age <- ks.test(data$age[data$credit_class == "good"],
                       data$age[data$credit_class == "bad"])
ks_test_age

# Analysis 2-3: How do different age groups impact credit class?
data$age_group <- cut(data$age, breaks = c(-Inf, 30, 50, Inf), labels = c("<30", "30-50", ">50"))
anova_age_group <- aov(age ~ credit_class * age_group, data = data)
summary(anova_age_group)


# Objective 3: To Investigate the Influence of Savings Status on Loan Credit Class. 
# Analysis 3-1: Is there a significant association between savings status and credit class?
ggplot(data, aes(x = savings_status, fill = credit_class)) +
  geom_bar(position = "dodge") +
  labs(title = "Savings Status vs Credit Class", x = "Savings Status", y = "Count") +
  theme_minimal()

ggsave("savings_status_vs_credit_class.png")

table_savings <- table(data$savings_status, data$credit_class)
chi_savings <- chisq.test(table_savings)
chi_savings

# Analysis 3-2: Does savings status interact with other variables (e.g., age) to influence credit class?
interaction_model_savings <- glm(credit_class_binary ~ savings_status * age, 
                                 data = data, family = binomial)
summary(interaction_model_savings)

# Analysis 3-3: How does savings status influence loan duration and credit amount across credit classes?
# Savings Status and Loan Duration
ggplot(data, aes(x = savings_status, y = duration, fill = credit_class)) +
  geom_boxplot() +
  labs(title = "Savings Status vs Loan Duration by Credit Class", 
       x = "Savings Status", y = "Loan Duration") +
  theme_minimal()

ggsave("savings_status_vs_loan_duration_by_credit_class.png")

anova_savings_duration <- aov(duration ~ credit_class * savings_status, data = data)
summary(anova_savings_duration)

# Savings Status and Credit Amount
ggplot(data, aes(x = savings_status, y = credit_amount, fill = credit_class)) +
  geom_boxplot() +
  labs(title = "Savings Status vs Credit Amount by Credit Class", 
       x = "Savings Status", y = "Credit Amount") +
  theme_minimal()

ggsave("savings_status_vs_credit_amount_by_credit_class.png")

anova_savings_credit <- aov(credit_amount ~ credit_class * savings_status, data = data)
summary(anova_savings_credit)

# Objective 4: To explore the trends in loan amounts across credit classes.
# Analysis 4-1: How does the credit amount distribution vary between credit classes?
ggplot(data, aes(x = credit_class, y = credit_amount, fill = credit_class)) +
  geom_boxplot() +
  labs(title = "Credit Amount vs Credit Class", 
       x = "Credit Class", y = "Credit Amount") +
  theme_minimal()

ggsave("credit_amount_vs_credit_class.png")

anova_credit_amount <- aov(credit_amount ~ credit_class, data = data)
summary(anova_credit_amount)

# Analysis 4-2: What are the key factors influencing credit amount in relation to credit class?

set.seed(123)
rf_model <- randomForest(credit_class_binary ~ duration + age + savings_status + credit_amount, 
                         data = data, importance = TRUE)
importance(rf_model)
varImpPlot(rf_model)

# Analysis 4-3: What is the relationship between credit amount and savings status within credit classes?
ggplot(data, aes(x = credit_class, y = credit_amount, fill = savings_status)) +
  geom_boxplot() +
  facet_wrap(~savings_status) +
  labs(title = "Credit Amount by Savings Status and Credit Class", 
       x = "Credit Class", y = "Credit Amount") +
  theme_minimal()

anova_credit_savings <- aov(credit_amount ~ credit_class * savings_status, data = data)
summary(anova_credit_savings)

# Analysis 4-4: Correlation between credit amount and loan duration for different credit classes
correlation_by_class <- data %>%
  group_by(credit_class) %>%
  summarize(correlation = cor(duration, credit_amount, use = "complete.obs"))

print(correlation_by_class)
