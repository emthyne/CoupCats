#------------------------------------------------------------------------------------------------#
#Front-end stuff
#------------------------------------------------------------------------------------------------#  

#Building final dataset; running analyses

#1. clear all
rm(list = ls())
#2. set working directory
#setwd("~/R/coupcats") # Set working file. 
setwd("C:/Users/clayt/OneDrive - University of Kentucky/elements/current_research/coupcats") #Clay at home
#setwd("C:/Users/clthyn2/OneDrive - University of Kentucky/elements/current_research/coupcats") #clay at work
#3. install packages
#source("https://raw.githubusercontent.com/thynec/CoupCats/refs/heads/main/packages.R") 
#4. load libraries
#source("https://raw.githubusercontent.com/thynec/CoupCats/refs/heads/main/libraries.R") 
#------------------------------------------------------------------------------------------------#
#merge DFs together
#------------------------------------------------------------------------------------------------#

#2.a.domestic political
url <- "https://raw.githubusercontent.com/thynec/CoupCats/data/2.a.base_data.csv.gz"
base_data.2a <- fread(url)
rm(url)
#2.b.domestic economic
url <- "https://raw.githubusercontent.com/thynec/CoupCats/data/2.b.base_data.csv.gz"
base_data.2b <- fread(url)
rm(url)
#2.c.political instability
url <- "https://raw.githubusercontent.com/thynec/CoupCats/data/2.c.base_data.csv.gz"
base_data.2c <- fread(url)
rm(url)
#2.d.military variables
url <- "https://raw.githubusercontent.com/thynec/CoupCats/data/2.d.base_data.csv.gz"
base_data.2d <- fread(url)
rm(url)
#2.e.international variables
url <- "https://raw.githubusercontent.com/thynec/CoupCats/data/2.e.base_data.csv.gz"
base_data.2e <- fread(url)
rm(url)

base_data <- base_data.2a
rm(base_data.2a)
base_data.2b <- base_data.2b %>%
  dplyr::select(-country, -coup_attempt, -coup_successful, -coup_failed, -pce, -pce2, -pce3)
base_data <- base_data %>%
  left_join(base_data.2b, by=c("ccode", "year", "month"))
rm(base_data.2b)
base_data.2c <- base_data.2c %>%
  dplyr::select(-country, -coup_attempt, -coup_successful, -coup_failed, -pce, -pce2, -pce3)
base_data <- base_data %>%
  left_join(base_data.2c, by=c("ccode", "year", "month"))
rm(base_data.2c)    
base_data.2d <- base_data.2d %>%
  dplyr::select(-country, -coup_attempt, -coup_successful, -coup_failed, -pce, -pce2, -pce3)
base_data <- base_data %>%
  left_join(base_data.2d, by=c("ccode", "year", "month"))
rm(base_data.2d)    
base_data.2e <- base_data.2e %>%
  dplyr::select(-country, -coup_attempt, -coup_successful, -coup_failed, -pce, -pce2, -pce3)
base_data <- base_data %>%
  left_join(base_data.2e, by=c("ccode", "year", "month"))
rm(base_data.2e)

write.csv(base_data, "base_data.csv", row.names = FALSE)

# Filtering out rows with NAs. 
columns <- c("pres_elec_lag", "polyarchy", "polyarchy2", 
             "lgdppcl", "ch_gdppcl", 
             "cw", 
             "cold", "e_asia_pacific", "LA_carrib", "MENA", "N_america", "S_asia", "Sub_africa", 
             "pce", "pce2", "pce3")

base_data2 <- base_data[complete.cases(base_data[, ..columns]), ] 

#------------------------------------------------------------------------------------------------#  
#Baseline model
#------------------------------------------------------------------------------------------------#  

#logit with coup attempt as dv
#Clay added robust SEs on 03/19/25; use this format for other regressions (feglm<-glm at beginning; 'cluster = ~ccode' at end))
coup_logit <- feglm(coup_attempt ~ 
                      pres_elec_lag + polyarchy + polyarchy2 + milreg + #2.a. domestic political
                      lgdppcl + ch_gdppcl + #2.b. domestic economic
                      cw + mobilization + #2.c. political instability
                      solqual +  #2.d. military vars
                      cold + e_asia_pacific + LA_carrib + MENA + N_america + S_asia + Sub_africa + #intl vars
                      pce + pce2 + pce3, #autocorrelation vars, 
                    data = base_data, family = 'binomial', cluster = ~ccode)
summary(coup_logit)

#------------------------------------------------------------------------------------------------#  
#Pretty table of baseline model - NEED to replace var names with labels at some point
#------------------------------------------------------------------------------------------------#  

# Marginal effects 
mfxL <- margins(coup_logit, type = 'response') # Effect of each predictor expressed as a probability (Average Marginal Effect)
summary(mfxL) # R rounds to 0 (virtually no effect)--remember, coups are rare events! 
print(mfxL, digits = 6) 

# Calculate DFBETAs
dfbetas_values <- dfbetas(coup_logit) # Large DFBETAs indicate a specific observation (row) has a strong influence.

# Find observations with any DFBETA greater than 2/sqrt(n). 
influential_obsL <- apply(abs(dfbetas_values), 1, function(x) any(x > 2)) # 2 is arbitrary; common threshold for influence.

# Display influential observations
which(influential_obsL) # No single observation significantly alters the model. 
rm(dfbetas_values, influential_obsL)

# Building logit table
model_summary <- tidy(coup_logit) 
formatted_table <- gt(model_summary) 
formatted_table <- fmt_number(
  formatted_table,
  columns = c("estimate", "std.error", "statistic", "p.value"),
  decimals = 4) 
formatted_table <- tab_style(
  formatted_table,
  style = list(cell_text(weight = "bold")),
  locations = cells_column_labels())
formatted_table <- data_color(
  formatted_table,
  columns = "p.value",
  colors = col_numeric(
    palette = c("red", "black"),
    domain = c(0, 0.05)))
formatted_table <- tab_header(
  formatted_table,
  title = md("**Logistic Regression Summary**"),
  subtitle = md("*Effect of Social and Military variables on Coup Attempts")) 
formatted_table <- tab_source_note(
  formatted_table,
  source_note = "Significant p-values are highlighted in red.")

print(formatted_table)
rm(model_summary, formatted_table)

# Building marginal effects table. 
mfxL_df <- as.data.frame(summary(mfxL))
formatted_table <- gt(mfxL_df)
formatted_table <- fmt_number(
  formatted_table,
  columns = c("AME", "SE", "z", "p", "lower", "upper"),
  decimals = 4)
formatted_table <- tab_style(
  formatted_table,
  style = list(cell_text(weight = "bold")),
  locations = cells_column_labels())
formatted_table <- data_color(
  formatted_table,
  columns = "p",
  colors = scales::col_numeric(
    palette = c("red", "black"),
    domain = c(0, 0.05)))
formatted_table <- tab_header(
  formatted_table,
  title = md("**Marginal Effects Summary**"),
  subtitle = md("*Average Marginal Effects from Logistic Regression*"))
formatted_table <- tab_source_note(
  formatted_table,
  source_note = "Significant p-values are highlighted in red.")

print(formatted_table)
rm(formatted_table)

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------
#probit with coup attempt as dv and population total, median age, military expenditure (total and percent of GDP)
coup_probit <- glm(coup_attempt ~ 
                     pres_elec_lag + polyarchy + polyarchy2 + #2.a. domestic political
                     lgdppcl + ch_gdppcl + #2.b. domestic economic
                     cw + #2.c. political instability
                     #NEED military vars here
                     cold + e_asia_pacific + LA_carrib + MENA + N_america + S_asia + Sub_africa + #intl vars
                     pce + pce2 + pce3, #autocorrelation vars, 
                   data = base_data, family = binomial(link = "probit"))
summary(coup_probit)

#marginal effects for probit
mfxP <- margins(coup_probit) #marginal efffects
summary(mfxP) # R rounds to 0
print(mfxP, digits = 6) # do this to see actual values

# Calculate DFBETAs
dfbetas_values <- dfbetas(coup_probit)

# Find observations with any DFBETA greater than 2/sqrt(n) in absolute value. Can change to 2 for basic threshold
influential_obsP <- apply(abs(dfbetas_values), 1, function(x) any(x > 2))

# Display influential observations
which(influential_obsP)



# Probit table using GT, error message about color scale, fix rounding and which "statistic"
# Convert model summary into a tidy dataframe
model_summary <- tidy(coup_probit) 
# Create the table
formatted_table <- gt(model_summary)
# Format numerical columns
formatted_table <- fmt_number(
  formatted_table,
  columns = c("estimate", "std.error", "statistic", "p.value"),
  decimals = 4)
# Bold column labels
formatted_table <- tab_style(
  formatted_table,
  style = list(cell_text(weight = "bold")),
  locations = cells_column_labels())
# Highlight significant p-values in red
formatted_table <- data_color(
  formatted_table,
  columns = "p.value",
  colors = col_numeric(
    palette = c("red", "black"),
    domain = c(0, 0.05)))
# Title and Subtitle
formatted_table <- tab_header(
  formatted_table,
  title = md("**Probit Regression Summary**"),
  subtitle = md("*Effect of Social and Military Variables on Coup Attemps*")) #should probably change this subtitle
# Footer note
formatted_table <- tab_source_note(
  formatted_table,
  source_note = "Significant p-values are highlighted in red.")

print(formatted_table)


# Marginal effects table using GT
# Convert to a tidy dataframe
mfxP_df <- as.data.frame(summary(mfxL))
# Create the table
formatted_table <- gt(mfxL_df)
# Format numerical columns for readability
formatted_table <- fmt_number(
  formatted_table,
  columns = c("AME", "SE", "z", "p", "lower", "upper"),
  decimals = 4)
# Bold column labels
formatted_table <- tab_style(
  formatted_table,
  style = list(cell_text(weight = "bold")),
  locations = cells_column_labels())
# Highlight significant p-values in red (p < 0.05)
formatted_table <- data_color(
  formatted_table,
  columns = "p",
  colors = scales::col_numeric(
    palette = c("red", "black"),
    domain = c(0, 0.05)))
# Add title and subtitle
formatted_table <- tab_header(
  formatted_table,
  title = md("**Marginal Effects Summary**"),
  subtitle = md("*Average Marginal Effects from Probit Regression*"))
# Add footer note
formatted_table <- tab_source_note(
  formatted_table,
  source_note = "Significant p-values are highlighted in red.")
# Print the formatted table
print(formatted_table)

#checking 1s and 0s 
value_counts <- table(base_data$coup_attempt)
value_counts
#-------------------------------------------------------------------------------------------------------------------
#DIAGNOSTICS (both models)
#Accuracy Logit
predicted_logit <- predict(coup_logit, type = 'response') #predicted probability

# Convert probabilities to binary class labels
predicted_classes <- ifelse(predicted_logit > 0.0034, 1, 0)
predicted_classes <- factor(predicted_classes, levels = c(0, 1))  # Specify levels 0 and 1
actual_classes <- factor(base_data2$coup_attempt, levels = c(0, 1))  # Ensure actual labels have the same levels
# Create confusion matrix
conf_matrix <- confusionMatrix(as.factor(predicted_classes), as.factor(base_data2$coup_attempt))

# View confusion matrix
conf_matrix




#Accuracy Probit
predicted_probit <- predict(coup_probit, type = 'response') #predicted probabilites

predicted_classes <- ifelse(predicted_probit > 0.0034, 1, 0)
predicted_classes <- factor(predicted_classes, levels = c(0, 1))  # Specify levels 0 and 1
actual_classes <- factor(base_data2$coup_attempt, levels = c(0, 1))  # Ensure actual labels have the same levels
# Create confusion matrix
conf_matrix <- confusionMatrix(as.factor(predicted_classes), as.factor(base_data2$coup_attempt))

# View confusion matrix
conf_matrix


#logit pearson residuals
pearson_residuals_logit <- residuals(coup_logit, type = "pearson")
pearson_residuals_logit


#plotted logit pearson residuals
# Create a data frame for plotting
plot_data <- data.frame(
  observation = 1:length(pearson_residuals_logit),  # X-axis: Observation indices
  pearson_residual = pearson_residuals_logit )       # Y-axis: Pearson residuals

ggplot(plot_data, aes(x = observation, y = pearson_residual)) +
  geom_point() +  # Scatter plot
  labs(x = "Observations", y = "Pearson Residuals", title = "Pearson Residuals Plot") +
  theme_minimal()  # Optional: minimal theme for clean look

# Find the index of the largest absolute Pearson residual
max_residual_index <- which.max(abs(pearson_residuals_logit))

# Print the index of the observation with the largest residual
cat("The observation with the largest residual is at index:", max_residual_index, "\n")


#find Cook's Distance (for influential outliers)
logit_CooksD = cooks.distance(coup_logit)

#Logit plotting Cook's Distance (base, need to add clarity and labels)
#Create a data frame for plotting
plot_data <- data.frame(
  observation = 1:length(logit_CooksD),  # X-axis: Observation indices
  cooks_distance = logit_CooksD          # Y-axis: Cook's distances
)
#Make plot
ggplot(plot_data, aes(x = observation, y = cooks_distance)) +
  geom_point() +  # Scatter plot of Cook's distances
  geom_hline(yintercept = 4 / length(logit_CooksD), linetype = "dashed", color = "red") +  # Threshold line
  labs(x = "Observations", y = "Cook's Distance", title = "Cook's Distance Plot for Logit Model") +
  theme_minimal()  # Optional: minimal theme for clean look



#Probit pearson residuals (preferred)
pearson_residuals_probit <- residuals(coup_probit, type = "pearson")
pearson_residuals_probit

#plotting pearson residuals
plot_data <- data.frame(
  observation = 1:length(pearson_residuals_probit),  # X-axis: Observation indices
  pearson_residual = pearson_residuals_probit )       # Y-axis: Pearson residuals

#Make plot
ggplot(plot_data, aes(x = observation, y = pearson_residual)) +
  geom_point() +  # Scatter plot
  labs(x = "Observations", y = "Pearson Residuals", title = "Pearson Residuals Plot") +
  theme_minimal()  # Optional: minimal theme for clean look 
# Find the index of the largest absolute Pearson residual
max_residual_index <- which.max(abs(pearson_residuals_probit))

# Print the index of the observation with the largest residual
cat("The observation with the largest residual is at index:", max_residual_index, "\n")


#find Cook's Distance (for influential outliers)
probit_CooksD = cooks.distance(coup_probit)

#probit plotting Cook's Distance (base, need to add clarity and labels)
plot_data <- data.frame(
  observation = 1:length(probit_CooksD),  # X-axis: Observation indices
  cooks_distance = probit_CooksD          # Y-axis: Cook's distances
)
#Make plot
ggplot(plot_data, aes(x = observation, y = cooks_distance)) +
  geom_point() +  # Scatter plot of Cook's distances
  geom_hline(yintercept = 4 / length(probit_CooksD), linetype = "dashed", color = "red") +  # Threshold line
  labs(x = "Observations", y = "Cook's Distance", title = "Cook's Distance Plot for Probit Model") +
  theme_minimal()  # Optional: minimal theme for clean look


vif(coup_logit)
vif(coup_probit)

#heteroscedasticity test (check in OLS)
#run linear model 
OLS_coup <- lm(coup_attempt ~ 
                 pres_elec_lag + polyarchy + polyarchy2 + #2.a. domestic political
                 lgdppcl + ch_gdppcl + #2.b. domestic economic
                 cw + #2.c. political instability
                 #NEED military vars here
                 cold + e_asia_pacific + LA_carrib + MENA + N_america + S_asia + Sub_africa + #intl vars
                 pce + pce2 + pce3, #autocorrelation vars, 
               data = base_data)

#Breusch-Pagan test, if significant, heteroscedasticity probable in MLE model, further testing (hetprobit)
bptest(OLS_coup)

# --------------------------- Test, Train, Split --------------------------- #

# set.seed(9) # For replication. 

# Taking 70% randomly, stratifying by coup attempt. 
training <- base_data2 %>%
  group_by(coup_attempt) %>%
  sample_frac(0.7) %>%
  ungroup()

# Other 30%. 
testing <- anti_join(base_data2, training)

# Running logit model. 
training_logit <- feglm(
  coup_attempt ~ pres_elec_lag + polyarchy + polyarchy2 + milreg + 
    lgdppcl + ch_gdppcl + cw + mobilization + solqual +  
    cold + e_asia_pacific + LA_carrib + MENA + N_america + 
    S_asia + Sub_africa + pce + pce2 + pce3,
  data = training, family = 'binomial', cluster = ~ccode)
summary(training_logit)

# Predicting based on test data. 
predicted_logit <- predict(training_logit, newdata = testing, type = 'response')
predicted_classes <- ifelse(predicted_logit > 0.0034, 1, 0) # Test for better threshold. 

# Create confusion matrix
conf_matrix <- confusionMatrix(factor(predicted_classes, levels = c(0, 1)), factor(testing$coup_attempt, levels = c(0, 1)))
print(conf_matrix) # Switch specificity & sensitivity in your head (positive class = 0)

rm(training, testing, training_logit, conf_matrix, predicted_logit, predicted_classes)


                          
# --------------------------- Test, Train, Split --------------------------- #

set.seed(9)

remove_perc = 0.20
cutoff = 0.038 
oversample_factor = 5

# Under-sampling: Remove months from non-coup countries based on remove_perc. 
coup_countries <- base_data2 %>%
  filter(euro_cent_asia == 1 | N_america == 1) %>%
  group_by(ccode) %>%
  summarise(has_coup = any(coup_attempt == 1), .groups = "drop")
df2 <- base_data2 %>%
  left_join(coup_countries, by = "ccode") %>%
  mutate(has_coup = !is.na(has_coup) & has_coup) # New variable. 

# Separating data into different groups. 
non_coup_data <- df2 %>% 
  filter((euro_cent_asia == 1 | N_america == 1) & has_coup == FALSE)
other_countries <- df2 %>%
  filter((euro_cent_asia == 0 & N_america == 0) | has_coup == TRUE)

# Randomly removing observations from non-coup countries. 
non_coup_data <- non_coup_data %>%
  sample_frac(1 - remove_perc)

model_data <- bind_rows(other_countries, non_coup_data) %>%
  select(-has_coup)

# Oversampling coup cases using bootstrapping. 
coup_cases <- model_data %>% 
  filter(coup_attempt == 1)
no_coup_cases <- model_data %>% 
  filter(coup_attempt == 0)
coup_bootstrap <- coup_cases %>%
  sample_n(size = nrow(coup_cases) * oversample_factor, replace = TRUE)
balanced_data <- bind_rows(coup_bootstrap, no_coup_cases)

# Splitting the data into training and testing sets. 
training <- balanced_data %>%
  group_by(coup_attempt) %>%
  sample_frac(0.7) %>%
  ungroup()
testing <- anti_join(balanced_data, training)

# Running the logistic regression model. 
training_logit <- feglm(coup_attempt ~ 
                          pres_elec_lag + polyarchy + polyarchy2 + milreg + 
                          lgdppcl + ch_gdppcl + cw + mobilization + solqual +  
                          cold + e_asia_pacific + LA_carrib + MENA + N_america + 
                          S_asia + Sub_africa + pce + pce2 + pce3, 
                        data = training, family = 'binomial', cluster = ~ccode)

# Predicting based on test data
predicted_logit <- predict(training_logit, newdata = testing, type = 'response')
predicted_classes <- ifelse(predicted_logit > cutoff, 1, 0) 

# Creating confusion matrix
conf_matrix <- confusionMatrix(factor(predicted_classes, levels = c(0, 1)), factor(testing$coup_attempt, levels = c(0, 1)))
print(conf_matrix) # Switch specificity & sensitivity in your head (positive class = 0)


# --------------------------- Model developing --------------------------- #

# Define transformations. 
remove_perc_range <- seq(0, 1, by = 0.05) # Range for removing non-coup cases
cutoff_range <- seq(0.001, 0.05, by = 0.001) # Range for cutoff values
oversample_range <- 1:5 # Range for oversampling coup cases 

# Create an empty results data frame
results <- data.frame(remove_perc = numeric(),
                      cutoff = numeric(),
                      oversample_factor = numeric(),
                      accuracy = numeric(),
                      specificity = numeric(),
                      sensitivity = numeric())

set.seed(9) # For replication. 

library(pbapply)
progress_bar <- pboptions(type = 3)  # Simple progress bar. 

# Looping through remove_perc values. 
for (remove_perc in remove_perc_range) {
  
  # Looping through cutoff values. 
  for (cutoff in cutoff_range) {
    
    # Looping through over-sampling values. 
    for (oversample_factor in oversample_range) {
      
      # Under-sampling: Remove months from non-coup countries based on remove_perc. 
      coup_countries <- base_data2 %>%
        filter(euro_cent_asia == 1 | N_america == 1) %>%
        group_by(ccode) %>%
        summarise(has_coup = any(coup_attempt == 1), .groups = "drop")
      df <- base_data2 %>%
        left_join(coup_countries, by = "ccode") %>%
        mutate(has_coup = !is.na(has_coup) & has_coup) # New variable. 
      
      # Separating data into different groups. 
      non_coup_data <- df %>% 
        filter((euro_cent_asia == 1 | N_america == 1) & has_coup == FALSE)
      other_countries <- df %>%
        filter((euro_cent_asia == 0 & N_america == 0) | has_coup == TRUE)
      
      # Randomly removing observations from non-coup countries. 
      non_coup_data <- non_coup_data %>%
        sample_frac(1 - remove_perc)
      
      model_data <- bind_rows(other_countries, non_coup_data) %>%
        select(-has_coup)
      
      # Oversampling coup cases using bootstrapping. 
      coup_cases <- model_data %>% filter(coup_attempt == 1)
      no_coup_cases <- model_data %>% filter(coup_attempt == 0)
      coup_bootstrap <- coup_cases %>%
        sample_n(size = nrow(coup_cases) * oversample_factor, replace = TRUE)
      balanced_data <- bind_rows(coup_bootstrap, no_coup_cases)
      
      # Splitting the data into training and testing sets. 
      training <- balanced_data %>%
        group_by(coup_attempt) %>%
        sample_frac(0.7) %>%
        ungroup()
      testing <- anti_join(balanced_data, training)
      
      # Running the logistic regression model. 
      training_logit <- feglm(coup_attempt ~ 
                                pres_elec_lag + polyarchy + polyarchy2 + milreg + 
                                lgdppcl + ch_gdppcl + cw + mobilization + solqual +  
                                cold + e_asia_pacific + LA_carrib + MENA + N_america + 
                                S_asia + Sub_africa + pce + pce2 + pce3, 
                              data = training, family = 'binomial', cluster = ~ccode)
      
      # Predicting based on test data
      predicted_logit <- predict(training_logit, newdata = testing, type = 'response')
      predicted_classes <- ifelse(predicted_logit > cutoff, 1, 0) 
      
      # Creating confusion matrix
      conf_matrix <- confusionMatrix(factor(predicted_classes, levels = c(0, 1)), factor(testing$coup_attempt, levels = c(0, 1)))
      
      # Storing the results. 
      results <- rbind(results, data.frame(remove_perc = remove_perc, 
                                           cutoff = cutoff, 
                                           oversample_factor = oversample_factor,
                                           accuracy = conf_matrix$overall['Accuracy'],
                                           specificity = conf_matrix$byClass['Specificity'],
                                           sensitivity = conf_matrix$byClass['Sensitivity']))
      
      # Cleaning up for the next iteration. 
      rm(training, testing, training_logit, conf_matrix, predicted_classes, predicted_logit, balanced_data)
    }
  }
}

# Exporting to Excel. 
write.csv(results, "model_results.csv")

# Scatter plot of Sensitivity vs Specificity
ggplot(results, aes(x = specificity, y = sensitivity)) +
  geom_point(aes(color = oversample_factor, size = remove_perc), alpha = 0.7) +
  labs(title = "Sensitivity vs Specificity",
       x = "Specificity",
       y = "Sensitivity") +
  theme_minimal() +
  scale_color_gradient(low = "blue", high = "red") +
  theme(legend.position = "bottom")

df <- results %>%
  filter(sensitivity > 0.8) %>%
  filter(specificity > 0.8)
