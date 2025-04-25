
# Model 1A - Firearm Assault (No Covariates)

# Set working directory
setwd("C:/Users/victo/OneDrive/Documents/Dissertation/Guns/R2")

# Load required libraries
library(ggrepel)
library(magrittr)
library(dplyr)
library(augsynth)
library(tidyr)
library(ggplot2)
library(readr)

# Load your data
mydata <- read_csv("chapter1log.csv")

# Step 1: Prepare clean panel data
analysis_df <- mydata %>%
  select(Year, Month, City, FASSact_per_100k) %>%
  mutate(
    Year = as.numeric(Year),
    Month = as.numeric(Month),
    YearMonth = Year * 100 + Month,
    City = as.character(City)
  ) %>%
  filter(
    !is.na(FASSact_per_100k),
    City != "Watsonville",  # Exclude Watsonville
    !YearMonth %in% c(202311, 202312)  # Drop Nov & Dec 2023 for all
  ) %>%
  distinct(City, YearMonth, .keep_all = TRUE)

# Step 2: Create `Date` Variable (YYYY-MM-01 Format)
analysis_df <- analysis_df %>%
  mutate(Date = as.Date(paste(Year, Month, "01", sep = "-")))

# Step 3: Define the Treatment Date for San Jose
treatment_date <- as.Date("2023-01-01")

# Step 4: Create `treated` Variable (Treatment Status for San Jose)
analysis_df <- analysis_df %>%
  mutate(
    treated = ifelse(City == "San Jose" & Date >= treatment_date, 1, 0)
  )

# Save cleaned data with date variable
write_csv(analysis_df, "1A_FASSact_cleaned_with_date.csv")

# Print the first few rows to verify the new Date and treated variable
print(head(analysis_df))

# Step 2: Run Augmented Synthetic Control
syn <- augsynth(
  FASSact_per_100k ~ treated,
  unit = City,
  time = Date,
  data = analysis_df,
  progfunc = "None",
  scm = TRUE
)

# Step 3: Summary
summary(syn)
# Save the summary as text instead of a CSV
syn_summary <- capture.output(summary(syn))
writeLines(syn_summary, "1A_syn_summary.txt")
# Extract ATT estimate and confidence intervals
att_results <- data.frame(summary(syn)$att)
write.csv(att_results, "1A_syn_att_results.csv")


# Plot
plot(syn)
ggsave("1A_syn_plot.png")

# Step 4: Default test stat
syn_stat_neg <- capture.output(summary(syn, stat_func = function(x) -sum(x)))
writeLines(syn_stat_neg, "1A_syn_stat_neg.txt")

syn_stat_abs <- capture.output(summary(syn, stat_func = function(x) abs(sum(x))))
writeLines(syn_stat_abs, "1A_syn_stat_abs.txt")

# Plot with Jackknife
plot(syn, inf_type = "jackknife+")
ggsave("1A_syn_jackknife_plot.png")

# Step 5: With Ridge
asyn <- augsynth(
  FASSact_per_100k ~ treated,
  unit = City,
  time = Date,
  data = analysis_df,
  progfunc = "Ridge",
  scm = TRUE
)
summary(asyn)
# Save the summary as text instead of a CSV
asyn_summary <- capture.output(summary(asyn))
writeLines(asyn_summary, "1A_asyn_summary.txt")
# Extract ATT estimate and confidence intervals
aatt_results <- data.frame(summary(asyn)$att)
write.csv(aatt_results, "1A_asyn_att_results.csv")


# Plots with Ridge
plot(asyn, cv = T)
ggsave("1A_asyn_cv_plot.png")

plot(asyn)
ggsave("1A_asyn_plot.png")

# Step 4: Default test stat
asyn_stat_neg <- capture.output(summary(asyn, stat_func = function(x) -sum(x)))
writeLines(syn_stat_neg, "1A_asyn_stat_neg.txt")

asyn_stat_abs <- capture.output(summary(asyn, stat_func = function(x) abs(sum(x))))
writeLines(syn_stat_abs, "1A_asyn_stat_abs.txt")

# Compute Pre-treatment RMSPE (Manual)
Y_treated <- syn$Y[, 1]
Y_synth <- syn$Y_hat[, 1]
pre_period_index <- which(syn$time < syn$t_int)
valid_index <- pre_period_index[!is.na(Y_treated[pre_period_index]) & !is.na(Y_synth[pre_period_index])]
errors <- (Y_treated[valid_index] - Y_synth[valid_index])^2
rmspe <- sqrt(mean(errors))
sst <- sum((Y_treated[valid_index] - mean(Y_treated[valid_index]))^2)
sse <- sum(errors)
r_squared <- 1 - sse / sst

# Print results and save to file
cat("✅ Pre-treatment RMSPE:", round(rmspe, 4), "\n")
cat("📊 Pre-treatment R-squared:", round(r_squared, 4), "\n")
write.csv(data.frame(RMSPE = round(rmspe, 4), R_squared = round(r_squared, 4)), "1A_rmspe_r_squared.csv")

# Model 1B - Firearm Assault (With Covariates)

# Set working directory
setwd("C:/Users/victo/OneDrive/Documents/Dissertation/Guns/R2")

# Load required libraries
library(ggrepel)
library(magrittr)
library(dplyr)
library(augsynth)
library(tidyr)
library(ggplot2)
library(readr)

# Load your data
mydata <- read_csv("chapter1log.csv")

# Step 1: Prepare clean panel data
analysis_df <- mydata %>%
  select(Year, Month, City, FASSact_per_100k, `Police Officers`, `Median Household Income`) %>%
  mutate(
    Year = as.numeric(Year),
    Month = as.numeric(Month),
    YearMonth = Year * 100 + Month,
    City = as.character(City)
  ) %>%
  filter(
    !is.na(FASSact_per_100k),
    City != "Watsonville",  # Exclude Watsonville
    !YearMonth %in% c(202311, 202312)  # Drop Nov & Dec 2023 for all
  ) %>%
  distinct(City, YearMonth, .keep_all = TRUE)

# Step 2: Create `Date` Variable (YYYY-MM-01 Format)
analysis_df <- analysis_df %>%
  mutate(Date = as.Date(paste(Year, Month, "01", sep = "-")))

# Step 3: Define the Treatment Date for San Jose
treatment_date <- as.Date("2023-01-01")

# Step 4: Create `treated` Variable (Treatment Status for San Jose)
analysis_df <- analysis_df %>%
  mutate(
    treated = ifelse(City == "San Jose" & Date >= treatment_date, 1, 0)
  )

# Save cleaned data with date variable
write_csv(analysis_df, "1B_FASSact_cleaned_with_date.csv")

# Step 5: Run Augmented Synthetic Control with Covariates
covsyn <- augsynth(
  FASSact_per_100k ~ treated | `Police Officers` + `Median Household Income`,
  unit = City,
  time = Date,
  data = analysis_df,
  progfunc = "Ridge",
  scm = TRUE
)

# Save summary and plot
covsyn_summary <- capture.output(summary(covsyn))
writeLines(covsyn_summary, "1B_covsyn_summary.txt")
plot(covsyn)
ggsave("1B_covsyn_plot.png")

# Ridge-ASCM without Covariates
asyn <- augsynth(
  FASSact_per_100k ~ treated,
  unit = City,
  time = Date,
  data = analysis_df,
  progfunc = "Ridge",
  scm = TRUE
)

# Residualized Augmented Synthetic Control
covsyn_resid <- augsynth(
  FASSact_per_100k ~ treated | `Police Officers` + `Median Household Income`,
  unit = City,
  time = Date,
  data = analysis_df,
  progfunc = "Ridge",
  scm = TRUE,
  lambda = asyn$lambda,
  residualize = TRUE
)

# Save summary and plot for Residualized ASCM
covsyn_resid_summary <- capture.output(summary(covsyn_resid))
writeLines(covsyn_resid_summary, "1B_covsyn_resid_summary.txt")
plot(covsyn_resid)
ggsave("1B_covsyn_resid_plot.png")

# Demeaned Augmented Synthetic Control
desyn <- augsynth(
  FASSact_per_100k ~ treated,
  unit = City,
  time = Date,
  data = analysis_df,
  progfunc = "None",
  scm = TRUE,
  fixedeff = TRUE
)

# Save summary and plot for Demeaned ASCM
desyn_summary <- capture.output(summary(desyn))
writeLines(desyn_summary, "1B_desyn_summary.txt")
plot(desyn)
ggsave("1B_desyn_plot.png")


