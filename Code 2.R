

# Model 1C- Violent crimes with balanced auxilliary covariates- Female veterans

# Set working directory
setwd("C:/Users/victo/OneDrive/Documents/Dissertation/Psy/R3")

# Load Required Libraries
library(ggrepel)
library(magrittr)
library(dplyr)
library(augsynth)
library(tidyr)
library(ggplot2)

# Load your data
mydata <- read.csv("chapter1log.csv")

# Step 1: Extract Relevant Columns
analysis_df <- mydata %>%
  select(Year, Month, City, Violent_per_100k, Property_per_100k, Female.Veterans, Population.Density, Gini.Index..Income.Inequality.)

# Step 2: Create `Date` Variable (YYYY-MM Format)
analysis_df <- analysis_df %>%
  mutate(Date = as.Date(paste(Year, Month, "01", sep = "-")))

# Step 3: Define Treatment Dates for Treated Cities
treatment_dates <- data.frame(
  City = c("Oakland", "Santa Cruz", "Arcata", "San Francisco", "Berkeley"),
  Tdate = as.Date(c("2019-06-01", "2020-01-01", "2021-10-01", "2022-09-01", "2023-07-01"))
)

# Step 4: Merge `Tdate` into `analysis_df`
analysis_df <- analysis_df %>%
  left_join(treatment_dates, by = "City") 

# Step 5: Create `Treat` Variable (Treatment Status)
analysis_df <- analysis_df %>%
  mutate(
    Tdate = ifelse(is.na(Tdate), Inf, Tdate),
    Treat = ifelse(Date >= Tdate, 1, 0)
  )

# Ensure correct formatting
analysis_df <- analysis_df %>%
  mutate(Tdate = as.Date(Tdate, origin = "1970-01-01"))

# Step 6: Run the multisynth model
synth_model <- multisynth(
  Violent_per_100k ~ Treat | Female.Veterans + Population.Density + Gini.Index..Income.Inequality.,   
  unit = City,                
  time = Date,                
  data = analysis_df         
)

# ✅ **Save Result 1: Nu Values**
write.csv(synth_model$nu, "1cResults_Nu.csv", row.names = TRUE)

# ✅ **Save Result 2: Model Summary**
synth_summary <- summary(synth_model)
capture.output(print(synth_summary), file = "1cResults_Model_Summary.txt")

# ✅ **Save Result 3: Extract ATT**
write.csv(synth_summary$att, "1cResults_ATT.csv", row.names = FALSE)

# ✅ **Save Result 4: ATT Plot (All Levels)**
png("1cResults_ATT_Plot.png", width=800, height=600)
plot(synth_summary)
dev.off()

# ✅ **Compute RMSE for Pre-Treatment Fit**
pre_treatment_periods <- synth_summary$att[synth_summary$att$Time < 0, ]
pre_treatment_rmse <- sqrt(mean(pre_treatment_periods$Estimate^2, na.rm = TRUE))

# ✅ **Save Result 5: Pre-Treatment RMSE**
write.csv(data.frame(Pre_Treatment_RMSE = pre_treatment_rmse), "1cResults_PreTreatment_RMSE.csv", row.names = FALSE)

# ✅ **Save Result 6: ATT Plot (Average Levels)**
png("1cResults_ATT_Plot_Average.png", width=800, height=600)
plot(synth_summary, levels = "Average")
dev.off()

# ✅ **Compute Pre-Treatment Fit for Each Unit**
pre_treatment_df <- synth_summary$att[synth_summary$att$Time < 0, ] %>%
  drop_na(Estimate)

pre_treatment_rmse_by_unit <- pre_treatment_df %>%
  group_by(Level) %>%
  summarise(RMSE = sqrt(mean(Estimate^2, na.rm = TRUE)))

# ✅ **Save Result 7: Pre-Treatment RMSE for Each Unit**
write.csv(pre_treatment_rmse_by_unit, "1cResults_PreTreatment_RMSE_By_Unit.csv", row.names = FALSE)

# ✅ **Run Placebo Test**
placebo_model <- multisynth(Violent_per_100k ~ Treat | Female.Veterans + Population.Density + Gini.Index..Income.Inequality., 
                            unit = City, 
                            time = Date, 
                            data = analysis_df, 
                            permute = TRUE)

placebo_summary <- summary(placebo_model)

# ✅ **Save Result 8: Placebo Test Summary**
capture.output(print(placebo_summary), file = "1cResults_Placebo_Summary.txt")

# ✅ **Save Placebo Plot**
png("1cResults_Placebo_Plot.png", width=800, height=600)
plot(placebo_summary)
dev.off()

# ✅ **Save Result 9: Individual Unit ATT Plots**
png("1cResults_ATT_Individual_Units.png", width=800, height=600)
plot(synth_summary, levels = unique(analysis_df$City))
dev.off()

# ✅ **Extract Weights and Save**
synth_weights_df <- as.data.frame(synth_model$weights)
synth_weights_df$Donor_City <- rownames(synth_weights_df)

synth_weights_long <- pivot_longer(
  synth_weights_df, 
  cols = -Donor_City, 
  names_to = "Treated_Unit", 
  values_to = "Weight"
)

treated_units <- unique(analysis_df$City[analysis_df$Treat == 1])

treated_unit_mapping <- data.frame(
  Treated_Unit = unique(synth_weights_long$Treated_Unit),
  Treated_City = treated_units
)

synth_weights_named <- left_join(synth_weights_long, treated_unit_mapping, by = "Treated_Unit") %>%
  select(Donor_City, Treated_City, Weight)

synth_weights_filtered <- synth_weights_named %>%
  filter(Weight > 0) %>%
  arrange(Treated_City, desc(Weight))

# ✅ **Save Result 10: Synthetic Control Weights**
write.csv(synth_weights_filtered, "1cResults_Synthetic_Weights.csv", row.names = FALSE)

# ✅ **Plot Observed vs. Synthetic Fit and Save**
observed_vs_synth_plot <- ggplot(synth_summary$att, aes(x = Time, y = Estimate, group = Level)) +
  geom_line(aes(color = Level), size = 1) +
  geom_point(aes(color = Level), size = 2) +
  labs(
    title = "Observed vs. Synthetic Control",
    x = "Time Since Treatment",
    y = "Difference (Observed - Synthetic)",
    color = "Treated Unit"
  ) +
  theme_minimal()

# Save plot
ggsave("1cResults_Observed_vs_Synthetic_Fit.png", plot = observed_vs_synth_plot, width = 8, height = 6)

print("✅ All tasks completed. Results saved successfully.")


#################################PLOT THE SYNTHETIC UNIT WEIGHTS

############################# ---- PLOT 1: Improved Bar Plot for Donor Weights ----
# Create a directory to store individual plots (if it doesn’t exist)
if (!dir.exists("1cSynthetic_Weights_Plot")) {
  dir.create("1cSynthetic_Weights_Plot")
}

# Filter out donor cities that have zero weights for all treated cities
synth_weights_filtered <- synth_weights_named %>%
  filter(Weight > 0) %>%
  group_by(Treated_City) %>%
  filter(sum(Weight) > 0) %>%  # Ensures cities with no weights at all are dropped
  arrange(Treated_City, desc(Weight)) %>%
  ungroup()

# ✅ **Plot for Each Treated City Individually**
for (treated_city in unique(synth_weights_filtered$Treated_City)) {
  
  plot_data <- synth_weights_filtered %>% filter(Treated_City == treated_city)
  
  # Create a bar plot for each treated city separately
  p <- ggplot(plot_data, aes(x = reorder(Donor_City, Weight), y = Weight, fill = Donor_City)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(title = paste("1cSynthetic Weights for", treated_city),
         x = "Donor Cities", y = "Weight") +
    theme_minimal() +
    theme(legend.position = "none", axis.text.y = element_text(size = 8))
  
  # Save each plot
  ggsave(filename = paste0("1cSynthetic_Weights_", gsub(" ", "_", treated_city), ".png"), plot = p, width = 10, height = 6)
}


############VARY NU

# Define a sequence of nu values
#nu_values <- unique(c(seq(0, 1, by = 0.05))
nu_values <- seq(0, 1, by = 0.05)


# Initialize a dataframe to store ATT results
att_results <- data.frame(Nu = numeric(), ATT_Estimate = numeric(), ATT_Std_Error = numeric())


# Loop over different nu values
for (nu in nu_values) {
  
  # Run the multisynth model with a fixed nu
  synth_model <- multisynth(
    Violent_per_100k ~ Treat | Female.Veterans + Population.Density + Gini.Index..Income.Inequality.,   
    unit = City,                
    time = Date,                
    data = analysis_df,
    nu = nu  # Fix nu at each iteration
  )
  
  # Extract the ATT estimate and standard error (Ensure single value)
  synth_summary <- summary(synth_model)
  att_value <- tail(synth_summary$att$Estimate[synth_summary$att$Level == "Average"], 1)
  att_std_error <- tail(synth_summary$att$Std.Error[synth_summary$att$Level == "Average"], 1)
  
  # Ensure we are storing only valid values
  if (length(att_value) == 1 && length(att_std_error) == 1 && !is.na(att_value) && !is.na(att_std_error)) {
    att_results <- rbind(att_results, data.frame(Nu = nu, ATT_Estimate = att_value, ATT_Std_Error = att_std_error))
  } else {
    print(paste("Skipping nu =", nu, "due to missing ATT or Std. Error"))
  }
}



print(att_results)


if (nrow(att_results) > 0) {
  write.csv(att_results, "1cSimulation_ATT_vs_Nu.csv", row.names = FALSE)
} else {
  print("No ATT results to save. Check the loop or model output.")
}

# Load required libraries
library(ggplot2)

# Define Y-axis limits to zoom in (adjust as needed)
y_min <- min(att_results$ATT_Estimate - att_results$ATT_Std_Error, na.rm = TRUE)
y_max <- max(att_results$ATT_Estimate + att_results$ATT_Std_Error, na.rm = TRUE)

# Create the ATT vs. Nu plot with zoomed-in Y-axis
ggplot(att_results, aes(x = Nu, y = ATT_Estimate)) +
  geom_line(color = "black") +                      # Line showing ATT variation
  geom_ribbon(aes(ymin = ATT_Estimate - ATT_Std_Error, 
                  ymax = ATT_Estimate + ATT_Std_Error), 
              fill = "gray", alpha = 0.5) +         # Confidence interval (shaded)
  geom_point(color = "red", size = 3) +             # Highlight individual points
  labs(title = "Effect of Varying Nu on ATT Estimate",
       x = expression(nu), 
       y = "ATT Estimate") +
  theme_minimal() +
  ylim(y_min, y_max)                                # 🔥 Zoom in on Y-axis

# Save the plot with high resolution
ggsave("1cSimulation_ATT_vs_Nu.png", width = 7, height = 5, dpi = 300)


