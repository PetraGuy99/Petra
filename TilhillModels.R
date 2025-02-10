setwd('C:/dev/code/Petra')
library(dplyr)
library(ggplot2)
library(readxl)
library(tidyr)
library(lme4)
library(geepack)
library(car)


#since the single site data has only 1 tree species and 1 emf and a control and
#treatment plot, there are not enough levels for a mixed effect model - i.e. only 2
#where here plot or treatment are effectively the same thing.
#In addition, since this data is repeated measures the points are not independant 
#and hence glms are not appropriate.
#Therefore a GEE model is the right thing
#BUT if the measurements at each time point are NOT correlated, then it doesnt
#matter and a GLM can be used.

#you can create an autocorrelation function - but clearly - the height at t2 will 
#be correlated with the height at t1

#
#read data.This is all data recorded, each group is a different size, but that
#doesnt matter for this analysis

#reading GEE sheet, which was munged to fit to required data shape of the GEE model
#But otherwise, it doesnt matter, its long format with reduced columns
data = read_excel('../../data/TilhillWorkingCopy.xlsx', sheet = 'GEEdata')

#delete NA rows
data = data %>% drop_na(height)

#look at quick summary
mean_heights <- data %>%
  group_by(time, treatment,tree) %>%
  summarize(mean_height = mean(height), .groups = "drop")

print(mean_heights)

#boxplots of all the poonts
ggplot(data, aes(x = factor(time), y = height, fill = treatment)) +
  geom_boxplot() +
  labs(title = "Tilhill, height measurements across three time points",
       x = "Months since planting",
       y = "Height",
       fill = "Treatment") +
  scale_fill_manual(values = c("Control" = "orange", "Pellet" = "darkgreen")) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1) )+
  theme(
    legend.position = "right", # Adjust legend position
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 16),
    axis.title.y = element_text(size = 16) ,
    axis.title.x = element_text(size = 16) # Adjust x-axis labels
  )

#just fit a linear model!
linear = lm(height~time+treatment+treatment*time, data = data)

summary(linear)

coefficients <- coef(linear)
conf_int <- confint(linear)

plot_data <- data.frame(
  Term = names(coefficients),
  Estimate = coefficients,
  Lower = conf_int[, 1],
  Upper = conf_int[, 2]
)

plot_data = plot_data[-1,] # remove intercept - not useful
ggplot(plot_data, aes(x = Term, y = Estimate)) +
  geom_point(size = 3, color = "blue") +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(
    title = "Coefficient Estimates with Confidence Intervals",
    x = "Predictors",
    y = "Estimate"
  )
#the interaction between treatment and time is not significant

#check the model
plot(residuals(linear), main = 'Residuals vs Fitted')
qqnorm(residuals(linear))
qqline(residuals(linear), col = "red")

# Shapiro-Wilk test
shapiro.test(residuals(linear)) # Ho = data is Normal, so low p means we refit this

##############################################################################

#GLM - log link

glm.log = glm(height~ time + treatment + time*treatment, data = data, family = gaussian(link = log))
summary(glm.log)
coefficients <- coef(glm.log)
conf_int <- confint(glm.log)

plot_data <- data.frame(
  Term = names(coefficients),
  Estimate = coefficients,
  Lower = conf_int[, 1],
  Upper = conf_int[, 2]
)

plot_data = plot_data[-1,] # remove intercept - not useful
ggplot(plot_data, aes(x = Term, y = Estimate)) +
  geom_point(size = 3, color = "blue") +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(
    title = "Coefficient Estimates with Confidence Intervals",
    x = "Predictors",
    y = "Estimate"
  )

# Calculate R-squared
deviance <- summary(glm.log)$deviance
null_deviance <- summary(glm.log)$null.deviance
rsquared <- 1 - (deviance / null_deviance)

# Print the R-squared value
print(rsquared)

plot(residuals(glm.log), main = 'Residuals vs Fitted')
qqnorm(residuals(glm.log))
qqline(residuals(glm.log), col = "red")

# Shapiro-Wilk test
shapiro.test(residuals(glm.log)) # Ho = data is Normal, so low p means we refut this

#predict for exp model
data$predicted_log_height <- predict(glm.log, newdata = data, re.form = NULL)

# Back-transform predictions to the original scale (exponentiate)
data$predicted_height_exp <- exp(data$predicted_log_height)

# Plot actual vs predicted heights
ggplot(data, aes(x = time, y = height, color = treatment)) +
  geom_point() +  # Actual data points
  geom_line(aes(y = predicted_height_exp), size = 1, linetype = "dashed") +  # Exponentiated predicted values
  labs(title = "Actual vs Predicted Heights - GLM log link", x = "Time", y = "Height") +
  scale_colour_manual(values = c("Control" = "orange", "Pellet" = "darkgreen")) + 
  theme(
    legend.position = "right", # Adjust legend position
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 16) ,
    axis.title.x = element_text(size = 16) # Adjust x-axis labels
  )

###############################################################################

#GEE

#create a 0,1 column for the treatment and put on order of 0,1
data$binary = ifelse(data$treatment == 'Control',0,1)
#put the data in order before running
data <- data[order(data$binary, data$time), ]

gee_model <- geeglm(height ~ time*binary, 
                    id = plot,  # plot is the grouping factor (id variable)
                    family = gaussian,  # assuming normal distribution for height
                    corstr = "exchangeable",  # correlation structure (exchangeable here)
                    data = data) #gives same result as the linear mixed effect model


summary(gee_model) # not working 28/1/2025 - NaNs imply matrix singular, but not sure why

###################################################################################

#mixed effects - even though shouldnt really do it with only 2 random effects

mixed_model_exp = mixed_model <- lmer(log(height) ~ time * treatment +(1 | plot), data = data) 
summary(mixed_model_exp)

###############################################################################

#what about if I randomly sample 100 from first two time time points and run models with balanced
#measurements
month0 = data %>% filter(time==0)
mean_control0 <- mean(month0$height[month0$treatment == "Control"])
mean_treatment0 <- mean(month0$height[month0$treatment == "Pellet"])

month11 = data%>% filter(time == 11)
mean_control11 <- mean(month11$height[month11$treatment == "Control"])
mean_treatment11 <- mean(month11$height[month11$treatment == "Pellet"])

month17 = data %>% filter(time == 17)
mean_control17 <- mean(month17$height[month17$treatment == "Control"])
mean_treatment17 <- mean(month17$height[month17$treatment == "Pellet"])

#look at the distributions
g1 = ggplot(month0, aes(x = height, fill = treatment)) +
  geom_histogram(position = "identity", alpha = 0.3, binwidth = 2) + 
  scale_fill_manual(values = c( "orange","darkgreen")) +  # Custom colors for the treatments
  labs(title = "initial heights", x = "", y = "Frequency")+
  geom_vline(xintercept = mean_control0, color = "orange", linetype = "dashed", size = 1.2) +  # Control mean
  geom_vline(xintercept = mean_treatment0, color = "green", linetype = "dashed", size = 1.2) +
  scale_x_continuous(limits = c(15, 135), breaks = seq(15, 135, by = 10)) + 
  #theme(
    #plot.margin = margin(2, 2, 2, 2),  # Reduce extra space around the plot
    #aspect.ratio = 1.25  # Makes the plot taller and narrower
  #) +
theme(
  legend.position = "none", # Adjust legend position
  axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
  axis.text.y = element_text(size = 12),
  axis.title.y = element_text(size = 16) ,
  axis.title.x = element_text(size = 1) # Adjust x-axis labels
)+
  theme(
    panel.background = element_blank(),  # Remove panel background
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.border = element_rect(color = "black", fill = NA)  # Add border if needed
  )

g2 = ggplot(month11, aes(x = height, fill = treatment)) +
  geom_histogram(position = "identity", alpha = 0.3, binwidth = 2) + 
  scale_fill_manual(values = c( "orange","darkgreen")) +# Custom colors for the treatments
  labs(title = "Month 11", x = "", y = "Frequency")+
  geom_vline(xintercept = mean_control11, color = "orange", linetype = "dashed", size = 1.2) +  # Control mean
  geom_vline(xintercept = mean_treatment11, color = "green", linetype = "dashed", size = 1.2) +
  scale_x_continuous(limits = c(15, 135), breaks = seq(15, 135, by = 10)) + 
 
    # theme(
  #   plot.margin = margin(2, 2, 2, 2),  # Reduce extra space around the plot
  #   aspect.ratio = 1.25  # Makes the plot taller and narrower
  # ) +
  theme(
    legend.position = "none", # Adjust legend position
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 16) ,
    axis.title.x = element_text(size = 1) # Adjust x-axis labels
  )+
  theme(
    panel.background = element_blank(),  # Remove panel background
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.border = element_rect(color = "black", fill = NA)  # Add border if needed
  )

g3 = ggplot(month17, aes(x = height, fill = treatment)) +
  geom_histogram(position = "identity", alpha = 0.3, binwidth = 2) + 
  scale_fill_manual(values = c( "orange","darkgreen")) + # Custom colors for the treatments
  labs(title = "Month 17", x = "", y = "Frequency")+
  geom_vline(xintercept = mean_control17, color = "orange", linetype = "dashed", size = 1.2) +  # Control mean
  geom_vline(xintercept = mean_treatment17, color = "green", linetype = "dashed", size = 1.2) +
  scale_x_continuous(limits = c(15, 135), breaks = seq(15, 135, by = 10)) + 
  
  scale_y_continuous(breaks = c(2,4,6,8))+
   theme(
   legend.position = "none", # Adjust legend position
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 16) ,
    axis.title.x = element_text(size = 1),
    panel.background = element_blank(),  # Remove panel background
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.border = element_rect(color = "black", fill = NA)  # Add border if needed
  )

#run g3 with legend to extarct the legend as a grob , then turn it off, and plot the legend grob
#in grid.arrange
#legend = ggplotGrob(g3)$grobs[[which(sapply(ggplotGrob(g3)$grobs, function(x) x$name) == "guide-box")]]

#plot the hisotgrams for the other time points
#library(gridExtra)
grid.arrange(g1,g2,g3,legend, ncol = 1)


#######BOOTSTRAPPING####################
timepoint3 = data %>% filter(time == 17)

rsq <- function(model) {
  1 - sum(residuals(model, type = "deviance")^2) / sum((model$y - mean(model$y))^2)
}
results = list()

for (i in 1:100) {
  
  # Subset the data for time points 1 and 2
  subset_data <- data %>% filter(time %in% c(0, 11))
  
  # Randomly sample 100 points from each treatment group at each time point
  sampled_data <- subset_data %>%
    group_by(treatment, time) %>%
    sample_n(100, replace = FALSE)  # Sample 100 points per treatment per time point
  
  # Fit the GLM for each sampled subset
  glm_model <- glm(height ~ treatment * time, data = sampled_data, family = gaussian(link = log) )
  
  # Store both the GLM coefficients and p-values in the list
  model_summary <- summary(glm_model)
  coefficients <- model_summary$coefficients
  R2 = rsq(glm_model)
  
  results[[i]] <- list(
    coefficients = coefficients[,1],  # Store coefficient estimates
    p_values = coefficients[,4] , # Store p-values
    Rsquared = R2 #store R2
  )
}

# Extract the coefficients and p-values from the list
coefficients_list <- lapply(results, function(x) x$coefficients)  # Extract coefficients
p_values_list <- lapply(results, function(x) x$p_values) # Extract p-values
R2_values_list=  lapply(results, function(x) x$Rsquared)

# Combine the coefficients and p-values into matrices
coefficients_matrix <- do.call(cbind, coefficients_list)
p_values_matrix <- do.call(cbind, p_values_list)
Rsquared_matrix <- do.call(cbind, R2_values_list)

# Compute the mean and standard deviation of each coefficient across 100 runs
mean_coefficients <- apply(coefficients_matrix, 1, mean)
sd_coefficients <- apply(coefficients_matrix, 1, sd)


# Compute the mean p-values across 100 runs
mean_p_values <- apply(p_values_matrix, 1, mean)
mean_R2_values <- apply(Rsquared_matrix, 1, mean)

# Create a data frame for plotting
coefficients_df <- data.frame(
  Coefficient = rownames(coefficients_matrix),
  Mean = mean_coefficients,
  SD = sd_coefficients,
  P_Value = mean_p_values,
  R2 = mean_R2_values
  
)

#remove intercept and pellet effect
coefficients_df = coefficients_df[-c(1,2),]
coefficients_df$Coefficient = c('time','growth rate') #rename for axis neatness
# Plot the mean coefficients as points with error bars representing the standard deviation
ggplot(coefficients_df, aes(x = Coefficient, y = Mean)) +
  geom_point(size = 5, color = "darkgreen") +  # Plot points for mean coefficients
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.2) +  # Error bars for SD
  labs(title = "Mean Coefficients with Standard Deviation", 
       x = "Coefficient", 
       y = "Mean Coefficient Value") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 16, face = "bold"),  
    axis.title.y = element_text(size = 16, face = "bold") 
  )+
  ylim(0,0.025)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better visibility

# Print mean p-values for reference
print(coefficients_df)

count_significant_p_values <- sum(p_values_matrix[4,] <= 0.05)

# Print the result
print(count_significant_p_values)

###########################

#since the treated trees being smaller at the start than the control trees is an artifact 
#of what trees the contractors planted where - why not add the mean difference back on 
#set everything to a 'zero' start point. Add the mean difference at the start on to all the treatment trees 
 
#mean difference at start
#look at quick summary
mean_heights <- data %>%
  group_by(time, treatment,tree) %>%
  summarize(mean_height = mean(height), .groups = "drop")

height_adjustment = as.numeric(mean_heights[1,4] - mean_heights[2,4])

#add this back to every treatment column

data.adjusted <- data %>%
  mutate(across(contains("height"), ~ ifelse(treatment == "Pellet", . + height_adjustment, .)))

#box plots now look like...
ggplot(data.adjusted, aes(x = factor(time), y = height, fill = treatment)) +
  geom_boxplot() +
  labs(title = "Tilhill, height measurements across three time points",
       x = "Months since planting",
       y = "Height",
       fill = "Treatment") +
  scale_fill_manual(values = c("Control" = "orange", "Pellet" = "darkgreen")) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1) )+
  theme(
    legend.position = "right", # Adjust legend position
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 16) ,
    axis.title.x = element_text(size = 16) # Adjust x-axis labels
  )


#now repeat the models to see if they are easier to picture and understand the coefficients

timepoint3 = data.adjusted %>% filter(time == 17)

rsq <- function(model) {
  1 - sum(residuals(model, type = "deviance")^2) / sum((model$y - mean(model$y))^2)
}
results = list()

for (i in 1:100) {
  
  # Subset the data for time points 1 and 2
  subset_data <- data.adjusted %>% filter(time %in% c(0, 11))
  
  # Randomly sample 100 points from each treatment group at each time point
  sampled_data <- subset_data %>%
    group_by(treatment, time) %>%
    sample_n(100, replace = FALSE)  # Sample 100 points per treatment per time point
  
  # Fit the GLM for each sampled subset
  glm_model <- glm(height ~ treatment * time, data = sampled_data, family = gaussian(link = log) )
  
  # Store both the GLM coefficients and p-values in the list
  model_summary <- summary(glm_model)
  coefficients <- model_summary$coefficients
  R2 = rsq(glm_model)
  
  results[[i]] <- list(
    coefficients = coefficients[,1],  # Store coefficient estimates
    p_values = coefficients[,4] , # Store p-values
    Rsquared = R2 #store R2
  )
}

# Extract the coefficients and p-values from the list
coefficients_list <- lapply(results, function(x) x$coefficients)  # Extract coefficients
p_values_list <- lapply(results, function(x) x$p_values) # Extract p-values
R2_values_list=  lapply(results, function(x) x$Rsquared)

# Combine the coefficients and p-values into matrices
coefficients_matrix <- do.call(cbind, coefficients_list)
p_values_matrix <- do.call(cbind, p_values_list)
Rsquared_matrix <- do.call(cbind, R2_values_list)

# Compute the mean and standard deviation of each coefficient across 100 runs
mean_coefficients <- apply(coefficients_matrix, 1, mean)
sd_coefficients <- apply(coefficients_matrix, 1, sd)


# Compute the mean p-values across 100 runs
mean_p_values <- apply(p_values_matrix, 1, mean)
mean_R2_values <- apply(Rsquared_matrix, 1, mean)

# Create a data frame for plotting
coefficients_df <- data.frame(
  Coefficient = rownames(coefficients_matrix),
  Mean = mean_coefficients,
  SD = sd_coefficients,
  P_Value = mean_p_values,
  R2 = mean_R2_values
  
)

#remove intercept
coefficients_df = coefficients_df[-1,]
# Plot the mean coefficients as points with error bars representing the standard deviation
ggplot(coefficients_df, aes(x = Coefficient, y = Mean)) +
  geom_point(size = 4, color = "darkgreen") +  # Plot points for mean coefficients
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.2) +  # Error bars for SD
  labs(title = "Mean Coefficients with Standard Deviation", 
       x = "Coefficient", 
       y = "Mean Coefficient Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better visibility

# Print mean p-values for reference
print(coefficients_df)

count_significant_p_values <- sum(p_values_matrix[4,] <= 0.05)

# Print the result
print(count_significant_p_values)



################################################################

###DOING THE MEAN GRAPHS AGAIN

group_means <- data %>%
  group_by(treatment, time) %>%
  summarize(Mean_Height = mean(height, na.rm = TRUE), .groups = "drop") 

# Subtract the minimum mean height from each group's mean height
group_means_adj <- group_means %>%
  group_by(treatment) %>%  # Group by treatment to get the minimum mean for each treatment
  mutate(Min_Treatment_Mean = min(Mean_Height)) %>%  # Find the minimum mean for each treatment group
  ungroup() %>%  # Remove grouping
  mutate(Mean_Height_Adjusted = Mean_Height - Min_Treatment_Mean)

ggplot(group_means_adj, aes(x = time, y = Mean_Height_Adjusted, color = treatment)) +
  geom_point(size = 4) +  # Plot points
  geom_line(size = 1) +   # Connect the points with a line
  scale_color_manual(values = c("Control" = "orange", "Pellet" = "darkgreen")) + # Custom colors for treatments
  labs(title = "Adjusted Mean Heights", x = "Time(months)", y = "Adjusted Mean Height(cm)") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )





