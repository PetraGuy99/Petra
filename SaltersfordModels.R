setwd('C:/dev/code/Petra')

library(dplyr)
library(ggplot2)
library(readxl)
library(tidyr)

data= read_excel('../../data/Saltersford_WorkingSheetCopy.xlsx', sheet = 'Combined')
data = data %>% drop_na(Height_2024)

#tidy some colnames
data = data%>% 
  rename(
    height = Height_2024,
    treatment = Treatment,
    time = timepoint,
    tree = Tree_Species
  )
#tidy column text
data$treatment <- ifelse(data$treatment == "Fungi_Treatment", "Pellet", 
                         ifelse(data$treatment == "Fungi_Control", "Control", data$treatment))

#get the plot averages for time points 0,1,2 for control,treatment for oak,birch


#look at quick summary
group_means <- data %>%
  group_by(time, treatment,tree) %>%
  summarize(mean_height = mean(height), .groups = "drop")

print(group_means)


#look at data across the three time points


ggplot(data, aes(x = factor(time), y = height, fill = treatment)) +
  geom_boxplot(width = 0.5) +
  labs(title = "Saltersford, height measurements across three time points",
       x = "Months since planting",
       y = "Height",
       fill = "Treatment") +
  scale_fill_manual(values = c("Control" = "orange", "Fertiliser" = 'yellow', "Pellet" = "darkgreen")) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    strip.text = element_text(size = 14, face = "bold") 
  ) +
  facet_wrap(~ tree)



#try linear model

linear = lm(height ~ time + treatment + time*tree + treatment*tree, data = data)

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



#check the model
plot(residuals(linear), main = 'Residuals vs Fitted')
qqnorm(residuals(linear))
qqline(residuals(linear), col = "red")

# Shapiro-Wilk test
shapiro.test(residuals(linear)) # Ho = data is normal so low p means not

###############################################################################
#split the data by tree because easier to visualize? ANd does the difference in growth
#rates of birch compared to oak give the negative growth term, which isnt really
#what we're after

data.birch = data %>% filter(tree =='Betula_pendula')
data.oak =  data %>% filter(tree == 'Quercus_robur')

linear = lm(height ~ treatment*time, data = data.oak)

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



#check the model
plot(residuals(linear), main = 'Residuals vs Fitted')
qqnorm(residuals(linear))
qqline(residuals(linear), col = "red")

# Shapiro-Wilk test
shapiro.test(residuals(linear)) # Ho = data is normal so low p means not

#########################################################################

#try glm
glm.log = glm(height~ time + treatment + time*treatment, data = data.oak, family = gaussian(link = log))
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

########################################################################
#Bootstrapping

#smallest set for birch is 50 in control
#smallest subset for oak is 92 in teratment final time point

data.birch = data %>% filter(tree =='Betula_pendula')
data.oak =  data %>% filter(tree == 'Quercus_robur')


#OAK#/Birch bootstrapping################################


results = list()

for (i in 1:100) {
  
  # Randomly sample 100 points from each treatment group at each time point
  sampled_data <- data.birch %>% # change for which tree spp
    group_by(treatment, time) %>%
    sample_n(50, replace = FALSE)  # Sample 50 for birch, 91 for oak points per treatment per time point
  
  # Fit the GLM for each sampled subset
  linear = lm(height ~ treatment*time, data = sampled_data)
  
  # Store both the GLM coefficients and p-values in the list
  model_summary <- summary(linear)
  coefficients <- model_summary$coefficients
  R2 = summary(linear)$r.squared
  
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
coefficients_df$Coefficient = c('time','time*Pellet') #rename for axis neatness
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
  #ylim(0,0.025)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better visibility

# Print mean p-values for reference
print(coefficients_df)

count_significant_p_values <- sum(p_values_matrix[4,] <= 0.05)

# Print the result
print(count_significant_p_values)


############################################################################


# we may have introduced an error at time point 3 because we resampled 100 trees
#which may have been different to those sampled at earlier times - for birch they definitely were different
#because we added new trees - only  control and 59 treatment were sampled at time point 1

#delete these outliers and see what the data looks like

#what are the medians for oak and birch at month 11

#look at quick summary
group_medians = data %>%
  group_by(time, treatment,tree) %>%
  summarize(median_height = median(height), .groups = "drop")

print(group_medians)
#remove trees from time 17 month which are:bp.control < 102, bl.pellet < 106
#oak.control < 73.5, oak.treatment < 70

#calculate growth as average for the plot - initial average
#subtract initial to get a zero start point

bp_control0 = group_means[1,4] - group_means[1,4]
bp_control1 = group_means[2,4] - group_means[1,4]
bp_control2 = group_means[3,4] - group_means[1,4]

bp_pellet0 = group_means[8,4] - group_means[8,4]
bp_pellet1 = group_means[9,4] - group_means[8,4]
bp_pellet2 = group_means[10,4] - group_means[8,4]

qr_control0 = group_means[4,4] - group_means[4,4]
qr_control1 = group_means[5,4] - group_means[4,4]
qr_control2 = group_means[6,4] - group_means[4,4]

qr_pellet0 = group_means[11,4] - group_means[11,4]
qr_pellet1 = group_means[12,4] - group_means[11,4]
qr_pellet2 = group_means[13,4] - group_means[11,4]

#make these into a df

df = rbind.data.frame(bp_control0,bp_control1,bp_control2,
                      bp_pellet0,bp_pellet1,bp_pellet2,
                      qr_control0,qr_control1,qr_control2,
                      qr_pellet0, qr_pellet1, qr_pellet2 )
df$treat = c('control','control','control',
             'pellet','pellet','pellet',
             'control','control','control',
             'pellet','pellet','pellet')
df$tree = c(rep('birch',6), rep('oak',6))
df$Year = rep(c(0,11,17),4)

library(ggplot2)

# Create the plot
ggplot(df, aes(x = Year, y = Mean_Height, color = treat, linetype = tree)) +
  geom_line(size = 1) +  # Add lines
  geom_point(size = 3) + # Add points for better visualization
  labs(
    title = "Change in average plot height for Betula pendula and Quercus robur at CWT Saltersford ",
    x = "Months since planting",
    y = "Change in average plot height (cm)"
  ) +
  scale_color_manual(
    values = c("control" = "orange", "pellet" = "darkgreen")
  ) +
  theme_minimal() +
  theme(
    legend.position = "right", # Adjust legend position
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 16) ,
    axis.title.x = element_text(size = 16) # Adjust x-axis labels
  )

bp_control0 = group_means[1,4] - group_means[1,4]
bp_control1 = group_means[2,4] - group_means[1,4]
bp_control2 = group_means[3,4] - group_means[1,4]

bp_pellet0 = group_means[8,4] - group_means[8,4]
bp_pellet1 = group_means[9,4] - group_means[8,4]
bp_pellet2 = group_means[10,4] - group_means[8,4]

qr_control0 = group_means[4,4] - group_means[4,4]
qr_control1 = group_means[5,4] - group_means[4,4]
qr_control2 = group_means[6,4] - group_means[4,4]

qr_pellet0 = group_means[11,4] - group_means[11,4]
qr_pellet1 = group_means[12,4] - group_means[11,4]
qr_pellet2 = group_means[13,4] - group_means[11,4]

#make these into a df

df = rbind.data.frame(bp_control0,bp_control1,bp_control2,
                      bp_pellet0,bp_pellet1,bp_pellet2,
                      qr_control0,qr_control1,qr_control2,
                      qr_pellet0, qr_pellet1, qr_pellet2 )
df$treat = c('control','control','control',
             'pellet','pellet','pellet',
             'control','control','control',
             'pellet','pellet','pellet')
df$tree = c(rep('birch',6), rep('oak',6))
df$Year = rep(c(0,11,17),4)

library(ggplot2)

# Create the plot
ggplot(df, aes(x = Year, y = Mean_Height, color = treat, linetype = tree)) +
  geom_line(size = 1) +  # Add lines
  geom_point(size = 3) + # Add points for better visualization
  labs(
    title = "Change in average plot height for Betula pendula and Quercus robur at CWT Saltersford ",
    x = "Months since planting",
    y = "Change in average plot height (cm)"
  ) +
  scale_color_manual(
    values = c("control" = "orange", "pellet" = "darkgreen")
  ) +
  theme_minimal() +
  theme(
    legend.position = "right", # Adjust legend position
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 16) ,
    axis.title.x = element_text(size = 16) # Adjust x-axis labels
  )
##relative change plots######################################


bp_control0 = group_means[1,4] - group_means[1,4]
bp_control1 = (group_means[2,4] - group_means[1,4])/group_means[1,4]
bp_control2 = (group_means[3,4] - group_means[1,4])/group_means[1,4]

bp_pellet0 = group_means[8,4] - group_means[8,4]
bp_pellet1 = (group_means[9,4] - group_means[8,4])/group_means[8,4]
bp_pellet2 = (group_means[10,4] - group_means[8,4])/group_means[8,4]

qr_control0 = group_means[4,4] - group_means[4,4]
qr_control1 = (group_means[5,4] - group_means[4,4])/group_means[4,4]
qr_control2 = (group_means[6,4] - group_means[4,4])/group_means[4,4]

qr_pellet0 = group_means[11,4] - group_means[11,4]
qr_pellet1 = (group_means[12,4] - group_means[11,4])/group_means[11,4]
qr_pellet2 = (group_means[13,4] - group_means[11,4])/group_means[11,4]

#make these into a df

df = rbind.data.frame(bp_control0,bp_control1,bp_control2,
                      bp_pellet0,bp_pellet1,bp_pellet2,
                      qr_control0,qr_control1,qr_control2,
                      qr_pellet0, qr_pellet1, qr_pellet2 )
df$treat = c('control','control','control',
             'pellet','pellet','pellet',
             'control','control','control',
             'pellet','pellet','pellet')
df$tree = c(rep('birch',6), rep('oak',6))
df$Year = rep(c(0,11,17),4)

library(ggplot2)

# Create the plot
ggplot(df, aes(x = Year, y = Mean_Height, color = treat, linetype = tree)) +
  geom_line(size = 1) +  # Add lines
  geom_point(size = 3) + # Add points for better visualization
  labs(
    title = "Relative change in average plot height for Betula pendula and Quercus robur at CWT Saltersford ",
    x = "Months since planting",
    y = "Relative change in average plot height"
  ) +
  scale_color_manual(
    values = c("control" = "orange", "pellet" = "darkgreen")
  ) +
  theme_minimal() +
  theme(
    legend.position = "right", # Adjust legend position
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 16) ,
    axis.title.x = element_text(size = 16) # Adjust x-axis labels
  )





