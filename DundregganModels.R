setwd('C:/dev/code/Petra')

library(dplyr)
library(ggplot2)
library(readxl)
library(tidyr)

data = read_excel('../../data/Dundreggan_Workingheet.xlsx', sheet = 'Combined all')

data = data %>% drop_na(Height)

#tidy some colnames
data = data%>% 
  rename(
    height = Height,
    treatment = Treatment,
    time = timepoint
  )
#tidy column text
data$treatment <- ifelse(data$treatment == "Fungi_Treatment", "Pellet", 
                       ifelse(data$treatment == "Fungi_Control", "Control", data$treatment))

#look at quick summary
mean_heights <- data %>%
  group_by( time, Site, treatment) %>%
  summarize(mean_height = mean(height), .groups = "drop")

print(mean_heights)

#boxplots of all the poont
ggplot(data, aes(x = factor(time), y = height, fill = treatment)) +
  geom_boxplot(width = 0.5) +
  labs(title = "Dundreggan, height measurements across three time points",
       x = "Months since planting",
       y = "Height",
       fill = "Treatment") +
  scale_fill_manual(values = c("Control" = "orange", "Fertiliser" = 'blue', "Pellet" = "darkgreen")) + 
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
  facet_wrap(~ Site)  # Facet by Site

#########################################################################

#just fit a linear model, all sites together
linear = lm(height ~ time + Site + Site*treatment + Site*time, data = data)

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
shapiro.test(residuals(linear)) # Ho = data is normal so low p means not

#GLM - log link###########################################################

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
  scale_colour_manual(values = c("Control" = "orange", "Fertiliser" = "yellow", "Pellet" = "darkgreen")) + 
  theme(
    legend.position = "right", # Adjust legend position
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 16) ,
    axis.title.x = element_text(size = 16) # Adjust x-axis labels
  )

########################
#problem with those models is it get very confusing because control does not appear
#at each site. Going to create 2 different data sets for site 1 and 2 with
#control replicated at each and treat it as 2 different sites to model

data.1 = data %>% filter(Site == 'Site_1')
data.2 = data %>% filter(Site == 'Site_2')
data.control = data %>% filter(Site == 'Site_3')

data_site1 = rbind.data.frame(data.1,data.control)
data_site2 = rbind.data.frame(data.2, data.control)

#try linear model

linear = lm(height ~ time*treatment, data = data_site2) # change for each site

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

#replot without only the interaction terms for clarity
plot_data = plot_data[-c(1:3),]

#add row to do color
plot_data$treatment = c('Fertiliser','Pellet')

#add row to make axis names nicer
plot_data$Term = c("time*Fertiliser", "time*Pellet")

ggplot(plot_data, aes(x = Term, y = Estimate, colour = treatment)) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  scale_color_manual(values = c("Fertiliser" = "orange", "Pellet" = "green")) + 
  theme_minimal() +
  theme(
  axis.text.x = element_blank(),
  axis.text.y = element_text(size = 16),
  axis.title.x = element_text(size = 16, face = "bold"),  
  axis.title.y = element_text(size = 16, face = "bold") 
  )+
  labs(
    title = "Coefficient Estimates with Confidence Intervals",
    x = "growth rate",
    y = "Estimate"
  )

#the interaction between treatment and time is not significant

#check the model
plot(residuals(linear), main = 'Residuals vs Fitted')
qqnorm(residuals(linear))
qqline(residuals(linear), col = "red")

# Shapiro-Wilk test
shapiro.test(residuals(linear)) # Ho = data is normal so low p means not


############################################################################

#try glm log link and see if fit is any better

glm.log = glm(height~ time + treatment + time*treatment, data = data_site2, family = gaussian(link = log))
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
##########################################################################
#the time points are unbalanced at site 1 we sampled less at point 3, but at site 2 we sample more

#SITE 1. Control = 202,202,100. Pellet = 144,145,100, Fert = 84,92,100
#Random re samples of 80 at each time point.

rsq <- function(model) {
  1 - sum(residuals(model, type = "deviance")^2) / sum((model$y - mean(model$y))^2)
}

data = data_site2
results = list()

for (i in 1:100) {
  
 
  # Randomly sample 100 points from each treatment group at each time point
  sampled_data <- data %>%
    group_by(treatment, time) %>%
    sample_n(60, replace = FALSE)  # Sample 100 points per treatment per time point
  
  # Fit the LM for each sampled subset
  linear = lm(height ~ time*treatment, data = sampled_data)
  
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






###########################################################################
#get the plot averages for time points 0,1,2 for control,treatment for oak,birch

#plot 1,2 are treatment/fertiliser, plot 3 is control

group_means <- data %>%
  group_by(treatment, time, Site) %>%
  summarize(Mean_Height = mean(height, na.rm = TRUE), .groups = "drop")

#calculate growth as average for the plot - initial average
#subtract initial to get a zero start point

control0= group_means[7,4] - group_means[7,4]
control1 = group_means[8,4] - group_means[7,4]
control2 = group_means[9,4] - group_means[7,4]

plot1_pellet0 = group_means[10,4] - group_means[10,4]
plot1_pellet1 = group_means[12,4] - group_means[10,4]
plot1_pellet2 = group_means[14,4] - group_means[10,4]

plot2_pellet0 = group_means[11,4] - group_means[11,4]
plot2_pellet1 = group_means[13,4] - group_means[11,4]
plot2_pellet3 = group_means[15,4] - group_means[11,4]

plot1_fert0= group_means[1,4] - group_means[1,4]
plot1_fert1 = group_means[3,4] - group_means[1,4]
plot1_fert2 = group_means[5,4] - group_means[1,4]

plot2_fert0= group_means[2,4] - group_means[2,4]
plot2_fert1 = group_means[4,4] - group_means[2,4]
plot2_fert2 = group_means[6,4] - group_means[2,4]

#make these into a df

df = rbind.data.frame(control0,control1,control2,
                      plot1_pellet0,plot1_pellet1,plot1_pellet2,
                      plot2_pellet0,plot2_pellet1,plot2_pellet3,
                      plot1_fert0, plot1_fert1, plot1_fert2,
                      plot2_fert0, plot2_fert1, plot2_fert2)
df$treat = c('control','control','control',
             'pellet1','pellet1','pellet1',
             'pellet2','pellet2','pellet2',
             'fert1','fert1','fert1',
             'fert2','fert2','fert2'
                          )

df$Year = rep(c(0,9,15),5)
df$plot = c(rep('c',3), rep(1,3), rep(2,3), rep(1,3), rep(2,3))

library(ggplot2)

# Create the plot
ggplot(df, aes(x = Year, y = Mean_Height, color = treat, linetype = plot)) +
  geom_line(size = 1) +  # Add lines
  geom_point(size = 3) + # Add points for better visualization
  labs(
    title = "Change in average plot height for Betula pubescens at Trees for Life",
    x = "Months since planting",
    y = "Change in average plot height (cm)"
  ) +
  scale_color_manual(
    values = c("control" = "orange", "pellet1" = "darkgreen", "pellet2" = "darkgreen", "fert1" = "red", "fert2" = "red")
  ) +
  theme_minimal() +
  theme(
    legend.position = "right", # Adjust legend position
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 16) ,
    axis.title.x = element_text(size = 16) # Adjust x-axis labels
  )
####DO relative change, i.e. final - initial/ initial

control0= group_means[7,4] - group_means[7,4]
control1 = (group_means[8,4] - group_means[7,4])/group_means[7,4]
control2 = (group_means[9,4] - group_means[7,4])/group_means[7,4]

plot1_pellet0 = group_means[10,4] - group_means[10,4]
plot1_pellet1 = (group_means[12,4] - group_means[10,4])/group_means[10,4]
plot1_pellet2 = (group_means[14,4] - group_means[10,4])/group_means[10,4]

plot2_pellet0 = group_means[11,4] - group_means[11,4]
plot2_pellet1 = (group_means[13,4] - group_means[11,4])/group_means[11,4]
plot2_pellet3 = (group_means[15,4] - group_means[11,4])/group_means[11,4]

plot1_fert0= group_means[1,4] - group_means[1,4]
plot1_fert1 = (group_means[3,4] - group_means[1,4])/group_means[1,4]
plot1_fert2 = (group_means[5,4] - group_means[1,4])/group_means[1,4]

plot2_fert0= group_means[2,4] - group_means[2,4]
plot2_fert1 = (group_means[4,4] - group_means[2,4])/group_means[2,4]
plot2_fert2 = (group_means[6,4] - group_means[2,4])/group_means[2,4]

#make these into a df

df = rbind.data.frame(control0,control1,control2,
                      plot1_pellet0,plot1_pellet1,plot1_pellet2,
                      plot2_pellet0,plot2_pellet1,plot2_pellet3,
                      plot1_fert0, plot1_fert1, plot1_fert2,
                      plot2_fert0, plot2_fert1, plot2_fert2)
df$treat = c('control','control','control',
             'pellet1','pellet1','pellet1',
             'pellet2','pellet2','pellet2',
             'fert1','fert1','fert1',
             'fert2','fert2','fert2'
)

df$Year = rep(c(0,9,15),5)
df$plot = c(rep('c',3), rep(1,3), rep(2,3), rep(1,3), rep(2,3))

library(ggplot2)

# Create the plot
ggplot(df, aes(x = Year, y = Mean_Height, color = treat, linetype = plot)) +
  geom_line(size = 1) +  # Add lines
  geom_point(size = 3) + # Add points for better visualization
  labs(
    title = "Relative change in average plot height for Betula pubescens at Trees for Life",
    x = "Months since planting",
    y = "Relative change in average plot height"
  ) +
  scale_color_manual(
    values = c("control" = "orange", "pellet1" = "darkgreen", "pellet2" = "darkgreen", "fert1" = "red", "fert2" = "red")
  ) +
  theme_minimal() +
  theme(
    legend.position = "right", # Adjust legend position
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 16) ,
    axis.title.x = element_text(size = 16) # Adjust x-axis labels
  )


######Average across the two plots
#take site_1 and site_2 and sum all the data for pellet and fert

pellet = filter(dundreggan, Site %in%  c('Site_1', 'Site_2') & Treatment == 'Fungi_Treatment')
fert = filter(dundreggan, Site %in%  c('Site_1', 'Site_2') & Treatment == 'Fertiliser')
control = dundreggan %>% filter(Treatment == 'Fungi_Control')


pellet_means = pellet %>%  group_by(timepoint) %>%
  summarize(Mean_Height = mean(Height, na.rm = TRUE), .groups = "drop")
fert_means =  fert %>% group_by(timepoint) %>%
  summarize(Mean_Height = mean(Height, na.rm = TRUE), .groups = "drop")
control_means = control %>% group_by(timepoint) %>%
  summarize(Mean_Height = mean(Height, na.rm = TRUE), .groups = "drop")

df = rbind.data.frame(pellet_means, fert_means, control_means)
df$treatment = c(rep('pellet',3), rep('fertiliser',3), rep('control',3))

df$mean_change = c(pellet_means[1,2] - pellet_means[1,2],
                   pellet_means[2,2] - pellet_means[1,2],
                   pellet_means[3,2] - pellet_means[1,2],
                   fert_means[1,2] - fert_means[1,2],
                   fert_means[2,2] - fert_means[1,2],
                   fert_means[3,2] - fert_means[1,2],
                   control_means[1,2] - control_means[1,2],
                   control_means[2,2] - control_means[1,2],
                   control_means[3,2] - control_means[1,2])

df$relmeanchange = c(pellet_means[1,2] - pellet_means[1,2],
                     (pellet_means[2,2] - pellet_means[1,2])/pellet_means[1,2],
                     (pellet_means[3,2] - pellet_means[1,2])/pellet_means[1,2],
                     fert_means[1,2] - fert_means[1,2],
                     (fert_means[2,2] - fert_means[1,2])/fert_means[1,2],
                     (fert_means[3,2] - fert_means[1,2])/fert_means[1,2],
                     control_means[1,2] - control_means[1,2],
                     (control_means[2,2] - control_means[1,2])/control_means[1,2],
                     (control_means[3,2] - control_means[1,2])/control_means[1,2])
df$relmeanchange = unlist(df$relmeanchange)
df$mean_change = unlist(df$mean_change)

ggplot(df, aes(x = timepoint, y = relmeanchange, color = treatment)) +
  geom_line(size = 1) +  # Add lines
  geom_point(size = 3) + # Add points for better visualization
  labs(
    title = "Relative change in average plot height for Betula pubescens at Trees for Life",
    x = "Months since planting",
    y = "Relative change in average plot height"
  ) +
  scale_color_manual(
    values = c("control" = "orange", "pellet" = "darkgreen", "fertiliser" = "red", "fert2" = "red")
  ) +
  theme_minimal() +
  theme(
    legend.position = "right", # Adjust legend position
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 16) ,
    axis.title.x = element_text(size = 16) # Adjust x-axis labels
  )

ggplot(df, aes(x = timepoint, y = mean_change, color = treatment)) +
  geom_line(size = 1) +  # Add lines
  geom_point(size = 3) + # Add points for better visualization
  labs(
    title = "Mean change in average plot height for Betula pubescens at Trees for Life",
    x = "Months since planting",
    y = "Mean change in average plot height (cm)"
  ) +
  scale_color_manual(
    values = c("control" = "orange", "pellet" = "darkgreen", "fertiliser" = "red", "fert2" = "red")
  ) +
  theme_minimal() +
  theme(
    legend.position = "right", # Adjust legend position
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 16) ,
    axis.title.x = element_text(size = 16) # Adjust x-axis labels
  )
###############################################################################
#histograms
#we have data_site1 and data_site2

data.1 = data %>% filter(Site == 'Site_1')
data.2 = data %>% filter(Site == 'Site_2')
data.control = data %>% filter(Site == 'Site_3')

data_site1 = rbind.data.frame(data.1,data.control)
data_site2 = rbind.data.frame(data.2, data.control)

#site1/2 = change the filter here
month0 = data_site1 %>% filter(time==0)
mean_control0 <- mean(month0$height[month0$treatment == "Control"])
mean_fert0 = mean(month0$height[month0$treatment == "Fertiliser"])
mean_treatment0 <- mean(month0$height[month0$treatment == "Pellet"])

month9 = data_site1%>% filter(time == 9)
mean_control9 <- mean(month9$height[month9$treatment == "Control"])
mean_fert9 = mean(month0$height[month0$treatment == "Fertiliser"])
mean_treatment9 <- mean(month9$height[month9$treatment == "Pellet"])

month15 = data_site1 %>% filter(time == 15)
mean_control15 <- mean(month15$height[month15$treatment == "Control"])
mean_fert15 = mean(month15$height[month15$treatment == "Fertiliser"])
mean_treatment15 <- mean(month15$height[month15$treatment == "Pellet"])

#look at the distributions
g1 = ggplot(month0, aes(x = height, fill = treatment)) +
  geom_histogram(position = "identity", alpha = 0.3, binwidth = 2) + 
  scale_fill_manual(values = c("Control" = "orange", "Fertiliser" = 'blue', "Pellet" = "darkgreen")) +  # Custom colors for the treatments
  labs(title = "initial heights", x = "", y = "Frequency")+
  geom_vline(xintercept = mean_control0, color = "orange", linetype = "dashed", size = 1.2) +  # Control mean
  geom_vline(xintercept = mean_treatment0, color = "green", linetype = "dashed", size = 1.2) +
  geom_vline(xintercept = mean_fert0, color = "blue", linetype = "dashed", size = 1.2) +
  scale_x_continuous(limits = c(25, 115)) + 
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

g2 = ggplot(month9, aes(x = height, fill = treatment)) +
  geom_histogram(position = "identity", alpha = 0.3, binwidth = 2) + 
  scale_fill_manual(values = c("Control" = "orange", "Fertiliser" = 'blue', "Pellet" = "darkgreen")) +  # Custom colors for the treatments
  labs(title = "Month 11", x = "", y = "Frequency")+
  geom_vline(xintercept = mean_control9, color = "orange", linetype = "dashed", size = 1.2) +  # Control mean
  geom_vline(xintercept = mean_treatment9, color = "green", linetype = "dashed", size = 1.2) +
  geom_vline(xintercept = mean_fert9, color = "blue", linetype = "dashed", size = 1.2) +
  scale_x_continuous(limits = c(25, 115)) + 
  
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

g3 = ggplot(month15, aes(x = height, fill = treatment)) +
  geom_histogram(position = "identity", alpha = 0.3, binwidth = 2) + 
  scale_fill_manual(values = c("Control" = "orange", "Fertiliser" = 'blue', "Pellet" = "darkgreen")) +  # Custom colors for the treatments
  labs(title = "Month 15", x = "", y = "Frequency")+
  geom_vline(xintercept = mean_control15, color = "orange", linetype = "dashed", size = 1.2) +  # Control mean
  geom_vline(xintercept = mean_treatment15, color = "green", linetype = "dashed", size = 1.2) +
  geom_vline(xintercept = mean_fert15, color = "blue", linetype = "dashed", size = 1.2) +
  scale_x_continuous(limits = c(25, 115)) + 
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

###############################################

#MEANS PLOTS AGAIN, BY SITE, NORMALISED TO START AT 0
#SITE1 = DATA1 ETC

data.1 = data %>% filter(Site == 'Site_1')
data.2 = data %>% filter(Site == 'Site_2')
data.control = data %>% filter(Site == 'Site_3')

data_site1 = rbind.data.frame(data.1,data.control)
data_site2 = rbind.data.frame(data.2, data.control)

group_means <- data_site1 %>%
  group_by(treatment, time, Site) %>%
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
  scale_color_manual(values = c("Control" = "orange", "Fertiliser" = 'blue', "Pellet" = "darkgreen")) + # Custom colors for treatments
  labs(title = '', x = "Time(months)", y = "Adjusted Mean Height(cm)") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 18),
    axis.text.y = element_text(size = 18),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18)
  )


#redoing in black and white for a publication

ggplot(group_means_adj, aes(x = time, y = Mean_Height_Adjusted, color = treatment, shape = treatment)) +
  geom_point(size = 4) +  # Plot points
  geom_line(size = 1) +   # Connect the points with a line
  scale_color_manual(values = c(
    "Control" = "black",
    "Fertiliser" = "black",
    "Pellet" = "black"
  )) +
  scale_shape_manual(values = c(
    "Control" = 17,      # triangle
    "Fertiliser" = 16,   # circle
    "Pellet" = 15        # square
  )) +
  labs(
    title = "Adjusted Mean Heights",
    x = "Time (months)",
    y = "Adjusted Mean Height (cm)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )

