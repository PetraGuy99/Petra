setwd('C:/dev/code/Petra')
library(dplyr)
library(ggplot2)
library(readxl)
library(tidyr)
library(geepack)
library(lme4)
library(lmerTest)
library(MASS)
#read in data. longform. need a column for the random effect/grouping var
#in this data treatment/plot/pellet etc are basically the same grouping var
#so it doesnt matter. Because there is only one emf and one treatment and one 
#control plot at the site.

#read data.This is all data recorded, each group is a different size, but that
#doesnt matter for this analysis
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
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 16) ,
    axis.title.x = element_text(size = 16) # Adjust x-axis labels
  )



##check distribution of height
hist(data$height) #right skew
hist(log(data$height)) # looks more Normal

#models
#1. linear random intercept
#linear model
mixed_model = mixed_model <- lmer(height ~ time * treatment + (1 | plot), data = data) #AIC~-778

#2. exponential random intercetp
#exponential model###################
mixed_model_exp = mixed_model <- lmer(log(height) ~ time * treatment +(1 | plot), data = data) #AIC~-778

#3.linear random slopes
model <- lmer(height ~ time * treatment + (treatment | plot), data = data)#does not converge

#4.exponential random slopes
model <- lmer(log(height) ~ time * treatment + (treatment | plot), data = data)#does not converge

model <- lmer(log(height) ~ time * treatment + (treatment | plot), 
              data = data, 
              control = lmerControl(optimizer = "nloptwrap", 
                                    optCtrl = list(maxfun = 10000))) #also does not converge

#5 GEE
gee_model <- geeglm(height ~ time * treatment, 
                    id = plot,  # plot is the grouping factor (id variable)
                    family = gaussian,  # assuming normal distribution for height
                    corstr = "exchangeable",  # correlation structure (exchangeable here)
                    data = data) #gives same result as the linear mixed effect model

mydata = data

#predict for exp model
mydata$predicted_log_height <- predict(mixed_model_exp, newdata = mydata, re.form = NULL)

# Back-transform predictions to the original scale (exponentiate)
mydata$predicted_height_exp <- exp(mydata$predicted_log_height)

# Plot actual vs predicted heights
ggplot(mydata, aes(x = time, y = height, color = treatment)) +
  geom_point() +  # Actual data points
  geom_line(aes(y = predicted_height_exp), size = 1, linetype = "dashed") +  # Exponentiated predicted values
  labs(title = "Actual vs Predicted Heights (Exponential Model)", x = "Time", y = "Height") +
  scale_colour_manual(values = c("Control" = "orange", "Pellet" = "darkgreen")) + 
  theme(
    legend.position = "right", # Adjust legend position
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 16) ,
    axis.title.x = element_text(size = 16) # Adjust x-axis labels
  )


#get the confidence intervals and plot exp model
conf_int = confint(mixed_model_exp, level = 0.95)

# Convert to a data frame for easier plotting
conf_int_df <- as.data.frame(conf_int)
conf_int_df$term <- rownames(conf_int_df)  # Add the term names for plotting

fixed_effects <- fixef(mixed_model_exp)
conf_int_df$estimate <- fixed_effects[conf_int_df$term]

# Plot the fixed effects and their confidence intervals
data = conf_int_df[4:6,]

data$term = c('time','treatment','time:treatment')

ggplot(data, aes(x = term, ymin = `2.5 %`, ymax = `97.5 %`, y = estimate)) +
  geom_pointrange(color = "blue", size = 1) +  # Point for the coefficient estimate and error bars for the CI
  labs(title = "Fixed Effects with 95% Confidence Intervals", 
       x = "Fixed Effects", y = "Estimate") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(
    legend.position = "right", # Adjust legend position
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 16) ,
    axis.title.x = element_text(size = 16) # Adjust x-axis labels
  )


#faffing about with other options and looking at fits. ignore this
#####another option###
#from Bolker - fit a fixed effects model to each group.

control = data %>% filter(treatment == 'Control')
treatment = data %>% filter(treatment == 'Pellet')

#check distribution of points at each time point
 # Time Point 1
shapiro.test(sqrt(control$height[control$time == 0]) ) # Time Point 0
shapiro.test(your_data$height[your_data$time == 3]) 

#try boxcox
bc <- boxcox(control$height~ control$time)
lambda <- bc$x[which.max(bc$y)]

control$transformed = ((control$height^lambda - 1)/lambda)
#test again
shapiro.test((control$transformed[control$time == 0])) 

#linear model###########
model.control = lm(control$height ~ control$time)
model.treatment = lm(treatment$height~ treatment$time)

#check models

plot(residuals(model.control), main = 'Residuals vs Fitted')
plot(residuals(model.treatment), main = 'Residuals vs Fitted')

qqnorm(residuals(model.treatment))
qqline(residuals(model.treatment), col = "red")

# Shapiro-Wilk test
shapiro.test(residuals(model.control))

###look ant control distributions across time
control.1 = control %>% filter(time==0) %>% select(height)
control.1$time = 1
control.2 = control %>% filter(time == 11) %>% select(height)
control.2$time = 2
control.3 = control %>% filter(time == 17) %>% select(height)
control.3$time = 3
control.data = rbind.data.frame(control.1, control.2, control.3)

ggplot(control.data, aes(x = height, fill = as.factor(time), color = as.factor(time))) +
    geom_density(alpha = 0.7) +
  labs(title = "Overlapping Histograms and Density Curves of Height Distributions",
       x = "Height", y = "Density", fill = "Time Point", color = "Time Point") +
  theme_minimal()
 