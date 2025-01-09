setwd('C:/dev/code/Petra')
library(dplyr)
library(ggplot2)
library(readxl)
library(tidyr)
library(geepack)
library(lme4)
library(lmerTest)

#read in data. longform. need a column for the random effect/grouping var
#in this data treatment/plot/pellet etc are basically the same grouping var
#so it doesnt matter. Because there is only one emf and one treatment and one 
#control plot at the site.

#read data.This is all data recorded, each group is a different size, but that
#doesnt matter for this analysis
data = read_excel('../../data/Saltersford_WorkingSheet.xlsx', sheet = 'GEEdata')

#delete NA rows
data = data %>% drop_na(height)

#look at quick summary
mean_heights <- data %>%
  group_by(time, treatment,tree) %>%
  summarize(mean_height = mean(height), .groups = "drop")

print(mean_heights)

#boxplots of all the poonts
ggplot(data, aes(x = factor(time), y = height, fill = interaction(tree,treatment))) +
  geom_boxplot() +
  labs(title = "Saltersford, height measurements across three time points",
       x = "Months since planting",
       y = "Height",
       fill = "Treatment") +
  scale_fill_manual(
    values = c(
      "B_pendula.Control" = "darkorange",
      "Q_robur.Control" = "orange",
      "B_pendula.Pellet" = "green",
      "Q_robur.Pellet" = "darkgreen"
    )
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1) )+
  theme(
    legend.position = "right", # Adjust legend position
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 16) ,
    axis.title.x = element_text(size = 16) # Adjust x-axis labels
  )

######################################################

#linear model
data.bp = data%>% filter( tree == 'B_pendula')
data.qr = data %>% filter(tree == 'Q_robur')
mixed_model = mixed_model <- lmer(height ~ time * treatment  +(1 | plot), data = data.qr)

#mydata = data.bp

#mydata$predicted_height <- predict(mixed_model, newdata = mydata, re.form = NULL)

# Plot actual vs predicted
ggplot(mydata, aes(x = time, y = height, color = treatment)) +
  geom_point() +
  geom_line(aes(y = predicted_height), size = 1, linetype = "dashed") +
  labs(title = "Actual vs Predicted Heights", x = "Time", y = "Height") +
  scale_colour_manual(values = c("Control" = "orange", "Pellet" = "darkgreen")) + 
  theme_minimal()

###############################################################################
#exponential model
#do oak and birch separately?
data.bp = data%>% filter( tree == 'B_pendula')
data.qr = data %>% filter(tree == 'Q_robur')

mixed_model_exp = mixed_model <- lmer(log(height) ~ time * treatment + treatment*tree +(1 | plot), data = data)


mydata = data.bp

#predict for exp model
mydata$predicted_log_height <- predict(mixed_model_exp, newdata = mydata, re.form = NULL)

# Back-transform predictions to the original scale (exponentiate)
mydata$predicted_height_exp <- exp(mydata$predicted_log_height)

# Plot actual vs predicted heights
ggplot(mydata, aes(x = time, y = height, color = treatment)) +
  geom_point() +  # Actual data points
  geom_line(aes(y = predicted_height_exp), size = 1, linetype = "dashed") +  # Exponentiated predicted values
  labs(title = "Actual vs Predicted Heights (Exponential Model)", x = "Time", y = "height") +
  scale_colour_manual(values = c("Control" = "orange", "Pellet" = "darkgreen")) + 
  theme(
    legend.position = "right", # Adjust legend position
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 16) ,
    axis.title.x = element_text(size = 16) # Adjust x-axis labels
  )
theme_minimal()

#which ia better fit?
AIC(mixed_model)
AIC(mixed_model_exp)



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


#repeat that for linear model

#get the confidence intervals and plot exp model
conf_int = confint(mixed_model, level = 0.95)

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

