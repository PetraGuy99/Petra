#soil analysis 
setwd('C:/dev/code/Petra')
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)


soil = read_excel('../../data/soildata.xlsx')

#tidy data
#select required columns
soildata = soil[,c(1,2,3,10:20)]


#tidy some < in P column
soildata$P = gsub("<", "", soildata$P)

#remove the brackets
soildata$P <- gsub(" \\(.*\\)", "", soildata$P)
soildata$K <- gsub(" \\(.*\\)", "", soildata$K)
soildata$Mg <- gsub(" \\(.*\\)", "", soildata$Mg)



#P,Mg and K are char
soildata$P <- as.numeric(unlist(soildata$P))
soildata$K <- as.numeric(unlist(soildata$K))
soildata$Mg <- as.numeric(unlist(soildata$Mg))

###############################################################################
#normalize for plotting###if you want...
normalize <- function(x) {
  return((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}
soildata_norm <- as.data.frame(lapply(soildata, function(col) if(is.numeric(col)) normalize(col) else col))




################################################################################

#plot all in one go?
df_long <- soildata_norm[, 3:ncol(soildata_norm)] %>% 
  pivot_longer(cols = -SiteCode,  # Keep SiteCode column intact
               names_to = "Category", 
               values_to = "Value")


ggplot(df_long, aes(x = Category, y = Value)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(y = "normalised value", x = 'soil property') +
  facet_wrap(~ SiteCode, ncol = 1)  +
  theme_minimal()

################################################################################
#variance in each parameter 

soil_variance =  apply(soildata_norm[,c(4:14)], 2, var)
variances_df <- data.frame(
  Column = names(soil_variance),  # Column names as factor
  Variance = soil_variance         # Variance values
)

# Plot the variances using ggplot2
ggplot(variances_df, aes(x = Column, y = Variance, fill = Column)) +
  geom_bar(stat = "identity") +  # Create a bar plot
  labs(title = "Variance for soil property", x = "", y = "normalised variance") +
  theme_minimal() +
  theme(legend.position = "none")  # Remove the legend
###############################################################################


#can I plot the average pH, say, with box plots for each site on top
#remove col 1 and 2

df_long <- soildata[, 3:ncol(soildata)] %>% 
  pivot_longer(cols = -SiteCode,  # Keep SiteCode column intact
               names_to = "SoilProperty", 
               values_to = "Value")
df = soildata[,-c(1,2)]
df_avg <- df %>%
  select(SiteCode, 2:12) %>%
  group_by(SiteCode) %>%
  summarise(across(everything(), \(x) mean(x, na.rm = TRUE)))

################################################################################
library(corrplot)
corr_matrix <- cor(df_avg[,-1])

# Plot the correlation matrix
corrplot(corr_matrix, method = "circle", type = "upper", 
         tl.col = "black", tl.cex = 0.8, addCoef.col = "black")


#remove dundreggan because thats where the NAs are and the ? in the corrplot
df_AG = df_avg %>% filter(SiteCode != 'AG')
corr_matrix <- cor(df_AG[,-1])

# Plot the correlation matrix
corrplot(corr_matrix, method = "circle", type = "upper", 
         tl.col = "black", tl.cex = 0.8, addCoef.col = "black")


#pH pH##########################################################

########

df_pH = df_long %>% filter(SoilProperty == 'pH')

df_avg <- df_avg %>%
  arrange(pH)

# Order SiteCode in df_pH by the median pH value for box plots
df_pH$SiteCode <- factor(df_pH$SiteCode, levels = df_pH %>%
                           group_by(SiteCode) %>%
                           summarise(MedianValue = median(Value, na.rm = TRUE)) %>%
                           arrange(MedianValue) %>%
                           pull(SiteCode))

# Plotting
ggplot() +
  # Boxplot for pH by site, ordered by median pH values
  geom_boxplot(data = df_pH, aes(x = SiteCode, y = Value, fill = SiteCode), alpha = 0.3) +
  
  # Line graph for average pH from df_avg, ordered by average pH values
  geom_line(data = df_avg, aes(x = SiteCode, y = pH, group = 1), color = "red", size = 1.2) +
  
  # Add points for the average pH values
  geom_point(data = df_avg, aes(x = SiteCode, y = pH), color = "red", size = 3) +
  
  # Labels and theme
  labs(title = "pH Values by Site with Average pH Line", x = "Site Code", y = "pH Value") +
  theme_minimal() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))


#TotalC####

df_TotalC = df_long %>% filter(SoilProperty == 'TotalC')

df_avg <- df_avg %>%
  arrange(TotalC)

# Order SiteCode in df_pH by the median pH value for box plots
df_TotalC$SiteCode <- factor(df_TotalC$SiteCode, levels = df_TotalC %>%
                           group_by(SiteCode) %>%
                           summarise(MedianValue = median(Value, na.rm = TRUE)) %>%
                           arrange(MedianValue) %>%
                           pull(SiteCode))

# Plotting
ggplot() +
  # Boxplot for pH by site, ordered by median pH values
  geom_boxplot(data = df_TotalC, aes(x = SiteCode, y = Value, fill = SiteCode), alpha = 0.3) +
  
  # Line graph for average pH from df_avg, ordered by average pH values
  geom_line(data = df_avg, aes(x = SiteCode, y = TotalC, group = 1), color = "red", size = 1.2) +
  
  # Add points for the average pH values
  geom_point(data = df_avg, aes(x = SiteCode, y = TotalC), color = "red", size = 3) +
  
  # Labels and theme
  labs(title = "Total Values by Site with Average TotalC", x = "Site Code", y = "TotalC") +
  theme_minimal() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))


##### Total N
df_TotalN = df_long %>% filter(SoilProperty == 'TotalN')

df_avg <- df_avg %>%
  arrange(TotalN)

# Order SiteCode in df_pH by the median pH value for box plots
df_TotalN$SiteCode <- factor(df_TotalN$SiteCode, levels = df_TotalN %>%
                               group_by(SiteCode) %>%
                               summarise(MedianValue = median(Value, na.rm = TRUE)) %>%
                               arrange(MedianValue) %>%
                               pull(SiteCode))

# Plotting
ggplot() +
  # Boxplot for pH by site, ordered by median pH values
  geom_boxplot(data = df_TotalN, aes(x = SiteCode, y = Value, fill = SiteCode), alpha = 0.3) +
  
  # Line graph for average pH from df_avg, ordered by average pH values
  geom_line(data = df_avg, aes(x = SiteCode, y = TotalN, group = 1), color = "red", size = 1.2) +
  
  # Add points for the average pH values
  geom_point(data = df_avg, aes(x = SiteCode, y = TotalN), color = "red", size = 3) +
  
  # Labels and theme
  labs(title = "Total Values by Site with Average TotalN", x = "Site Code", y = "TotalN") +
  theme_minimal() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))

############ P

df_P = df_long %>% filter(SoilProperty == 'P')

df_avg <- df_avg %>%
  arrange(P)

# Order SiteCode in df_pH by the median pH value for box plots
df_P$SiteCode <- factor(df_P$SiteCode, levels = df_P %>%
                               group_by(SiteCode) %>%
                               summarise(MedianValue = median(Value, na.rm = TRUE)) %>%
                               arrange(MedianValue) %>%
                               pull(SiteCode))

# Plotting
ggplot() +
  # Boxplot for pH by site, ordered by median pH values
  geom_boxplot(data = df_P, aes(x = SiteCode, y = Value, fill = SiteCode), alpha = 0.3) +
  
  # Line graph for average pH from df_avg, ordered by average pH values
  geom_line(data = df_avg, aes(x = SiteCode, y = P, group = 1), color = "red", size = 1.2) +
  
  # Add points for the average pH values
  geom_point(data = df_avg, aes(x = SiteCode, y = P), color = "red", size = 3) +
  
  # Labels and theme
  labs(title = "Total Values by Site with Average P", x = "Site Code", y = "P (mg/l)") +
  theme_minimal() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))

######## K

df_K = df_long %>% filter(SoilProperty == 'K')

df_avg <- df_avg %>%
  arrange(K)

# Order SiteCode in df_pH by the median pH value for box plots
df_K$SiteCode <- factor(df_K$SiteCode, levels = df_K %>%
                          group_by(SiteCode) %>%
                          summarise(MedianValue = median(Value, na.rm = TRUE)) %>%
                          arrange(MedianValue) %>%
                          pull(SiteCode))

# Plotting
ggplot() +
  # Boxplot for pH by site, ordered by median pH values
  geom_boxplot(data = df_K, aes(x = SiteCode, y = Value, fill = SiteCode), alpha = 0.3) +
  
  # Line graph for average pH from df_avg, ordered by average pH values
  geom_line(data = df_avg, aes(x = SiteCode, y = K, group = 1), color = "red", size = 1.2) +
  
  # Add points for the average pH values
  geom_point(data = df_avg, aes(x = SiteCode, y = K), color = "red", size = 3) +
  
  # Labels and theme
  labs(title = "Total Values by Site with Average K", x = "Site Code", y = "K (mg/l)") +
  theme_minimal() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))


################# Mg

df_Mg = df_long %>% filter(SoilProperty == 'Mg')

df_avg <- df_avg %>%
  arrange(Mg)

# Order SiteCode in df_pH by the median pH value for box plots
df_Mg$SiteCode <- factor(df_Mg$SiteCode, levels = df_Mg %>%
                          group_by(SiteCode) %>%
                          summarise(MedianValue = median(Value, na.rm = TRUE)) %>%
                          arrange(MedianValue) %>%
                          pull(SiteCode))

# Plotting
ggplot() +
  # Boxplot for pH by site, ordered by median pH values
  geom_boxplot(data = df_Mg, aes(x = SiteCode, y = Value, fill = SiteCode), alpha = 0.3) +
  
  # Line graph for average pH from df_avg, ordered by average pH values
  geom_line(data = df_avg, aes(x = SiteCode, y = Mg, group = 1), color = "red", size = 1.2) +
  
  # Add points for the average pH values
  geom_point(data = df_avg, aes(x = SiteCode, y = Mg), color = "red", size = 3) +
  
  # Labels and theme
  labs(title = "Total Values by Site with Average Mg", x = "Site Code", y = "Mg (mg/l)") +
  theme_minimal() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))

#### stored C

df_StoredC= df_long %>% filter(SoilProperty == 'OCStock(t/ha)')

df_avg <- df_avg %>%
  arrange('OCStock(t/ha)')

# Order SiteCode in df_pH by the median pH value for box plots
df_StoredC$SiteCode <- factor(df_StoredC$SiteCode, levels = df_StoredC %>%
                           group_by(SiteCode) %>%
                           summarise(MedianValue = median(Value, na.rm = TRUE)) %>%
                           arrange(MedianValue) %>%
                           pull(SiteCode))

# Plotting
ggplot() +
  # Boxplot for pH by site, ordered by median pH values
  geom_boxplot(data = df_StoredC, aes(x = SiteCode, y = Value, fill = SiteCode), alpha = 0.3) +
  
  # Line graph for average pH from df_avg, ordered by average pH values
  geom_line(data = df_avg, aes(x = SiteCode, y = `OCStock(t/ha)`, group = 1), color = "red", size = 1.2) +
  
  # Add points for the average pH values
  geom_point(data = df_avg, aes(x = SiteCode, y = `OCStock(t/ha)`), color = "red", size = 3) +
  
  # Labels and theme
  labs(title = "Total Values by Site with Average Stored C", x = "Site Code", y = "Stored C (t/ha)") +
  theme_minimal() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))


### Forrest Estate graphs

#single plot chart ### if you want ...for a customer...but normalised doesnt help
#here because no point comparing ActiveC to pH on the same chart 
#need multiple single box plots for each soil property

df.customer = soildata %>% filter(`SiteCode` == 'AB')

df.customer_long <- df.customer[, 4:14] %>% 
  pivot_longer(cols = everything(), 
               names_to = "Category", 
               values_to = "Value")

# Plot the box plot...for normalised data...but if you want un-normalise...see next
ggplot(df.customer_long, aes(x = Category, y = Value)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Forrest Estate soil properties", x = "soil property", y = "normalised value") +
  theme_minimal()

# Boxplot with facet_wrap
ggplot(df.customer_long, aes(x = "", y = Value)) +
  geom_boxplot() +
  facet_wrap(~Category, scales = "free_y") +  # Allows each facet to have its own y-scale
  labs(x = NULL, y = "Value", title = "Soil Property Distributions") +
  theme_bw()

NorwayT = df.customer %>% filter(Sample %in% c('AB1','AB3','AB6'))
NorwayC = df.customer %>% filter(Sample %in% c('AB8','AB11','AB12'))
SitkaT = df.customer %>% filter(Sample %in% c('AB16','AB17','AB18'))
SitkaC = df.customer %>% filter(Sample %in% c('AB13','AB14','AB15'))
PineT = df.customer %>% filter(Sample %in% c('AB2','AB4','AB7'))
PineC = df.customer %>% filter(Sample %in% c('AB5','AB9','AB10'))


# are control and treatment different for any tree species?

library(purrr)
library(flextable)
#put the differebt species df in here
Control = PineC
Treatment = PineT
soil_properties <- names(Treatment)[4:14]

# Perform t-tests for each soil property
t_test_results <- map_df(soil_properties, ~{
  test <- t.test(Treatment[[.x]], Control[[.x]], var.equal = TRUE)  # Set var.equal=FALSE if variances are unequal
  
  tibble(
    Property = .x,
    Mean_Treatment = round(mean(Treatment[[.x]], na.rm = TRUE), 2),
    Mean_Control = round(mean(Control[[.x]], na.rm = TRUE), 2),
    p_value = round(test$p.value, 2)
  )
})
t_test_results %>%
  mutate(p_value = signif(p_value, 3)) %>%  # Round p-values for readability
  flextable() %>%
  colformat_num(j = c("Mean_Treatment", "Mean_Control"), digits = 2) %>%
  set_caption("T-Test Results for control and treatment soils") %>%
  theme_vanilla()%>% autofit()

###Highlands Rewilding Graphs
df.customer = soildata %>% filter(`SiteCode` == 'AA')

df.customer_long <- df.customer[, 4:14] %>% 
  pivot_longer(cols = everything(), 
               names_to = "Category", 
               values_to = "Value")

# Boxplot with facet_wrap
ggplot(df.customer_long, aes(x = "", y = Value)) +
  geom_boxplot() +
  facet_wrap(~Category, scales = "free_y") +  # Allows each facet to have its own y-scale
  labs(x = NULL, y = "Value", title = "Soil Property Distributions") +
  theme_bw()

HighlandsT = df.customer %>% filter(Sample %in% c('AA6','AA1','AA2'))
HighlandsC = df.customer %>% filter(Sample %in% c('AA3','AA4','AA5'))
##or try
HighlandsT = df.customer %>% filter(Sample %in% c('AA5','AA1','AA2'))
HighlandsC = df.customer %>% filter(Sample %in% c('AA3','AA4','AA6'))


Control = HighlandsC
Treatment =HighlandsT
soil_properties <- names(Treatment)[4:14]

# Perform t-tests for each soil property
t_test_results <- map_df(soil_properties, ~{
  test <- t.test(Treatment[[.x]], Control[[.x]], var.equal = TRUE)  # Set var.equal=FALSE if variances are unequal
  
  tibble(
    Property = .x,
    Mean_Treatment = round(mean(Treatment[[.x]], na.rm = TRUE), 2),
    Mean_Control = round(mean(Control[[.x]], na.rm = TRUE), 2),
    p_value = round(test$p.value, 2)
  )
})
t_test_results %>%
  mutate(p_value = signif(p_value, 3)) %>%  # Round p-values for readability
  flextable() %>%
  colformat_num(j = c("Mean_Treatment", "Mean_Control"), digits = 2) %>%
  set_caption("T-Test Results for control and treatment soils") %>%
  theme_vanilla()%>% autofit()



###Balcaskie


#single plot chart ### if you want ...for a customer...but normalised doesnt help
#here because no point comparing ActiveC to pH on the same chart 
#need multiple single box plots for each soil property

df.customer = soildata %>% filter(`SiteCode` == 'AC')

df.customer_long <- df.customer[, 4:14] %>% 
  pivot_longer(cols = everything(), 
               names_to = "Category", 
               values_to = "Value")


# Boxplot with facet_wrap
ggplot(df.customer_long, aes(x = "", y = Value)) +
  geom_boxplot() +
  facet_wrap(~Category, scales = "free_y") +  # Allows each facet to have its own y-scale
  labs(x = NULL, y = "Value", title = "Soil Property Distributions") +
  theme_bw()

Treat = df.customer %>% filter(Sample %in% c('AC1','AC2','AC3'))
Cont = df.customer %>% filter(Sample %in% c('AC4','AC5','AC6'))



# are control and treatment different for any tree species?

library(purrr)
library(flextable)
#put the different species df in here
Control = Cont
Treatment =Treat
soil_properties <- names(Treatment)[4:14]

# Perform t-tests for each soil property
t_test_results <- map_df(soil_properties, ~{
  test <- t.test(Treatment[[.x]], Control[[.x]], var.equal = TRUE)  # Set var.equal=FALSE if variances are unequal
  
  tibble(
    Property = .x,
    Mean_Treatment = round(mean(Treatment[[.x]], na.rm = TRUE), 2),
    Mean_Control = round(mean(Control[[.x]], na.rm = TRUE), 2),
    p_value = round(test$p.value, 2)
  )
})
t_test_results %>%
  mutate(p_value = signif(p_value, 3)) %>%  # Round p-values for readability
  flextable() %>%
  colformat_num(j = c("Mean_Treatment", "Mean_Control"), digits = 2) %>%
  set_caption("T-Test Results for control and treatment soils") %>%
  theme_vanilla()%>% autofit()

#### Saltersford


#single plot chart ### if you want ...for a customer...but normalised doesnt help
#here because no point comparing ActiveC to pH on the same chart 
#need multiple single box plots for each soil property

df.customer = soildata %>% filter(`SiteCode` == 'AD')

df.customer_long <- df.customer[, 4:14] %>% 
  pivot_longer(cols = everything(), 
               names_to = "Category", 
               values_to = "Value")


# Boxplot with facet_wrap
ggplot(df.customer_long, aes(x = "", y = Value)) +
  geom_boxplot() +
  facet_wrap(~Category, scales = "free_y") +  # Allows each facet to have its own y-scale
  labs(x = NULL, y = "Value", title = "Soil Property Distributions") +
  theme_bw()

Treat = df.customer %>% filter(Sample %in% c('AD4','AD5','AD6'))
Cont = df.customer %>% filter(Sample %in% c('AD1','AD2','AD3'))



# are control and treatment different for any tree species?

library(purrr)
library(flextable)
#put the differebt species df in here
Control = Cont
Treatment =Treat
soil_properties <- names(Treatment)[4:14]

# Perform t-tests for each soil property
t_test_results <- map_df(soil_properties, ~{
  test <- t.test(Treatment[[.x]], Control[[.x]], var.equal = TRUE)  # Set var.equal=FALSE if variances are unequal
  
  tibble(
    Property = .x,
    Mean_Treatment = round(mean(Treatment[[.x]], na.rm = TRUE), 2),
    Mean_Control = round(mean(Control[[.x]], na.rm = TRUE), 2),
    p_value = round(test$p.value, 2)
  )
})
t_test_results %>%
  mutate(p_value = signif(p_value, 3)) %>%  # Round p-values for readability
  flextable() %>%
  colformat_num(j = c("Mean_Treatment", "Mean_Control"), digits = 2) %>%
  set_caption("T-Test Results for control and treatment soils") %>%
  theme_vanilla()%>% autofit()

###### Forrest - forgot this one?

#single plot chart ### if you want ...for a customer...but normalised doesnt help
#here because no point comparing ActiveC to pH on the same chart 
#need multiple single box plots for each soil property

df.customer = soildata %>% filter(`SiteCode` == 'AB')

df.customer_long <- df.customer[, 4:14] %>% 
  pivot_longer(cols = everything(), 
               names_to = "Category", 
               values_to = "Value")


# Boxplot with facet_wrap
ggplot(df.customer_long, aes(x = "", y = Value)) +
  geom_boxplot() +
  facet_wrap(~Category, scales = "free_y") +  # Allows each facet to have its own y-scale
  labs(x = NULL, y = "Value", title = "Soil Property Distributions") +
  theme_bw()

Treat = df.customer %>% filter(Sample %in% c('AB4','AB5','AB6'))
Cont = df.customer %>% filter(Sample %in% c('AB1','AB2','AB3'))



# are control and treatment different for any tree species?

library(purrr)
library(flextable)
#put the different species df in here
Control = Cont
Treatment =Treat
soil_properties <- names(Treatment)[4:14]

# Perform t-tests for each soil property
t_test_results <- map_df(soil_properties, ~{
  test <- t.test(Treatment[[.x]], Control[[.x]], var.equal = TRUE)  # Set var.equal=FALSE if variances are unequal
  
  tibble(
    Property = .x,
    Mean_Treatment = round(mean(Treatment[[.x]], na.rm = TRUE), 2),
    Mean_Control = round(mean(Control[[.x]], na.rm = TRUE), 2),
    p_value = round(test$p.value, 2)
  )
})
t_test_results %>%
  mutate(p_value = signif(p_value, 3)) %>%  # Round p-values for readability
  flextable() %>%
  colformat_num(j = c("Mean_Treatment", "Mean_Control"), digits = 2) %>%
  set_caption("T-Test Results for control and treatment soils") %>%
  theme_vanilla()%>% autofit()


###### Knoydart

#single plot chart ### if you want ...for a customer...but normalised doesnt help
#here because no point comparing ActiveC to pH on the same chart 
#need multiple single box plots for each soil property

df.customer = soildata %>% filter(`SiteCode` == 'AE')

df.customer_long <- df.customer[, 4:14] %>% 
  pivot_longer(cols = everything(), 
               names_to = "Category", 
               values_to = "Value")


# Boxplot with facet_wrap
ggplot(df.customer_long, aes(x = "", y = Value)) +
  geom_boxplot() +
  facet_wrap(~Category, scales = "free_y") +  # Allows each facet to have its own y-scale
  labs(x = NULL, y = "Value", title = "Soil Property Distributions") +
  theme_bw()

Treat = df.customer %>% filter(Sample %in% c('AE4','AE5','AE6'))
Cont = df.customer %>% filter(Sample %in% c('AE1','AE2','AE3'))



# are control and treatment different for any tree species?

library(purrr)
library(flextable)
#put the different species df in here
Control = Cont
Treatment =Treat
soil_properties <- names(Treatment)[4:14]

# Perform t-tests for each soil property
t_test_results <- map_df(soil_properties, ~{
  test <- t.test(Treatment[[.x]], Control[[.x]], var.equal = TRUE)  # Set var.equal=FALSE if variances are unequal
  
  tibble(
    Property = .x,
    Mean_Treatment = round(mean(Treatment[[.x]], na.rm = TRUE), 2),
    Mean_Control = round(mean(Control[[.x]], na.rm = TRUE), 2),
    p_value = round(test$p.value, 2)
  )
})
t_test_results %>%
  mutate(p_value = signif(p_value, 3)) %>%  # Round p-values for readability
  flextable() %>%
  colformat_num(j = c("Mean_Treatment", "Mean_Control"), digits = 2) %>%
  set_caption("T-Test Results for control and treatment soils") %>%
  theme_vanilla()%>% autofit()



###### Knoydart

#single plot chart ### if you want ...for a customer...but normalised doesnt help
#here because no point comparing ActiveC to pH on the same chart 
#need multiple single box plots for each soil property

df.customer = soildata %>% filter(`SiteCode` == 'AE')

df.customer_long <- df.customer[, 4:14] %>% 
  pivot_longer(cols = everything(), 
               names_to = "Category", 
               values_to = "Value")


# Boxplot with facet_wrap
ggplot(df.customer_long, aes(x = "", y = Value)) +
  geom_boxplot() +
  facet_wrap(~Category, scales = "free_y") +  # Allows each facet to have its own y-scale
  labs(x = NULL, y = "Value", title = "Soil Property Distributions") +
  theme_bw()

Treat = df.customer %>% filter(Sample %in% c('AE4','AE5','AE6'))
Cont = df.customer %>% filter(Sample %in% c('AE1','AE2','AE3'))



# are control and treatment different for any tree species?

library(purrr)
library(flextable)
#put the different species df in here
Control = Cont
Treatment =Treat
soil_properties <- names(Treatment)[4:14]

# Perform t-tests for each soil property
t_test_results <- map_df(soil_properties, ~{
  test <- t.test(Treatment[[.x]], Control[[.x]], var.equal = TRUE)  # Set var.equal=FALSE if variances are unequal
  
  tibble(
    Property = .x,
    Mean_Treatment = round(mean(Treatment[[.x]], na.rm = TRUE), 2),
    Mean_Control = round(mean(Control[[.x]], na.rm = TRUE), 2),
    p_value = round(test$p.value, 2)
  )
})
t_test_results %>%
  mutate(p_value = signif(p_value, 3)) %>%  # Round p-values for readability
  flextable() %>%
  colformat_num(j = c("Mean_Treatment", "Mean_Control"), digits = 2) %>%
  set_caption("T-Test Results for control and treatment soils") %>%
  theme_vanilla()%>% autofit()


###### Nick Hoare

#single plot chart ### if you want ...for a customer...but normalised doesnt help
#here because no point comparing ActiveC to pH on the same chart 
#need multiple single box plots for each soil property

df.customer = soildata %>% filter(`SiteCode` == 'AF')

df.customer_long <- df.customer[, 4:14] %>% 
  pivot_longer(cols = everything(), 
               names_to = "Category", 
               values_to = "Value")


# Boxplot with facet_wrap
ggplot(df.customer_long, aes(x = "", y = Value)) +
  geom_boxplot() +
  facet_wrap(~Category, scales = "free_y") +  # Allows each facet to have its own y-scale
  labs(x = NULL, y = "Value", title = "Soil Property Distributions") +
  theme_bw()

Treat = df.customer %>% filter(Sample %in% c('AF4','AF5','AF6'))
Cont = df.customer %>% filter(Sample %in% c('AF1','AF2','AF3'))



# are control and treatment different for any tree species?

library(purrr)
library(flextable)
#put the different species df in here
Control = Cont
Treatment =Treat
soil_properties <- names(Treatment)[4:14]

# Perform t-tests for each soil property
t_test_results <- map_df(soil_properties, ~{
  test <- t.test(Treatment[[.x]], Control[[.x]], var.equal = TRUE)  # Set var.equal=FALSE if variances are unequal
  
  tibble(
    Property = .x,
    Mean_Treatment = round(mean(Treatment[[.x]], na.rm = TRUE), 2),
    Mean_Control = round(mean(Control[[.x]], na.rm = TRUE), 2),
    p_value = round(test$p.value, 2)
  )
})
t_test_results %>%
  mutate(p_value = signif(p_value, 3)) %>%  # Round p-values for readability
  flextable() %>%
  colformat_num(j = c("Mean_Treatment", "Mean_Control"), digits = 2) %>%
  set_caption("T-Test Results for control and treatment soils") %>%
  theme_vanilla()%>% autofit()


###### Dundreggan

#single plot chart ### if you want ...for a customer...but normalised doesnt help
#here because no point comparing ActiveC to pH on the same chart 
#need multiple single box plots for each soil property

df.customer = soildata %>% filter(`SiteCode` == 'AG')

# REMOVE COLS WITH NAS - WHERE WE DIDNT GET ENOUGH SOIL FOR cARBON = 6,9,14

df.customer =  df.customer[,-c(6,9,14)]

df.customer_long <- df.customer[, 4:11] %>% 
  pivot_longer(cols = everything(), 
               names_to = "Category", 
               values_to = "Value")


# Boxplot with facet_wrap
ggplot(df.customer_long, aes(x = "", y = Value)) +
  geom_boxplot() +
  facet_wrap(~Category, scales = "free_y") +  # Allows each facet to have its own y-scale
  labs(x = NULL, y = "Value", title = "Soil Property Distributions") +
  theme_bw()

Treat = df.customer %>% filter(Sample %in% c('AG2','AG7','AG8'))
Cont = df.customer %>% filter(Sample %in% c('AG3','AG4','AG9'))

#THE OTHER TREATMENT AREA
Treat = df.customer %>% filter(Sample %in% c('AG1','AG5','AG6'))
Cont = df.customer %>% filter(Sample %in% c('AG3','AG4','AG9'))

# are control and treatment different for any tree species?

library(purrr)
library(flextable)
#put the different species df in here
Control = Cont
Treatment =Treat
soil_properties <- names(Treatment)[4:11]

# Perform t-tests for each soil property
t_test_results <- map_df(soil_properties, ~{
  test <- t.test(Treatment[[.x]], Control[[.x]], var.equal = TRUE)  # Set var.equal=FALSE if variances are unequal
  
  tibble(
    Property = .x,
    Mean_Treatment = round(mean(Treatment[[.x]], na.rm = TRUE), 2),
    Mean_Control = round(mean(Control[[.x]], na.rm = TRUE), 2),
    p_value = round(test$p.value, 2)
  )
})
t_test_results %>%
  mutate(p_value = signif(p_value, 3)) %>%  # Round p-values for readability
  flextable() %>%
  colformat_num(j = c("Mean_Treatment", "Mean_Control"), digits = 2) %>%
  set_caption("T-Test Results for control and treatment soils") %>%
  theme_vanilla()%>% autofit()







