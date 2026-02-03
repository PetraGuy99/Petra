setwd('C:/dev/code/Petra/field')


library(dplyr)
library(ggplot2)
library(readxl)
library(purrr)
library(kableExtra) 
library(stringr)
library(tidyr)
library(brms)
library(bayesplot)

##Galbraith BA#######################################
#Only going to include the sitka plots, because we have sitka at multiple sites
#BA also has norway and pinus contorta, but no other sites do in this analysis

sheets <- excel_sheets('../../../data/field/Galbraith2025.xlsx')  # get sheet names

# read all sheets into a named list, suppressing the New names messages
all_sheets <- suppressMessages(
  lapply(sheets, function(s) {
    read_excel('../../../data/field/Galbraith2025.xlsx', sheet = s)
  })
)
names(all_sheets) <- sheets

#remove meta
all_sheets <- all_sheets[-c(1,2)]

#add the delta_h in now
all_sheets <- map(all_sheets, ~ .x %>%
                    mutate(delta_h   = Height_1 - Height_0))

all_sheets <- map(
  all_sheets,
  ~ .x %>% dplyr::select(any_of(c("Tree_Species", "Fungi_Treatment", "Health_1", "delta_h")))
)

#now to fix the Treatment columns, Col name Fungi_Treatment becomes Treatment, 

all_sheets <- map(
  all_sheets,
  ~ .x %>%
    dplyr::rename(Treatment = Fungi_Treatment) %>%
    dplyr::mutate(
      Treatment = dplyr::recode(
        Treatment,
        Fungi_control = "Control",
        Treatment = 'Pellet'
      )
    )
)
names(all_sheets) = sheets[-c(1,2)]
#bind sheets 1.12,1.13 etc and allocate as site BA1, 2.12, 2.13 etc and allocate as site BA2
df_long_BA <- imap_dfr(
  all_sheets,
  ~ .x %>%
    mutate(
      Site = case_when(
        str_detect(.y, "^Sitka 1\\.")           ~ "BA1",
        str_detect(.y, "^(Sitka 2\\.|Pine|NS)") ~ "BA2",
        TRUE ~ NA_character_
      )
    )
)

#add emf - but we're not sure whats heb and pax, so this site is just EMF

df_long_BA$EMF = 'PAX'

# BA1 and BA2 considered as 2 sitka sites because they are not close.
#Might need to filter out the Fertiliser treatment as only at this site
##############################################################################

## Tillhill Olivia BB

sheets <- excel_sheets('../../../data/field/TilhillOlivia2025.xlsx')  # get sheet names

# read all sheets into a named list, suppressing the New names messages
all_sheets <- suppressMessages(
  lapply(sheets, function(s) {
    read_excel('../../../data/field/TilhillOlivia2025.xlsx', sheet = s)
  })
)
names(all_sheets) <- sheets

all_sheets <- all_sheets[-c(1,2)]

#fix the colnames issue
all_sheets <- map(
  all_sheets,
  ~ .x %>%
    rename_with(~ str_replace(.x, "Height\\s+_cm_", "Height_cm_"))
)

#add the delta_h in now
all_sheets <- map(all_sheets, ~ .x %>%
                    mutate(
                      delta_h   = Height_cm_1 - Height_cm_0 ))

all_sheets <-  map(all_sheets, ~ .x %>%
              dplyr::select(Tree_Species, Treatment, Health_1, Comment_1, delta_h)) 

all_sheets <- map(
  all_sheets,
  ~ .x %>%
      dplyr::mutate(
      Treatment = dplyr::recode(
        Treatment,
        Fungi_Control = "Control",
        Treatment = 'Pellet'
      )
    ))
#make long df, not taking plots nested within site, too complex, just treat as single site
#make long df
df_long_BB <- bind_rows( all_sheets)

#remove hazel and Scots - or not - why did I remove these?
#trees = c("Betula_pubescens","Quercus"  ,"Alnus" )
#df_long_BB =  df_long_BB %>% filter(Tree_Species %in% trees)

#for this site, remove all rows where comment_1 == Dt, or dead
df_long_BB <- df_long_BB %>%
  filter(is.na(Comment_1) | !trimws(Comment_1) %in% c("Dead top", "Dt"))


#now remove the comment_1 column
df_long_BB = df_long_BB[-4]

#add site names
df_long_BB$Site = 'BB'
df_long_BB$EMF = 'LACAM'

##############################################################################
# GreshamKathryn  BJ

sheets <- excel_sheets('../../../data/field/GreshamKathryn2025.xlsx')  # get sheet names

# read all sheets into a named list, suppressing the New names messages
all_sheets <- suppressMessages(
  lapply(sheets, function(s) {
    read_excel('../../../data/field/GreshamKathryn2025.xlsx', sheet = s)
  })
)
names(all_sheets) <- sheets

#dont want the Pellet Control plot - where blank pellet was used, just need combine plot
#where treatment and control were put together 
all_sheets <- all_sheets[3]

#add the delta_h in now
all_sheets <- map(all_sheets, ~ .x %>%
                    mutate(
                      delta_h   = Height_1 - Height_0 ))


all_sheets <-  map(all_sheets, ~ .x %>%
                     dplyr::select(Tree_Species, Comment_1, Treatment, Health_1, delta_h)) 


df_long_BJ <- bind_rows( all_sheets)

#remove dead tops
df_long_BJ <- df_long_BJ %>%
  filter(is.na(Comment_1) | !trimws(Comment_1) == "Dt")


#now remove the comment_1 column
df_long_BJ = df_long_BJ[-2]

#add site name
df_long_BJ$Site = 'BJ'
df_long_BJ$EMF = 'AMRUB'

############################################################################

#Tievedooley  BK


sheets <- excel_sheets('../../../data/field/Tievedooly2025.xlsx')  # get sheet names

# read all sheets into a named list, suppressing the New names messages
all_sheets <- suppressMessages(
  lapply(sheets, function(s) {
    read_excel('../../../data/field/Tievedooly2025.xlsx', sheet = s)
  })
)
names(all_sheets) <- sheets

#remove meta etc
all_sheets <- all_sheets[-c(1,2)]


#fix the space in the Height _1 etc
all_sheets <- lapply(all_sheets, function(df) {
  df <- df %>%
    rename(
      Height_1 = dplyr::any_of("Height _1"),
      Height_0 = dplyr::any_of("Height _0")
    )
})
#add the delta_h in now
all_sheets <- map(all_sheets, ~ .x %>%
                    mutate(
                      delta_h   = Height_1 - Height_0 ))

all_sheets <-  map(all_sheets, ~ .x %>%
                     dplyr::select(Tree_Species,  Treatment, Health_1, delta_h)) 

df_long_BK <- bind_rows( all_sheets)

df_long_BK$Site = 'BK'
df_long_BK$EMF = 'PAX'

###############################################################################

#TillhillPaige  BM

sheets <- excel_sheets('../../../data/field/TillhillPaige2025.xlsx')  # get sheet names

# read all sheets into a named list, suppressing the New names messages
all_sheets <- suppressMessages(
  lapply(sheets, function(s) {
    read_excel('../../../data/field/TillhillPaige2025.xlsx', sheet = s)
  })
)
names(all_sheets) <- sheets

#remove meta etc
all_sheets <- all_sheets[-c(1,2)]

#add the delta_h in now
all_sheets <- map(all_sheets, ~ .x %>%
                    mutate(
                      delta_h   = Height_1 - Height_0 ))

#fix col names
all_sheets <- map(
  all_sheets,
  ~ .x %>%
    dplyr::rename(Treatment = Fungi_Treatment) %>%
    dplyr::mutate(
      Treatment = dplyr::recode(
        Treatment,
        Fungi_control = "Control",
        Treatment = 'Pellet'
      )
    )
)

all_sheets <-  map(all_sheets, ~ .x %>%
                     dplyr::select(Tree_Species, Treatment, Health_1, delta_h)) 

df_long_BM <- bind_rows( all_sheets)

df_long_BM$Site = 'BM'
df_long_BM$EMF = 'LACAM'

###############################################################################

#Balcaskie AC
sheets <- excel_sheets('../../../data/field/Balcaskie2025.xlsx')  # get sheet names

# read all sheets into a named list, suppressing the New names messages
all_sheets <- suppressMessages(
  lapply(sheets, function(s) {
    read_excel('../../../data/field/Balcaskie2025.xlsx', sheet = s)
  })
)
names(all_sheets) <- sheets

#remove meta etc
all_sheets <- all_sheets[-c(1,5,6)]


#add the delta_h in now
all_sheets <- map(all_sheets, ~ .x %>%
                    mutate(
                      delta_h   = Height_1 - Height_0 ))


all_sheets <-  map(all_sheets, ~ .x %>%
                     dplyr::select(Tree_Species, Treatment, Health_1, delta_h)) 

df_long_AC <- bind_rows( all_sheets)

#remove other tree species from here, only oak and beech at this site
trees = c('Fagus_sylvatica', 'Quercus_robur')
df_long_AC =  df_long_AC %>% filter(Tree_Species %in% trees)

df_long_AC$Site = 'AC'

#At this site, on inspection of raw - health_1 are all browsed trees and should be removed
df_long_AC = df_long_AC %>% filter(Health_1 != 1)
df_long_AC$EMF = 'EMF'
###############################################################################

#Borders Trust  BI

sheets <- excel_sheets('../../../data/field/BordersTrust2025.xlsx')  # get sheet names

# read all sheets into a named list, suppressing the New names messages
all_sheets <- suppressMessages(
  lapply(sheets, function(s) {
    read_excel('../../../data/field/BordersTrust2025.xlsx', sheet = s)
  })
)
names(all_sheets) <- sheets

#remove meta etc
all_sheets <- all_sheets[-c(1,2)]

#fix col names
all_sheets <- map(
  all_sheets,
  ~ .x %>%
    dplyr::rename(Treatment = Fungi_Treatment) %>%
    dplyr::mutate(
      Treatment = dplyr::recode(
        Treatment,
        Fungi_Control = "Control",
        Treated = 'Pellet'
      )
    )
)

#add the delta_h in now
all_sheets <- map(all_sheets, ~ .x %>%
                    mutate(
                      delta_h   = Height_1 - Height_0 ))

all_sheets <-  map(all_sheets, ~ .x %>%
                     dplyr::select(Tree_Species, Treatment, Health_1, delta_h)) 


df_long_BI <- imap_dfr(
  all_sheets,   # your list of plots
  ~ .x %>%
    mutate(
      Site = case_when(
        str_detect(.y, "Plot[1-3]") ~ "BI1", # 650m
        str_detect(.y, "Plot[4-6]") ~ "BI2", #600m
        str_detect(.y, "Plot[7-9]") ~ "BI3", #550m
        TRUE ~ NA_character_
      )
    )
)

df_long_BI$EMF = 'LACAM'
###############################################################################

#BD RDS Camquhart

sheets <- excel_sheets('../../../data/field/RDSCamquhart2025.xlsx')  # get sheet names

# read all sheets into a named list, suppressing the New names messages
all_sheets <- suppressMessages(
  lapply(sheets, function(s) {
    read_excel('../../../data/field/RDSCamquhart2025.xlsx', sheet = s)
  })
)
names(all_sheets) <- sheets

#remove meta etc
all_sheets <- all_sheets[-c(1,2,5)]


#add the delta_h in now
all_sheets <- map(all_sheets, ~ .x %>%
                    mutate(
                      delta_h   = Height_1 - Height_0 ))

all_sheets <-  map(all_sheets, ~ .x %>%
                     dplyr::select(Tree_Species, Treatment, Health_1, delta_h, Comment_1)) 


df_long_BD <- bind_rows( all_sheets)

#remove dead tops
df_long_BD <- df_long_BD %>%
  filter(is.na(Comment_1) | !trimws(Comment_1) == "Dt")

#remove comments column
df_long_BD =  df_long_BD[-5]

df_long_BD$Site = 'BD'
df_long_BD$EMF = 'PAX'

#################################################################################

#combine into one long df
all_data = 
  rbind.data.frame(df_long_AC, df_long_BA, df_long_BB, df_long_BD, df_long_BI,
                   df_long_BJ, df_long_BK, df_long_BM)

#correct some entries, Quercus, Quercus_robur, Betula, Betula_pubescens and Betula pubesens
all_data <- all_data %>%
  mutate(
    Tree_Species = case_when(
      Tree_Species == "Picea_abies"       ~ "PiceaA",
      Tree_Species == "Picea_sitchensis"  ~ "PiceaS",
      TRUE ~ sub("_.*$", "", Tree_Species)
    )
  )

#Look at distribution of tree species across sites
# Pivot to wide table: rows = Tree_species, columns = Site

#Now filter remaining delta_h > -1

all_data_pos = all_data %>% filter (delta_h >= -1)

# need to adjust the data s.t. all > 0.1, i.e. -1 and 0 become 0.1
all_data_pos <- all_data_pos %>%
  mutate(delta_h = ifelse(delta_h <= 0, 0.1, delta_h))

all_data_pos$Treatment = as.factor(all_data_pos$Treatment)

#use all data except fertiliser
data = all_data_pos %>% filter (Treatment != 'Fertiliser')

#data now ready from here - 

###########################################################################
#FILTER THE DATA SETS

#basic data explorations
#data = all positive data without fertilizer with values of -1 - 0 made to be 0.1)
#data = data


#model set 2 - lets remove the alder and Corylus
data = data %>% filter(Tree_Species != 'Alnus')
data = data %>% filter(Tree_Species != 'Corylus')

#model set 3
data = data %>% filter(Site != 'BM')

#############################################################################

#table of number of trees by site
# change the data name here to print which table
#can pring all data for eg - to see how many we have lost when delete -ves
table_ready <- data %>%  
  count(Tree_Species, Site, Treatment) %>%
  pivot_wider(
    names_from = Site,
    values_from = n,
    values_fill = 0
  ) %>%
  arrange(Treatment, Tree_Species)

library(DT)

datatable(table_ready, options = list(pageLength = 10), caption = "Tree counts by Species, Site, and Treatment")

write.csv(table_ready, file = '../../../results/datasummary.csv')

#a box plot check to see if -ves remain, and shows spread of delta_h
ggplot(all_data_pos, aes(x = factor(Health_1), y = delta_h)) +
  geom_boxplot(fill = "skyblue", color = "black") +  # color & fill
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # horizontal line at 0
  labs(
    x = "Health class",
    y = "Change in height (cm)",
  ) +
  theme_bw() +
  theme(
    text = element_text(size = 12)
  )
#####_________________________________________________________________________

#different mean plots and tables

mean_table <- data %>%
  group_by(Site, Tree_Species, Treatment,EMF) %>%
  summarise(
    mean_delta_h = mean(delta_h, na.rm = TRUE),
   .groups = "drop"
  ) %>%
  arrange(Site, Tree_Species, Treatment, EMF)

#show delta h by site and tree species as a reality check
#this is diff in cm average across that site/tree spp
mean_table

mean_wide <- mean_table %>%
  pivot_wider(
    names_from = Treatment,
    values_from = mean_delta_h
  )

# as above but make a % difference column
mean_wide <- mean_wide %>%
  mutate(
    diff_vs_control = Pellet - Control, # or Fertiliser - Control, if needed
    percent_diff = ((Pellet - Control)/Control)*100 
  )



mean_wide

#plot of cm differences between pellet and control across sites by tree sp
#good for highlighting any potential odd ball sites
ggplot(mean_wide, aes(x = Tree_Species, y = diff_vs_control, fill = Tree_Species)) +
  geom_col(position = "dodge") +
  facet_wrap(~ Site) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    x = "Tree Species",
    y = "Difference vs Control cm",
    title = "Treatment effect relative to Control per Site and Tree Species"
  ) +
  theme_bw() +
  theme(
    text = element_text(size = 12),
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#plot of % differences between pellet and control across sites by tree sp
#good for highlighting any potential odd ball sites
ggplot(mean_wide, aes(x = Tree_Species, y = percent_diff, fill = Tree_Species)) +
  geom_col(position = "dodge") +
  facet_wrap(~ Site) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    x = "Tree Species",
    y = "Difference vs Control cm",
    title = "Treatment effect relative to Control per Site and Tree Species"
  ) +
  theme_bw() +
  theme(
    text = element_text(size = 12),
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#same again but by emf - to visualise whether the emf are behaving the same everywhere
ggplot(mean_wide, aes(x = EMF, y = percent_diff, fill = EMF)) +
  geom_col(position = "dodge") +   # bar plot; use geom_point() for points
  facet_wrap(~ Site) +             # one facet per site
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # line at zero
  labs(
    x = "EMF",
    y = "Difference vs Control %",
    title = "Treatment effect relative to Control per Site and EMF"
  ) +
  theme_bw() +
  theme(
    text = element_text(size = 12),
    legend.position = "none"
  )



#visualise the change in heights by treatment
mean_df <- data %>%
  group_by(Treatment) %>%
  summarise(mean_delta_h = mean(delta_h), .groups = "drop")

ggplot(data, aes(x = delta_h, fill = Treatment)) +
  geom_histogram(
    bins = 60,
    alpha = 0.4,
    position = "identity"
  ) +
  geom_vline(
    data = mean_df,
    aes(xintercept = mean_delta_h, colour = Treatment),
    linewidth = 1.1
  ) +
  labs(
    x = "Change in height (Δh)",
    y = "Count"
  )

#Finally - for model comparisons - what is the raw mean of the control and pellet

mean_raw <- data %>%
  group_by(Treatment) %>%
  summarise(
    mean_delta_h = mean(delta_h, na.rm = TRUE),
    .groups = "drop"
  ) 
############################################################################

#______________ MODELS

#############################################################################

library(brms)
# first we could just ask - does treatment have any effect - ignore the different emf.
#all_data pos is only Pellet and Control, lets remove fertilizer for now.


model_1 <- brm(
  delta_h ~ Treatment + (1 | Site),
  data = data,
  family = lognormal(),
  chains = 4, cores = 4,
  control = list(adapt_delta = 0.95)
)

model_trunc <- brm(
  delta_h | trunc(lb = 0, ub = 100) ~ Treatment + (1 | Site),
  data = data,
  family = lognormal(),
  chains = 4, cores = 4,
  control = list(adapt_delta = 0.95)
)

model_gamma <- brm(
  delta_h ~ Treatment + (1 | Site),
  data = data,
  family = Gamma(link = "log"),
  chains = 4, cores = 4,
  control = list(adapt_delta = 0.95)
)

# MODEL EVALUATION

library(performance) #FOR BAYES r2
#model to evaluate

model = model_gamma

#1..Bayes_r2 prop variance in ind which can be explained by dep
R2 = bayes_R2(model)

pp_check(model, 'dens_overlay') +xlim(0,100)
pp_check(model, 'stat')
pp_check(model, stat = "sd")
pp_check(model, type = "stat", stat = "median", bins = 50)+xlim(0,15)
pp_check(model, "stat_2d")

#2.. Model comparisons
loo_1 = loo(model_1)
loo_gamma = loo(model_gamma)

#is abs value difference in elpd_diff > se
library(loo)
loo_compare(loo_1, loo_gamma)

#3.. Visualise

#get all the posterior drawers not just the model parameters
library(tidybayes)
draw = as_draws_df(model)

#extra bit to show prob that change is > 5% - because is that more meaniful than > 0?
prob_pos =mean(draw$b_TreatmentPellet > log(1.05))


posterior_pred <- draw %>%
  # Compute linear predictor for each draw
  mutate(
    delta_h_ctrl = exp(b_Intercept),
    delta_h_pellet = exp(b_Intercept + b_TreatmentPellet)
  ) %>%
  select(delta_h_ctrl, delta_h_pellet) %>%
  pivot_longer(everything(), names_to = "Treatment", values_to = "delta_h") %>%
  mutate(Treatment = recode(Treatment,
                            "delta_h_ctrl" = "Control",
                            "delta_h_pellet" = "Pellet"))

mean_data <- data %>%
  group_by(Treatment) %>%
  summarise(
    mean_delta_h = mean(delta_h, na.rm = TRUE),
    .groups = "drop"
  )


ggplot() +
  # raw observed data
  geom_jitter(
    data = data,
    aes(x = Treatment, y = delta_h),
    width = 0.1,
    alpha = 0.5,
    colour = "black"
  ) +
  
  # posterior fitted values (on original scale)
  geom_jitter(
    data = posterior_pred,
    aes(x = Treatment, y = delta_h),
    width = 0.1,
    alpha = 0.25,
    colour = "blue"
  ) +
  
  # raw data means
  geom_point(
    data = mean_data,
    aes(x = Treatment, y = mean_delta_h),
    colour = "red",
    size = 3
  ) +
  
  labs(
    y = "Change in height (Δh)",
    x = "Treatment"
  ) +
  
  theme_classic()


# Helper function - enter model name below
summarise_treatment <- function(model, treatment_name = "TreatmentPellet") {
  library(posterior)
  
  # Extract posterior draws
  draws <- as_draws_df(model)
  
  # Control (baseline)
  control_draws <- exp(draws$b_Intercept)
  control_mean <- mean(control_draws)
  control_CrI <- quantile(control_draws, c(0.025, 0.975))
  
  # Treatment
  treat_draws <- exp(draws$b_Intercept + draws[[paste0("b_", treatment_name)]])
  treat_mean <- mean(treat_draws)
  treat_CrI <- quantile(treat_draws, c(0.025, 0.975))
  
  # % increase relative to control
  pct_draws <- (treat_draws / control_draws - 1) * 100
  pct_mean <- mean(pct_draws)
  pct_CrI <- quantile(pct_draws, c(0.025, 0.975))
  
  # Posterior probability treatment > control
  prob_positive <- mean(treat_draws > control_draws)
  
  # Return results
  data.frame(
    Group = c("Control", treatment_name, paste0(treatment_name, "_%increase")),
    Mean = c(control_mean, treat_mean, pct_mean),
    CI_low = c(control_CrI[1], treat_CrI[1], pct_CrI[1]),
    CI_high = c(control_CrI[2], treat_CrI[2], pct_CrI[2]),
    Prob_positive = c(NA, prob_positive, prob_positive)
  )
}

summarise_treatment(model)

mean(pellet_draws > control_draws)


###############################################################################

#variance and the null model

model_null <- brm(
  delta_h ~ 1 + (1 | Site),
  data = data,
  family = Gamma(link = "log"),
  chains = 4, cores = 4,
  control = list(adapt_delta = 0.95)
)

bayes_R2(model_null)

