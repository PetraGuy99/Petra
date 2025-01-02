setwd('C:/dev/code/Petra')

library(dplyr)
library(ggplot2)
library(readxl)
library(tidyr)

forrest = read_excel('../../data/Forrest_WorkingSheet.xlsx', sheet = 'survival')
 df = forrest%>% select(c(treatment,norm_density, norm_ht, norm_rcd))
 df_long = df %>% pivot_longer(cols = c(norm_density, norm_ht, norm_rcd),
                               names_to = 'Variable',
                               values_to = 'Value')
 
 ggplot(df_long, aes(x = Variable, y = Value, fill = treatment)) +
   geom_bar(
     stat = "identity",
     position = position_dodge(width = 0.9), # Slightly larger than bar width
     width = 0.75                            # Slightly narrower bars
   ) +
   scale_fill_manual(
     values = c("control" = "orange", "pellet,area1" = "darkgreen", "pellet,area2" = "darkgreen")  # Custom colors
   ) +
   labs(title = "Normalised survival, height and rcd for Pinus sylvestris, Forrest Estate ",
        x = "",
        y = "Normalised value") +
   theme_minimal()
 
   