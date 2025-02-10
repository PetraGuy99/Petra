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
     values = c("control" = "orange", "pellet,area1" = "darkgreen", "pellet,area2" = "palegreen")  # Custom colors
   ) +
   labs(title = "Normalised survival, height and rcd for Pinus sylvestris, Forrest Estate ",
        x = "",
        y = "") +
   theme(
     axis.text.x = element_text(size =16),
     axis.text.y = element_text(size = 16),
     axis.title.x = element_text(size = 16, face = "bold"),  
     axis.title.y = element_text(size = 16, face = "bold"),
     panel.background = element_rect(fill = "white", color = "white"),  # Remove gray background
     plot.background = element_rect(fill = "white", color = "white"),  # Remove plot background
     panel.grid.major = element_blank(),  # Optionally remove grid lines
     panel.grid.minor = element_blank() 
   )
   theme_minimal()
 
   