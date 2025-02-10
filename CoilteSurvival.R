setwd('C:/dev/code/Petra')


library(dplyr)
library(ggplot2)
library(readxl)
library(tidyr)
#
data = read_excel('../../data/Copy of Coillte Statistics Sheet.xlsx', sheet = 'Combined')
#heights reading is as char because some data items in there - deleted 
#'REMOVED  ISSUE'
#'#also removed 'Salix Cutting and a blank Tree Species

#chnage some names to tidy for chart
data$Treatment <- ifelse(data$Treatment == "Fungi_Treatment", "Pellet", 
                         ifelse(data$Treatment == "Fungi_Control", "Control", data$Treatment))

#tidy tree names
data$Tree_Species <- ifelse(data$Tree_Species == "Salix_Nursery", "Salix", data$Tree_Species) 
                         
#survival/mortality is counted as the number of blanks in the height_2024 column

summary_table <- data %>%
  group_by(Tree_Species,Treatment) %>%
  summarize(
    count = n(),  # Total number of rows in each group
    blanks_in_height = sum(is.na(Height_cm_1) | Height_cm_1 == ""),
    #zero_in_height = sum(Height_cm_1 == 0, na.rm = TRUE), # Count blank or NA values in Height
  )

#there werent any zeros in the height column

survival.df = as.data.frame(summary_table)

#change col names
colnames(survival.df) <- c("Tree_Species", "Treatment", "Total", "Died")
#remove last two rows of odd salix
survival.df = survival.df[-c(13,14),]

#create survived column
survival.df$survived = survival.df$Total - survival.df$Died

survival.df$mortality =  (survival.df$Died/survival.df$Total)*100

#remove pines, zero mortality
survival.df = survival.df[-c(7,8),]

ggplot(survival.df, aes(x = Tree_Species, y = mortality, fill = Treatment)) +
  geom_bar(stat = "identity", position = "dodge") +  # Dodge to place bars side by side
  scale_fill_manual(values = c("Pellet" = "darkgreen", "Control" = "orange")) +  # Custom colors
  labs(title = "", x = "Tree Species", y = "Mortality %") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better visibility
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )

survival.test = survival.df %>% group_by(Tree_Species) %>%
  summarize(
    p_value = prop.test(
      x = c(survived[Treatment == "Fungi_Treatment"], survived[Treatment == "Fungi_Control"]), 
      n = c(Total[Treatment == "Fungi_Treatment"], Total[Treatment == "Fungi_Control"])
    )$p.value
  )

print(survival_tests)


#bar plots survival


#at Coilte there were many dead tops - but  i think the heights were still recorded
#so ht year2 < ht year 1, these should prob be reomved from analysis

#find trees where ht year 2< ht year1 and the NAs - which are the dead
nodead <- data %>% filter(Height_cm_1 > Height_cm_0, !is.na(Height_cm_1))

#remove the rows which are 'Salix Cutting' There 



#now summarise means by tee species
#need to munge the data to make long
#get rid of unwanted columns then pivot

#use change in het col tho
nodead$deltaht = nodead$Height_cm_1-nodead$Height_cm_0

data.selected = nodead[,c(2,5,20)]

mean_heights <- data.selected %>%
  group_by(Treatment, Tree_Species) %>%
  summarize(mean_ht_dif = mean(deltaht, na.rm = TRUE), .groups = "drop"  )

 print(mean_heights)
 
 #Even removing the reductions in height and dead has not given a different picture 
 #for the growth rates, which are still smaller for most treatment plots
 
 #could be worth counting the number of trees in each group where there was a height reduction
 
 deadtops =  nodead <- data %>% filter(Height_cm_1 < Height_cm_0, !is.na(Height_cm_1))
 
 row_count_deadtops <- deadtops %>%
    group_by(Treatment, Tree_Species) %>%
   summarise(num_rows = n(), .groups = "drop")
 
 print(row_count_deadtops) # no - still looks bad in the treatment plots
