####################################
# Plots of Crimea War Mortality Data 
# Graham Wheeler
# 07/05/2020
####################################

#############################
# Install and load packages #
#############################

install.packages("gganimate")
install.packages("gifski")
install.packages("av")
install.packages("magick")
install.packages("tidyr")
install.packages("tidyverse")
library(gganimate)
library(gifski)
library(av)
library(magick)
library(tidyr)
library(tidyverse)

rm(list=ls())

#######################################
# Load data - using "HistData package #
#######################################

install.packages("HistData")
library(HistData)
data<-Nightingale
colnames(data)[c(1,4:10)]<-c("Combined_Date","Average_Army_Size","Deaths_Zymotic_Diseases", "Deaths_Wounds_Injuries", "Deaths_Other", "AMR_Zymotic_diseases", "AMR_Wounds_injuries", "AMR_Other")

###########################################
# Load data - using RSS .xlsx file
# Visit https://statsyss.wordpress.com/2020/05/02/florence-nightingale-dataviz-competition-for-children-and-adults/
# and download the corresponding .xlsx file
###########################################

install.packages("xlsx")
library(xlsx)
data<-read.xlsx("fn_data-1.xlsx", sheetIndex = 1, header=TRUE)
data[,1]<-gsub(" ", "", data[,1])
Combined_Date <- format(strptime(paste0(01,"-",data[,1],"-",data[,2]), format = "%d-%b-%Y"), "%d-%m-%Y")
data$Combined_Date<-as.Date(Combined_Date, format = "%d-%m-%Y")

################
# Prepare data #
################

# Add variables for Monthly mortality rates
MMR_Zymotic_diseases<-data$AMR_Zymotic_diseases/12
MMR_Wounds_injuries<-data$AMR_Wounds_injuries/12
MMR_Other<-data$AMR_Other/12
data<-cbind(data, MMR_Zymotic_diseases, MMR_Wounds_injuries, MMR_Other)
plot_labels<-paste0(data$Month," ", data$Year)

# Prepare reduced data frame
red_data1<-data[,c("Combined_Date", "Month", "Year", "Average_Army_Size", "MMR_Zymotic_diseases", "MMR_Wounds_injuries", "MMR_Other")]
red_data2<-data[,c("Combined_Date", "Month", "Year", "Deaths_Zymotic_Diseases", "Deaths_Wounds_Injuries", "Deaths_Other")]
index <- 1:dim(red_data1)[1]
red_data1<-cbind(red_data1, index)

# Reshape reduced data frame to long format
red_data_long1<-gather(red_data1, death_cause, MMR, MMR_Zymotic_diseases:MMR_Other, factor_key=TRUE)
red_data_long2<-gather(red_data2, death_cause2, death_number, Deaths_Zymotic_Diseases:Deaths_Other, factor_key=TRUE)
red_data_long<-cbind(red_data_long1, red_data_long2)
red_data_long<-red_data_long[,-c(8:11)]

#######################################
# Generate Combined Line and Bar Plot #
#######################################

nframes <- 400
fps <- 20
width <- 700

# Generate static line plot
p4_1 <- ggplot(red_data_long, aes(x = factor(Combined_Date), y = MMR, group = death_cause)) + 
  scale_color_viridis_d() + 
  geom_segment(aes(xend = 24.1, yend = MMR), linetype = 2, colour = 'grey') +
  geom_vline(xintercept=12.5, linetype="dashed", size = 1, colour = "blue") +
  geom_line(aes(colour = death_cause), size = 3, alpha = 0.5) + 
  guides(color = FALSE, size = FALSE) + 
  geom_text(aes(x = 20, label = "Sanitation improvements made", y = 100, size = 2)) +
  scale_x_discrete(name = "\nDate (month/year)", labels=plot_labels) + 
  scale_y_continuous(name = "\nMonthly Mortality Rate (deaths per 1000 soldiers)", limits = c(0,100), breaks = c(0,20,40,60,80,100)) + 
  labs(title = "The Causes of Mortality in the Army in the East (1854-1856)", subtitle = "Monthly mortality rate over time by cause of death") + 
  geom_text(aes(x = 24.2, label = rep(c("Zymotic Diseases", "Wounds and Injuries", "Other"), each = 24)), hjust = 0) +
  coord_cartesian(clip = 'off') +
  theme(plot.margin = margin(5.5, 105.5, 5.5, 5.5), axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5), panel.background = element_rect(fill = "white", colour = "white")) + 
  geom_point(aes(group = seq_along(index)), size = 2)

# Data frame for arrow
df <- data.frame(x1 = 15.5, x2 = 12.75, y1 = 100, y2 = 95)
# Add arrow
p4_1 <- p4_1 + geom_segment(
  aes(x = x1, y = y1, xend = x2, yend = y2),
  data = df,
  arrow = arrow(length = unit(0.03, "npc")), inherit.aes = FALSE, colour = "blue", size = 1)

# Preview static line plot
# p4_1

anim4_1 <- p4_1 + transition_reveal(index)
anim4_1 <- animate(anim4_1, nframes = nframes, fps = fps, width = width, height = 450)

# Preview animated line plot
# anim4_1

# Generate static bar plot
p4_2 <- ggplot(red_data1, aes(x = factor(Combined_Date), y = Average_Army_Size)) +
  geom_bar(color = "grey", fill = "grey", stat = "identity") + 
  geom_vline(xintercept=12.5, linetype="dashed", size = 1, colour = "blue")+
  guides(fill = FALSE, size = FALSE) + 
  geom_text(aes(x = 20, label = "Sanitation improvements made", y = 50000, size = 2)) +
  labs(caption = "Original data from F. Nightingale | Source: 'HistData' CRAN package | Plot by @DrGWheeler | #FloViz") + 
  theme_minimal() +
  theme(plot.margin = margin(5.5, 105.5, 5.5, 5.5), 
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = NA),
        panel.ontop = TRUE, axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_x_discrete(name = "\nDate (month/year)", labels=plot_labels) + 
  scale_y_continuous(name = "Estimated Army Size", limits = c(0,50000), breaks = c(0,10000,20000,30000,40000,50000))

# Data frame for arrow
df <- data.frame(x1 = 15.5, x2 = 12.75, y1 = 50000, y2 = 47500)
# Add arrow
p4_2 <- p4_2 + geom_segment(
  aes(x = x1, y = y1, xend = x2, yend = y2),
  data = df,
  arrow = arrow(length = unit(0.03, "npc")), inherit.aes = FALSE, colour = "blue", size = 1)

# Preview static line plot
# p4_2

anim4_2 <- p4_2 + transition_states(index,transition_length = 3, state_length = 1, wrap = FALSE) +
  shadow_mark() + 
  enter_grow() +
  enter_fade()
anim4_2 <- animate(anim4_2, nframes = nframes, fps = fps, width = width, height = 250)

# Preview animated line plot
# anim4_2

# Now combine both plots
a_mgif <- image_read(anim4_1)
b_mgif <- image_read(anim4_2)

new_gif <- image_append(c(a_mgif[1], b_mgif[1]), stack = TRUE)
for(i in 2:nframes){
  combined <- image_append(c(a_mgif[i], b_mgif[i]), stack = TRUE)
  new_gif <- c(new_gif, combined)
}

# Preview combined animated plot as gif
# new_gif

# Save gif
image_write(new_gif, path="Nightingale2020.gif")

#######
# END #
#######