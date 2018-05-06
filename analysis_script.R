library(tidyverse)
library(ggmap)

month_colors <- c("#4424D6", "blue", "#347C98", "#66B032", "#B2D732", "#FEFE33", "#FCCC1A", "#FB9902","#FC600A","#FE2712",
                  "#C21460", "#8601AF", "#4424D6")
  
boston <- qmap(location = "Boston, MA", maptype = "satellite", zoom = 12)

crime <- readRDS("filtered_crime.RDS")
year <- 2015:2018

for (ii in 1:length(year)){
  
  nam_cur <- paste("crime", year[ii], sep = "_")
  nam_in <- paste("crime_in", year[ii], sep = "_")
  nam_month <- paste("crime_per_month", year[ii], sep = "_")
  nam_week <- paste("crime_per_week", year[ii], sep = "_")
  nam_day <- paste("crime_per_day", year[ii], sep = "_")
  nam_hour <- paste("crime_per_hour", year[ii], sep = "_")
  
  
  cur_crime <- crime %>% 
    filter(YEAR == year[ii])
  
  crimes_in <- cur_crime %>% 
    select(OFFENSE_CODE) %>% 
    group_by(OFFENSE_CODE) %>% 
    summarize(`CRIME TOTALS` = n()) %>% 
    mutate(Proportion = `CRIME TOTALS` / sum(`CRIME TOTALS`))
  
  crime_month <- cur_crime %>% 
    select(MONTH) %>% 
    group_by(MONTH) %>% 
    summarize(`CRIME TOTALS` = n()) %>% 
    mutate(Proportion = `CRIME TOTALS` / sum(`CRIME TOTALS`))
  
  crime_week <- cur_crime %>% 
    select(DAY_OF_WEEK, HOUR) %>% 
    group_by(DAY_OF_WEEK, HOUR) %>% 
    summarize(`CRIME TOTALS` = n()) %>% 
    mutate(Proportion = `CRIME TOTALS` / sum(`CRIME TOTALS`))
  
  crime_day <- cur_crime %>% 
    select(DAY) %>% 
    group_by(DAY) %>% 
    summarize(`CRIME TOTALS` = n()) %>% 
    mutate(Proportion = `CRIME TOTALS` / sum(`CRIME TOTALS`))
  
  crime_hour <- cur_crime %>% 
    select(HOUR) %>% 
    group_by(HOUR) %>% 
    summarize(`CRIME TOTALS` = n()) %>% 
    mutate(Proportion = `CRIME TOTALS` / sum(`CRIME TOTALS`))
    
  assign(nam_cur, cur_crime)
  assign(nam_in, crimes_in)
  assign(nam_month, crime_month)
  assign(nam_day, crime_day)
  assign(nam_hour, crime_hour)
  assign(nam_week, crime_week)
  
}

rm(nam_in, nam_cur, nam_day, nam_hour, nam_month, ii, nam_week)

#######  2015

# Crime Type
ggplot(crime_2015, aes(x=OFFENSE_CODE_GROUP)) + 
  geom_bar( stat="count", color = "black", fill = "#C21460") +
  theme(axis.text.x = element_text(size = 10, angle = 90, vjust = 0.1),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.text = element_text(size = 10),
        legend.title = element_text(face = "bold", hjust = -1)) +
  labs(title="Crimes in 2015", 
       x="Crime Type",
       y = "# of Times")

# Month
ggplot(crime_per_month_2015, aes(x = 6:12, y = `CRIME TOTALS`)) +
  geom_line(size = 1.2)+
  geom_point(size = 3, colour = month_colors[6:12]) + 
  theme(axis.text.x = element_text(size = 10),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.text = element_text(size = 10)) +
  labs(title="Crimes in 2015", 
       x= "Month",
       y = "# of Crimes") + 
  scale_x_continuous(breaks = 6:12,
                     labels= month.name[6:12])



# Day

ggplot(crime_per_day_2015, aes(x = 1:31, y = `CRIME TOTALS`)) +
  geom_line(size = 1.2)+
  geom_point(size = 3) + 
  theme(axis.text.x = element_text(size = 10),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.text = element_text(size = 10)) +
  labs(title="Crimes Per Day of Month in 2015", 
       x= "Day",
       y = "# of Crimes")

chisq.test(crime_per_day_2015["CRIME TOTALS"])
day_table <- rbind(week_chisq$observed, week_chisq$expected, week_chisq$residuals)
rownames(day_table) <- c("Obs.","Exp.","Res.")
colnames(day_table) <- crime_per_day_2015["DAY"]

# Week

ggplot(crime_per_week_2015, aes(x = DAY_OF_WEEK, y = Proportion, fill = HOUR)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient2(low = "#000333", mid = "yellow", midpoint = 12, high = "#000066",
                    labels = 0:23,
                    breaks = 0:23,
                    guide = guide_legend(
                      title.position = "top",
                      title = "Hour",
                      label.position = "right",
                      label.hjust = 0.5,
                      label.vjust = 1,
                      title.vjust = 1,
                      title.hjust = .5,
                      title.theme = element_text(face = "bold", angle = 0),
                      label.theme = element_text(angle = 0)
                    )
  ) +
  labs(title="Distribution of Crimes per Hour in 2015", 
       x= "Day of Week",
       y = "Proportion") + 
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12), 
        axis.title.x = element_text(face = "bold", size = 15),
        axis.title.y = element_text(face = "bold", size = 15)) +
  scale_x_discrete(limits=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))

week_2015 <- crime_2015 %>% select(DAY_OF_WEEK) %>% group_by(DAY_OF_WEEK) %>% summarize(TOTAL = n())
chisq.test(week_2015["TOTAL"])
week_table <- rbind(week_chisq$observed, week_chisq$expected, week_chisq$residuals)
rownames(week_table) <- c("Obs.","Exp.","Res.")
colnames(week_table) <- c("Friday","Monday","Saturday","Sunday","Thursday","Tuesday","Wednesday")

# Time

ggplot(crime_per_hour_2015, aes(x = 1:24, y = `CRIME TOTALS`)) +
  geom_line(size = 1.2)+
  geom_point(size = 3) + 
  theme(axis.text.x = element_text(size = 10),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.text = element_text(size = 10)) +
  labs(title="Crimes Per Hour", 
       x= "Hour",
       y = "# of Crimes")

# Location
ggmap(boston, extent = "device") +
  geom_density2d(data = crime_2015,
                 aes(x = Long, y = Lat), size = 0.3) + 
  stat_density2d(data = crime_2015, aes(x = Long, y = Lat,
                                        fill = ..level.., alpha = ..level..),
                 size = 0.01, bins = 16, geom = "polygon") + 
  scale_fill_gradient(low = "green", high = "red", 
                      guide = FALSE) +
  scale_alpha(range = c(0, 0.3), guide = FALSE) +
  labs(title="Crime Distribution in 2015")


#####  2016

# Crime Type
ggplot(crime_2016, aes(x=OFFENSE_CODE_GROUP)) + 
  geom_bar( stat="count", color = "black", fill = "#8601AF") +
  theme(axis.text.x = element_text(size = 10, angle = 90, vjust = 0.1),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.text = element_text(size = 10),
        legend.title = element_text(face = "bold", hjust = -1)) +
  labs(title="Crimes in 2016", 
       x="Crime Type",
       y = "# of Times")

# Month
ggplot(crime_per_month_2016, aes(x = 1:12, y = `CRIME TOTALS`)) +
  geom_line(size = 1.2)+
  geom_point(size = 3, colour = month_colors) + 
  theme(axis.text.x = element_text(size = 10),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.text = element_text(size = 10)) +
  labs(title="Crimes in 2016", 
       x= "Month",
       y = "# of Crimes") + 
  scale_x_continuous(breaks = seq(1,12,1),
                     labels= month.name[1:12])

# Day

ggplot(crime_per_day_2016, aes(x = 1:31, y = `CRIME TOTALS`)) +
  geom_line(size = 1.2)+
  geom_point(size = 3) + 
  theme(axis.text.x = element_text(size = 10),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.text = element_text(size = 10)) +
  labs(title="Crimes Per Day of Month in 2016", 
       x= "Day",
       y = "# of Crimes")

# Week

ggplot(crime_per_week_2016, aes(x = DAY_OF_WEEK, y = Proportion, fill = HOUR)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient2(low = "#000333", mid = "yellow", midpoint = 12, high = "#000066",
                       labels = 0:23,
                       breaks = 0:23,
                       guide = guide_legend(
                         title.position = "top",
                         title = "Hour",
                         label.position = "right",
                         label.hjust = 0.5,
                         label.vjust = 1,
                         title.vjust = 1,
                         title.hjust = .5,
                         title.theme = element_text(face = "bold", angle = 0),
                         label.theme = element_text(angle = 0)
                       )
  ) +
  labs(title="Distribution of Crimes per Hour in 2016", 
       x= "Day of Week",
       y = "Proportion") + 
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12), 
        axis.title.x = element_text(face = "bold", size = 15),
        axis.title.y = element_text(face = "bold", size = 15)) +
  scale_x_discrete(limits=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))

# Time

ggplot(crime_per_hour_2016, aes(x = 1:24, y = `CRIME TOTALS`)) +
  geom_line(size = 1.2)+
  geom_point(size = 3) + 
  theme(axis.text.x = element_text(size = 10),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.text = element_text(size = 10)) +
  labs(title="Crimes Per Hour in 2016", 
       x= "Hour",
       y = "# of Crimes")

# Location
ggmap(boston, extent = "device") +
  geom_density2d(data = crime_2016,
                 aes(x = Long, y = Lat), size = 0.3) + 
  stat_density2d(data = crime_2016, aes(x = Long, y = Lat,
                                        fill = ..level.., alpha = ..level..),
                 size = 0.01, bins = 16, geom = "polygon") + 
  scale_fill_gradient(low = "green", high = "red", 
                      guide = FALSE) +
  scale_alpha(range = c(0, 0.3), guide = FALSE) +
  labs(title="Crime Distribution in 2016")


####  2017

# Crime Type
ggplot(crime_2017, aes(x=OFFENSE_CODE_GROUP)) + 
  geom_bar( stat="count", color = "black", fill = "#4424D6") +
  theme(axis.text.x = element_text(size = 10, angle = 90, vjust = 0.1),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.text = element_text(size = 10),
        legend.title = element_text(face = "bold", hjust = -1)) +
  labs(title="Crimes in 2017", 
       x="Crime Type",
       y = "# of Times")

# Month
ggplot(crime_per_month_2017, aes(x = 1:12, y = `CRIME TOTALS`)) +
  geom_line(size = 1.2)+
  geom_point(size = 3, colour = month_colors) + 
  theme(axis.text.x = element_text(size = 10),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.text = element_text(size = 10)) +
  labs(title="Crimes in 2017", 
       x= "Month",
       y = "# of Crimes") + 
  scale_x_continuous(breaks = seq(1,12,1),
                     labels= month.name[1:12])

# Day

ggplot(crime_per_day_2017, aes(x = 1:31, y = `CRIME TOTALS`)) +
  geom_line(size = 1.2)+
  geom_point(size = 3) + 
  theme(axis.text.x = element_text(size = 10),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.text = element_text(size = 10)) +
  labs(title="Crimes Per Day of Month in 2017", 
       x= "Day",
       y = "# of Crimes")

# Week

ggplot(crime_per_week_2017, aes(x = DAY_OF_WEEK, y = Proportion, fill = HOUR)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient2(low = "#000333", mid = "yellow", midpoint = 12, high = "#000066",
                       labels = 0:23,
                       breaks = 0:23,
                       guide = guide_legend(
                         title.position = "top",
                         title = "Hour",
                         label.position = "right",
                         label.hjust = 0.5,
                         label.vjust = 1,
                         title.vjust = 1,
                         title.hjust = .5,
                         title.theme = element_text(face = "bold", angle = 0),
                         label.theme = element_text(angle = 0)
                       )
  ) +
  labs(title="Distribution of Crimes per Hour in 2017", 
       x= "Day of Week",
       y = "Proportion") + 
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12), 
        axis.title.x = element_text(face = "bold", size = 15),
        axis.title.y = element_text(face = "bold", size = 15)) +
  scale_x_discrete(limits=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))

# Time

ggplot(crime_per_hour_2017, aes(x = 1:24, y = `CRIME TOTALS`)) +
  geom_line(size = 1.2)+
  geom_point(size = 3) + 
  theme(axis.text.x = element_text(size = 10),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.text = element_text(size = 10)) +
  labs(title="Crimes Per Hour", 
       x= "Hour",
       y = "# of Crimes")

# Location
ggmap(boston, extent = "device") +
  geom_density2d(data = crime_2017,
                 aes(x = Long, y = Lat), size = 0.3) + 
  stat_density2d(data = crime_2017, aes(x = Long, y = Lat,
                                        fill = ..level.., alpha = ..level..),
                 size = 0.01, bins = 16, geom = "polygon") + 
  scale_fill_gradient(low = "green", high = "red", 
                      guide = FALSE) +
  scale_alpha(range = c(0, 0.3), guide = FALSE) +
  labs(title="Crime Distribution in 2017")


###### 2018

# Crime Type
ggplot(crime_2018, aes(x=OFFENSE_CODE_GROUP)) + 
  geom_bar( stat="count", color = "black", fill = "#347C98") +
  theme(axis.text.x = element_text(size = 10, angle = 90, vjust = 0.1),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.text = element_text(size = 10),
        legend.title = element_text(face = "bold", hjust = -1)) +
  labs(title="Crimes in 2018", 
       x="Crime Type",
       y = "# of Times")

# Month
ggplot(crime_per_month_2018, aes(x = 1:5, y = `CRIME TOTALS`)) +
  geom_line(size = 1.2)+
  geom_point(size = 3, colour = month_colors[1:5]) + 
  theme(axis.text.x = element_text(size = 10),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.text = element_text(size = 10)) +
  labs(title="Crimes in 2018", 
       x= "Month",
       y = "# of Crimes") + 
  scale_x_continuous(breaks = seq(1,5,1),
                     labels= month.name[1:5])

# Day

ggplot(crime_per_day_2018, aes(x = 1:31, y = `CRIME TOTALS`)) +
  geom_line(size = 1.2)+
  geom_point(size = 3) + 
  theme(axis.text.x = element_text(size = 10),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.text = element_text(size = 10)) +
  labs(title="Crimes Per Day of Month in 2018", 
       x= "Day",
       y = "# of Crimes")

# Week

ggplot(crime_per_week_2018, aes(x = DAY_OF_WEEK, y = Proportion, fill = HOUR)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient2(low = "#000333", mid = "yellow", midpoint = 12, high = "#000066",
                       labels = 0:23,
                       breaks = 0:23,
                       guide = guide_legend(
                         title.position = "top",
                         title = "Hour",
                         label.position = "right",
                         label.hjust = 0.5,
                         label.vjust = 1,
                         title.vjust = 1,
                         title.hjust = .5,
                         title.theme = element_text(face = "bold", angle = 0),
                         label.theme = element_text(angle = 0)
                       )
  ) +
  labs(title="Distribution of Crimes per Hour in 2018", 
       x= "Day of Week",
       y = "Proportion") + 
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12), 
        axis.title.x = element_text(face = "bold", size = 15),
        axis.title.y = element_text(face = "bold", size = 15)) +
  scale_x_discrete(limits=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))

# Time

ggplot(crime_per_hour_2018, aes(x = 1:24, y = `CRIME TOTALS`)) +
  geom_line(size = 1.2)+
  geom_point(size = 3) + 
  theme(axis.text.x = element_text(size = 10),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.text = element_text(size = 10)) +
  labs(title="Crimes Per Hour", 
       x= "Hour",
       y = "# of Crimes")

# Location
ggmap(boston, extent = "device") +
  geom_density2d(data = crime_2018,
                 aes(x = Long, y = Lat), size = 0.3) + 
  stat_density2d(data = crime_2018, aes(x = Long, y = Lat,
                     fill = ..level.., alpha = ..level..),
                   size = 0.01, bins = 16, geom = "polygon") + 
  scale_fill_gradient(low = "green", high = "red", 
                      guide = FALSE) +
  scale_alpha(range = c(0, 0.3), guide = FALSE) +
  labs(title="Crime Distribution in 2018")



#### Cumulative

# Year
crimes_per_year <- crime %>% 
  select(YEAR) %>% 
  group_by(YEAR) %>% 
  summarize(`CRIME TOTALS` = n()) %>% 
  mutate(Proportion = `CRIME TOTALS` / sum(`CRIME TOTALS`))

# Distribution
crime_distribution <- crime %>% 
  select(YEAR, OFFENSE_CODE_GROUP) %>% 
  filter( YEAR == 2016 | YEAR == 2017) %>% 
  group_by(OFFENSE_CODE_GROUP) %>% 
  summarize(`2016` = n())

crime_difference = 

# Month
crimes_per_month_year <- crime %>% 
  select(MONTH, YEAR) %>% 
  group_by(YEAR, MONTH) %>% 
  summarize(`CRIME TOTALS` = n()) %>% 
  mutate(Proportion = `CRIME TOTALS` / sum(`CRIME TOTALS`))

ggplot(crimes_per_month_year, aes(x = YEAR, y = Proportion, fill = factor(MONTH))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = month_colors,
                    labels = month.name,
                        guide = guide_legend(
                          title.position = "top",
                          title = "Month",
                          label.position = "right",
                          label.hjust = 0.5,
                          label.vjust = 1,
                          title.vjust = 1,
                          title.hjust = .5,
                          title.theme = element_text(face = "bold", angle = 0),
                          label.theme = element_text(angle = 0)
                        )
  ) +
  labs(title="Distribution of Crimes per Month", 
       x= "Year",
       y = "Proportion") + 
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12), 
        axis.title.x = element_text(face = "bold", size = 15),
        axis.title.y = element_text(face = "bold", size = 15))





