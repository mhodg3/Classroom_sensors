library(readr)
library(tidyverse)
library(lubridate)
library(RcppRoll)
library(reshape2)

sensor_data <- read_csv("log.csv")
sensor_data$Box_ID = as.factor(sensor_data$Box_ID)
sensor_data$Dust1 = sensor_data(sensor_data$Dust1)
sensor_data$Dust10 = sensor_data(sensor_data$Dust10)
sensor_data$Dust2_5 = sensor_data(sensor_data$Dust2_5)
sensor_data$Dust1_orig = sensor_data$Dust1
sensor_data$Dust10_orig = sensor_data$Dust10
sensor_data$Dust2_5_orig = sensor_data$Dust2_5
sensor_data$Dust1[sensor_data$Dust1 > 8] = NA
sensor_data$Dust10[sensor_data$Dust10 > 8] = NA
sensor_data$Dust2_5[sensor_data$Dust2_5 > 8] = NA


Box93_87 <- filter(sensor_data, Box_ID == 87 | Box_ID == 93)
Box40_46 <- filter(sensor_data, Box_ID == 40 | Box_ID == 46)
Box105_59 <- filter(sensor_data, Box_ID == 59 | Box_ID == 105)
#write.csv(box93_87, file = "Box93_87.csv")
#write.csv(box105_59, file = "Box106_59.csv")
#write.csv(box40_46, file = "Box40_46.csv")
#write(box93_87, file = "Box93_87.RData")

#Box93_87 <- read_csv("Box93_87.csv")

Box93_87 = mutate(Box93_87,Time_received = ymd_hms(Time_received))
Box93_87 = mutate(Box93_87,Time_sent = ymd_hms(Time_sent))

Box93_87 = Box93_87 %>%
  group_by(Box_ID) %>%
  arrange(Time_sent) %>%
  mutate(moving_median_temp = roll_median(Temperature, 5, align="right", fill=0)) %>%
  mutate(moving_median_Dust1 = roll_median(Dust1, 15, align="right", fill=0)) %>%
  mutate(moving_median_Dust10 = roll_median(Dust10, 15, align="right", fill=0)) %>%
  mutate(moving_median_Dust2_5 = roll_median(Dust2_5, 15, align="right", fill=0)) %>%
  mutate(Dust10_err = Dust10_orig - moving_median_Dust10) %>%
  mutate(Dust1_err = Dust1_orig - moving_median_Dust1) %>%
  mutate(Dust2_5_err = Dust2_5_orig - moving_median_Dust2_5) %>%
  mutate(moving_median_Hum = roll_median(Humidity, 10, align="right", fill=0)) %>%
  mutate(moving_median_Pres = roll_median(Presence, 15, align="right", fill=0)) %>%
  mutate(moving_median_CO2 = roll_median(CO2, 15, align="right", fill=0))

#look at the average tem each hour over entire trial
temp_by_hour = Box93_87 %>% group_by(hour(Time_sent)) %>% summarize(avg=median(Temperature))
plot(temp_by_hour)



meltdf <- melt(df,id="Time_sent")
ggplot(Box93_87 %>% filter(Time_sent >= "2017-09-25"),aes(x=Time_sent,y=moving_median_temp,colour=Box_ID,group=Box_ID)) + geom_line()

ggplot(Box93_87 %>% filter(Time_sent >= "2017-09-25"),aes(x=Time_sent,y=moving_median_Dust1,colour=Box_ID,group=Box_ID)) + geom_line()

ggplot(Box93_87 %>% filter(Time_sent >= "2017-09-25" & Time_sent <= "2017-09-26"),aes(x=Time_sent,y=moving_median_Dust10,colour=Box_ID,group=Box_ID)) + geom_line()

ggplot(Box93_87 %>% filter(Time_sent >= "2017-09-25"),aes(x=Time_sent,y=moving_median_Dust2_5,colour=Box_ID,group=Box_ID)) + geom_line()

ggplot(Box93_87 %>% filter(Time_sent >= "2017-09-25" & Time_sent <= "2017-10-25"),aes(x=Time_sent,y=moving_median_Hum,colour=Box_ID,group=Box_ID)) + geom_line()

p1 = ggplot(Box93_87%>% filter(Time_sent >= "2017-11-01"),aes(x=Time_sent,y=moving_median_Pres,colour=Box_ID,group=Box_ID)) + geom_line(alpha = 0.3)

ggplot(Box93_87,aes(x=Time_sent,y=moving_median_CO2,colour=Box_ID,group=Box_ID)) + geom_line()


p1 + geom_line(data = Box93_87 %>% filter(Time_sent >= "2017-09-25"), colour = "blue", size = 1.5)


### plot the CO2 and presence together --------------------------
plot_data_h = Box93_87%>% filter(Time_sent >= "2017-11-01"& Box_ID==93) %>% 
  mutate(value = moving_median_Hum, sensor = 2, col_ind = 1) %>%
  select(Time_sent,value,sensor,col_ind)
min_H = min(plot_data_h$value)
range_H = max(plot_data_h$value)-min_H
plot_data_p = Box93_87%>% filter(Time_sent >= "2017-11-01"& Box_ID==93) %>% 
  mutate(value = (moving_median_Pres*range_H)+min_H, sensor = 1, col_ind = 2) %>%
  select(Time_sent,value,sensor,col_ind)

plot_data = bind_rows(plot_data_p, plot_data_h)

meltdf <- melt(df,id="Time_sent")
ggplot(plot_data,aes(x=Time_sent,y=value,colour=col_ind ,group=sensor)) + geom_line() +
  theme(legend.position="none")

### plot the CO2 and presence together --------------------------


### function to combine plots --------------------------
ggplot(Box93_87 %>% filter(Time_sent >= "2017-09-25" & Time_sent <= "2017-10-25"),aes(x=Time_sent,y=Humidity,colour=Box_ID,group=Box_ID)) + geom_line()

hist(sensor_data$Dust1[sensor_data$Dust1>0])
hist(sensor_data$Dust2_5[sensor_data$Dust2_5>0])
hist(sensor_data$Dust10[sensor_data$Dust10>0])



# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


### combo plots of Dust all sizes --------------------------
p1 = ggplot(Box93_87 %>% filter(Time_sent >= "2017-09-25" & Time_sent <= "2017-09-27"& Box_ID==93),aes(x=Time_sent,y=moving_median_Dust1,colour=Box_ID,group=Box_ID)) + geom_line()
p2_5 = ggplot(Box93_87 %>% filter(Time_sent >= "2017-09-25" & Time_sent <= "2017-09-27" & Box_ID==93),aes(x=Time_sent,y=moving_median_Dust2_5,colour=Box_ID,group=Box_ID)) + geom_line()
p10 = ggplot(Box93_87 %>% filter(Time_sent >= "2017-09-25" & Time_sent <= "2017-09-27"& Box_ID==93),aes(x=Time_sent,y=moving_median_Dust10,colour=Box_ID,group=Box_ID)) + geom_line()
multiplot(p1, p2_5, p10, cols=1)


p1 = ggplot(Box93_87 %>% filter(Time_sent >= "2017-09-25" ),aes(x=Time_sent,y=Dust1_err,colour=Box_ID,group=Box_ID)) + geom_line()
p2_5 = ggplot(Box93_87 %>% filter(Time_sent >= "2017-09-25" ),aes(x=Time_sent,y=Dust2_5_err,colour=Box_ID,group=Box_ID)) + geom_line()
p10 = ggplot(Box93_87 %>% filter(Time_sent >= "2017-09-25" ),aes(x=Time_sent,y=Dust10_err,colour=Box_ID,group=Box_ID)) + geom_line()
multiplot(p1, p2_5, p10, cols=1)
