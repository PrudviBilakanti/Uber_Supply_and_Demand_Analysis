library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)


# Read uberdf Data

uberdf <- read.csv('uber Request Data.csv', stringsAsFactors = F)

# Change Pickuppoint and Status to factors which will help us during plotting. 

uberdf$Pickup.point <- as.factor(uberdf$Pickup.point)
uberdf$Status <- as.factor(uberdf$Status)

# Data Cleaning / Data Preperation

# Verify any missing values in the columns that we are going to use for further analysis.

sum(is.na(uberdf$Request.id)) # Returns Zero, meaning no missing values
sum(is.na(uberdf$Pickup.point)) # Returns Zero
sum(is.na(uberdf$Request.timestamp)) # Returns Zero


#Check if there are any duplicate rows
sum(duplicated(uberdf))

# Format and bring the Requested time column into same date format using Lubridate package

uberdf$Request.timestamp <-  parse_date_time(uberdf$Request.timestamp,
                                           orders = c("%d/%m/%Y %H:%M",
                                                      "%d/%m/%Y %H:%M:%S",
                                                      "%d-%m-%Y %H:%M:%S",
                                                      "%d-%m-%Y %H:%M"))

# As our focus is on identifying the supply and demand gap, Request time stamp, Pickup point and status
# are key three features that are sufficient for analysis. 


# Extract hour and day from request time stamp for further analysis.

uberdf$Req_day <- day(uberdf$Request.timestamp)
uberdf$Req_Hr <- hour(uberdf$Request.timestamp)



############################### UNIVARIATE ANALYSIS ################################.

# Pickup point Column

# As we are plotting frequency of a categorical variable, a bar chart would be ideal choice.
# Using labs to change the main title, x axis, y axis and legend titles.
# Using geom text to label the count on bars.
# Using theme to align the main title in the centre

ggplot(uberdf,aes(x=Pickup.point,fill=Pickup.point))+
  geom_bar()+
  labs(title='Demand at each pickup point', 
       x='Pickup Point',
       y='No.Of Requests',
       fill="Pickup Point")+
  geom_text(aes(y=(..count..),label=(..count..)),stat='count',size = 3, hjust = 0.5, vjust = 3)+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))

 

table(uberdf$Pickup.point)
# Airport    City 
# 3238    3507

# In Percentages.
prop.table(table(uberdf$Pickup.point))
# Airport      City 
# 0.4800593 0.5199407 

# Univariate analysis of request point gives us an idea That the number of Cab requests were almost same at Airport and the City.


# Status Column

# Using bar chart with labs to plot frequency of requests against Request Status categorical variable .
# using geom_text to label the frequency on each bar and theme to align title in the center. 

ggplot(uberdf,aes(x=Status, fill=Status))+
  geom_bar()+
  geom_text(aes(y=(..count..),label = (..count..)),stat='count',size = 3, hjust = 0.5, vjust = 3)+
  labs(title='Overall Supply and Demand analysis', 
       x='Status',
       y='No.Of Requests',
       fill="Status")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))

table(uberdf$Status)
prop.table(table(uberdf$Status))

# Cancelled No Cars Available    Trip Completed 
# 1264              2650              2831

# Cancelled No Cars Available    Trip Completed 
# 0.1873981         0.3928836         0.4197183 

# Univariate analysis of Status column gives us the info that only 42% of the total demand was supplied.  


# Univariate analysis of Requested time stamp Column.
# Using univariate analysis of day and hour column, Lets understand how the demand varies hourly on every day by plotting a frequency plot.

# day coloumn  

# Using bar chart to plot frequency of requests at every hour.
# using geom_text to label the frequency on each bar and theme to align title in the center. 
# facet_grid to split the graph by day

ggplot(uberdf,aes(x=factor(Req_Hr)))+
  geom_bar(aes(y=..count..),fill='#00bfff')+
  ggtitle('Demand at every hour on each day')+
  xlab("Hour")+
  ylab("No.of Requests")+
    facet_grid(Req_day~.)+
  geom_text(aes(y=(..count..),label=(..count..)),stat='count',hjust = 0.5,vjust=1.5,size=3)+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))

# From the chart it is evident that the trend of demand per hour looks same on day every day. 

# Hour Column

# Using bar chart to plot total number of requests on all days at every hour.
# using geom_text to label the frequency on each bar and theme to align title in the center. 

ggplot(uberdf,aes(x=factor(Req_Hr)))+
  geom_bar(aes(y=..count..),fill='#00bfff')+
  ggtitle('Demand at each hour')+
  xlab("Hour")+
  ylab("No.of Requests")+
  geom_text(aes(y=(..count..),label=(..count..)),stat='count',hjust = 0.5,vjust=1.5,size=3)+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))
  
# From the graph it is clear that the demand is high during 2 time slots. That is 5 AM to 9 AM and 5PM to 9 PM


# Lets segment Hourly column into parts of the day.
# Benefits - Its Cumbersome to perform analysis on each hour. Segementation will help improve data visualization and analysis.


  uberdf$part_of_day[between(uberdf$Req_Hr,0,4)] <- 'early_mng'
  
  uberdf$part_of_day[between(uberdf$Req_Hr,5,9)] <- "mng_peak_hrs"
  
  uberdf$part_of_day[between(uberdf$Req_Hr,10,16)] <- "Office_Hrs"
  
  uberdf$part_of_day[between(uberdf$Req_Hr,17,21)] <- "eve_peak_hrs"
  
  uberdf$part_of_day[between(uberdf$Req_Hr,22,23)] <- "late_night"
  
  # Convert the part of day into ordered factorial.
  
  uberdf$part_of_day <- factor(uberdf$part_of_day, levels = c('early_mng','mng_peak_hrs','Office_Hrs','eve_peak_hrs','late_night'),ordered = T)
  
  # Now lets plot the demand at different parts of the day.
  # Using bar chart to plot total number of requests during different time slots of a day..
  # using geom_text to label the frequency on each bar and theme to align title in the center. 
  
  ggplot(uberdf,aes(x=part_of_day))+
    geom_bar(fill='#00bfff')+
    ggtitle('Demand at different parts of the day')+
    xlab("Part of day")+
    ylab("No.Of Requests")+
    geom_text(aes(y=(..count..),label=(..count..)),stat='count',hjust = 0.5,vjust=1.5,size=3)+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5))
  
  table(uberdf$part_of_day)
 # early_mng mng_peak_hrs   Office_Hrs eve_peak_hrs   late_night 
 #  578         2103         1224         2342          498 
  
# In percentages
  prop.table(table(uberdf$part_of_day))
# early_mng mng_peak_hrs   Office_Hrs eve_peak_hrs   late_night 
# 0.08569311   0.31178651   0.18146775   0.34722016   0.07383247

 # 65% of the overall cab requests are during Morning and evening peak hours.
  
# Conclusion on Univariate analysis:
  
#  1) From the Univariate analysis, we found that there are 2 peak demand timeslots for Uber (5AM to 9 AM and 5PM to 9PM) 
#  2) Overall Only 42 % of the demand was supplied. 58 % of requests were either cancelled or No cars available for rides
#  3) Overall demand at Airport and City looks to be same.
  
  
###################### BI-VARIATE ANALYSIS #################################
  
# Using Bi-Variate analysis let us understand the Problem about why there was only 42% of cab requests were served.
# Problem area is  Request point ? or Request time ? or Both ? Lets  try findout using Bi Variate analysis.
  
# Lets do Bri variate analysis on (part_of_day & Pickup point) and (part_of_day & Status).
  
# Bivariate analysis of part_of_day & Pickup point
# Using bar plot with dodge position to give us an idea on demand at each pickup point. 
# added geom_text to add count labels on the graph and theme to align the title at the center.
  
  
  ggplot(uberdf,aes(x=part_of_day,fill=Pickup.point))+
    geom_bar(position = 'dodge')+
    labs(title='Demand at different parts of the day at Airport and the City', 
         x='Part of day',
         y='No.Of Requests',
         fill="Pickup point")+
    geom_text(aes(y=(..count..),label=(..count..)),stat='count',hjust = 0.5,vjust=1.5,size=3,
              position = position_dodge(width = 1))+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5))
  
# Calculate the demand percentage at the Airport and City during different parts of the day
  
  uberdf %>%
    group_by(uberdf$part_of_day,uberdf$Pickup.point) %>%
    summarise(cnt = n()) %>%
    mutate(rel.freq = paste0(round(100 * cnt/sum(cnt), 0), "%"))
  
  
# 80% of the demand during mng_peak_hrs is at the City and 77% of the demand during eve_peak_hrs is at the Airport.
  
  
# Bivariate analysis of part_of_day & Status
# Using bar plot with dodge position to give us an idea on demand and supply gap during different timeslots of a dat. 
# added geom_text to add count labels on the graph and theme to align the title at the center. 
  
  ggplot(uberdf,aes(x=part_of_day,fill=Status))+
    geom_bar(position = 'dodge')+
    labs(title='Demand and Supply at different parts of the day', 
         x='Part of day',
         y='No.Of Requests',
         fill="Status")+
    geom_text(aes(y=(..count..),label=(..count..)),stat='count',hjust = 0.5,vjust=1.5,size=3,
              position = position_dodge(width = 1))+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5))
  
# Calculate the demand and supply ration during different parts of the day
    
  uberdf %>%
    group_by(uberdf$part_of_day,uberdf$Status) %>%
    summarise(cnt = n()) %>%
    mutate(rel.freq = paste0(round(100 * cnt/sum(cnt), 0), "%"))
  
# From the graph it is evident that the supply to demand gap is very High at 3 points. Ealr_mng, mng_peak_hrs and eve_ peak_hrs
# That is supply is less than 50 % of overall requests during these specified timeslots.
# Lets focus on identifying problem points for the high demanding timeslots (mng_peak_hrs & eve_peak_hrs).
  
# Lets subset the data with part_of_day = mng_peak_hours
  
 mng_peak_hrs_df <- subset(uberdf,uberdf$part_of_day == 'mng_peak_hrs')
    
# Lets see Where the gap is highest at Airport or City during morning peak hours?
 
 # Using bar plot with dodge position to give us an idea on demand and supply gap at pickup points during morning peak hours. 
 # added geom_text to add count labels on the graph and theme to align the title at the center.
 
 ggplot(mng_peak_hrs_df, aes(x=Pickup.point,fill=Status))+
   geom_bar(position = 'dodge')+
   labs(title='Demand and Supply gap at Airport and City during Morning peak hours', 
        x='Pickup Point',
        y='No.Of Requests',
        fill="Status")+
      geom_text(aes(y=(..count..),label=(..count..)),stat='count',hjust = 0.5,vjust=1.5,size=3,
             position = position_dodge(width = 1))+
   theme_bw()+
   theme(plot.title = element_text(hjust = 0.5))
  
# Calculate the demand and supply gap at airport and city during mng_peak_hrs in %
 
 mng_peak_hrs_df %>%
   group_by(mng_peak_hrs_df$Pickup.point,mng_peak_hrs_df$Status) %>%
   summarise(cnt = n()) %>%
   mutate(Percent = paste0(round(100 * cnt/sum(cnt), 0), "%"))
 
 
# From the graph and numbers it is very clear that 49% percent of requests that were made from city to airport 
# Were cancelled by the drivers during morning peak hour time slot. Where as 90% of requests made at airport were fulfilled.
# So there is a visible problem with the requests made from City to Airport in the mn_peak_hrs.
 
# Lets subset the data with another problematic timeslot i.e eve_peak_hrs
 
 eve_peak_hrs_df <- subset(uberdf,uberdf$part_of_day == 'eve_peak_hrs')
 
 
 # Lets see Where the gap is highest at Airport or City ?
 
# Using bar chart with dodge position to get an idea on difference in supply and demand during evening peak hours 
# using geom_text to add label counts on each bar and theme to align title at center
 
 ggplot(eve_peak_hrs_df, aes(x=Pickup.point,fill=Status))+
   geom_bar(position = 'dodge')+
   labs(title='Demand and Supply gap at Aiport and City during Eve peak hours', 
        x='Pickup point',
        y='No.Of Requests',
        fill="Status")+
   geom_text(aes(y=(..count..),label=(..count..)),stat='count',hjust = 0.5,vjust=1.5,size=3,
             position = position_dodge(width = 1))+
   theme_bw()+
   theme(plot.title = element_text(hjust = 0.5))
 
 
 # Calculate the demand and supply gap at airport and city during eve_peak_hrs in %
 
 eve_peak_hrs_df %>%
   group_by(eve_peak_hrs_df$Pickup.point,eve_peak_hrs_df$Status) %>%
   summarise(cnt = n()) %>%
   mutate(Percent = paste0(round(100 * cnt/sum(cnt), 0), "%"))
 
 
 # From the graph and numbers it is evident that there 73% of the requests that were requested from Airport to City
 # were not processed as there were " NO CARS AVAILABLE" during the peiod.
 
 # Bi Variate analysis conclusion 
 
  # There are two pressing problems for Uber
 
 # 1 - 49% of the Requests made during morning peak hours from the City were cancelled by Drivers leading to a
 #     high gap in supply and demand
 
 # 2 - 73% of the Requests made during evening peak hours from the Airport were not fulfilled due to the shortage
 #     of Cabs.
 
 
 
 
  

  