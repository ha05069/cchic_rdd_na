#Main Analysis & Base table data for NA for RDD Project

library(cleanEHR)
library(tidyverse)
library(yaml)
library(DT)
library(data.table)
library(dplyr)
library(dygraphs)
library(lubridate)

load("C:/Users/mwils/Desktop/ACF 2017/Critical Care Datathon 2017/Data Set/anon_public_da1000.RData")

lookup.items('noradrenaline')
lookup.items('blood pressure')
lookup.items('mean arterial pressure')
lookup.items('urine output')

na <- yaml.load_file("C:/Users/mwils/Desktop/ACF 2018/CCHIC/Regression Discontinuity Design Project/Noradrenaline")

dt <- create_cctable(anon_ccd, na, freq = 1)
dt <- dt$torigin
dt <- dt[, NIHR_HIC_ICU_0110.meta :=NULL]
dt <- dt[, site :=NULL]
str(dt)


setnames(dt, "NIHR_HIC_ICU_0470", "na")
setnames(dt, "NIHR_HIC_ICU_0110", "map")
setnames(dt, "NIHR_HIC_ICU_0162", "uop")
dt <- setcolorder(dt, c("episode_id", "time", "map", "na", "uop"))
str(dt)

#Filter out implausible values, replace NAs in na with 0
setDF(dt)

dt <- filter(dt, map >0)
dt <- filter(dt, na < 2)
dt <- dt %>% 
  replace_na(list(na = 0))
head(dt)

#Calculate variable deltas

dt <- dt %>% 
  group_by(episode_id) %>%
  mutate(delta_map = c(NA, diff(map)))
dt <- dt %>%
  group_by(episode_id) %>%
  mutate(delta_na = c(NA, diff(na)))
dt <- dt %>%
  group_by(episode_id) %>%
  mutate(delta_uop = c(NA, diff(uop)))
setcolorder(dt, c("episode_id", "time", "map", "delta_map", "na", "delta_na", "uop", "delta_uop"))
head(dt)
str(dt)

#Calculate the the frequency of na changes (increase, decrease, continue) and calculate preceding map for each change

dt <- mutate(dt,
              na_esc = if_else(delta_na > 0, 1, 0))
dt <- mutate(dt,
              na_decr = if_else(delta_na < 0, 1, 0))
dt <- mutate(dt,
              na_cont = if_else(delta_na == 0, 1, 0))
dt <- mutate(dt,
              starting_map = map - delta_map)
head(dt)

#For na increases:

dt_incr <- select(dt, episode_id, starting_map, na_esc)
dt_incr <- filter(dt_incr, na_esc == 1)
dt_incr <- dt_incr %>% group_by(starting_map, na_esc) %>% mutate(count = n())
dt_incr <- ungroup(dt_incr)
dt_incr <- select(dt_incr, starting_map, count)

ggplot(dt_incr, aes(x = starting_map, title = "MAP Preceding NA Escalation"), 
       scale_x_continuous(50, 100)) + geom_density()

#For na decreases:

dt_decr <- select(dt, episode_id, starting_map, na_decr)
dt_decr <- filter(dt_decr, na_decr == "1")
dt_decr <- dt_decr %>% group_by(starting_map, na_decr) %>% mutate(count = n())
dt_decr <- ungroup(dt_decr)
dt_decr <- select(dt_decr, starting_map, count)
ggplot(dt_decr, aes(x=starting_map, y=count)) + geom_point() + geom_smooth()

#For na continues:

dt_cont <- select(dt, episode_id, starting_map, na_cont)
dt_cont <- filter(dt_cont, na_cont == "1")
dt_cont <- dt_cont %>% group_by(starting_map, na_cont) %>% mutate(count = n())
dt_cont <- ungroup(dt_cont)
dt_cont <- select(dt_cont, starting_map, count)
ggplot(dt_cont, aes(x=starting_map, y=count)) + geom_jitter() + geom_smooth()

#Next steps:
#Identify all the time points where na is switched on, together with the preceding map value
#This gives the range of maps over which na is started, identifying a threshold value for the RDD

dt <- dt %>% mutate(na_off = na == 0) %>%
  mutate(na_on = na>0) %>%
  mutate(na_next = c(na_on[-1], F))

dt_switch_on <- filter(dt, na_off == T & na_next == T)


#This gives us the number of times that na goes from 0 to positive in the next hour, together with the map at na == 0 and in the preceding hour.
#Next calculate the spread of starting map values for these patients who went on to receive na.

#Order by starting map value:

dt_switch_on <- dt_switch_on[order(dt_switch_on$starting_map),]
head(dt_switch_on)
#Subset the starting map into a new variable to produce a table of counts

count <- as.data.frame(table(dt_switch_on$starting_map))
count
count_plot <- ggplot(count, aes(x= Var1, y = Freq)) + geom_col() + ggtitle("Frequency of Mean Arterial Pressures Preceding Introduction of Noradrenaline") + labs(y = "Frequency", x = "Mean Arterial Pressure (mmHg)")
count_plot

#Summary:

#Currently I have produced data which describes the decision frequency for changes in noradrenaline dose as well as the range of maps recorded in the hour preceding the initiation of noradrenaline.
#I have also calculated the number of vasopressor free days for each patient during their admission.

#Next Steps:

#Merge the vfd data, create a new df which has two columns - x = starting map, y = vfd
#Input this into a RDD model so that X = map preceding introduction of na and Y = vasopressor free days

dt3 <- left_join(dt_switch_on, dt_vfd, by=c("episode_id"))

dt_rdd <- dt3[, c(12, 17)]
dt_rdd <- dt_rdd[-52,] #Because row 52 contained NA for starting map which may have been causing errors
dt_rdd

#=========>>> RDD_NA_AppliedRDD

#Next steps:
#The initial plots of starting_map and vfd don't seem to show any clear discontinuity (visually) and throw errors with McCrary Test (? why).
#?problem with initial presention of the data or premise? Is the problem that 50 odd data points is not enough to show the discontinuity?

#It's probably due to the fact that there is no relationship between 1 preceding blood pressure and starting na, looking at the frequency plot
#for na switch ons (count_plot) the most common map to then start na from was 67, then 72, then 69.  

#What about binning the blood pressures into ranges of 5mmHg again?
##Look up how to do this - ?group_by + bin....??


##Ideas:

#Think about mapping the changes in map prior to initial switch on of na.  I.e. in the period of x hours preceding the initiation of na the average blood pressure 

