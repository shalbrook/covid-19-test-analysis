library(tidyverse)
library(jsonlite)
library(tidycensus)

covid <- fromJSON("https://covidtracking.com/api/v1/states/daily.json")
covid$date <- as.Date(as.character(covid$date), format="%Y%m%d")
# usable data really starts on 3/17
covid <- covid %>%
  filter(date >= "2020-03-17") %>%
  mutate(posrate = positive/posNeg, posratedaily = positiveIncrease/negativeIncrease)

# plot positive test rate by day for states of your choice
covid %>%
  filter(date >= "2020-05-15" &
           state %in% c("TX","SC","MI","CA","GA","FL","AZ")) %>%
  ggplot(aes(date,posrate,color=state)) + geom_line() #+ geom_point()

# which states have the highest average positive test rate?
covid %>%
  group_by(state) %>%
  summarise(meanposrate = mean(posrate)) %>%
  arrange(desc(meanposrate))

#covidtx <- covid %>% filter(state == "TX")

# what do the trends of all states look like?
## start on apr 1 since the early data is very noisy
covidf <- filter(covid, posrate < 1 & date >= "2020-05-15")
ggplot(covidf, aes(date,posrate)) +
  geom_line(aes(group=state), alpha=0.3) +
  geom_line(data=filter(covidf,state=="TX"), color="red", alpha=0.6)

# use tidycensus to pull state pop and calculate death rate per capita (per million)

pops <- get_acs(geography="state",variables=c(pop="B01001_001"),year=2018) %>%
  mutate(state = state.abb[match(NAME,state.name)])

# could join this to the full data to plot death rate over time

coviddeaths <-
  group_by(covid, state) %>%
  summarise(totaldeaths = max(death, na.rm = TRUE)) %>%
  inner_join(.,pops) %>%
  mutate(deathspermillion = totaldeaths * 1000000 / estimate) %>%
  select(state,totaldeaths,NAME,estimate,deathspermillion) %>%
  arrange(desc(deathspermillion))
#View(coviddeaths)

#coviddeaths %>%
#  ggplot(aes(x = deathspermillion, y = reorder(NAME, deathspermillion))) + 
#  geom_point()
coviddeaths %>%
  ggplot(aes(x = reorder(NAME, deathspermillion), y = deathspermillion)) +
  geom_bar(stat = "identity", aes(fill = estimate)) +
  coord_flip()

# chart daily deaths per million for all states
# with specified states highlighted in a unique color
# calculate it
coviddailydeaths <-
  inner_join(covid,pops) %>%
  mutate(dailydeathspermillion = deathIncrease * 1000000 / estimate) %>%
  filter(dailydeathspermillion >= 0) %>%
  select(date,state,death,NAME,estimate,deathIncrease,dailydeathspermillion) %>%
  arrange(desc(dailydeathspermillion))
#View(coviddailydeaths)

# a couple different plots
# the data as is are kind of noisy
coviddailydeaths %>% 
  ggplot(aes(date,dailydeathspermillion)) +
  geom_line(aes(group=state), alpha=0.2) +
  geom_line(data=filter(coviddailydeaths, state %in% c("TX","NJ","NY","CA","MI","PA")),
            aes(color=state))
# smooth version
coviddailydeaths %>% 
  filter(date >= "2020-05-15") %>%
  ggplot(aes(date,dailydeathspermillion)) +
  #geom_smooth(aes(group=state), color="gray", se = FALSE) +
  geom_smooth(data=filter(coviddailydeaths,
                          state %in% c("TX","AZ","CA","MI","FL","WA","AZ","GA") &
                            date >= "2020-05-15"),
              aes(color=state), se=FALSE)

# curve is on the way down in many states, but it does appear some are still
# going up. are they? if so identify those.

# also, wapo says the texas new-infection rate is not going down. verify/
# visualize that in the data and compare with other states.

################ NEW

# plot a variable by day for states of your choice
covid %>%
  filter(date >= "2020-05-15" & state %in% c("TX","FL","SC","CA","AZ","GA")) %>%
  ggplot(aes(date,hospitalizedCurrently,color=state)) + geom_line() #+ geom_point()

covid %>%
  inner_join(pops) %>%
  filter(date >= "2020-05-15" & state %in% c("TX","FL","SC","CA","AZ","GA")) %>%
  mutate(hospCurrPerMillion = hospitalizedCurrently * 1000000 / estimate) %>%
  ggplot(aes(date,hospCurrPerMillion,color=state)) + geom_line() + geom_smooth(se=FALSE) #+ geom_point()
