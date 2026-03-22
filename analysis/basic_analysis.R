library(here)
library(tidyverse)
library(lme4)
library(lmerTest)

processed_data_directory <- here("..","data","processed_data")
file_name <- "verbal_complexity"

#load data
processed_data <- read_csv(here(processed_data_directory,paste0(file_name,"-processed-data.csv")))

#quick look
ggplot(filter(processed_data,word_count<75),aes(stimulus_complexity,word_count))+
  geom_point()+
  geom_smooth(method="loess")

#compute average by item
item_avg <- processed_data %>%
  group_by(figure,stimulus_complexity) %>%
  summarize(
    N=n(),
    avg=mean(word_count)
  )

ggplot(processed_data,aes(stimulus_complexity,word_count))+
  geom_point(data=item_avg,aes(y=avg))+
  geom_smooth(method="loess")

ggplot(filter(item_avg,N>2),aes(stimulus_complexity,avg))+
  geom_point()+
  geom_smooth(method="loess")

processed_data <- processed_data %>%
  ungroup() %>%
  mutate(
    surprisal_binned = floor(stimulus_complexity/20)*20
  ) %>%
  relocate(surprisal_binned,.after = stimulus_complexity)

item_avg_binned <- processed_data %>%
  group_by(surprisal_binned) %>%
  summarize(
    N=n(),
    avg=mean(word_count),
    sd=sd(word_count),
    sem = sd / sqrt(N)
  )

ggplot(item_avg_binned,aes(surprisal_binned,avg))+
  geom_point(size=1.5)+
  geom_errorbar(aes(ymin=avg-sem,ymax=avg+sem),width=0)+
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1)+
  theme_bw(base_size=16)+
  xlab("Complexity (surprisal, binned)")+
  ylab("Average Word Count")


#quadratic model
processed_data <- processed_data %>%
  mutate(
    surprisal_z = scale(stimulus_complexity),
    surprisal_z_sq = surprisal_z^2
  )

#fit quadratic mixed-effects model
m <- lmer(word_count ~ surprisal_z+surprisal_z_sq+ (1+surprisal_z+surprisal_z_sq||participant_id)+(1|figure),data=processed_data)
summary(m) #boundary fit, removing covariance between random slopes helps

