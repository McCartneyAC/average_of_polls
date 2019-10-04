library(tidyverse)
library(readxl)
library(dplyr)
library(purrr)
library(tibble)
library(ggplot2)
library(lubridate)
library(reshape2)
library(scales)
library(forcats)
count_to_by<-function(to, by){
    nums<-1:(to/by)
    nums*by
}
library(modelr)

dat <- dat %>% 
gather(`Biden`, `Booker`, `Sanders`, `Warren`, `Harris`, 
       `Buttigieg`, `Yang`, `ORourke`, `Gabbard`, 
       `Castro`, `Klobuchar`, `Bullock`, `Williamson`, `Bennet`, `deBlasio`, `Steyer`,`Delaney`,
      key = "Candidate", value = "Percent")
      
lo.mod.biden <- dat  %>% 
    filter(Candidate == "Biden")  %>% 
    mutate(date = as.numeric(mdy(date))-17532)  %>% 
    loess(data = ., Percent ~ date + house_effect)


dat_nested <- dat  %>% 
  group_by(Candidate)  %>% 
  nest()

candidate_model <- function(df) {
        loess(data=df, Percent ~ as.numeric(mdy(date)) + house_effect)
}

models <- map(dat_nested$data, candidate_model)

dat_nested <- dat_nested %>% 
  mutate(model = map(data, candidate_model))
  
dat_nested <- dat_nested %>% 
  mutate(
    pred = map2(data, model, add_predictions)
  )

predictions <- dat_nested  %>% 
    unnest(pred)

predictions  %>% 
    filter(Candidate != "Bennet")  %>% 
    filter(Candidate != "deBlasio")  %>% 
    filter(Candidate != "Steyer")  %>% 
    filter(Candidate != "Delaney")  %>% 
    ggplot(aes( 
        x = mdy(.$date), y = pred, color = Candidate
    )) + 
    #geom_smooth(
    #    aes(linetype = Candidate), 
    #    method = "loess", 
       # method = "lm"
    #    span = .27
    #) + 
    #geom_line(aes(linetype = Candidate)) +
    geom_point(alpha = 0.8, stroke = 0) +
    #geom_jitter(alpha = 0.2) +
    theme_light() + 
   scale_color_manual(values = palate) +
    labs(
        title = "Average of Polls with error", 
        subtitle = today(),
        x = "Date") +
    geom_vline(xintercept = as.numeric(as.Date("2019-06-27")), alpha = 0.3) + 
    geom_vline(xintercept = as.numeric(as.Date("2019-07-31")), alpha = 0.3) + 
    geom_vline(xintercept = as.numeric(as.Date("2019-09-12")), alpha = 0.3) + 
    #geom_hline(yintercept = 26.5) +
    scale_y_continuous(limits = c(0,40), 
                      breaks = count_to_by(40,5) )+ 
    scale_x_date(
        #limits= c(as.Date("2019-03-1"), as.Date("2019-09-19")),
        breaks = date_breaks("months"),
      labels = date_format("%b")) + 
    NULL
