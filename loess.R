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
    df  %>% 
        mutate(date = as.numeric(mdy(date))-17532)  %>% 
        loess(data=., Percent ~ date + house_effect)
}

models <- map(dat_nested$data, candidate_model)

dat_nested <- dat_nested %>% 
  mutate(model = map(data, candidate_model))
  
dat_nested <- dat_nested %>% 
  mutate(
    resid = map2(data, model, add_residuals)
  ) #Why does this throw this error I did it exactly as hadley said to!?
  
