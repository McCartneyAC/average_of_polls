library(tidyverse)
library(readxl)
library(dplyr)
library(tibble)
library(ggplot2)
library(lubridate)
library(reshape2)
library(scales)
library(forcats)

dat <- dat %>% 
  gather(`Biden`, `Booker`, `Sanders`, `Warren`, `Harris`, 
         `Buttigieg`, `Yang`, `ORourke`, `Gabbard`, 
         `Castro`, `Klobuchar`, `Bullock`, `Williamson`, `Bennet`, `deBlasio`, `Steyer`,
         key = "Candidate", value = "Percent")

dat2 <-dat  %>% #ggplot doesn't support more than n() linetypes so remove 3 candidates:
  filter(Candidate != "Bennet")  %>% 
  filter(Candidate != "deBlasio")  %>% 
  filter(Candidate != "Steyer")


run_graphic<-function(dat2, points = FALSE, restricted = FALSE){
  palate<-c(Warren = "#115740", Bennet = "#b9975b", Booker = "#00b388", Bullock = "#cab64b", 
            Buttigieg = "#64ccc9", Castro = "#789D4a",Gabbard= "#789f90", ORourke =  "#5b6770", 
            Klobuchar = "#f0b323",Harris = "#83434e", Sanders = "#e56a54", Biden = "#183028", 
            Williamson = "#00313c",Yang = "#cc5500")
  
  
  p <- if(restricted == TRUE){
    dat2  %>% 
      filter(Candidate %in% c("Warren",  "Biden", "Sanders"))  %>% 
      ggplot(aes( 
        x = mdy(.$date), y = Percent, color = Candidate
      )) + 
      geom_smooth(
        aes(linetype = Candidate), 
        method = "loess", 
        # method = "lm"
        span = .27
      ) + 
      #geom_point(alpha = 0.2, stroke = 0) +
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
      geom_vline(xintercept = as.numeric(as.Date("2019-10-15")), alpha = 0.3) +   
      #geom_hline(yintercept = 26.7) +
      scale_y_continuous(limits = c(0,40), 
                         breaks = count_to_by(40,5) )+ 
      scale_x_date(
        #limits= c(as.Date("2019-03-1"), today()),
        breaks = date_breaks("months"),
        labels = date_format("%b")) + 
      NULL
  } else {
    dat2  %>% 
      #filter(Candidate %in% c("Warren",  "Biden"))  %>% 
      ggplot(aes( 
        x = mdy(.$date), y = Percent, color = Candidate
      )) + 
      geom_smooth(
        aes(linetype = Candidate), 
        method = "loess", 
        # method = "lm"
        span = .27
      ) + 
      #geom_point(alpha = 0.2, stroke = 0) +
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
      geom_vline(xintercept = as.numeric(as.Date("2019-10-15")), alpha = 0.3) +   
      #geom_hline(yintercept = 26.7) +
      scale_y_continuous(limits = c(0,40), 
                         breaks = count_to_by(40,5) )+ 
      scale_x_date(
        #limits= c(as.Date("2019-03-1"), today()),
        breaks = date_breaks("months"),
        labels = date_format("%b")) + 
      NULL
  }
  
  if (points == TRUE){
    p + geom_point(alpha = 0.2, stroke = 0) 
  } else {return(p)}
}


# facet:
dat  %>% 
  ggplot(aes( 
    x = mdy(.$date), y = Percent, color = Candidate
  )) + 
  geom_smooth(
    #aes(linetype = Candidate), 
    method = "loess", 
    span = .25
  ) + 
  # geom_point(alpha = 0.2) +
  theme_light() + 
  # scale_colour_wm() + 
  #scale_color_manual(values = palate) +
  labs(
    title = "RCP Average of Polls with error", 
    x = "Date") +
  geom_vline(xintercept = as.numeric(as.Date("2019-06-27")), alpha = 0.3) + 
  geom_vline(xintercept = as.numeric(as.Date("2019-07-31")), alpha = 0.3) + 
  geom_vline(xintercept = as.numeric(as.Date("2019-09-12")), alpha = 0.3) + 
  scale_y_continuous(limits = c(0,40)) + 
  scale_x_date(
    #breaks = date_breaks("months"),
    labels = date_format("%b")) + 
  facet_wrap(~Candidate) + 
  guides(color = FALSE) + 
  NULL