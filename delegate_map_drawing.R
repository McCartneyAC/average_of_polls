library(mccrr)
library(tidyverse)
library(extrafont)
library(drlib)
library(geofacet)
library(ggthemes)
delegates_data<-paste_data()
delegates_data
delegates_data2<-delegates_data %>% 
  gather(`Bennet`,`Biden`,`Bloomberg`,  `Buttigieg`,
         `Gabbard`, `Klobuchar`,`Patrick`,`Sanders`, 
         `Steyer`, `Warren`,`Yang`,    
         key = "Candidate", value = "Delegates_count")   %>% 
  group_by(Candidate) %>%
  summarize(total = sum(Delegates_count, na.rm = TRUE))


delegates_data2
palate<-c( "#115740",  "#cc5500",  "#00313c", "#e56a54",
           "#83434e",  "#00b388",  "#f0b323", "#5b6770", 
           "#64ccc9",  "#789D4a",  "#789f90",  "#cab64b",
           "#183028",  "#b9975b")
theme_typewriter <- function() {
  ggplot2::theme_light()+
    ggplot2::theme(text = ggplot2::element_text(family = "Special Elite")) 
}

delegates_data2 %>% 
  ggplot(aes(x = fct_reorder(Candidate, total), y = total, fill = Candidate)) +
  geom_col() + 
  coord_flip() +
  labs(title = "Pledged Delegates by Candidate",
       x = "Candidate",
       y = "Total Pledged Delegates", 
       subtitle = mccrr::wrapper("Dotted line indicates required delegates for victory")
       ) +
  theme_typewriter()+
  scale_fill_manual(values = palate) +
  guides(fill = FALSE) + 
  geom_hline(yintercept = 1990, linetype = "dotted")

#
#



install.packages(c("statebins", "geofacet"))

delegates_data3 <- delegates_data %>% 
  select(-Yang, -Steyer, -Gabbard, -Bennet, -Patrick) %>% 
  gather(`Biden`,`Bloomberg`,  `Buttigieg`,
         `Klobuchar`,`Sanders`, 
          `Warren`,   
         key = "Candidate", value = "Delegates_count")
delegates_data3

delegates_data3 %>% 
  ggplot(aes(x = "", 
             y = Delegates_count, 
             fill = Candidate,)) +
  geom_col() + 
  labs(title = "Pledged Delegates by Candidate",
     y = "",
     x = ""
      # subtitle = mccrr::wrapper("Dotted line indicates required delegates for victory")
  ) +
  theme_typewriter()+
  scale_fill_manual(values = palate) +
  facet_geo(~State,scales = "free", grid = "us_state_grid2") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(), 
        panel.grid.major = element_blank()
        
        ) +
  NULL

delegates_data3 %>% 
  ggplot(aes(x = reorder_within(Candidate, by = Delegates_count, within = State),
              # fct_reorder(Candidate, Delegates_count), 
             y = Delegates_count, 
             fill = Candidate,
             label = Candidate)) +
  geom_col() + 
  geom_text(color = "#FFFFFF", size = 3, hjust = 0, y = 0, # aes(y = (0 + nchar(Candidate)/2))
            family = "Special Elite"
  ) +
  coord_flip() +
  labs(title = "Pledged Delegates by Candidate",
       y = "",
       x = ""
       # subtitle = mccrr::wrapper("Dotted line indicates required delegates for victory")
  ) +
  theme_typewriter()+
  scale_x_reordered() +
  scale_fill_manual(values = palate) +
  facet_geo(~State, scales = "free") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(), 
        panel.grid.major = element_blank()
        
  ) +
  NULL
