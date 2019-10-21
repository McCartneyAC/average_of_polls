library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(reshape2)
library(scales)
library(forcats)
library(tidyverse)

rcp<-read_csv("https://raw.githubusercontent.com/McCartneyAC/average_of_polls/master/rcp3.csv")

candid_list<-c("Bennet", "Biden", "Booker", "Bullock", "Buttigieg",
               "Castro", "deBlasio", "Gabbard", "Harris",  "Klobuchar",
                "ORourke","Sanders","Steyer", 
               "Warren", "Williamson",  "Yang")


ui <- fluidPage(
  
  titlePanel("2020 Democratic Nomination Average of Polls"),
  
  sidebarLayout(
    sidebarPanel(
      dateInput("stardate", 
                "Start Date", 
                value = "2018-10-01"),
      checkboxInput("debates", "Show Debate Dates", value = TRUE),
      checkboxInput("jitter", "Show Individual Points", value = FALSE),
      selectizeInput(
        'candids', 'Filter Candidates', choices = candid_list, multiple = TRUE, 
        selected = c("Biden", "Booker", "Sanders", "Warren", "Harris",
                     "Buttigieg", "Yang", "ORourke", "Gabbard", "Castro",
                     "Klobuchar", "Bullock", "Williamson")
      ), 
      helpText("Note: You can't select more than 16 candidates.")
      
    ), #SidebarPanel
    
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotOutput("plt")
                 ), #plot
        tabPanel("About", 
                 tags$h3("Average of Polls data (but displayed with error)"), 
                 tags$p("RealClearPolitics has an", 
                        tags$a(href = "https://www.realclearpolitics.com/epolls/2020/president/us/2020_democratic_presidential_nomination-6730.html", "excellent display"), 
                        "for average of polls, but it has two main problems: first, their average point is just a moving average of the N most recent polls (without repeating polls from the same firm) for the last approximately 10 days. This makes the pattern of the moving average susceptible to outliers, i.e., it looks wigglier than it ought to. This wouldn't be a huge problem if it weren't for the second issue, namely that RCP doesn't display any error around its moving average. This graph uses the same polling data RCP uses, but generates a LOESS curve for its average, which won't be as wiggly, and draws an error band around it (but for when such a band would require polling to be below zero). The third main problem is that you can't zoom in to check on the lowest-tier candidates because their graph maxes out at a certain number. This should address all three of those concerns."),
                 tags$p("The Eventual goal: used mixed-effects to correct for 'House Effect' on polling error. However, this has so far eluded me as it may require me to switch from LOESS smoothing to a GAM or splines, both of which I'm reluctant to do. Here, the House Effect of each firm is taken from fivethirtyeight's dataset (see their github repo ", 
                        tags$a(href = "https://github.com/fivethirtyeight/data/tree/master/pollster-ratings", "here"), 
                        " ). For Harris polling, the house effect is for both HarrisX and for Harvard Harris until I can figure out a discernment between the two. This is not meant to be a perfect statistical adjustment, just some additional variance to control for with the hope of quieting these effects. In this case, + is |+D| and - is |+R|."),
                 tags$p(tags$a(href = "https://github.com/McCartneyAC/average_of_polls/blob/master/display.R", 
                               "See the GitHub repo for this project to view the data, code, and other things"))
                 ) #about
        
      ) #tabset panel
    ) #main panel
  ) # sidebar layout
)# fluidpage


server <- function(input, output){

  rcp <- rcp  %>% 
    gather(`Biden`, `Booker`, `Sanders`, `Warren`, `Harris`, 
           `Buttigieg`, `Yang`, `ORourke`, `Gabbard`, 
           `Castro`, `Klobuchar`, `Bullock`, `Williamson`, `Bennet`, `deBlasio`, `Steyer`,
           key = "Candidate", value = "Percent")  %>% 
    filter(Candidate != "Bennet")  %>% 
    filter(Candidate != "deBlasio")  %>% 
    filter(Candidate != "Steyer")
  
  palate<-c( "#115740",  "#b9975b",  "#00b388",  "#cab64b", 
              "#64ccc9",  "#789D4a",  "#789f90",    "#5b6770", 
              "#f0b323",  "#83434e",  "#e56a54",   "#183028", 
              "#00313c",  "#cc5500")
  
  
  
  output$plt <- renderPlot({
    p <- rcp  %>%
      filter(Candidate %in% input$candids)  %>%
      ggplot(aes(
        x = mdy(.$date),
        y = Percent,
        color = Candidate
      )) +
      geom_smooth(aes(linetype = Candidate),
                  method = "loess",
                  span = .27) +
      theme_light() +
      scale_color_manual(values = palate) +
      labs(title = "Average of Polls with error",
           subtitle = "Updated Last: October 20, 2019",
           x = "Date") +
      
      scale_y_continuous(limits = c(0, 40),
                         breaks = seq(0, 40, by = 5)) +
      scale_x_date(
        limits = c(input$stardate, today()),
        breaks = date_breaks("months"),
        labels = date_format("%b")
      ) +
      NULL
    
    if (input$debates == TRUE) {
      if (input$jitter == TRUE) {
        p +
          geom_vline(xintercept = as.numeric(as.Date("2019-06-27")),alpha = 0.3,size = 1) +
          geom_vline(xintercept = as.numeric(as.Date("2019-07-31")),alpha = 0.3,size = 1) +
          geom_vline(xintercept = as.numeric(as.Date("2019-09-12")),alpha = 0.3,size = 1) +
          geom_vline(xintercept = as.numeric(as.Date("2019-10-15")),alpha = 0.3,size = 1) +
          geom_jitter(alpha = 0.2, stroke = 0)
      } else if (input$jitter == FALSE) {
        # if jitter is false
        p +
          geom_vline(xintercept = as.numeric(as.Date("2019-06-27")),alpha = 0.3,size = 1) +
          geom_vline(xintercept = as.numeric(as.Date("2019-07-31")),alpha = 0.3,size = 1) +
          geom_vline(xintercept = as.numeric(as.Date("2019-09-12")),alpha = 0.3,size = 1) +
          geom_vline(xintercept = as.numeric(as.Date("2019-10-15")),alpha = 0.3,size = 1)
      } else if (input$debates == FALSE) {
        # if debates is false:
        if (input$jitter == TRUE) {
          p +
            geom_jitter(alpha = 0.2, stroke = 0)
        } else {
          p
        }
      }
    }
    
  }, height = 550)
  
  
  
  
  
}



shinyApp(ui = ui, server = server)
