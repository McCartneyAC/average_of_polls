library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(reshape2)
library(scales)
library(forcats)
library(tidyverse)
library(shinythemes)
library(fontawesome)
library(DT)
library(ggrepel)
library(ggthemes)
library(ggvoronoi)
rcp<-read_csv("https://raw.githubusercontent.com/McCartneyAC/average_of_polls/master/rcp3.csv")

candid_list<-c("Bennet", "Biden", "Booker", "Bullock", "Buttigieg",
               "Castro", "deBlasio", "Gabbard", "Harris",  "Klobuchar",
                "ORourke","Sanders","Steyer", 
               "Warren", "Williamson",  "Yang")

calc_dist <- function(x, y) {
  distance <- function(x1, y1, x2, y2) {
    c2 <- ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)
    sqrt(c2)
  }
  candidates_list <- 	tribble(
    ~candidate,~party,~leftright,~updown,
    "Bennet","Democratic",8.5,6,
    "Biden","Democratic",5.5,3.5,
    "Booker","Democratic",4,2.5,
    "Bullock","Democratic",5,5,
    "Buttigieg","Democratic",6.5,4.5,
    "Castro","Democratic",6.5,4.5,
    "Deblasio","Democratic",1,1.5,
    "Delaney","Democratic",4,3.5,
    "Gabbard","Democratic",-1.5,-1.5,
    "Gillibrand","Democratic",5,4.5,
    "Gravel","Democratic",-0.5,-1.5,
    "Harris","Democratic",5,4,
    "Hickenlooper","Democratic",4.5,3,
    "Inslee","Democratic",9,2,
    "Klobuchar","Democratic",5,5,
    "Moulton","Democratic",5.5,4,
    "O'Rourke","Democratic",7,5.5,
    "Ryan","Democratic",2.5,3,
    "Sanders","Democratic",-1.5,-1,
    "Sestak","Democratic",5.5,2,
    "Warren","Democratic",0.5,1,
    "Williamson","Democratic",2,-1.5,
    "Yang","Democratic",7,1,
    "Hawkins","Green",-5,-3,
    "Vohra","Libertarian",10,1.5,
    "Corker","Republican",10,8.5,
    "Hogan","Republican",10,8,
    "Kasich","Republican",8,9,
    "Pence","Republican",10,8.5,
    "Trump","Republican",8.5,8.5,
    "Weld","Republican",9.5,4.5
    
  )
  candidates_list %>%
    mutate(dist = distance(x, y, .$leftright, .$updown)) %>%
    arrange(dist) %>%
    select(candidate, party, dist)
}
candidates_list_voronoi <- 	tribble(
  ~candidate,~party,~leftright,~updown,
  "Bennet","Democratic",8.5,6,
  "Biden","Democratic",5.5,3.5,
  "Booker","Democratic",4,2.5,
  "Buttigieg/Castro","Democratic",6.5,4.5,
  "Delaney","Democratic",4,3.5,
  "Gabbard","Democratic",-1.5,-1.5,
  "Harris","Democratic",5,4,
  "Bullock/Klobuchar","Democratic",5,5,
  "Sanders","Democratic",-1.5,-1,
  "Sestak","Democratic",5.5,2,
  "Warren","Democratic",0.5,1,
  "Williamson","Democratic",2,-1.5,
  "Yang","Democratic",7,1,
  "Hawkins","Green",-5,-3,
  "Vohra","Libertarian",10,1.5,
  "Corker/Pence","Republican",10,8.5,
  "Hogan","Republican",10,8,
  "Kasich","Republican",9,8,
  "Trump","Republican",8.5,8.5,
  "Weld","Republican",9.5,4.5
)
outline.df <- data.frame(x = c(-10, 10, 10, -10),
                         y = c(-10, -10, 10, 10)
                         )


dbts<-list(geom_vline(xintercept = as.numeric(as.Date("2019-06-27")),alpha = 0.3,size = 1) ,
           geom_vline(xintercept = as.numeric(as.Date("2019-07-31")),alpha = 0.3,size = 1) ,
           geom_vline(xintercept = as.numeric(as.Date("2019-09-12")),alpha = 0.3,size = 1) ,
           geom_vline(xintercept = as.numeric(as.Date("2019-10-15")),alpha = 0.3,size = 1) ,
           geom_vline(xintercept = as.numeric(as.Date("2019-11-20")),alpha = 0.3,size = 1) ,
           geom_vline(xintercept = as.numeric(as.Date("2019-12-19")),alpha = 0.3,size = 1) )

ui <- fluidPage(theme = shinytheme("flatly"), 
  
  titlePanel("2020 Democratic Nomination Average of Polls"),
  
  sidebarLayout(
    sidebarPanel(
      dateInput("stardate", 
                "Start Date", 
                value = "2018-10-01"),
      checkboxInput("debates", "Show Debate Dates", value = TRUE),
      checkboxInput("jitter", "Show Individual Points", value = FALSE),
      numericInput("zoomed", 
                   "Zoom to Percent", 
                   value = 40),   
      helpText("Note: things get a bit weird if you zoom smaller than your leading candidate's best day, so toggle accordingly."),
      selectizeInput(
        'candids', 'Filter Candidates', choices = candid_list, multiple = TRUE, 
        selected = c("Biden", "Booker", "Sanders", "Warren", "Harris",
                     "Buttigieg", "Yang", "ORourke", "Gabbard", "Castro",
                     "Klobuchar", "Bullock", "Williamson")
      ), 
      helpText("Note: You can't select more than 16 candidates."),
      tags$br(),
      tags$a(href = "https://github.com/McCartneyAC/average_of_polls/", 
             fa("github", height = 25))
      
    ), #SidebarPanel
    
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotOutput("plt")
                 ), #plot
        tabPanel("About", 
                 tags$h3("Average of Polls Data"), tags$br(),
                 tags$h4("Motivation"),
                 tags$p("RealClearPolitics has an", tags$a(href = "https://www.realclearpolitics.com/epolls/2020/president/us/2020_democratic_presidential_nomination-6730.html", "excellent display"), "for average of polls, but it has two main problems: first, their average point is just a moving average of the N most recent polls (without repeating polls from the same firm) for the last approximately 10 days. This makes the pattern of the moving average susceptible to outliers, i.e., it looks wigglier than it ought to. This wouldn't be a huge problem if it weren't for the second issue, namely that RCP doesn't display any error around its moving average."), 
                 tags$p("This graph uses the same polling data RCP uses, but generates a LOESS curve for its average, which won't be as wiggly, and draws an error band around it (but for when such a band would require polling to be below zero). The third main problem of RCP has to do with lower-tier candidates is that you can't zoom in to check on the lowest-tier candidates because their graph maxes out at a certain number. This app should address all three of those concerns."),
                 tags$h4("The Eventual Goal"),
                 tags$p("Use mixed-effects to correct for 'House Effect' on polling error. However, this has so far eluded me as it may require me to switch from LOESS smoothing to a GAM or splines, both of which I'm reluctant to do. Here, the House Effect of each firm is taken from fivethirtyeight's dataset (see their github repo ",  tags$a(href = "https://github.com/fivethirtyeight/data/tree/master/pollster-ratings", "here"), " ). For Harris polling, the house effect is for both HarrisX and for Harvard Harris until I can figure out a discernment between the two. This is not meant to be a perfect statistical adjustment, just some additional variance to control for with the hope of quieting these effects. In this case, + is |+D| and - is |+R|."),
                 tags$br(), 
                 tags$h4("Some Technical Stuff:"),
                 tags$p( "(1) As noted above, the points for each poll are smoothed using local estimation. The key assumed parameter for this is the span, which I have set to 0.27 for the simple reason that higher spans made the polls look like public opinion changed only a monthly basis and a lower span made it seem as if public opinion changed whimsically with each passing news cycle. The assumption that this is false can and should be critiqued, but I have elected not to include a slider for LOESS span for the time being. To me, 0.27 seems just the right amount of wiggly.",tags$br(),tags$br(),"(2) The eager poll-watchers out there may note that Harris' jump from the first debate seems to start prior to that debate happening. This isn't an error--every poll happens over a span of days and I was faced with the decision about whether to code them based on their first day or their last day. Because on the day I began tracking the data I wanted to know about the response to a particular news cycle, I elected to use the day the poll began, in order to ensure that I was getting the first polls that were done only after that news cycle. I'm too set in my dataset now to change that, though again there are reasonable critiques for this decision.",tags$br(),tags$br(), "(3) Datapoints. Some firms don't poll on every candidate (*Cough cough SurveyUSA*) and this creates NAs in the dataset. RCP handles these by coding NAs as zero, which is obviously incorrect, but I can't reasonably be bothered to re-code every datapoint for 160+ polls. Additionally, some surveys give their datapoints in odd ways, e.g. '<1%', which I've chosen to code as 0.5. In general, everything is rounded to the nearest integer, which probably doesn't systematically bias the data but is certainly annoying."),
                 tags$br(), 
                 tags$p(tags$a(href = "https://github.com/McCartneyAC/average_of_polls/", 
                               fa("github", height = 25), tags$br(), "to view the data, code, and contact me."))
                 ), #about
        tabPanel("See the Polls",
                 DTOutput('dt')
        ), #see the polls
        tabPanel("Political Compass", 
                 tags$h2("But for whom should I vote?"),
                 fluidRow(
                   tags$br(),
                   column(4,tags$p("I don't know. But ", tags$a(href = "https://www.politicalcompass.org/test", "politicalcompass.org"), "has a survey you can take and you can see where you line up with the candidates. If you've already taken their survey, here are the 2020 primary candidates mapped for you to see where you fall in relation to the field. Interestingly, Donald Trump and Bernie Sanders have both moved to be more toward the libertarian pole since the 2016 primary season."),
                          numericInput("u_leftright", 
                                       "Your Left / Right Score", 
                                       value = 0, min = -10, max = 10),  
                          numericInput("u_updown", 
                                       "Your Authoritarian / Libertarian Score", 
                                       value = 0, min = -10, max = 10),  
                          helpText("Note: positive scores go up and to the right"),
                          actionButton("distance_button", "Calculate My Closest Candidate")
                          ),
                   
                   column(4,plotOutput("voronoise")
                          )
                 ), #fluidrow
                 fluidRow(
                   tags$br(),
                   tags$br(),
                   tags$br(),
                   tags$br(),
                   tags$br(),
                   tags$br(),
                   tags$br(),
                   tags$br(),
                   tags$br(),
                   tags$br(),
                   DTOutput("user_dist_df")
                 ) #fluidRow
        ) # compass
      ) #tabset panel
    ) #main panel
  ) # sidebar layout
)# fluidpage


server <- function(input, output){

  rcp2 <- rcp  %>% 
    gather(`Biden`, `Booker`, `Sanders`, `Warren`, `Harris`, 
           `Buttigieg`, `Yang`, `ORourke`, `Gabbard`, `Delaney`,
           `Castro`, `Klobuchar`, `Bullock`, `Williamson`, `Bennet`, `deBlasio`, `Steyer`, 
           key = "Candidate", value = "Percent")  
  
  palate<-c( "#115740",  "#b9975b",  "#00b388",  "#cab64b", 
             "#64ccc9",  "#789D4a",  "#789f90",  "#5b6770", 
             "#f0b323",  "#83434e",  "#e56a54",  "#183028", 
             "#00313c",  "#cc5500")
  
  output$dt <- renderDT(rcp,
                        options = list(
                          lengthChange = TRUE,
                          scrollX = TRUE,
                          rownames = FALSE,
                          initComplete = JS(
                            "function(settings, json) {",
                            "$(this.api().table().header()).css({'background-color': '#2c3e50', 'color': '#fff'});",
                            "}"
                          )
                        ) #list
                      ) #renderDT
  
  
  output$plt <- renderPlot({
    p <- rcp2  %>%
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
           subtitle = "Updated Last: October 21, 2019",
           x = "Date") +
      
      scale_y_continuous(limits = c(0, input$zoomed),
                         breaks = seq(0, input$zoomed, by = 5)) +
      scale_x_date(
        limits = c(input$stardate, today()),
        breaks = date_breaks("months"),
        labels = date_format("%b")
      ) +
      NULL
    

    
    if (input$debates == TRUE & input$jitter == TRUE) {
      p + dbts + geom_jitter(alpha = 0.3, stroke = 0)
    } else if (input$debates == TRUE & input$jitter == FALSE) {
      p + dbts
    } else if (input$debates == FALSE & input$jitter == TRUE) {
      p + geom_jitter(alpha = 0.3, stroke = 0)
    } else if (input$debates == FALSE & input$jitter == FALSE) {
      p + NULL
    }

    
  }, height = 550)
  
  

  
  
  # user distance calc module
  
  u_leftrightR<-eventReactive(input$distance_button,{
    input$u_leftright
  })
  u_updownR<-eventReactive(input$distance_button,{
    input$u_updown
  })
  
  user_distances<-reactive({ 
    calc_dist(u_leftrightR(), u_updownR()) %>% 
    as.data.frame() %>% 
    mutate(dist = round(dist, digits = 2))
    })
  
  output$user_dist_df <- renderDT(user_distances(), 
    options = list(
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#2c3e50', 'color': '#fff'});",
        "}"
      )
    )
  )
  
  
  output$voronoise <- renderPlot({
    
    plt <- candidates_list_voronoi %>%
      ggplot(aes(leftright, updown, label = candidate, fill = candidate)) +
      geom_voronoi(outline = outline.df, color = "black") +
      scale_x_continuous(limits = c(-10, 10), breaks = seq(-10, 10, 1)) +
      scale_y_continuous(limits = c(-10, 10), breaks = seq(-10, 10, 1)) +
      guides(fill = FALSE) +
      geom_vline(xintercept = 0) +
      geom_hline(yintercept = 0) +
      geom_label_repel(alpha = 0.8) +
      geom_point() +
      theme_few() +
      labs(
        title = "Areas of 2016 Candidates",
        subtitle = "Values from politicalcompass.org",
        x = "Economic",
        y = "Authoritarian",
        caption = "Color blocks represent the area on the map wherein all points are closest to a given candidate"
      ) +
      scale_fill_tableau(palette = "Tableau 20")
    
    if (input$distance_button==0) {
      plt
    } else {
      plt + geom_point(aes(x = u_leftrightR(), y = u_updownR()),colour = "red",size = 3)
    }
    
  }, height = 600, width = 600) 
  
}






shinyApp(ui = ui, server = server)
