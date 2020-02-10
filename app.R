
source("global.R") # required for average of polls
#source("functions.R") # required for regression
ui <- dashboardPage(skin = "black",
  dashboardHeader(title = "Back of the Envelope"),
  dashboardSidebar(
    sidebarMenu(id = "sidebar",
                tags$br(),
                   menuItem("About", tabName = "grand_about", icon = icon("book")),
                   tags$br(),
                   tags$h3("2020 Election:"),
                   menuItem("National Polls", tabName = "natl_polls", icon = icon("line-chart")), #globe
                   menuItem("Primary/Caucus", tabName = "prim_cauc", icon = icon("area-chart")),
                   menuItem("Political Compass", tabName = "p_compass", icon = icon("compass")),
                   menuItem("Modal Voter", tabName = "modal_voter", icon = icon("address-card"),
                            badgeLabel = "pending", badgeColor = "red"),
                   menuItem("Delegates", tabName = "de_legates", icon = icon("bar-chart"),
                            badgeLabel = "new", badgeColor = "green"),
                   tags$h3("Regression:"),
                   menuItem("Upload & Model", tabName = "reg_about", icon = icon("upload")),
                   menuItem("Data Set", tabName = "reg_data", icon = icon("superscript")),
                   menuItem("Describe", tabName = "reg_desc", icon = icon("list-ol")),
                   menuItem("Correlation", tabName = "reg_cor", icon = icon("th")),
                   menuItem("Plot", tabName = "reg_plot", icon = icon("line-chart"),
                            badgeLabel = "partial", badgeColor = "orange"),
                   menuItem("Summary", tabName = "reg_sum", icon = icon("list")),
                   menuItem("Outliers", tabName = "reg_outlier", icon = icon("sliders"),
                            badgeLabel = "pending", badgeColor = "red")
    )# sidebarmenu
                   ), #sidebar 
  dashboardBody(
    tabItems(
      
      
      #masthead
      tabItem(tabName = "grand_about", 
              box(title = "About", 
                  tags$p("Back of the Envelope is the culmination of two (and maybe more?) project ideas that I have worked on in the year 2019. When I first learned to use Shiny R, I couldn't get the idea out of my head that someone should build a point-and-click style regression tool that utilized all and only those presets that I found helpful and that gave its output in ways that I tended to use when doing homework or preparing presentations and publications. Several extant R packages were outputting results in APA format our otherwise had defaults that were best-in-the-industry for a grad student. After leaving grad school, I put this idea into practice. The original version used wired.js and R's xkcd package to make all regression plots and fonts look hand-drawn. In this iteration, I have restored defaults so you can use it directly in publications. You're welcome...but I do miss the sketchiness."),
                  tags$p("A few months later, I began obsessively following 2020 democratic nomination polls and eventually I got annoyed with all the most popular trackers' issues: no error bars, too wiggly, not displaying enough candidates, can't zoom, etc. Eventually I started plotting the data myself, and it turned into a shiny app."), 
                  tags$p("What the two ideas have in common is a dangerous amount of statistical sophistication: enough to give it a strong semblance of accuracy, but leaving out the super technical details that might be important for research publication. Back of the Envelope regression is good enough for stat homework and basic pubs. Back of the Envelope 2020 Polling is good for getting a sense of the field that's less sensationalized than what you see on other sites. Neither project is polished, hence the overarching title.")),
              box(title= "Credit",
                  tags$p("Back of the Envelope was built with myriad R packages, among them: Shiny & shinydashboard,  DT, psych, SjPlot, MASS, mccrr, and the tidyverse."), 
                  socialButton(url = "https://github.com/McCartneyAC/average_of_polls/", type = "github")
                  )#box
              ), #tabItem
      
    # 2020 Election

      
      
      
    # # National POlls
    tabItem(tabName = "natl_polls",
            box(title = "Controls", width = 3,
                dateInput("stardate", 
                          "Start Date", 
                          value = "2018-10-01"),
                checkboxInput("debates", "Show Debate Dates", value = TRUE),
                checkboxInput("jitter", "Show Individual Points", value = FALSE),
                numericInput("zoomed", 
                             "Zoom to Percent", 
                             value = 40),   
                helpText("Note: things get a bit weird if you zoom smaller than your 
                         leading candidate's best day, so toggle accordingly."),
                selectizeInput(
                  'candids', 'Filter Candidates', choices = candid_list, multiple = TRUE, 
                  selected = c("Biden",  "Booker", "Bloomberg", "Buttigieg", 
                               "Gabbard", "Klobuchar","Sanders", "Steyer",
                               "Warren","Patrick", "Yang")
                ), 
                helpText("Note: You can't select more than 16 candidates.")

                ), #box for controls
            tabBox(
              title = "Poll Averages",
              id = "natl_polls_tab",
              height = "700px",
              width =9,
              tabPanel("Democratic Primary Poll Averages", plotOutput("plt")),
              tabPanel("About", 
                       tags$h3("Average of Polls Data"), tags$br(),
                       tags$p("Last Updated: 11/11/2019"),
                       tags$h4("Motivation"),
                       tags$p("RealClearPolitics has an", tags$a(href = "https://www.realclearpolitics.com/epolls/2020/president/us/2020_democratic_presidential_nomination-6730.html", "excellent display"), "for average of polls, but it has two main problems: first, their average point is just a moving average of the N most recent polls (without repeating polls from the same firm) for the last approximately 10 days. This makes the pattern of the moving average susceptible to outliers, i.e., it looks wigglier than it ought to. This wouldn't be a huge problem if it weren't for the second issue, namely that RCP doesn't display any error around its moving average."), 
                       tags$p("This graph uses the same polling data RCP uses, but generates a LOESS curve for its average, which won't be as wiggly, and draws an error band around it (but for when such a band would require polling to be below zero). The third main problem of RCP has to do with lower-tier candidates is that you can't zoom in to check on the lowest-tier candidates because their graph maxes out at a certain number. This app should address all three of those concerns."),
                       tags$h4("The Eventual Goal"),
                       tags$p("Use mixed-effects to correct for 'House Effect' on polling error. However, this has so far eluded me as it may require me to switch from LOESS smoothing to a GAM or splines, both of which I'm reluctant to do. Here, the House Effect of each firm is taken from fivethirtyeight's dataset (see their github repo ",  tags$a(href = "https://github.com/fivethirtyeight/data/tree/master/pollster-ratings", "here"), " ). For Harris polling, the house effect is for both HarrisX and for Harvard Harris until I can figure out a discernment between the two. This is not meant to be a perfect statistical adjustment, just some additional variance to control for with the hope of quieting these effects. In this case, + is |+D| and - is |+R|."),
                       tags$br(), 
                       tags$h4("Some Technical Stuff:"),
                       tags$p( "(1) As noted above, the points for each poll are smoothed using local estimation. The key assumed parameter for this is the span, which I have set to 0.27 for the simple reason that higher spans made the polls look like public opinion changed only a monthly basis and a lower span made it seem as if public opinion changed whimsically with each passing news cycle. The assumption that this is false can and should be critiqued, but I have elected not to include a slider for LOESS span for the time being. To me, 0.27 seems just the right amount of wiggly. Also, it seems to do the best job of reducing error around Harris' rise and fall after the first debate, so that gives me confidence it's operating on the right time scale.",tags$br(),tags$br(),"(2) The eager poll-watchers out there may note that Harris' jump from the first debate seems to start prior to that debate happening. This isn't an error--every poll happens over a span of days and I was faced with the decision about whether to code them based on their first day or their last day. Because on the day I began tracking the data I wanted to know about the response to a particular news cycle, I elected to use the day the poll began, in order to ensure that I was getting the first polls that were done only after that news cycle. I'm too set in my dataset now to change that, though again there are reasonable critiques for this decision.",tags$br(),tags$br(), "(3) Datapoints. Some firms don't poll on every candidate (*Cough cough SurveyUSA*) and this creates NAs in the dataset. RCP handles these by coding NAs as zero, which is obviously incorrect, but I can't reasonably be bothered to re-code every datapoint for 160+ polls. Additionally, some surveys give their datapoints in odd ways, e.g. '<1%', which I've chosen to code as 0.5. In general, everything else is rounded to the nearest integer, which probably doesn't systematically bias the data but is certainly annoying.")
                       ),
              tabPanel("Democratic Primary Polls",
                       DTOutput('dt'))
            ) # tabBox national polls
), # tabitem national polls






    # # Primary and Caucus
    tabItem(tabName = "prim_cauc",
            box(title = "Controls", width = 3,
                # input$candids_state
                # input$state_state
                # input$zoomed_state
                # input$stardate_state
                selectInput("state_state", "Select your Primary/Caucus State:", 
                            c("Iowa", "Nevada ", "New Hampshire","South Carolina"), 
                            selected = "Iowa"),
                dateInput("stardate_state", 
                          "Start Date", 
                          value = "2019-01-01"),
                numericInput("zoomed_state", 
                             "Zoom to Percent", 
                             value = 75),   
                helpText("Note: things get a bit weird if you zoom smaller than your 
                         leading candidate's best day, so toggle accordingly."),
                selectizeInput(
                  'candids_state', 'Filter Candidates', choices = candid_list, multiple = TRUE, 
                  selected = c("Biden", "Booker", "Sanders", "Warren", "Harris",
                               "Buttigieg", "Yang", "Gabbard")
                ), #selectize
                helpText("Note: You can't select more than 16 candidates.")
            ),             
            tabBox(
              title = "Primary / Caucus Poll Averages",
              id = "state_polls_tab",
              height = "700px",
              width =9,
              tabPanel("State Primary Poll Averages",
                       helpText("Note: Smoothing fails with too few datapoints. Don't overinterpret."), 
                       plotOutput("stateplt")),
              tabPanel("Democratic Primary Polls",
                       DTOutput('dt_state'))
            ) #tabbox

            
            
            ), #tabitem state polls   






    # # Political Compass
    tabItem(tabName = "p_compass",
            tags$h2("But for whom should I vote?"),
            tags$p("Updated 11/1/2019"), 
            fluidRow(
              box(width = 4, tags$p("I don't know. But ", tags$a(href = "https://www.politicalcompass.org/test", "politicalcompass.org"), "has a survey you can take and you can see where you line up with the candidates. If you've already taken their survey, here are the 2020 primary candidates mapped for you to see where you fall in relation to the field. Enter your scores below to see yourself on the diagram and to get a list of candidates."),
                  numericInput("u_leftright", 
                               "Your Left / Right Score", 
                               value = 0, min = -10, max = 10),  
                  numericInput("u_updown", 
                               "Your Authoritarian / Libertarian Score", 
                               value = 0, min = -10, max = 10),  
                  helpText("Note: positive scores go up and to the right"),
                  actionButton("distance_button", "Calculate My Closest Candidate")),
              box(width = 7, height = 620, plotOutput("voronoise"))
            ), #fluidrow
            fluidRow(
              tags$br(),
              tags$br(),
              tags$br(),
              tags$br(),
              tags$br(),
              DTOutput("user_dist_df")
            ) #fluidRow
            ), #tabitem Compass







    # # Modal Voter
    tabItem(tabName = "modal_voter"),






    # # Delegates
    tabItem(tabName = "de_legates", 
            box(title = "The Delegate Hunt",
                tags$p("When candidates start accruing delegates, this box will track them."),
                tags$p("(But will this track superdelegates? Probably not)"),
                plotOutput("delegatesbar")
                ) #box
            ), #tabitem
    
    
    

    
    # Regression
    # # About
    tabItem(tabName = "reg_about",
            box( title = "Upload and Model",
                 fileInput("FileInput", "Input Your Data Set"),
                 helpText("Dataset must be one of: .csv, .sav, .dta, or .xlsx"),
                 tags$h3(tags$b("Build your Model:")), 
                 radioButtons(
                   inputId = "rgrssn", label = "Regression:",
                   choices = c("linear" = "linear",
                               "logistic" = "logistic")
                 ),
                 #shinywidget
                 materialSwitch(inputId = "rbst", label = "Robust Standard Errors"),
                 #wired_toggle(inputId = "rbst", label = "Robust Standard Errors"),
                 tags$p(tags$b("Select your variables for analysis:")),
                 selectInput(inputId = "responsevar",
                             label = "Your DV / Response Variable:", 
                             choices = NULL
                 ),
                 
                 selectizeInput("indevars", "Your IV / Predictor Variable(s):", 
                                choices = NULL, 
                                selected = NULL, 
                                multiple = TRUE,
                                options = NULL),
                 
                 selectInput(
                   inputId = "clstr",
                   label = "Clustering:",
                   choices = c("None",
                               "Fixed Effects",
                               "Cluster Standard Errors",
                               "Multilevel / LME / HLM"),
                   selected = NA
                 ), 
                 selectInput(inputId = "clust",
                             label = "Cluster Varibale: (coming soon)", 
                             choices = NULL
                 )
            ), #upload box
            box( title = "About",
            tags$p("This is intended to be a toy point-and-click-style regression tool to practice R Shiny application development and to enumerate the complexities available in regression analysis. Like R itself, this tool comes with absolutely no warranty. "),
            tags$p("The sketchy nature of the application is intended to deter its use for serious purposes and strengthen the feeling of it being a back-of-the-envelope tool for regression analysis. Use the features to quickly explore options for regression and their effect on your analysis, but resist the urge to p-hack."), 
            tags$h3("Use"),
            tags$p("Use the tool by uploading your own data set in one of the listed formats. Browse your data and examine the variables' descriptive statistics, as well as the table of correlations, then create your model to run."),
            tags$p(tags$b("What this app doesn't do:"), "This app does not allow for any kind of data preparation. Techniques such as interaction terms, exponential terms, or complex extensions such as regression discontinuity need to be done in whatever data-preparation program you choose to use (e.g. excel) before data can be uploaded and used here. For example, to include polynomials, create a new variable in your dataset that is x^2 and re-upload the dataset to run a new regression.  Fixed effects are supported (coming soon!) but if you wish to choose your reference category, you will need to create dummy variables in your dataset and re-upload."),
 
            tags$p(tags$b("last updated: 8/12/2019"))
            ) #box
            ), #tabItem
    # # Data Set
    tabItem(tabName = "reg_data",
            title = "Dataset",
            DT::dataTableOutput("reg_data_table")
            ),
    # # Describe
    tabItem(tabName = "reg_desc", title = "Describe", 
            DT::dataTableOutput("description")),
    # # Correlation
    tabItem(tabName = "reg_cor", title = "Correlations",
            plotOutput("cors")),
    # # Plot
    tabItem(tabName = "reg_plot", title = "Plot", 
            tabsetPanel(type = "tabs",
                        tabPanel("Marginal Effects",
                                 plotOutput("marginal")),
                        tabPanel("One IV", 
                                 plotOutput("bivariate"), 
                                 tags$p("residuals:"), 
                                 plotOutput("bivar_resid")
                        ),
                        tabPanel("Two IVs",
                                 plotOutput("trivariate")),
                        tabPanel("Added Variable Plots", 
                                 tags$h4("Added Variable Plots (forthcoming)"),
                                 selectInput(inputId = "restricted",
                                             label = "Select your Predictor", 
                                             choices = NULL
                                 ), 
                                 plotOutput("avplot")
                        )
            ) #tabset panel
            ), #tabitem 
    # # Summary
    tabItem(tabName = "reg_sum", title = "Output Summary", 
            box(
            tags$p("Be sure to include a null_model if LME is selected and if a cluster is chosen"),
            tags$br(), 
            htmlOutput("tabmodel")
            ) #box
            ),
    # # Outliers
    tabItem(tabName = "reg_outlier", title = "Outlier Analysis",
            tabsetPanel(type = "tabs",
                        tabPanel("Cook's Distance", 
                                 tags$p("Select one independent variable:")
                        ), # Cook's Distance
                        tabPanel("Leverage"),
                        tabPanel("Influence Index" 
                                 # car::influenceIndexPlot(model())
                        )
            ) #tabset panel
            )# tab item. (LAST ONE)
    
    
    
    
    
    ) #tabitems
  ) #Dashboard Body
) #Dashboard Page

server <- function(input, output, session) { 
  # 2020 Election:
  rcp2 <- rcp  %>% 
    gather(`Biden`, `Booker`, `Sanders`, `Warren`, `Harris`, 
           `Buttigieg`, `Yang`, `ORourke`, `Gabbard`, `Delaney`, `Patrick`, `Bloomberg`,
           `Castro`, `Klobuchar`, `Bullock`, `Williamson`, `Bennet`, `deBlasio`, `Steyer`, 
           key = "Candidate", value = "Percent")  
  
  rcp_state2<-rcp_state  %>%  
    gather(`Biden`, `Booker`, `Sanders`, `Warren`, `Harris`, 
           `Buttigieg`, `Yang`, `O'Rourke`, `Gabbard`, 
           `Castro`, `Klobuchar`,   `Bennet`,  `Steyer`, `Sestak`,
           key = "Candidate", value = "Percent")  
  
  delegates_data2<-delegates_data %>% 
    gather(`Biden`,  `Sanders`, `Warren`, 
           `Buttigieg`, `Yang`,  `Gabbard`, `Patrick`, `Bloomberg`,
           `Klobuchar`, `Bennet`,  `Steyer`, 
           key = "Candidate", value = "Delegates_count")   %>% 
    group_by(Candidate) %>%
    summarize(total = sum(Delegates_count))

output$delegatesbar <- renderPlot({
  delegates_data2 %>% 
    ggplot(aes(x = fct_reorder(Candidate, total), y = total, fill = Candidate)) +
    geom_col() + 
    coord_flip() +
    labs(title = "Pledged Delegates by Candidate",
         x = "Candidate",
         y = "Total Pledged Delegates", 
         subtitle = "Dotted line indicates required delegates for victory"
    ) + 
    theme_light() + 
    scale_fill_manual(values = palate) +
    guides(fill = FALSE) + 
    geom_hline(yintercept = 1990, linetype = "dotted")
})

  output$dt <- renderDT(rcp,
                        options = list(
                          lengthChange = TRUE,
                          scrollX = TRUE,
                          rownames = FALSE
                        )) #renderDT
  output$dt_state<-renderDT(
    rcp_state, 
    options = list(
      lengthChange = TRUE,
      scrollX = TRUE,
      rownames = FALSE
    ) #renderDTState
  )
  
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
  

  output$stateplt <- renderPlot({
    rcp_state2 %>% 
      filter(Candidate %in% input$candids_state) %>% 
      filter(state == input$state_state) %>% 
      ggplot(aes(
        x = mdy(.$Date), 
        y = Percent, 
        color = Candidate
      )) + geom_smooth(aes(linetype = Candidate),
                       method = "loess",
                       span = .3) +
      theme_light() +
      scale_color_manual(values = palate) +
      labs(title = paste0(input$state_state, ": Average of Polls with Error"),
           subtitle = "Updated Last: November 13, 2019",
           x = "Date") +
      
      scale_y_continuous(limits = c(0, input$zoomed_state),
                         breaks = seq(0, input$zoomed_state, by = 5)) +
      scale_x_date(
        limits = c(input$stardate_state, today()),
        breaks = date_breaks("months"),
        labels = date_format("%b")
      ) +
      NULL
    
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
  
  output$user_dist_df <- renderDT(user_distances())
  
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
  
  
  
  # REGRESSION
  
  # input the data set
  datasetInput <- reactive({
    infile <- input$FileInput
    if (is.null(infile))
      return(NULL)
    dat<-use(infile$datapath)
    names(dat) <-  gsub(" ", "_", names(dat), fixed = TRUE) 
    return(dat)
    #readr::read_csv(infile$datapath)
  })
  
  # Update elements of UI for features of data input
  observeEvent(datasetInput(), {
    updateSelectInput(session, "responsevar", choices = names(datasetInput()))
  })
  observeEvent(datasetInput(), {
    updateSelectInput(session, "clust", choices = names(datasetInput()))
  })
  observeEvent(datasetInput(), {
    updateSelectInput(session, "indevars", choices = names(datasetInput()))
  })
  observeEvent(datasetInput(), {
    updateSelectInput(session, "restricted", choices = names(datasetInput()))
  })
  
  
  
  
  # display the data
  output$reg_data_table = DT::renderDataTable(datasetInput())
  
  
  # Variables
  output$variable_names <- reactive({
    if (is.null(datasetInput()))
      return(NULL)
    gsub(" ", "_", names(datasetInput()), fixed = TRUE)
  })
  
  # Describe the dataset
  desc <- reactive({
    if (is.null(datasetInput()))
      return(NULL)
    psych::describe(datasetInput(), fast = T) %>%
      add_rownames(var = "Variable") %>%
      mutate(mean = round(mean, 2)) %>%
      mutate(sd = round(sd, 2)) %>%
      mutate(se = round(se, 2)) %>%
      mutate(min = round(min, 2)) %>%
      mutate(max = round(max, 2)) %>%
      mutate(range = round(range, 2))
  })
  
  # description table (psych::describe)
  output$description =  DT::renderDataTable(desc())
  
  # correlation plot
  output$cors <- renderPlot(
    datasetInput() %>%
      select_if(is_extant) %>%
      select_if(is_numeric) %>%
      sjp_corr(
        data = .,
        sort.corr = T,
        decimals = 2,
        na.deletion = "pairwise",
        show.p = FALSE
      ) +
      theme_light()
  )
  
  
  
  
  # regression formula
  feats <- reactive({
    if (length(input$indevars != 1)) {
      paste(input$indevars, collapse = " + ")
    } else {
      paste(input$indevars)
    }
  })
  
  regFormula <- reactive({
    as.formula(paste(input$responsevar, ' ~ ', feats()))
  })
  
  # Model Building
  linear <- reactive ({
    if (input$rbst) {
      MASS::rlm(regFormula(), data = datasetInput())
    } else {
      lm(regFormula(), data = datasetInput())
    }
  })
  
  logistic <- reactive({
    if (input$rbst) {
      robust::glmRob(
        regFormula(),
        data = datasetInput(),
        family = binomial(),
        method = "cubif"
      )
    } else {
      glm(regFormula(), data = datasetInput(), family = "binomial")
    }
  })
  
  model <- reactive({
    if (input$rgrssn == "logistic") {
      logistic()
    } else {
      linear()
    }
  })
  
  # Marginal Effects Plot:
  output$marginal <- renderPlot(
    plot_model(model())+
      theme_light()
  )
  
  output$model <- renderPrint({
    summary(model())
  })
  
  #Display Regression Model Summary:
  output$tabmodel <- renderUI({
    modeltab <- tab_model(model())
    HTML(modeltab$knitr)
  })
  
  
  # can this be exported to a sourced .R file to clean up the code? 
  xrange <- reactive({
    datasetInput() %>%
      select_(input$indevars) %>%
      range()
  })
  yrange <- reactive({
    datasetInput() %>%
      select_(input$responsevar) %>%
      range()
  })
  indvariable <- reactive({
    input$indevars
  })
  indvariable1<-reactive({
    input$indevars[1]
  })
  indvariable2<-reactive({
    input$indevars[2]
  })
  depvariable <- reactive({
    input$responsevar
  })
  model_predicted <- reactive({
    predict(model())   # Save the predicted values
  })
  model_residuals <-  reactive({
    residuals(model()) # Save the residual values
  })
  y_range_residual <- reactive({
    model_residuals() %>% 
      range()
  })
  
  
  #refactor this you fool. 
  output$bivariate <- renderPlot(if (input$rgrssn == "linear") {
    datasetInput() %>%
      ggplot(aes_string(x = indvariable(), y = depvariable())) +
      geom_point() +
      geom_smooth(method = "lm") +
      theme_light()
  } else if (input$rgrssn == "logistic") {
    datasetInput() %>%
      ggplot(aes_string(x = indvariable(), y = depvariable())) +
      geom_point() +
      geom_smooth(method = "glm",
                  method.args = list(family = "binomial")) +
      theme_light()
  } else {
    print(NULL)
  })
  
  output$bivar_resid <-  renderPlot(if (input$rgrssn == "linear") {
    datasetInput() %>%
      ggplot(aes_string(x = indvariable(), y = model_residuals())) +
      geom_point() +
      geom_smooth(method = "lm") +
      theme_light()
  } else if (input$rgrssn == "logistic") {
    print("Error is not normally distributed in logistic regression.")
  } else {
    print(NULL)
  })
  
  output$trivariate <- renderPlot(if (input$rgrssn == "linear") {
    datasetInput() %>%
      ggplot(aes_string(
        x = indvariable1(),
        y = depvariable(),
        color = indvariable2()
      )) +
      geom_point(alpha = 0.6) +
      theme(
        panel.grid.major = element_blank(),
        axis.ticks = element_line(colour = "black"),
        panel.background = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key = element_blank(),
        strip.background = element_blank()
      ) 
  } else if (input$rgrssn == "logistic") {
    datasetInput() %>%
      ggplot(aes_string(
        x = indvariable1(),
        y = depvariable(),
        color = indvariable2()
      )) +
      geom_point(alpha = 0.6) +
      geom_smooth(method = "glm",
                  method.args = list(family = "binomial")) +
      theme_light()
  } else {
    print(NULL)
  })
  
  
  # AV PLOT CONSTRUCTION
  rstrctd<-reactive({
    names(input$indevars)[names(input$indevars) != input$restricted]
  })
  rstrctdfeats<- reactive({
    paste(rstrctd(), collapse = " + ")
  })
  rstrctdformula <-  reactive({
    as.formula(paste(input$responsevar, ' ~ ', rstrctdfeats()))
  })
  # Model Building
  rstrctdlinear <- reactive ({
    if (input$rbst) {
      MASS::rlm(rstrctdformula(), data = datasetInput())
    } else {
      lm(rstrctdformula(), data = datasetInput())
    }
  })
  rstrctdlogistic <- reactive({
    if (input$rbst) {
      robust::glmRob(
        rstrctdformula(),
        data = datasetInput(),
        family = binomial(),
        method = "cubif"
      )
    } else {
      glm(rstrctdformula(), data = datasetInput(), family = "binomial")
    }
  })
  rstrctdmodel <- reactive({
    if (input$rgrssn == "logistic") {
      rstrctdlogistic()
    } else {
      rstrctdlinear()
    }
  })
  
  
  output$avplot <- renderPlot(
    gg_added_var(partial = rstrctdmodel(), extended = model()) 

  )
  
  
  
  
  
  }

shinyApp(ui, server)
