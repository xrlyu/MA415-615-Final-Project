library(shiny)
library(shinydashboard)
# install.packages("shinycustomloader")
library(shinycustomloader)

source("0_Reference.R")
source("4_Analysis.R")

ui <- dashboardPage(
  dashboardHeader(title = "Supply and Demand in Domestic Film Industry", titleWidth = 450),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Real-time Box Office", tabName = "boxoffice", icon = icon("film", lib = "glyphicon")),
      menuItem("Detailed Reports", icon = icon("pencil", lib = "glyphicon"),
               menuSubItem("General Analysis", tabName = "general_view", icon = icon("table")),
               menuSubItem("Oscars Best Picture", tabName = "oscars", icon = icon('camera-retro')),
               menuSubItem("Oscars Best Picture Prediction", tabName = "prediction", icon = icon('graduation-cap')))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "boxoffice",
              fluidPage(
                titlePanel("Check for Box Office Data"),
                sidebarPanel(
                  conditionalPanel('input.dataset == "Create Your Own"',
                                   dateRangeInput('daterange', label = 'Select Date', start = Sys.Date()-4, end = Sys.Date()-2),
                                   helpText("Box office data are only available until two days before today."),
                                   helpText("Please select a reasonable time range for short loading time."),
                                   checkboxGroupInput("show_vars_h", "Select Variables to Display",
                                                      choiceNames = c("distributor", "daily gross box office", "weekly gross box office", "total gross box office since first on view", "total number of days in theaters"),
                                                      choiceValues = c("distributor", "gross", "weekly_gross", "total_gross", "days"),
                                                      selected = c("distributor", "gross", "weekly_gross", "total_gross", "days"))
                                   ),
                  conditionalPanel('input.dataset == "Most Recent Week"',
                                   helpText("Time range:"),
                                   helpText(as.character(Sys.Date()-8), "to", as.character(Sys.Date()-2)),
                                   checkboxGroupInput("show_vars", "Select Variables to Display",
                                                      choiceNames = c("distributor", "daily gross box office", "weekly gross box office", "total gross box office since first on view", "total number of days has been in theaters"),
                                                      choiceValues = c("distributor", "gross", "weekly_gross", "total_gross", "days"),
                                                      selected = c("distributor", "gross", "weekly_gross", "total_gross", "days")))
                ),
                mainPanel(
                  tabsetPanel(
                    id = 'dataset',
                    tabPanel('Most Recent Week', withLoader(DT::dataTableOutput("mytable1"), type = 'html', loader = "loader5")),
                    tabPanel('Create Your Own', withLoader(DT::dataTableOutput("mytable2"), type = 'html', loader = "loader5"))
                  )
                )
              )
      ),
      
      # Second tab content
      tabItem(tabName = "general_view",
              fluidRow(
                box(title = "Inputs", status = "warning", solidHeader = TRUE,
                  checkboxGroupInput("select_year", label = "Select Year(s)",
                                     choices = seq(from = 2008, to = 2018, by = 1),
                                     selected = 2018, inline = TRUE),
                  checkboxGroupInput("select_month", label = "Select Month(s)",
                                     choiceNames = c("January", "Feburary", "March", "April", "May", "June", "July", "August",'September',"October", "November","December"),
                                     choiceValues = c("Jan", "Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct", "Nov", "Dec"),
                                     selected = c("Jan", "Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct", "Nov", "Dec"),
                                     inline = TRUE),
                  checkboxGroupInput("select_genres", "Select Genre(s)",
                                     choices = levels(final$genres), selected = levels(final$genres),
                                     inline = TRUE)
                ),
                
                box(title = "Box Office vs. Voting Score", status = "primary", solidHeader = TRUE,
                    withLoader(plotOutput("plot4"), type = "html", loader = "loader7"),
                    checkboxInput("line", label = "Add a fitted line", value = FALSE)),
                
                tabBox(title = "Continuous Variables Analysis Based On Genre", width = 12,
                       tabPanel("Box Office", withLoader(plotOutput("plot12"), type = "html", loader = "loader7")),
                       tabPanel("Voting Score", withLoader(plotOutput("plot13"), type = "html", loader = "loader7"))),
                
                tabBox(title = "Number of Movies Released by Month & Year", 
                       tabPanel("By Year", withLoader(plotOutput("plot5"), type = "html", loader = "loader7")),
                       tabPanel("By Month", withLoader(plotOutput("plot6"), type = "html", loader = "loader7")),
                       tabPanel("By Year & Month", withLoader(plotOutput("plot1"), type = "html", loader = "loader7")), width = 12
                ),
                
                tabBox(title = "Number of Movies Released Based on Genre",
                       tabPanel("By Year", withLoader(plotOutput("plot2"), type = "html", loader = "loader7")),
                       tabPanel("By Month", withLoader(plotOutput("plot3"), type = "html", loader = "loader7")), width = 12),
                
                mainPanel(h2("Detatiled Information"),
                          withLoader(DT::dataTableOutput("mytable3"), type = "html", loader = "pacman"))
)
              ),
      # third tab item
      tabItem(tabName = "oscars", 
              fluidPage(titlePanel("Movies Awarded Oscars Best Picture"),
                        box(title = "Box Office vs. Voting Score", status = "primary", solidHeader = TRUE,
                             withLoader(plotOutput("plot7"), type = "html", loader = "loader6"),
                             checkboxInput("line2", label = "Add a fitted line", value = FALSE)),
                       
                       box(title = "Inputs", status = "warning", solidHeader = TRUE,
                           checkboxGroupInput("oscars_select_year", label = "Select Year(s)",
                                              choices = seq(from = 2008, to = 2017, by = 1),
                                              selected = seq(from = 2008, to = 2017, by = 1), inline = TRUE)),
                      
                       tabBox(title = "Detailed Analysis",
                              tabPanel("By Month", withLoader(plotOutput("plot9"), type = "html", loader = "loader6")),
                              tabPanel("By Genre", withLoader(plotOutput("plot8"), type = "html", loader = "loader6"))),
                       
                       tabBox(title = "What makes them distinct?", width = 10,
                              tabPanel("Box office", withLoader(plotOutput("plot10"), type = "html", loader = "loader6"),
                                       checkboxInput("yearly_box", label = "Add yearly average box office", value = FALSE),
                                       checkboxInput("monthly_box", label = "Add monthly average box office", value = FALSE),
                                       checkboxInput("monthly_all_box", label = "Add monthly average box office (of all times)", value = FALSE),
                                       checkboxInput("year_high_box", label = "Add yearly highest gross box office", value = FALSE)),
                              tabPanel("Voting Score", withLoader(plotOutput("plot11"), type = "html", loader = "loader6"),
                                       checkboxInput("yearly", label = "Add yearly average box office", value = FALSE),
                                       checkboxInput("monthly", label = "Add monthly average box office", value = FALSE),
                                       checkboxInput("monthly_all", label = "Add monthly average box office (of all times)", value = FALSE),
                                       checkboxInput("year_high", label = "Add yearly highest gross box office", value = FALSE))),
                       
                       mainPanel(h2("Detatiled Information"),
                                 withLoader(DT::dataTableOutput("mytable4"), type = "html", loader = "pacman"))
              )),
        # last tab item
        tabItem(tabName = "prediction", 
                fluidPage(
                  titlePanel("Can any of these movies can win Oscars Best Picture?"),
                  box(title = "Select Movie(s)", status = "warning", solidHeader = TRUE,
                      checkboxGroupInput("movie", label = "", 
                                         choices = unique(most_recent$title),
                                         selected = "Acrimony", inline = TRUE)),
                  tabBox(title = "Detailed Analysis", width = 12, 
                         tabPanel("Release Month", withLoader(plotOutput("plot14"), type = "html", loader = "loader3")),
                         tabPanel("Movie Genre", withLoader(plotOutput("plot15"), type = "html", loader = "loader3")),
                         tabPanel("Box Office vs. Voting Score", withLoader(plotOutput("plot16"), type = "html", loader = 'loader3'))),
                  
                  mainPanel(h2("Detatiled Information"),
                            withLoader(DT::dataTableOutput("mytable5"), type = "html", loader = "pacman"))
                  
                ))

)
    )
  )


server <- function(input, output) {
  output$mytable1 <- DT::renderDataTable({
    
    # find the system date and use it to find box office data
    realtime <- get_box(seq(Sys.Date()-8, Sys.Date()-2, by = 1))
    movie <- realtime$movie
    DT::datatable(cbind(movie, realtime[, input$show_vars]))
  })
  
  output$mytable2 <- DT::renderDataTable({
    historicaltime <- get_box(seq(input$daterange[1], input$daterange[2], by = 1))
    movie <- historicaltime$movie
    DT::datatable(cbind(movie, historicaltime[, input$show_vars_h]))
  })
  
  output$mytable3 <- DT::renderDataTable({
    # create a new tibble that is responsive to user inputs
    final2 <- final
    final2$year <- as.character(final$year)
    final2$month <- as.character(final$month)
    final2$genres <- as.character(final$genres)
    
    input_y <- str_c(input$select_year, collapse = ", ")
    input_m <- str_c(input$select_month, collapse = ", " )
    input_g <- str_c(input$select_genres, collapse = ", ")
    
    final2 <- final2 %>% filter(str_detect(input_y, year)) %>% 
      filter(str_detect(input_m, month)) %>% 
      filter(str_detect(input_g, genres)) %>% 
      group_by(title, imdb_id, vote_average, year, month, gross) %>%
      summarise(genres = paste(genres, collapse = ", ")) %>%
      ungroup()
    
    final2$month <- fct_inorder(factor(final2$month))
      
    final2 <- final2 %>% dplyr::select(title, imdb_id, vote_average, year, month, genres, gross) %>% 
      arrange(year, month)
  
    final2$genres <- ifelse(final2$genres == "NA", NA, final2$genres)
    
    DT::datatable(final2)
  })
  
  output$plot1 <- renderPlot({
    final2 <- final
    final2$year <- as.character(final$year)
    final2$month <- as.character(final$month)
    final2$genres <- as.character(final$genres)
    
    input_y <- str_c(input$select_year, collapse = ", ")
    input_m <- str_c(input$select_month, collapse = ", " )
    input_g <- str_c(input$select_genres, collapse = ", ")
    
    final2 <- final2 %>% filter(str_detect(input_y, year)) %>% 
      filter(str_detect(input_m, month)) %>% 
      filter(str_detect(input_g, genres))
    
    final2$month <- fct_inorder(final2$month)
    final2$year <- factor(final2$year)
    
    ggplot(data = final2 %>% dplyr::select(-genres) %>% distinct(), aes(x = year)) +
      geom_bar(aes(fill = month), position = "dodge") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      labs(x = "", y = "Number of Movies Released")
  })
  
  output$plot2 <- renderPlot({
    final2 <- final
    final2$year <- as.character(final$year)
    final2$month <- as.character(final$month)
    final2$genres <- as.character(final$genres)
    
    input_y <- str_c(input$select_year, collapse = ", ")
    input_m <- str_c(input$select_month, collapse = ", " )
    input_g <- str_c(input$select_genres, collapse = ", ")
    
    final2 <- final2 %>% filter(str_detect(input_y, year)) %>% 
      filter(str_detect(input_m, month)) %>% 
      filter(str_detect(input_g, genres))
    
    final2$month <- fct_inorder(final2$month)
    final2$year <- factor(final2$year)
    
    ggplot(data = final2, aes(x = fct_infreq(final2$genres))) +
      geom_bar(aes(fill = year)) +
      geom_text(stat='count', aes(label=..count..), size = 3, hjust = -0.5) +
      coord_flip() +
      labs(x = "Genres", y ="Number of Movies Released")
  })
  
  output$plot3 <- renderPlot({
    final2 <- final
    final2$year <- as.character(final$year)
    final2$month <- as.character(final$month)
    final2$genres <- as.character(final$genres)
    
    input_y <- str_c(input$select_year, collapse = ", ")
    input_m <- str_c(input$select_month, collapse = ", " )
    input_g <- str_c(input$select_genres, collapse = ", ")
    
    final2 <- final2 %>% filter(str_detect(input_y, year)) %>% 
      filter(str_detect(input_m, month)) %>% 
      filter(str_detect(input_g, genres))
    
    final2$month <- fct_inorder(final2$month)
    final2$year <- factor(final2$year)
    
    ggplot(data = final2, aes(x = fct_infreq(final2$genres))) +
      geom_bar(aes(fill = month)) +
      geom_text(stat = 'count', aes(label = ..count..), size = 3, hjust= -0.5) +
      coord_flip() +
      labs(x= "Genres", y = "Number of Movies Released")
  })
  
  output$plot12 <- renderPlot({
    final2 <- final
    final2$year <- as.character(final$year)
    final2$month <- as.character(final$month)
    final2$genres <- as.character(final$genres)
    
    input_y <- str_c(input$select_year, collapse = ", ")
    input_m <- str_c(input$select_month, collapse = ", " )
    input_g <- str_c(input$select_genres, collapse = ", ")
    
    final2 <- final2 %>% filter(str_detect(input_y, year)) %>% 
      filter(str_detect(input_m, month)) %>% 
      filter(str_detect(input_g, genres))
    
    final2$month <- fct_inorder(final2$month)
    final2$year <- factor(final2$year) 
    
    ggplot(data = final2) +
      geom_boxplot(aes(x = genres, y = gross, color = genres)) +
      theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.position = "none") +
      labs(x = "Genres", y = "Gross Box Office (in Dollar)")
  })
  
  output$plot13 <- renderPlot({
    final2 <- final
    final2$year <- as.character(final$year)
    final2$month <- as.character(final$month)
    final2$genres <- as.character(final$genres)
    
    input_y <- str_c(input$select_year, collapse = ", ")
    input_m <- str_c(input$select_month, collapse = ", " )
    input_g <- str_c(input$select_genres, collapse = ", ")
    
    final2 <- final2 %>% filter(str_detect(input_y, year)) %>% 
      filter(str_detect(input_m, month)) %>% 
      filter(str_detect(input_g, genres))
    
    final2$month <- fct_inorder(final2$month)
    final2$year <- factor(final2$year)
    
    ggplot(data = final2) +
      geom_boxplot(aes(x = genres, y = vote_average, color = genres)) +
      theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.position = "none") +
      labs(x = "Genres", y = "Voting Score")
  })
  
  output$plot4 <- renderPlot({
    final2 <- final
    final2$year <- as.character(final$year)
    final2$month <- as.character(final$month)
    final2$genres <- as.character(final$genres)
    
    input_y <- str_c(input$select_year, collapse = ", ")
    input_m <- str_c(input$select_month, collapse = ", " )
    input_g <- str_c(input$select_genres, collapse = ", ")
    
    final2 <- final2 %>% filter(str_detect(input_y, year)) %>% 
      filter(str_detect(input_m, month)) %>% 
      filter(str_detect(input_g, genres))
    
    final2$month <- fct_inorder(final2$month)
    final2$year <- factor(final2$year)
    
    jitter <- ggplot(data = final2 %>% dplyr::select(-genres) %>% distinct(), aes(color = Year)) +
      geom_jitter(aes(x = vote_average, y = gross, color = year), alpha = 2/5, size = 3) +
      labs(x = "Voting Score", y = "Gross Box office (in Dollar)") 
    
    # use if else statement to react if users choose fitted line
    
    if (input$line == TRUE) {
      jitter + geom_smooth(aes(x = vote_average, y = gross), method = "lm", color = "salmon", size = 0.5)
    } else{
      jitter
    }
      
  })
  
  output$plot5 <- renderPlot({
    final2 <- final
    final2$year <- as.character(final$year)
    final2$month <- as.character(final$month)
    final2$genres <- as.character(final$genres)
    
    input_y <- str_c(input$select_year, collapse = ", ")
    input_m <- str_c(input$select_month, collapse = ", " )
    input_g <- str_c(input$select_genres, collapse = ", ")
    
    final2 <- final2 %>% filter(str_detect(input_y, year)) %>% 
      filter(str_detect(input_m, month)) %>% 
      filter(str_detect(input_g, genres))
    
    final2$month <- fct_inorder(final2$month)
    final2$year <- factor(final2$year)
    
    ggplot(data = final2 %>% dplyr::select(-genres) %>% distinct(), aes(x = year)) +
      geom_bar(aes(fill = year), position = "dodge") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      labs(x = "", y = "Number of Movies Released")
  })
  
  output$plot6 <- renderPlot({
    final2 <- final
    final2$year <- as.character(final$year)
    final2$month <- as.character(final$month)
    final2$genres <- as.character(final$genres)
    
    input_y <- str_c(input$select_year, collapse = ", ")
    input_m <- str_c(input$select_month, collapse = ", " )
    input_g <- str_c(input$select_genres, collapse = ", ")
    
    final2 <- final2 %>% filter(str_detect(input_y, year)) %>% 
      filter(str_detect(input_m, month)) %>% 
      filter(str_detect(input_g, genres))
    
    final2$month <- fct_inorder(final2$month)
    final2$year <- factor(final2$year)
    
    ggplot(data = final2 %>% dplyr::select(-genres) %>% distinct(), aes(x = month)) +
      geom_bar(aes(fill = month), position = "dodge") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      labs(x = "", y = "Number of Movies Released")
  })
  
  output$plot7 <- renderPlot({
    oscars2 <- oscars
    oscars2$year <- as.character(oscars$year)
    input_y <- str_c(input$oscars_select_year, collapse = ", ")
    oscars2 <- oscars2 %>% filter(str_detect(input_y, year))
    oscars2$year <- factor(oscars2$year)
    
    jitter <- ggplot(data = oscars2 %>% dplyr::select(-genres) %>% distinct(), aes(color = Year)) +
      geom_jitter(aes(x = vote_average, y = gross, color = year), alpha = 3/5, size = 3) +
      geom_text(aes(x = vote_average, y = gross, label = year), color = "salmon", size = 4,
                vjust = -0.7) +
      labs(x = "Voting Score", y = "Gross Box office (in Dollar)") 
    
    if (input$line2 == TRUE) {
      jitter + geom_smooth(aes(x = vote_average, y = gross), method = "lm", color = "salmon", size = 0.5)
    } else{
      jitter
    }
    
  })
  
  output$plot8 <- renderPlot({
    oscars2 <- oscars
    oscars2$year <- as.character(oscars$year)
    input_y <- str_c(input$oscars_select_year, collapse = ", ")
    oscars2 <- oscars2 %>% filter(str_detect(input_y, year))
    oscars2$year <- factor(oscars2$year)
    
    ggplot(data = oscars2) +
      geom_bar(aes(x = genres, fill = year)) +
      labs(x = "Genres", y = "Number of Movies")
  })
  
  output$plot9 <- renderPlot({
    oscars2 <- oscars
    oscars2$year <- as.character(oscars$year)
    input_y <- str_c(input$oscars_select_year, collapse = ", ")
    oscars2 <- oscars2 %>% filter(str_detect(input_y, year))
    oscars2$year <- factor(oscars2$year)
    
    ggplot(data = oscars2 %>% dplyr::select(-genres) %>% distinct()) +
      geom_bar(aes(x = month, fill = year)) + 
      labs (x = "", y = "Number of Movies")
  })
  
  output$mytable4 <- DT::renderDataTable({
    oscars2 <- oscars
    oscars2$year <- as.character(oscars$year)
    input_y <- str_c(input$oscars_select_year, collapse = ", ")
    
    oscars2 <- oscars2 %>% filter(str_detect(input_y, year)) %>% 
      group_by(title, imdb_id, vote_average, year, month, gross) %>%
      summarise(genres = paste(genres, collapse = ", ")) %>%
      ungroup()
    
    oscars2$year <- factor(oscars2$year)
    
    oscars2 <- oscars2 %>% dplyr::select(title, imdb_id,vote_average, year, month, genres, gross) %>% 
      arrange(year, month)
    
    oscars2$genres <- ifelse(oscars2$genres == "NA", NA, oscars2$genres)
    
    DT::datatable(oscars2)
  })
  
  output$plot10 <- renderPlot({
    oscars2 <- oscars
    oscars2$year <- as.character(oscars$year)
    input_y <- str_c(input$oscars_select_year, collapse = ", ")
    oscars2 <- oscars2 %>% filter(str_detect(input_y, year))
    oscars2$year <- factor(oscars2$year)
    
    h1 <- h_1
    h1$year <- as.character(h1$year)
    h1 <- h1 %>% filter(str_detect(input_y, year)) %>% dplyr::select(-genres) %>% distinct()
    h1$year <- factor(h1$year)
    
    box <- ggplot() +
      geom_line(data = oscars2 %>% dplyr::select(-genres) %>% distinct(),
                mapping = aes(x= year, y = gross, colour = "Oscars", group = 1), size = 1.5) +
                theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
                labs(x = "", y = "Gross Box Office (in Dollar)", color = 'Legend')
    
    if (input$yearly_box == TRUE) {
      box1 <- box + geom_line(data = oscars %>% dplyr::select(-genres) %>% distinct(),
                              mapping = aes(x = year, y = avg_y_gross, color = "Yearly Average", group = 2))
      box1
    } else {
      box1 <- box
      box1 }
    
    if (input$monthly_box == TRUE) {
        box2 <- box1 + geom_line(data = oscars %>% dplyr::select(-genres) %>% distinct(),
                              mapping = aes(x = year, y = avg_m_gross, color = "Monthly Average", group = 3))
        box2
      } else{
        box2 <- box1
        box2 }
    
    if (input$monthly_all_box == TRUE) {
        box3 <- box2 + geom_line(data = oscars %>% dplyr::select(-genres) %>% distinct(),
                                   mapping = aes(x = year, y = avg_alm_gross, color = "Monthly Average \nof All Years"), group = 4)
        box3
      } else{
        box3 <- box2
        box3 }
    
    if(input$year_high_box == TRUE) {
      box3 +  geom_line(data = h1, mapping = aes(x = year, y = gross, color = "Yearly Highest Gross", group =5))
    } else{
      box3
    }
  })
  
  output$plot11 <- renderPlot({
    oscars2 <- oscars
    oscars2$year <- as.character(oscars$year)
    input_y <- str_c(input$oscars_select_year, collapse = ", ")
    oscars2 <- oscars2 %>% filter(str_detect(input_y, year))
    oscars2$year <- factor(oscars2$year)
    
    h1 <- h_1
    h1$year <- as.character(h1$year)
    h1 <- h1 %>% filter(str_detect(input_y, year)) %>% dplyr::select(-genres) %>% distinct()
    h1$year <- factor(h1$year)
    
    box <-  ggplot() +
      geom_line(data = oscars2 %>% dplyr::select(-genres) %>% distinct(),
                mapping = aes(x= year, y = vote_average, colour = "Oscars", group = 1), size = 1.5) +
      theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
      labs(x = "", y = "Voting Score", color = 'Legend')
    
    if (input$yearly == TRUE) {
      box1 <- box + geom_line(data = oscars2 %>% dplyr::select(-genres) %>% distinct(),
                              mapping = aes(x = year, y = avg_y_rate, color = "Yearly Average", group = 2))
      box1
    } else {
      box1 <- box
      box1 }
    
    if (input$monthly == TRUE) {
      box2 <- box1 + geom_line(data = oscars2 %>% dplyr::select(-genres) %>% distinct(),
                               mapping = aes(x = year, y = avg_m_rate, color = "Monthly Average", group = 3))
      box2
    } else{
      box2 <- box1
      box2 }
    
    if (input$monthly_all == TRUE) {
      box3 <- box2 + geom_line(data = oscars2 %>% dplyr::select(-genres) %>% distinct(),
                              mapping = aes(x = year, y = avg_alm_rate, color = "Monthly Average \nof All Years",
                                                   group = 4))
      box3
    } else{
      box3 <- box2
      box3 }
    
    if(input$year_high == TRUE) {
      box4 <- box3 +  geom_line(data = h1, mapping = aes(x = year, y = vote_average, color = "Yearly Highest Gross", group = 5))
      box4
      } else{
      box4 <- box3
      box4
    }

  })
  
  output$plot14 <- renderPlot({
    
    most_recent2 <- most_recent
    most_recent2$title <- as.character(most_recent2$title)
    input_m <- str_c(input$movie, collapse = ", ")
    most_recent2 <- most_recent2 %>% filter(str_detect(input_m, title))
    
    ggplot(data = most_recent2 %>% dplyr::select(-genres) %>% distinct()) +
      geom_bar(aes(x = month, fill = title)) +
      theme(axis.text.x = element_text(angle = 60, hjust = 1),
            legend.position = "bottom", legend.text = element_text(size = 7)) +
      labs (x = "", y = "Number of Movies", fill = "Movie Titles")
  })
  
  output$plot15 <- renderPlot({
    
    most_recent2 <- most_recent
    most_recent2$title <- as.character(most_recent2$title)
    input_m <- str_c(input$movie, collapse = ", ")
    most_recent2 <- most_recent2 %>% filter(str_detect(input_m, title))

    ggplot(most_recent2) +
      geom_bar(aes(x = genres, fill = title)) +
      theme(axis.text.x = element_text(angle = 60, hjust = 1),
            legend.position = "bottom", legend.text = element_text(size = 7)) +
      labs(x = "Genres", y = "Number of Movies", fill = "Movie Titles")
  })

  output$plot16 <- renderPlot({
    
    most_recent2 <- most_recent
    most_recent2$title <- as.character(most_recent2$title)
    input_m <- str_c(input$movie, collapse = ", ")
    most_recent2 <- most_recent2 %>% filter(str_detect(input_m, title))
    
    oscars2 <- oscars
    oscars2$title <- as.character(oscars2$title)
    oscars2 <- oscars2 %>% filter(str_detect(input_m, title)) %>% dplyr::select(-genres) %>% distinct()

    ggplot() +
      labs(x = "Voting Score", y = "Gross Box Office (in Dollar)") +
      geom_label_repel(data = most_recent2 %>% dplyr::select(-genres) %>% distinct,
                       aes(x = vote_average, y = gross, label = title), size = 3) +
      geom_smooth(data = oscars %>% dplyr::select(-genres) %>% distinct,
                  aes(x = vote_average, y = gross), method = "lm", color = 'steelblue1', fullrange = TRUE)
  })
  
  output$mytable5 <- DT::renderDataTable({
    
    most_recent2 <- most_recent
    most_recent2$title <- as.character(most_recent2$title)
    input_m <- str_c(input$movie, collapse = ", ")
    most_recent2 <- most_recent2 %>% filter(str_detect(input_m, title)) %>% 
      group_by(title, imdb_id, vote_average, year, month, gross) %>%
      summarise(genres = paste(genres, collapse = ", ")) %>%
      ungroup()
    
    
    most_recent2 <- most_recent2 %>% dplyr::select(title, imdb_id,vote_average, year, month, genres, gross) %>% 
      arrange(year, month)
    
    most_recent2$genres <- ifelse(most_recent2$genres == "NA", NA, most_recent2$genres)
    
    DT::datatable(most_recent2)
  })
}

shinyApp(ui, server)
