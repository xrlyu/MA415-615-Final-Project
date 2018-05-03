library(shiny)
library(shinydashboard)
# install.packages("shinycustomloader")
library(shinycustomloader)

source("0_Reference.R")
source("4_Analysis.R")

ui <- dashboardPage(
  dashboardHeader(title = "Movie Data"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Real-time Box Office", tabName = "boxoffice", icon = icon("film", lib = "glyphicon")),
      menuItem("Detailed Reports", icon = icon("pencil", lib = "glyphicon"),
               menuSubItem("General Analysis", tabName = "general_view", icon = icon("table")))
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
                                   helpText("Box office data only available until two days prior to today."),
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
                    checkboxInput("line", label = "Add a smoonth line", value = FALSE)),
                
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
              ))
    )
  )


server <- function(input, output) {
  output$mytable1 <- DT::renderDataTable({
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
      group_by(title, imdb_id, distributor, vote_average, year, month, gross) %>%
      summarise(genres = paste(genres, collapse = ", ")) %>%
      ungroup()
    
    final2$month <- fct_inorder(factor(final2$month))
      
    final2 <- final2 %>% dplyr::select(title, imdb_id, distributor, vote_average, year, month, genres, gross) %>% 
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
      labs(x = "Genres", y ="Number of Movies")
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
      labs(x= "Genres", y = "Number of Movies")
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
      labs(x = "Voting Score", y = "Gross Box office") 
    
    if (input$line == TRUE) {
      jitter + geom_smooth(aes(x = jitter(vote_average), y = jitter(gross)), color = "salmon", size = 0.5)
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
}

shinyApp(ui, server)
