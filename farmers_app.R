#App for farmers Insights.
library("shiny")
library("ggplot2")
library("dplyr")
library("shinythemes")
#Load packages
#req_libs <- c("shiny", "shinythemes", "dplyr", "ggplot2")
# pack_list <- installed.packages()[1:nrow(installed.packages())]
# to_be_installed <- req_libs[!(req_libs %in% pack_list)]
# if(length(to_be_installed)>0) lapply(to_be_installed, FUN = install.packages)
# lapply(req_libs, FUN = library, character.only=TRUE)

user_interface <-
  fluidPage(
    theme = shinytheme("superhero"),
    titlePanel("Cimatic Data Visualizations and Insights For Farmers"),
    sidebarLayout(
      sidebarPanel = sidebarPanel(
        fileInput(
          inputId = "dt_file",
          label = "Import Climatic Data",
          accept = c(".csv", ".dta", ".txt", ".xlsx"),
          placeholder = "data not loaded",
          multiple = FALSE
        ),
        uiOutput(outputId = "station"),
        uiOutput(outputId = "cli_element"),
        uiOutput(outputId = "date_col"),
        uiOutput(outputId = "doy"),
        uiOutput(outputId = "year_col")
      ),
      mainPanel = mainPanel(tabsetPanel(
        tabPanel(title = "Inventory Plot", plotOutput("inv_plot")),
        tabPanel(title = "Boxplot Summaries", plotOutput("box_plot"), width = "100%", height = "100%"),
        tabPanel(title = "Element Summaries", dataTableOutput("sum_vales")),
        tabPanel(title = "View Data", dataTableOutput("data_sec"))
      ))
    )
  )

r_manipulations <- function(input, output) {
  #Set desired file limit
  if (Sys.getenv('SHINY_PORT') == "")
    options(shiny.maxRequestSize = 10000 * 1024 ^ 2)
  #Loading Data
  cli_data <- reactive({
    temp_file <- input$dt_file
    if (is.null(temp_file))
      return("No Data in The Memory")
    if (stringr::str_detect(temp_file, ".csv")) {
      fild <- read.csv(temp_file$datapath, na.strings = c("missing", "", " "))
    } else if (stringr::str_detect(temp_file, ".dta")) {
      fild <- haven::read_dta(temp_file$datapath, na = c("missing", "", " "))
    } else if (stringr::str_detect(temp_file, ".xlsx")) {
      fild <- openxlsx::read.xlsx(temp_file$datapath, na = c("missing", "", " "))
    }
    
  })
  
  output$date_col <- renderUI({
    nam <- names(cli_data())[which(names(cli_data()) == "date_col")]
    selectInput("date_input", choices = nam, label = "Date")
  })
  output$doy <- renderUI({
    nam <- names(cli_data())[which(names(cli_data()) == "doy")]
    selectInput("doy_input", choices = nam, label = "Day of Year")
  })
  output$station <- renderUI({
    nam <- names(cli_data())[which(names(cli_data()) == "stations")]
    selectInput("station_input", choices = nam, label = "Stations")
  })
  output$year_col <- renderUI({
    nam <- names(cli_data())[which(names(cli_data()) == "year")]
    selectInput("year_input", choices = nam, label = "Year")
  })
  output$cli_element <- renderUI({
    nam <-
      names(cli_data())[!stringr::str_detect(names(cli_data()), pattern = "year|stations|doy|date_col")]
    selectInput("cli_input", choices = nam, label = "Climate Element")
  })
  output$data_sec <- renderDataTable({
    data.frame(cli_data())
  })
  
  summaries <- reactive({
    library(dplyr)
    if (input$station_input == "stations") {
      summ_data <- cli_data()
      summ_data %>% group_by(year, stations) %>%
        summarize(
          rain_sum = sum(rain, na.rm = T),
          average_rain = mean(rain,na.rm = T),
          min_rainfall = min(rain, na.rm = T),
          maximum_rainfall =
            max(rain, na.rm = T),
          sum_missing = sum(is.na(rain), na.rm = T),
          days_in_a_year = max(doy, na.rm = T))
        
      
    } else{
      "Load the data to continue"
    }
    
  })
  
  output$inv_plot <- renderPlot({
     tryCatch({Full_data <- cli_data()
    #Preparing inventory plot for Quality Control
    x1 <-
      as.data.frame(ifelse(
        Full_data$rain == 0 |
          Full_data$rain < 0.8,
        "dry",
        ifelse(is.na(Full_data$rain), "missing", "rain")
      ))
    
    Full_data$recoded_rain <-
      as.vector(apply(
        x1,
        2,
        FUN = function(x) {
          replace(x, is.na(x), "missing")
        }
      ))
    Full_data <- arrange(Full_data, year)
    
    ggplot(Full_data, mapping = aes(x = year, y = doy, col = recoded_rain)) + geom_point() + xlab("Year") +
      ylab("Day of the Year") + ggtitle("Inventory plot for Quality Control") +
      theme(plot.title =
              element_text(hjust = 0.5)) + guides(color = guide_legend(title = "Rainy and Missing")) +
      theme_dark()},error= function(w){w<-"Load data to continue";return(w)})
  
  })
  output$box_plot <- renderPlot({
  
    tryCatch({
    Full_data <- cli_data()
    ggplot(data = Full_data,
           mapping = aes(
             x = as.factor(year),
             y = rain,
             fill = as.factor(year)
           )) + geom_boxplot() +
      ggtitle("Annual Rainfall Variation") + theme(
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 24),
        axis.title = element_text(size = 16)
      ) + xlab("Year") + ylab("Rainfall Amount")+coord_flip()},
    error= function(w){w<-"Load data to continue";return(w)})
  })
  
  #Rainfall yearly summary per station
  output$sum_vales <- renderDataTable({
    summaries()
  })
  
}


shinyApp(ui = user_interface, server = r_manipulations)