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
    titlePanel("Climatic data visualizations and insights"),
    sidebarLayout(
      sidebarPanel = sidebarPanel(
        fileInput(
          inputId = "dt_file",
          label = "csv, xlsx and stata(dta) files only:",
          accept = c(".csv", ".dta", ".txt", ".xlsx"),
          placeholder = "data not loaded",
          multiple = FALSE
        ),
        uiOutput(outputId = "station"),
        uiOutput(outputId = "year_val_start"),
        uiOutput(outputId = "year_val_end"),
        uiOutput(outputId = "month_val"),
        h4("Element and Days Name(s)"),
        uiOutput(outputId = "cli_element"),
        uiOutput(outputId = "doy"),
        shinythemes::themeSelector()
      ),
      mainPanel = mainPanel(
        tags$head(
          tags$style(
            type = "text/css",
            "
            #loadmessage {
            position: fixed;
            top: 0px;
            left: 0px;
            width: 100%;
            padding: 5px 0px 5px 0px;
            text-align: center;
            font-weight: bold;
            font-size: 100%;
            color: #000000;
            background-color:#0000ff;
            z-index: 105;
            }
            "
          )
        ),
        tabsetPanel(
          tabPanel(
            title = "Inventory Plot",
            plotOutput("inv_plot"),
            helpText("NOTE: To load Inventory Plot Please select doy from Days dropdown")
          ),
          tabPanel(
            title = "Annual Boxplot Summaries",
            plotOutput("box_plot"),
            width = "100%",
            height = "100%"
          ),
          tabPanel(title = "Monthly Histograms", plotOutput("hist_plots"), helpText(
            paste(
              "NOTE: To load Monthly Histogram Plot Please select day_in_month from Days dropdown.",
              "Start year value and end year value should be the same"
            )
          )),
          tabPanel(title = "Yearly Element Summaries", dataTableOutput("sum_vales")),
          tabPanel(title = "View Data", dataTableOutput("data_sec")),
          conditionalPanel(condition = "$('html').hasClass('shiny-busy')",
                           tags$div("Loading...", id = "loadmessage"))
        )
      )
    )
  )

r_manipulations <- function(input, output) {
  #Set desired file limit
  options(shiny.maxRequestSize = 10000 * 1024 ^ 2)
  #Loading Data
  cli_data <- reactive({
    temp_file <- input$dt_file
    if (is.null(temp_file))
      return("No Data in The Memory")
    if (stringr::str_detect(temp_file, ".csv")) {
      fild <- data.table::fread(temp_file$datapath, na.strings = c("missing", "", " ", "NA"))
    } else if (stringr::str_detect(temp_file, ".dta")) {
      fild <- haven::read_dta(temp_file$datapath, na = c("missing", "", " ", "NA"))
    } else if (stringr::str_detect(temp_file, ".xlsx")) {
      fild <- openxlsx::read.xlsx(temp_file$datapath, na = c("missing", "", " ", "NA"))
    }
    
  })
  
  #Data munging  
  
  inv_data <- reactive({
    start_data <- cli_data()
    y1= as.integer(unlist(input$year_val_start))
    y2= as.integer(unlist(input$year_val_end))
    stations_values<- input$station_vals
    dplyr::filter(start_data, year%in% y1:y2 & stations %in% stations_values)
    
  })
  
  hist_data<- reactive({
    start_year<-input$year_val_start
    end_year<-input$year_val_end
    stations_values<- input$station_vals
    if(start_year==end_year){ 
      month_lab<- input$month_values
      month_d <- as.data.frame(cli_data())
      filter(month_d, month==month_lab & year==start_year & stations %in% stations_values)
    }
  })
  
  
  summaries <- reactive({
    library(dplyr)
    tryCatch({if(!is.null(input$station_vals)){
      summ_data <- inv_data()
      summ_data %>% group_by(year, stations) %>%
        summarize(
          rain_sum = sum(rain, na.rm = T),
          average_rain = mean(rain,na.rm = T),
          min_rainfall = min(rain, na.rm = T),
          maximum_rainfall =
            max(rain, na.rm = T),
          sum_missing = sum(is.na(rain), na.rm = T),
          days_in_a_year = max(doy, na.rm = T))}},error= function(w){w<-"Load data to continue";return(w)})
  })
  
  
  start_vals <- reactive({
    values<- cli_data()[,"year"]
    values <- sort.int(as.integer(unlist((values[!duplicated(values)]))))
    
  })
  
  end_vals <- reactive({
    values<- cli_data()[,"year"]
    values <- sort.int(as.integer(unlist((values[!duplicated(values)]))), decreasing = TRUE)
  })
  
  st_vals <- reactive({
    values<- cli_data()[,"stations"]
    values <- values[!duplicated(values)]
    
  })
  month_vals <- reactive({
    values<- cli_data()[,"month"]
    values <- values[!duplicated(values)]
    
  })
  
  #Outputs
  
  output$month_val <- renderUI({
    tryCatch({selectInput("month_values", choices = month_vals(), label = "Month Values:")},error= function(w){w<-"";return(w)})
  })
  
  
  output$year_val_start <- renderUI({
    tryCatch({selectInput("year_val_start", choices = start_vals(), label = "Start Year Value:")},error= function(w){w<-"";return(w)})
  })
  
  output$year_val_end <- renderUI({
    tryCatch({ selectInput("year_val_end", choices = end_vals(), label = "End Year Value:")},error= function(w){w<-"";return(w)})
  })
  
  output$station <- renderUI({
    tryCatch({selectInput("station_vals", choices = st_vals(), label = "Station Names(Click in the input to select):", multiple = TRUE)},error= function(w){w<-"";return(w)})
  })
  
  output$date_col <- renderUI({
    nam <- names(cli_data())[which(names(cli_data()) == "date_col")]
    selectInput("date_input", choices = nam, label = "Date:")
  })
  output$doy <- renderUI({
    tryCatch({nam <- names(cli_data())[which(names(cli_data()) == "doy")]
    nam1 <- names(cli_data())[which(names(cli_data()) == "day_in_month")]
    selectInput("doy_input", choices = list(nam, nam1), label = "Days:")},error= function(w){w<-"";return(w)})
  })
  
  output$hist_plots<- renderPlot({
    tryCatch({if(input$doy_input!="doy" && input$year_val_start==input$year_val_end && !is.null(input$station_vals)){
      plot_data<-as.data.frame(hist_data())
      ggplot(plot_data, mapping = aes(x= day_in_month, y= rain))+geom_histogram(stat = "identity", aes(fill="blue")) + guides(fill=FALSE)+ggtitle(label = paste(input$month_values,input$year_val_start, "Rainfall Distribution", sep = " "))+theme(plot.title = element_text(hjust = 0.5))+theme_light()+xlab("Month Days")+ylab("Amount of Rainfall")+theme(panel.grid.minor = element_blank(),plot.title = element_text(hjust = 0.5, size = 24))}},error= function(w){w<-"Load data to continue";return(w)})
    
  })
  
  
  
  output$cli_element <- renderUI({
    nam <-
      names(cli_data())[!stringr::str_detect(names(cli_data()), pattern = "year|stations|doy|date_col|day_in_month|month")]
    selectInput("cli_input", choices = nam, label = "Climate Element:")
  })
  output$data_sec <- renderDataTable({
    tryCatch({data.frame(inv_data())},error= function(w){w<-"Load data to continue";return(w)})
  })
  
  
  
  output$inv_plot <- renderPlot({
    tryCatch({if(input$doy_input=="doy" & !is.null(input$station_vals)){
      
      Full_data <- inv_data()
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
        ylab("Day of the Year") + ggtitle("Inventory plot for Quality Control") +theme_light()+
        theme(
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 24)) + guides(color = guide_legend(title = "Rainy and Missing"))}},error= function(w){w<-"Load data to continue";return(w)})
    
  })
  output$box_plot <- renderPlot({
    
    tryCatch({if(!is.null(input$station_vals)){
      Full_data <- inv_data()
      ggplot(data = Full_data,
             mapping = aes(
               x = as.factor(year),
               y = rain,
               fill = as.factor(year)
             )) + geom_boxplot() +
        ggtitle("Annual Rainfall Variation")+theme_light() + theme(
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 24),
          axis.title = element_text(size = 16)
        ) + xlab("Year") + ylab("Rainfall Amount")+coord_flip()}},
      error= function(w){w<-"Load data to continue";return(w)})
  })
  
  #Rainfall yearly summary per station
  output$sum_vales <- renderDataTable({
    summaries()
  })
  
}

shinyApp(ui = user_interface, server = r_manipulations)