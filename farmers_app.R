#Climatic Data Visualizations

#Load packages
library(shiny)
library(shinythemes)
library(ggplot2)
library(plotly)
library(ggmap)
library(GGally)

user_interface <-
  fluidPage(
    titlePanel("Climatic data visualizations and insights"),
    sidebarLayout(
      sidebarPanel = sidebarPanel(
        fileInput(
          inputId = "dt_file",
          label = "Import Data (csv, xlsx and stata(dta) files only):",
          accept = c(".csv", ".dta", ".txt", ".xlsx"),
          placeholder = "data not loaded",
          multiple = FALSE
        ),
        fileInput(
          inputId = "shape_file",
          label = "Import Shapefile (Accepts .shp file only):",
          accept = c(".shp"),
          placeholder = "Shapefile not loaded",
          multiple = FALSE
        ),
        uiOutput(outputId = "county"),
        uiOutput(outputId = "year_val_start"),
        uiOutput(outputId = "year_val_end"),
        uiOutput(outputId = "month_val"),
        uiOutput(outputId = "cli_element"),
        sliderInput(inputId = "threshold", label = "Threshold (Use it onn Inventory plot):", min = 0, max = 1, value = 0.8),
        uiOutput(outputId = "doy"),
        checkboxInput(inputId="density", label="Add Densisty Plot", value = FALSE, width = "200px"),
        actionButton("goButton", "Submit"),
        
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
            plotlyOutput("inv_plot"),
            helpText("NOTE: To load Inventory Plot Please select doy from Days dropdown")
          ),
          tabPanel(
            title = "Annual Boxplot Summaries",
            plotlyOutput("box_plot"),
            width = "100%",
            height = "100%"
          ),
          tabPanel(title = "Monthly Histograms", plotlyOutput("hist_plots"), helpText(
            paste(
              "NOTE: To load Monthly Histogram Plot Please select day_in_month from Days dropdown.",
              "Start year value and end year value should be the same."
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
    county_values<- input$county_vals
    dplyr::filter(start_data, year%in% y1:y2 & county %in% county_values)
    
  })
  
  hist_data<- reactive({
      start_year<-input$year_val_start
    end_year<-input$year_val_end
    county_values<- input$county_vals
    if(start_year==end_year){ 
      month_lab<- input$month_values
      month_d <- as.data.frame(cli_data())
      filter(month_d, month==month_lab & year==start_year & county %in% county_values)
    }
  })
  
  
  summaries <- reactive({
    library(dplyr)
    tryCatch({if(!is.null(input$county_vals)){
      summ_data <- inv_data()
      summ_data %>% group_by(year, county) %>%
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
    values<- cli_data()[,"county"]
    values <- values[!duplicated(values)]
    
  })
  month_vals <- reactive({
    values<- cli_data()[,"month"]
    values <- values[!duplicated(values)]
    
  })
  
  #Outputs
  
  output$month_val <- renderUI({
    tryCatch({selectInput("month_values", choices = month_vals(), label = "Month Values (Use it on Histogram):")},error= function(w){w<-"";return(w)})
  })
  
  
  output$year_val_start <- renderUI({
    tryCatch({selectInput("year_val_start", choices = start_vals(), label = "Start Year Value:")},error= function(w){w<-"";return(w)})
  })
  
  output$year_val_end <- renderUI({
    tryCatch({ selectInput("year_val_end", choices = end_vals(), label = "End Year Value:")},error= function(w){w<-"";return(w)})
  })
  
  output$county <- renderUI({
    tryCatch({selectInput("county_vals", choices = st_vals(), label = "county Names(Click in the input to select):", multiple = FALSE)},error= function(w){w<-"";return(w)})
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
  
  output$hist_plots<- renderPlotly({
    req(input$goButton)
     if(input$doy_input!="doy" && input$year_val_start==input$year_val_end && !is.null(input$county_vals)){
      plot_data<-as.data.frame(hist_data())
      month<-input$month_values
      if(input$density==FALSE){
        p<- ggplot(plot_data, mapping = aes(x= day_in_month, y= rain))+geom_histogram(stat = "identity", fill= "green") + guides(fill=FALSE)+ggtitle(label = paste(input$month_values,input$year_val_start, "Rainfall Distribution", sep = " "))+theme_light()+xlab("Month Days")+ylab("Amount of Rainfall (mm)")+theme(panel.grid.minor = element_blank(),plot.title = element_text(hjust = 0.5, size = 18), legend.position = "none")

     }
     
     else{
         
       p<- ggplot(plot_data, mapping = aes(x= rain))+geom_density(fill="green") + guides(fill=FALSE)+ggtitle(label = paste(input$month_values,input$year_val_start, "Rainfall Distribution", sep = " "))+theme_light()+xlab("Amount of Rainfall (mm)")+ylab("Proportion")+theme(panel.grid.minor = element_blank(),plot.title = element_text(hjust = 0.5, size = 18), legend.position = "none")
       }
   
      
     
     }   
    
  })
  
  
  
  output$cli_element <- renderUI({
    nam <-
      names(cli_data())[!stringr::str_detect(names(cli_data()), pattern = "year|county|doy|date_col|day_in_month|month")]
    selectInput("cli_input", choices = nam, label = "Climate Element:")
  })
  output$data_sec <- renderDataTable({
    tryCatch({data.frame(inv_data())},error= function(w){w<-"Load data to continue";return(w)})
  })
  
  
  
  output$inv_plot <- renderPlotly({
    req(input$goButton)
    tryCatch({if(input$doy_input=="doy" & !is.null(input$county_vals)){
      
      Full_data <- inv_data()
      #Preparing inventory plot for Quality Control
      x1 <-
        as.data.frame(ifelse(
          Full_data$rain == 0 |
            Full_data$rain < input$threshold,
          "dry",
          ifelse(is.na(Full_data$rain), "missing", "rain")
        ))
      
      Full_data$Status<-
        as.vector(apply(
          x1,
          2,
          FUN = function(x) {
            replace(x, is.na(x), "missing")
          }
        ))
      Full_data <- arrange(Full_data, year)
      
     isolate(ggplot(Full_data, mapping = aes(x = year, y = doy, col = Status)) + geom_point() + xlab("Year") +
        ylab("Day of the Year") + ggtitle(paste0("Inventory plot for ", input$year_val_start, "-", input$year_val_end)) +theme_light()+
        theme(
          panel.grid.minor = element_blank(),
          legend.position = "none",
          plot.title = element_text(hjust = 0.5, size = 18)))}},error= function(w){w<-"Load data to continue";return(w)})
    
  })
  output$box_plot <- renderPlotly({
       req(input$goButton)
      tryCatch({if(!is.null(input$county_vals)){
      Full_data <- inv_data()
      ggplot(data = Full_data,
             mapping = aes(
               x = as.factor(year),
               y = rain,
               fill = as.factor(year)
             )) + geom_boxplot() +
        ggtitle(paste0("Annual Rainfall Variation for ",input$year_val_start, "-", input$year_val_end))+theme_light() + theme(
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 18),
          legend.position = "none",
          axis.title = element_text(size = 16)
        )+ xlab("Year") + ylab("Rainfall Amount (mm)")+coord_flip()}},
      error= function(w){w<-"Load data to continue";return(w)})
      })
  
  #Rainfall yearly summary per county
  output$sum_vales <- renderDataTable({
    summaries()
  })
  
}

shinyApp(ui = user_interface, server = r_manipulations)