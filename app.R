## Libraries and Source Files
library(shiny)
library(tidyverse)
library(stringr)
library(scales)

options(scipen = 999)

# Source file for Windows
Sys.setenv(R_GSCMD = "C:\\Program Files\\gs\\gs9.20\\bin\\gswin64.exe")
source('urban_institute_themes/urban_theme_windows.R')

# Source file for Mac
#source('urban_institute_themes/urban_theme_mac.R')

# Load Data

option_text <- read_csv("text/option.csv",
  col_types = cols(
    option = col_character(),
    text = col_character()
  ))

option_asset <- read_csv("text/asset.csv",
  col_types = cols(
    asset = col_character(),
    text = col_character()
  ))

options <- read_csv("data/options.csv",
  col_types = cols(
    Percentile = col_character(),
    Age = col_integer(),
    cohort = col_character(),
    value = col_double(),
    data_source = col_character(),
    Asset = col_character()
  ))

validation <- read_csv("data/validation.csv",
  col_types = cols(
    Age = col_integer(),
    Percentile = col_character(),
    cohort = col_character(),
    value = col_double(),
    data_source = col_character(),
    Asset = col_character()
  ))

ntiles <- bind_rows(options, validation)

rm(options, validation)

##
## Shiny
##

latoCSS <- "http://fonts.googleapis.com/css?family=Lato:300,400,700,900,300italic,400italic,700italic,900italic"

ui <- fluidPage(
  
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = latoCSS)),
  tags$head(tags$script(src = "pym.min.js")),
  
  theme = "shiny.css",
  
  fluidRow(
    
    column(12,
           
           p("blurb")
    )
  ),
  
  
  fluidRow(
    column(10,
           style = "position:relative",
           
           h4(textOutput("title")),
           h5(textOutput("subtitlea")),
           h5(textOutput("subtitleb")),
           
           plotOutput("chart", width = "100%", height = "400px")
           
    )
  ),
  
  fluidRow(
  
    column(6,
           selectInput(inputId = "option",
                       label = "Option",
                       choices = c("Baseline" = "Baseline",
                                   "Low fees" = "Low fees",
                                   "Rebalance every 5 years" = "Rebalance every 5 years",
                                   "Low participation" = "Low participation",
                                   "High participation" = "High participation",
                                   "Less risk" = "Less risk",
                                   "More risk" = "More risk",
                                   "No target date funds" = "No target date funds",
                                   "No auto-enrollment" = "No auto-enrollment",
                                   "No cash outs" = "No cash outs",
                                   "All Roth-401k accounts #1" = "All Roth-401k accounts #1",
                                   "All Roth-401k accounts #2" = "All Roth-401k accounts #2",
                                   "Mandated employer plans (60%)" = "Mandated employer plans (60%)",
                                   "Mandated employer plans (100%)" = "Mandated employer plans (100%)")
           ),
           
           selectInput(inputId = "asset",
                       label = "Asset",
                       choices = c("Total assets" = "Total assets",
                                   "Retirement account assets" = "Retirement account assets",
                                   "Financial assets" = "Financial assets",
                                   "Home equity" = "Home equity")
           ),           

           selectInput(inputId = "percentile",
                       label = "Percentile or Mean",
                       choices = c("Mean" = "Mean",
                                   "5th Percentile" = "5",
                                   "10th Percentile" = "10",
                                   "20th Percentile" = "20",
                                   "30th Percentile" = "30",
                                   "40th Percentile" = "40",
                                   "50th Percentile" = "50",
                                   "60th Percentile" = "60",
                                   "70th Percentile" = "70",
                                   "80th Percentile" = "80",
                                   "90th Percentile" = "90",
                                   "95th Percentile" = "95",
                                   "98th Percentile" = "98")
           ),         
           
           selectInput(inputId = "cohort",
                       label = "Cohort",
                       choices = c("All Cohorts" = "All",
                                   "1926-1930" = "1926-1930",
                                   "1931-1935" = "1931-1935",
                                   "1936-1940" = "1936-1940",
                                   "1941-1945" = "1941-1945",
                                   "1946-1950" = "1946-1950",
                                   "1951-1955" = "1951-1955",
                                   "1956-1960" = "1956-1960",
                                   "1961-1965" = "1961-1965",
                                   "1966-1970" = "1966-1970",
                                   "1971-1975" = "1971-1975")
           )      
           
    ), 
        
    column(6,
           checkboxGroupInput(inputId = "data_source", 
                              label = "Validation Data",
                              choices = c("HRS" = "HRS",
                                          "PSID" = "PSID",
                                          "SCF" = "SCF",
                                          "SIPP" = "SIPP"), 
                              selected = c("HRS", "PSID", "SCF", "SIPP"))
    )
  
  ),

  br(),
  
  fluidRow(
    column(12,
           downloadButton('download_data', 'Download Charted Data')
    )
  ),
  
  br(),
  
  fluidRow(
    
    column(12,
           
           # Explanation of Social Security Reform
           
           htmlOutput("text_option")
    )
  ),
  
  fluidRow(
    
    column(12,
           
           # Explanation of asset
           
           htmlOutput("text_asset")
    )
    
  ),
  
  
  fluidRow(
    column(12,
           
      htmlOutput("blurb")     
      
    )
  
  ),
  
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),  
  
  tags$script(src = "activatePym.js")
)

server <- function(input, output) {
  
  options(shiny.sanitize.errors = FALSE)
  
  output$title <- renderText({
    
    str_c(input$Percentile, input$asset, sep = " ")
    
  })
  
  output$subtitlea <- renderText({
    
    str_c(input$cohort, " Cohorts", sep = " ")
    
  })
  
  output$subtitleb <- renderText({
    str_c(input$asset, "/Average Earnings")
  })
  
  data_subset <- reactive({
    ntiles %>%
      filter(Percentile == input$percentile,
             Asset == input$asset,
             cohort == input$cohort,
             data_source %in% c(input$option, "HRS", "PSID", "SCF", "SIPP")) %>%
      mutate(value_subset = ifelse(data_source %in% c(input$data_source, input$option), value, NA),
             data_source = factor(data_source, levels = unique(data_source)))
  })  
  
  output$chart <- renderPlot({  
    
    data_subset() %>%  
      ggplot(aes(x = Age, y = value_subset, color = data_source)) +
      geom_line(size = 1) +
        scale_x_continuous(breaks = c(20, 30, 40, 50, 60, 70, 80, 90, 100)) +
        labs(y = NULL) + 
        theme(axis.line = element_blank()) 
      
  })  
  
  output$download_data <- downloadHandler(
    filename = function() { paste0(input$option, '.csv') },
    content = function(file) {
      write_csv(data_subset(), file)
    }
  )
 
  output$text_option <- renderText({
    
    as.character(
      option_text %>%
        filter(option == input$option) %>%
        select(text)
    )
    
  })
   
  
  output$text_asset <- renderText({
    
    as.character(
      option_asset %>%
        filter(asset == input$asset) %>%
        select(text)
    )
    
  })
}

shinyApp(ui = ui, server = server)