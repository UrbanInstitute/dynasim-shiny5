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
  )
)

option_asset <- read_csv("text/asset.csv",
  col_types = cols(
    asset = col_character(),
    text = col_character()
  )
)


factor_order <- c("Baseline", "HRS", "PSID", "SCF", "SIPP", "Reduce fees",
                  "Rebalance every 5 years", "Low participation",
                  "High participation", "Less risk","More risk",
                  "No target date funds", "No auto-enrollment",
                  "No cash outs", "All Roth-401k accounts #1",
                  "All Roth-401k accounts #2",
                  "Mandated employer plans (60%)",
                  "Mandated employer plans (100%)")

financial <- read_csv("data/financial-assets.csv",
  col_types = cols(
    Age = col_integer(),
    cohort = col_character(),
    data_source = col_character(),
    `10th Percentile` = col_double(),
    `20th Percentile` = col_double(),
    `30th Percentile` = col_double(),
    `40th Percentile` = col_double(),
    `5th percentile` = col_double(),
    `50th Percentile` = col_double(),
    `60th Percentile` = col_double(),
    `70th Percentile` = col_double(),
    `80th Percentile` = col_double(),
    `90th Percentile` = col_double(),
    `95th Percentile` = col_double(),
    `98th Percentile` = col_double(),
    Mean = col_double()
  )
) %>% mutate(data_source = factor(data_source, levels = factor_order))



home_equity <- read_csv("data/home-equity.csv", 
  col_types = cols(
    Age = col_integer(),
    cohort = col_character(),
    data_source = col_character(),
    `10th Percentile` = col_double(),
    `20th Percentile` = col_double(),
    `30th Percentile` = col_double(),
    `40th Percentile` = col_double(),
    `5th percentile` = col_double(),
    `50th Percentile` = col_double(),
    `60th Percentile` = col_double(),
    `70th Percentile` = col_double(),
    `80th Percentile` = col_double(),
    `90th Percentile` = col_double(),
    `95th Percentile` = col_double(),
    `98th Percentile` = col_double(),
    Mean = col_double()
  )
) %>% mutate(data_source = factor(data_source, levels = factor_order))

retirement_account <- read_csv("data/retirement-account-assets.csv", col_types = 
  cols(
    Age = col_integer(),
    cohort = col_character(),
    data_source = col_character(),
    `10th Percentile` = col_integer(),
    `20th Percentile` = col_double(),
    `30th Percentile` = col_double(),
    `40th Percentile` = col_double(),
    `5th percentile` = col_integer(),
    `50th Percentile` = col_double(),
    `60th Percentile` = col_double(),
    `70th Percentile` = col_double(),
    `80th Percentile` = col_double(),
    `90th Percentile` = col_double(),
    `95th Percentile` = col_double(),
    `98th Percentile` = col_double(),
    Mean = col_double()
  )
) %>% mutate(data_source = factor(data_source, levels = factor_order))

total <- read_csv("data/total-assets.csv",
  col_types = cols(
    Age = col_integer(),
    cohort = col_character(),
    data_source = col_character(),
    `10th Percentile` = col_double(),
    `20th Percentile` = col_double(),
    `30th Percentile` = col_double(),
    `40th Percentile` = col_double(),
    `5th percentile` = col_double(),
    `50th Percentile` = col_double(),
    `60th Percentile` = col_double(),
    `70th Percentile` = col_double(),
    `80th Percentile` = col_double(),
    `90th Percentile` = col_double(),
    `95th Percentile` = col_double(),
    `98th Percentile` = col_double(),
    Mean = col_double()
  )
) %>% mutate(data_source = factor(data_source, levels = factor_order))

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
           
           p("Long-run projections are sensitive to small changes in data and 
              assumptions. Use this interactive tool to compare changes in 
              DYNASIM's assumptions for defined-contribution pensions with data 
             from the Health and Retirement Study, Panel Study of Income 
             Dynamics, Survey of Consumer Finances, and Survey of Income and 
             Program Participation. ")
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
                                   "Reduce fees" = "Reduce fees",
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
                                   "5th Percentile" = "`5th Percentile`",
                                   "10th Percentile" = "`10th Percentile`",
                                   "20th Percentile" = "`20th Percentile`",
                                   "30th Percentile" = "`30th Percentile`",
                                   "40th Percentile" = "`40th Percentile`",
                                   "50th Percentile" = "`50th Percentile`",
                                   "60th Percentile" = "`60th Percentile`",
                                   "70th Percentile" = "`70th Percentile`",
                                   "80th Percentile" = "`80th Percentile`",
                                   "90th Percentile" = "`90th Percentile`",
                                   "95th Percentile" = "`95th Percentile`",
                                   "98th Percentile" = "`98th Percentile`")
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
                              choices = c("Baseline" = "Baseline",
                                          "HRS" = "HRS",
                                          "PSID" = "PSID",
                                          "SCF" = "SCF",
                                          "SIPP" = "SIPP"), 
                              selected = c("Baseline", "HRS", "PSID", "SCF", "SIPP"))
    )
  
  ),

  br(),
  
  fluidRow(
    column(12,
           downloadButton('download_data', 'Download charted data')
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
           
      HTML("<h4><a href='http://hrsonline.isr.umich.edu/'>Health and Retirement 
           Study:</a></h4><p>A national longitudinal study of approximately 
           20,000 respondents ages fifty and older that asks questions about 
           assets, health care, housing, and pensions. The study is conducted 
           every two years and began in 1992.</p>"),
      HTML("<h4><a href='https://psidonline.isr.umich.edu/'>Panel Study of 
           Income Dynamics:</a></h4><p>A national longitudinal study of more 
           than 18,000 individuals in 5,000 families that asks questions about 
           employment, income, wealth, expenditures, health, marriage, 
           childbearing, child development, philanthropy, and education. 
           Families were asked questions annually from 1968 to 1997 and every 
           other year after 1997.</p> "),
      HTML("<h4><a href='https://www.federalreserve.gov/econres/scfindex.htm'>Survey of Consumer Finances:</a></h4>
           <p>A national cross-sectional study of approximately 6,500 families 
           that focuses on balance sheets, pensions, income, and demographic 
           characteristics. The survey is conducted every three years and dates 
           back to 1983.</p> "),
      HTML("<h4><a href='https://www.census.gov/sipp/'>Survey of Income and 
           Program Participation:</a></h4><p>A continuous series of national 
           panels of 14,000 to 52,000 households that focuses on the interaction 
           between tax, transfer, and other government and private policies. 
           The survey began in 1983 and includes monthly data.</p>")
      
    )
  
  ),
  
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
    str_c(input$asset, "/average earnings")
  })
  
  filter_df <- function(df) {
    
    df %>%
      filter(cohort == input$cohort) %>%
      filter(data_source %in% c("Baseline", "HRS", "PSID", "SCF", "SIPP", input$option)) %>%
      select_("Age", "cohort", "data_source", value_subset = input$percentile) %>%
      mutate(value_subset = if_else(data_source %in% c(input$data_source, input$option), value_subset, as.numeric(NA)))
  } 
    
  data_subset <- reactive({
    
    if (input$asset == "Total assets") {
      filter_df(total)
    } else if (input$asset == "Retirement account assets") {
      filter_df(retirement_account)      
    } else if (input$asset == "Financial assets") {
      filter_df(financial)      
    } else if (input$asset == "Home equity") {
      filter_df(home_equity)      
    }
    
  })  
  
  output$chart <- renderPlot({  
    
    data_subset() %>%  
      ggplot(aes(x = Age, y = value_subset, color = data_source)) +
      geom_line(size = 1) +
        scale_x_continuous(breaks = c(20, 30, 40, 50, 60, 70, 80, 90, 100)) +
        labs(y = NULL) + 
        theme(axis.line = element_blank()) 
      
  })  
  
  
  
  
  financial %>%
    filter(cohort == "All") %>%
    filter(data_source %in% c("Baseline", "HRS", "PSID", "SCF", "SIPP")) %>%
    select_("Age", "cohort", "data_source", "`90th Percentile`") %>% 
    ggplot(aes(Age, `90th Percentile`, color = data_source)) +
    geom_line()
  
  
  
  
  
  
  
  
  
  
  
  
  
  
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