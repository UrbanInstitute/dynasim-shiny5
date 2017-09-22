# Libraries and Source Files
library(shiny)
library(tidyverse)
library(scales)

# Set options
options(shiny.sanitize.errors = TRUE)
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
                  "No target-date funds", "No auto-enrollment",
                  "No cash outs", "All Roth-401(K) accounts #1",
                  "All Roth-401(K) accounts #2",
                  "Mandated employer plans (60%)",
                  "Mandated employer plans (100%)", "Repeat the 1970s",
                  "RothIRA2", "RothIRA2allpart", "RothIRA2b", "RothIRA2c", 
                  "RothIRA2d", "RothIRA2e", "RothIRA2f", "RothIRA2g", 
                  "RothIRA2nocashout", "RothIRA3", "RothIRA3b", "RothIRA3c", 
                  "RothIRA3d", "RothIRA4", "RothIRA4b", "RothIRA4c", 
                  "RothIRA4d","RothIRAHighLimits", "RothIRALimit2", 
                  "RothIRALimit2b", "RothIRALimit2c", "RothIRALimit2d", 
                  "RothIRALimit2e", "RothIRALimit3", "RothIRALimit3b", 
                  "RothIRALimit4", "SAVEopt2", "SaveOpt2b", "SaveOpt2firm10", 
                  "SaveOpt3", "SAVEopt3b", "SAVEopt4", "SAVEopt4b", "BPC package")

financial <- read_csv("data/financial-assets.csv",
  col_types = cols(
    Age = col_integer(),
    cohort = col_character(),
    data_source = col_character(),
    `10th percentile` = col_double(),
    `20th percentile` = col_double(),
    `30th percentile` = col_double(),
    `40th percentile` = col_double(),
    `5th percentile` = col_double(),
    `50th percentile` = col_double(),
    `60th percentile` = col_double(),
    `70th percentile` = col_double(),
    `80th percentile` = col_double(),
    `90th percentile` = col_double(),
    `95th percentile` = col_double(),
    `98th percentile` = col_double(),
    Mean = col_double()
  )
) %>% mutate(data_source = factor(data_source, levels = factor_order))

home_equity <- read_csv("data/home-equity.csv", 
  col_types = cols(
    Age = col_integer(),
    cohort = col_character(),
    data_source = col_character(),
    `10th percentile` = col_double(),
    `20th percentile` = col_double(),
    `30th percentile` = col_double(),
    `40th percentile` = col_double(),
    `5th percentile` = col_double(),
    `50th percentile` = col_double(),
    `60th percentile` = col_double(),
    `70th percentile` = col_double(),
    `80th percentile` = col_double(),
    `90th percentile` = col_double(),
    `95th percentile` = col_double(),
    `98th percentile` = col_double(),
    Mean = col_double()
  )
) %>% mutate(data_source = factor(data_source, levels = factor_order))

retirement_account <- read_csv("data/retirement-account-assets.csv", col_types = 
  cols(
    Age = col_integer(),
    cohort = col_character(),
    data_source = col_character(),
    `10th percentile` = col_integer(),
    `20th percentile` = col_double(),
    `30th percentile` = col_double(),
    `40th percentile` = col_double(),
    `5th percentile` = col_integer(),
    `50th percentile` = col_double(),
    `60th percentile` = col_double(),
    `70th percentile` = col_double(),
    `80th percentile` = col_double(),
    `90th percentile` = col_double(),
    `95th percentile` = col_double(),
    `98th percentile` = col_double(),
    Mean = col_double()
  )
) %>% mutate(data_source = factor(data_source, levels = factor_order))

total <- read_csv("data/total-assets.csv",
  col_types = cols(
    Age = col_integer(),
    cohort = col_character(),
    data_source = col_character(),
    `10th percentile` = col_double(),
    `20th percentile` = col_double(),
    `30th percentile` = col_double(),
    `40th percentile` = col_double(),
    `5th percentile` = col_double(),
    `50th percentile` = col_double(),
    `60th percentile` = col_double(),
    `70th percentile` = col_double(),
    `80th percentile` = col_double(),
    `90th percentile` = col_double(),
    `95th percentile` = col_double(),
    `98th percentile` = col_double(),
    Mean = col_double()
  )
) %>% mutate(data_source = factor(data_source, levels = factor_order))

# Set colors so colors don't change as levels are added/drops
cols <- c("Baseline" = "#1696d2",
          "HRS" = "#fdbf11",
          "PSID" = "#000000",
          "SCF" = "#ec008b",              
          "SIPP" = "#d2d2d2",
          "Reduce fees" = "#55B748",
          "Rebalance every 5 years" = "#55B748",
          "Low participation" = "#55B748",
          "High participation" = "#55B748",
          "Less risk" = "#55B748",
          "More risk" = "#55B748",
          "No target-date funds" = "#55B748",
          "No auto-enrollment" = "#55B748",
          "No cash outs" = "#55B748",
          "All Roth-401(K) accounts #1" = "#55B748",
          "All Roth-401(K) accounts #2" = "#55B748",
#          "Mandated employer plans (60%)" = "#55B748", 
#          "Mandated employer plans (100%)" = "#55B748",
          "Repeat the 1970s" = "#55B748",
          "RothIRA2" = "#55B748",
          "RothIRA2allpart" = "#55B748",
          "RothIRA2b" = "#55B748",
          "RothIRA2c" = "#55B748",
          "RothIRA2d" = "#55B748",
          "RothIRA2e" = "#55B748",
          "RothIRA2f" = "#55B748",
          "RothIRA2g" = "#55B748",
          "RothIRA2nocashout" = "#55B748",
          "RothIRA3" = "#55B748",
          "RothIRA3b" = "#55B748",
          "RothIRA3c" = "#55B748",
          "RothIRA3d" = "#55B748",
          "RothIRA4" = "#55B748",
          "RothIRA4b" = "#55B748",
          "RothIRA4c" = "#55B748",
          "RothIRA4d" = "#55B748",
          "RothIRAHighLimits" = "#55B748",
          "RothIRALimit2" = "#55B748",
          "RothIRALimit2b" = "#55B748", 
          "RothIRALimit2c" = "#55B748",
          "RothIRALimit2d" = "#55B748",
          "RothIRALimit2e" = "#55B748",
          "RothIRALimit3" = "#55B748",
          "RothIRALimit3b" = "#55B748",
          "RothIRALimit4" = "#55B748",
          "SAVEopt2" = "#55B748",
          "SaveOpt2b" = "#55B748",
          "SaveOpt2firm10" = "#55B748",
          "SaveOpt3" = "#55B748",
          "SAVEopt3b" = "#55B748",
          "SAVEopt4" = "#55B748",
          "SAVEopt4b" = "#55B748",
          "BPC package" = "#55B748")

##
## Shiny
##

latoCSS <- "http://fonts.googleapis.com/css?family=Lato:300,400,700,900,300italic,400italic,700italic,900italic"

ui <- fluidPage(
  
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = latoCSS)),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css")),    
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
                                   "No target-date funds" = "No target-date funds",
                                   "No auto-enrollment" = "No auto-enrollment",
                                   "No cash outs" = "No cash outs",
                                   "All Roth-401(K) accounts #1" = "All Roth-401(K) accounts #1",
                                   "All Roth-401(K) accounts #2" = "All Roth-401(K) accounts #2",
#                                   "Mandated employer plans (60%)" = "Mandated employer plans (60%)",
#                                   "Mandated employer plans (100%)" = "Mandated employer plans (100%)",
                                   "Repeat the 1970s" = "Repeat the 1970s",
                                   "BPC package" = "BPC package",
                                   "RothIRA2" = "RothIRA2",
                                   "RothIRA2allpart" = "RothIRA2allpart",
                                   "RothIRA2b" = "RothIRA2b",
                                   "RothIRA2c" = "RothIRA2c",
                                   "RothIRA2d" = "RothIRA2d",
                                   "RothIRA2e" = "RothIRA2e",
                                   "RothIRA2f" = "RothIRA2f",
                                   "RothIRA2g" = "RothIRA2g",
                                   "RothIRA2nocashout" = "RothIRA2nocashout",
                                   "RothIRA3" = "RothIRA3",
                                   "RothIRA3b" = "RothIRA3b",
                                   "RothIRA3c" = "RothIRA3c",
                                   "RothIRA3d" = "RothIRA3d",
                                   "RothIRA4" = "RothIRA4",
                                   "RothIRA4b" = "RothIRA4b",
                                   "RothIRA4c" = "RothIRA4c",
                                   "RothIRA4d" = "RothIRA4d",
                                   "RothIRAHighLimits" = "RothIRAHighLimits",
                                   "RothIRALimit2" = "RothIRALimit2",
                                   "RothIRALimit2b" = "RothIRALimit2b",
                                   "RothIRALimit2c" = "RothIRALimit2c",
                                   "RothIRALimit2d" = "RothIRALimit2d",
                                   "RothIRALimit2e" = "RothIRALimit2e",
                                   "RothIRALimit3" = "RothIRALimit3",
                                   "RothIRALimit3b" = "RothIRALimit3b",
                                   "RothIRALimit4" = "RothIRALimit4",
                                   "SAVEopt2" = "SAVEopt2",
                                   "SaveOpt2b" = "SaveOpt2b",
                                   "SaveOpt2firm10" = "SaveOpt2firm10",
                                   "SaveOpt3" = "SaveOpt3",
                                   "SAVEopt3b" = "SAVEopt3b",
                                   "SAVEopt4" = "SAVEopt4",
                                   "SAVEopt4b" = "SAVEopt4b")
           ),
           
           selectInput(inputId = "asset",
                       label = "Asset",
                       choices = c("Retirement account assets" = "Retirement account assets",
                                   "Financial assets" = "Financial assets",
                                   "Home equity" = "Home equity",
                                   "Total assets" = "Total assets")
           ),           

           selectInput(inputId = "percentile",
                       label = "Percentile or Mean",
                       choices = c("Mean" = "Mean",
                                   "5th percentile" = "`5th percentile`",
                                   "10th percentile" = "`10th percentile`",
                                   "20th percentile" = "`20th percentile`",
                                   "30th percentile" = "`30th percentile`",
                                   "40th percentile" = "`40th percentile`",
                                   "50th percentile" = "`50th percentile`",
                                   "60th percentile" = "`60th percentile`",
                                   "70th percentile" = "`70th percentile`",
                                   "80th percentile" = "`80th percentile`",
                                   "90th percentile" = "`90th percentile`",
                                   "95th percentile" = "`95th percentile`",
                                   "98th percentile" = "`98th percentile`")
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
           20,000 respondents age 50 and older that asks questions about 
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
           characteristics. The survey is conducted every three years and began 
           in 1983.</p> "),
      HTML("<h4><a href='https://www.census.gov/sipp/'>Survey of Income and 
           Program Participation:</a></h4><p>A continuous series of national 
           panels of 14,000 to 52,000 households that focuses on the interaction 
           between tax, transfer, and other government and private policies. 
           The survey began in 1983 and includes monthly data.</p>")
      
    )
  
  ),
  
  br(),
  
  fluidRow(
    column(6,
           h3("About the data"),
           HTML("<p>The Urban Institute’s Dynamic Simulation of Income Model (DYNASIM) projects the size and characteristics (such as financial, health, and disability status) 
                of the US population for the next 75 years. Using the best and most recent data available, it helps sort out how profound social, economic, and demographic 
                shifts will likely affect older adults and their retirement as well astaxpayers, business, and government. The model can also show how outcomes would likely 
                evolve under changes to public policies, business practices, or individual behaviors.</p>"),
           HTML("<p><a href='https://www.urban.org/node/65826'>Read the DYNASIM primer</a></p>"),
           HTML("<p><a href='https://www.urban.org/research/publication/dynamic-simulation-income-model-dynasim-overview'>Review the DYNASIM documentation</a></p>")
           
           ),
    column(6,
           h3("Project Credits"),
           HTML("<p><i>This work was funded by the US Department of Labor’s Employee Benefits Security Administration. 
                We are grateful to them and to all our funders, who make it possible for Urban Institute to advance its mission.</i></p> 
                <p><i>The views expressed are those of the authors and should not be attributed to the Urban Institute, its trustees, 
                or its funders. Funders do not determine research findings or the insights and recommendations of our experts. 
                More information on our funding principles is available <a href='https://www.urban.org/support'>here</a>. 
                Read our terms of service <a href='https://www.urban.org/terms-service'>here</a></i>.</p>"),
           
           h5(HTML("<div class='credit-labels'>RESEARCH")),
           HTML("<div class='credit-names'><p><a href='https://www.urban.org/author/karen-e-smith'>Karen Smith</a></p></div>"),
           h5(HTML("<div class='credit-labels'>DESIGN AND DEVELOPMENT")),
           HTML("<div class='credit-names'><p><a href='https://www.urban.org/author/aaron-r-williams'>Aaron Williams</a>, <a href='https://www.urban.org/author/jerry-ta'>Jerry Ta</a>, and <a href='https://www.urban.org/author/benjamin-chartoff'>Ben Chartoff</a></p></div>"),
           h5(HTML("<div class='credit-labels'>EDITING")),
           HTML("<div class='credit-names'><p><a href='https://www.urban.org/author/michael-marazzi'>Michael Marazzi</a></p></div>"),
           h5(HTML("<div class='credit-labels'>WRITING")),
           HTML("<div class='credit-names'><p><a href = 'https://www.urban.org/author/karen-e-smith'>Karen Smith</a> and <a href='https://www.urban.org/author/aaron-r-williams'>Aaron Williams</a></p></div>"),
           
           HTML("Copyright &copy; <a href='https://www.urban.org/'>Urban Institute</a> 2017. View this project on <a href='https://github.com/urbaninstitute/dynasim-shiny1.git'>GitHub</a>.</p>")
           )
    ),
  
  tags$script(src = "activatePym.js")
)

server <- function(input, output) {
  
  options(shiny.sanitize.errors = FALSE)
  
  output$title <- renderText({
    paste(input$Percentile, input$asset, sep = " ")
  })
  
  output$subtitlea <- renderText({
    paste(input$cohort, " Cohorts", sep = " ")
  })
  
  output$subtitleb <- renderText({
    paste(input$asset, "(divided by average earnings)")
  })
 
  
   
  filter_df <- function(df) {
    
    print(input$option)
    
    df %>%
      filter(cohort == input$cohort) %>%
      filter(data_source %in% c("Baseline", "HRS", "PSID", "SCF", "SIPP", input$option)) %>%
      select_("Age", "cohort", "data_source", value_subset = input$percentile) %>%
      mutate(value_subset = if_else(data_source %in% c(input$data_source, input$option), as.numeric(value_subset), as.numeric(NA)))
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
        labs(y = NULL,
             caption = "DYNASIM3
                          Urban Institute") + 
        scale_color_manual(values = cols) +
        theme(axis.line = element_blank()) 
      
  })  
  
  output$download_data <- downloadHandler(
    filename = function() { paste0(input$option, '.csv') },
    content = function(file) {
      
      data_subset() %>%
        filter(!is.na(value_subset)) %>%
        write_csv(file)
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