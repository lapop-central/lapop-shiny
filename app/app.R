library(shiny)
library(lapop)
library(RStata)
setwd("C:/Users/plutowl/Documents/GitHub/lapop-shiny")


# Define UI for miles per gallon app ----
# Define UI for miles per gallon app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("LAPOP Data"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Selector for variable to plot against mpg ----
      selectInput("variable", "Variable:",
                  c("Support for Democracy" = "ing4",
                    "Crime Victimization" = "vic1ext")),
      
      # Input: Checkbox for whether outliers should be included ----
      checkboxInput("weighted", "Weighted", TRUE)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Formatted text for caption ----
      h3(textOutput("caption")),
      
      # Output: Plot of the requested variable against mpg ----
      plotOutput("hist"),
      
      plotOutput("ts")
      
      
    )
  )
)

# Tweak the "am" variable to have nicer factor labels -- since this
# doesn't rely on any user inputs, we can do this once at startup
# and then use the value throughout the lifetime of the app

# ym23 <- readstata13::read.dta13("Shiny data/Merge 2023 LAPOP AmericasBarometer (v1.0s).dta")
# select <- c("pais", "wave", "ing4", "vic1ext", "edre", "q1tc_r", "wealth", "edad", "weight1500")
# ym23s <- ym23[select]
# 
# ym21 <- readstata13::read.dta13("Shiny data/Merged_LAPOP_AmericasBarometer_2021_v1.2.dta")
# select21 <- c("pais", "wave", "ing4", "vic1ext", "edre", "q1tc_r", "wealth", "edad", "weight1500")
# ym21s <- ym21[select]

# gm <- readstata13::read.dta13("C:/Users/plutowl/Desktop/Shiny data/gm2123.dta", convert.factors = FALSE)
# select <- c("pais", "wave", "ing4", "vic1ext", "wealth", "edad")
# gms <- gm[select]

# gm <- readstata13::read.dta13("C:/Users/plutowl/Box/LAPOP Shared/2_Projects/2023 AB/BHS/Data Processing/BHS merge 2014-2023 LAPOP AmericasBarometer (v1.0s).dta", convert.factors = FALSE)

num_max <- 2
num <- paste(1:num_max, collapse = " ")

# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
  
  # Compute the formula text ----
  # This is in a reactive expression since it is shared by the
  # output$caption and output$mpgPlot functions
  formulaText <- reactive({
    paste(input$variable)
  })
  
  

  

  
  outcome <- reactive({
    # if(input$weighted == TRUE){
    #   input$variable * ym23s['weight1500']
    # } else{
      input$variable
    # }
  })
  
  # Return the formula text for printing as a caption ----
  output$caption <- renderText({
    formulaText()
  })
  
  # Generate a plot of the requested variable against mpg ----
  # and only exclude outliers if requested
  

  
  # output$hist <- renderPlot({
  #   # the_wt = ifelse(input$weighted, gms$weight1500, 1)
  #   # x = outcome() * the_wt
  #   # var = input$variable
  #   hist_df <- as.data.frame(prop.table(table(gms[[outcome()]]))*100)
  #   names(hist_df) <- c("cat", "prop")
  #   hist_df$proplabel <- paste(round(hist_df$prop), "%", sep = "")
  #   lapop_hist(hist_df)
  # })
  
  output$hist <- renderPlot({
    stata_cmd2 <- paste('use "C:\\Users\\plutowl\\Box\\LAPOP Shared\\2_Projects\\2023 AB\\BHS\\Data Processing\\BHS merge 2014-2023 LAPOP AmericasBarometer (v1.0s).dta", clear\n', 
                        "lpr_hist ", outcome(), ', filesave("hist.csv", replace) \n',
                        'lpr_ts ', outcome(), ' wave, num(', num, ') filesave("ts.csv", replace)')
    stata(stata_cmd2)
    hist_df <- read.csv("hist.csv")
    lapop_hist(hist_df)
  })
  
  output$ts <- renderPlot({
    ts_df <- read.csv("ts.csv")
    lapop_ts(ts_df)
  })
  
  
  
  # output$ts <- renderPlot({
  #   dta = gms %>%
  #     group_by(wave) %>%
  #     summarise_at(vars(input$variable), list(name = mean), na.rm = TRUE)
  #   dta$wave = as.character(dta$wave)
  #   dta$proplabel = paste(as.character(round(dta$name), 0))
  #   dta$lb = dta$name - 1
  #   dta$ub = dta$name + 1
  #   names(dta)[2] = "prop"
  #   lapop_ts(dta)
  # })
  
}


shinyApp(ui, server)

# runApp("C:/Users/plutowl/Documents/GitHub/lapop-shiny/shinyapp")
