library(shiny)
library(lapop)
library(haven)
library(dplyr)

setwd("C:/Users/plutowl/Documents/GitHub/lapop-shiny")

gm2123 <- read_dta("C:/Users/plutowl/Desktop/gm2123.dta")

perc_pos <- function(x, min, max){
  return(mean(between((x), min, max), na.rm = TRUE) * 100)
}

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
      
      selectInput("pais","Countries",
                  sort(levels(as_factor(gm2123$pais))[!is.na(gm2123$pais)]), 
                  multiple = TRUE,
                  selected = c("Paraguay", "Jamaica")),
      
      checkboxGroupInput("year", "Waves",
                         c("2018/19" = "2018/19",
                           "2021" = "2021",
                           "2023" = "2023"),
                         inline = TRUE), 
      
      # Input: Checkbox for whether outliers should be included ----
      checkboxInput("rescale", "Rescale", TRUE),
      
      conditionalPanel("input.rescale",
                       sliderInput("recode", 
                                   "Response values included in rescale?",
                                   min = 0, max = 10, value = c(5, 7))
                       )
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Formatted text for caption ----
      h3(textOutput("caption")),
      
      tabsetPanel(
        # Panel with plot ----
        tabPanel("Histogram", plotOutput("hist")),
        
        # Panel with plot ----
        tabPanel("Time Series", plotOutput("ts")),
        
        # Panel with plot ----
        tabPanel("Cross-Country", plotOutput("cc"))
      )
    )
  )
)


# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
  
  # Compute the formula text ----
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
  
  
  # 
  # Return the formula text for printing as a caption ----
  output$caption <- renderText({
    formulaText()
  })
  
  output$hist <- renderPlot({
    hist_df <- as.data.frame(prop.table(table(haven::as_factor(zap_missing(gm2123[[outcome()]]))))*100)
    names(hist_df) <- c("cat", "prop")
    hist_df$proplabel <- paste(round(hist_df$prop), "%", sep = "")
    lapop_hist(hist_df)
  })
  

  
  output$ts <- renderPlot({
    dta = gm2123 %>%
      group_by(wave) %>%
      summarise_at(vars(outcome()), 
                   list(prop = ~perc_pos(., min = input$recode[1], 
                                         max = input$recode[2]))) %>%
      mutate(proplabel = paste0(round(prop), "%"),
             lb = prop - 2,
             ub = prop + 2)
    lapop_ts(dta)
  })
  
  
  output$cc <- renderPlot({
    dta_cc = gm2123 %>% 
      filter(year == 2023) %>%
      group_by(vallabel = as_factor(pais)) %>%
      summarise_at(vars(outcome()), 
                   list(prop = ~perc_pos(., min = input$recode[1], 
                                         max = input$recode[2]))) %>%
      mutate(proplabel = paste0(round(prop), "%"),
             lb = prop - 2,
             ub = prop + 2)
    lapop_cc(dta_cc, sort = "hi-lo")
  })
  
}


shinyApp(ui, server)

# runApp("C:/Users/plutowl/Documents/GitHub/lapop-shiny/shinyapp")
