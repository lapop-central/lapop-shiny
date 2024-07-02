library(shiny)
library(lapop)
library(haven)
library(srvyr)
library(dplyr)
library(lazyeval)
lapop_fonts_design()

# setwd("C:/Users/plutowl/Documents/GitHub/lapop-shiny")

# gm <- read_dta("C:/Users/plutowl/Desktop/gmr.dta")
# 
# dstrata <- gm %>%
#   as_survey(strata = strata, weights = weight1500)

dstrata <- readRDS("C:/Users/plutowl/Desktop/gmrstrata.rds")

# Define UI for miles per gallon app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("AmericasBarometer Data Playground"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Selector for variable to plot against mpg ----
      selectInput("variable", "Variable:",
                  c("Support for Democracy" = "ing4",
                    "Crime Victimization" = "vic1ext")),
      
      selectInput("pais","Countries",
                  sort(levels(as_factor(dstrata$variables$pais)[!is.na(dstrata$variables$pais)])), 
                  multiple = TRUE,
                  selected = c("Argentina", "Bolivia", "Brazil", "Chile",
                               "Colombia", "Costa Rica", "Dominican Republic",
                               "Ecuador", "El Salvador", "Guatemala", "Haiti",
                               "Honduras", "Jamaica", "Mexico", "Nicaragua", 
                               "Panama", "Paraguay", "Peru", "Uruguay")),
      
      #this fixes a formatting issue with checkboxGroupInput below
      tags$head(
        tags$style(
          HTML(
            ".checkbox-inline { 
                    margin-left: 0px;
                    margin-right: 10px;
          }
         .checkbox-inline+.checkbox-inline {
                    margin-left: 0px;
                    margin-right: 10px;
          }
        "
          )
        ) 
      ),
      
      #this makes slider input only integers
      tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),
      
      checkboxGroupInput("wave", "Waves",
                         c("2004" = "2004",
                           "2006" = "2006",
                           "2008" = "2008",
                           "2010" = "2010",
                           "2012" = "2012",
                           "2014" = "2014",
                           "2016/17" = "2016/17",
                           "2018/19" = "2018/19",
                           "2021" = "2021",
                           "2023" = "2023"),
                         selected = c("2006", "2008", "2010", "2012", "2014",
                                      "2016/17", "2018/19", "2021", "2023"),
                         inline = TRUE), 
      
      # Input: Checkbox for whether outliers should be included ----
      # checkboxInput("rescale", "Dichotomize?", TRUE),
      
      
      # conditionalPanel(
      #   'input.tabs == "Time Series" | input.tabs == "Cross-Country" | input.tabs == "Demographic Breakdown"',
      #   sliderInput("recode",
      #               "Response values included in positive category?",
      #               min = 1,
      #               max = 7,
      #               # round = TRUE,
      #               step = 1,
      #               value = c(5, 7))
      # ),
      
      conditionalPanel(
        'input.tabs == "Time Series" | input.tabs == "Cross-Country" | input.tabs == "Demographic Breakdown"',
        uiOutput("sliderUI"),
      ),
      
      
      # uiOutput("mychoices"),
      
      conditionalPanel(
        'input.tabs == "Additional Breakdown"',
        sliderInput("recode",
                    "Response values included in positive category?",
                    min = 0, max = 7, value = c(5, 7)),
        selectInput("variable_sec", "Secondary Variable:",
                    c("Crime Victimization" = "vic1ext",
                      "Support for Democracy" = "ing4")),
      ),
      
      
      # sliderInput("recode",
      #             "Response values included in positive category?",
      #             min = 0, max = 7, value = c(5, 7)),
      
      actionButton("go", "Refresh")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Formatted text for caption ----
      h3(textOutput("caption")),
      h5(textOutput("wording")),
      
      tabsetPanel(id = "tabs",
        tabPanel("Histogram", plotOutput("hist")),
        
        tabPanel("Time Series", plotOutput("ts")),
        
        tabPanel("Cross-Country", plotOutput("cc")),
        
        tabPanel("Demographic Breakdown", plotOutput("mover")),
        
        tabPanel("Additional Breakdown")
        
        # tabPanel("Additional Breakdown", plotOutput("mover")),
        )
    )
  )
)


# Define server logic to plot various variables against mpg ----
server <- function(input, output, session) {
  
  # Compute the formula text ----
  formulaText <- reactive({
    paste(input$variable)
  })
  
  outcome <- reactive({
      input$variable
  })
  
  
    # observeEvent(input$variable,  {
    #   updateSliderInput(session = session, inputId = "recode",
    #                     min = min(as.numeric(dstrata[['variables']][[formulaText()]]), na.rm=TRUE),
    #                     max = max(as.numeric(dstrata[['variables']][[formulaText()]]), na.rm=TRUE),
    #                     # round = TRUE,
    #                     step = 1,
    #                     value = c(1, 1))
    # })
  
  sliderParams <- reactiveValues(valuex = c(1, 2))

  observeEvent(input$variable, {
    if (max(as.numeric(dstrata[['variables']][[formulaText()]]), na.rm=TRUE) == 7) {
      sliderParams$valuex <- c(5, 7)
    } else if (max(as.numeric(dstrata[['variables']][[formulaText()]]), na.rm=TRUE) == 2) {
      sliderParams$valuex <- c(1, 1)
    }
  })
  
  output$sliderUI <- renderUI({
    sliderInput(inputId = "recode",
                label = "Response values included in positive category?",
                min = min(as.numeric(dstrata[['variables']][[formulaText()]]), na.rm=TRUE), 
                max = max(as.numeric(dstrata[['variables']][[formulaText()]]), na.rm=TRUE), 
                value = sliderParams$valuex,
                step = 1)
  })
  
  # observeEvent(input$variable, {
  #   sliderParams$value <- input$slider
  # })
  


  
  

  
  dff <- eventReactive(input$go, ignoreNULL = FALSE, {
    dstrata %>%
      filter(as_factor(wave) %in% input$wave) %>%
      filter(pais_nam %in% input$pais)
  })
  
  
  # 
  # Return the formula text for printing as a caption ----
  output$caption <- renderText({
    paste("Short question label goes here")
  })
  
  output$wording <- renderText({
    paste("Full question wording goes here")
  })
  


  
  output$hist <- renderPlot({
    hist_df <- dff() %>%
      group_by(across(outcome())) %>%
      summarise(n = unweighted(n()))  %>%
      drop_na() %>%
      rename(cat = 1) %>%
      mutate(prop = prop.table(n) * 100,
             proplabel = paste(round(prop), "%", sep = ""),
             cat = as.character(haven::as_factor(cat)))  
    
     lapop_hist(hist_df, source_info = "LAPOP Lab, AmericasBarometer")
    })
    
  tsd <- eventReactive(input$go, ignoreNULL = FALSE, {
    dta_ts = dff() %>%
      group_by(as.character(as_factor(wave))) %>%
      summarise_at(vars(outcome()),
                   list(prop = ~survey_mean(between(., input$recode[1],
                                                    input$recode[2]),
                                            na.rm = TRUE,
                                            vartype = "ci") * 100)) %>%
      mutate(proplabel = paste0(round(prop), "%")) %>%
      rename(.,  wave = 1, lb = prop_low, ub = prop_upp)
    tsg <- lapop_ts(dta_ts, source_info = "LAPOP Lab, AmericasBarometer",
             subtitle = "% in positive category")
    return(tsg)
  })



  output$ts <- renderPlot({
    tsd()
  })


  ccd <- eventReactive(input$go, ignoreNULL = FALSE, {
    dta_cc = dff() %>%
      group_by(vallabel = pais_lab) %>%
      summarise_at(vars(outcome()),
                   list(prop = ~survey_mean(between(., input$recode[1], 
                                                    input$recode[2]), 
                                            na.rm = TRUE, 
                                            vartype = "ci") * 100)) %>%
      mutate(proplabel = paste0(round(prop), "%")) %>%
      rename(.,  lb = prop_low, ub = prop_upp)
    
    ccg <- lapop_cc(dta_cc, sort = "hi-lo", subtitle = "% in positive category",
             source_info = "LAPOP Lab, AmericasBarometer")
    return(ccg)
  })
  
  output$cc <- renderPlot({
    ccd()
  })
  
  
  # output$cc <- renderPlot({
  # 
  #   dta_cc = dff() %>%
  #     group_by(vallabel = pais_lab) %>%
  #     summarise_at(vars(outcome()),
  #                  list(prop = ~survey_mean(between(., input$recode[1], 
  #                                                   input$recode[2]), 
  #                                           na.rm = TRUE, 
  #                                           vartype = "ci") * 100)) %>%
  #     mutate(proplabel = paste0(round(prop), "%")) %>%
  #     rename(.,  lb = prop_low, ub = prop_upp)
  # 
  #   lapop_cc(dta_cc, sort = "hi-lo", subtitle = "% in positive category",
  #            source_info = "LAPOP Lab, AmericasBarometer")
  # })
  
  moverd <- eventReactive(input$go, ignoreNULL = FALSE, {
    
  dta_mover_ge = dff() %>%
    # filter(as_factor(wave) %in% input$wave) %>%
    # filter(pais_nam %in% input$pais) %>%
    group_by(vallabel = haven::as_factor(zap_missing(gendermc))) %>%
    summarise_at(vars(outcome()),
                 list(prop = ~survey_mean(between(., input$recode[1], 
                                                  input$recode[2]), 
                                          na.rm = TRUE, 
                                          vartype = "ci") * 100)) %>%
    mutate(varlabel = "Gender",
           proplabel = paste0(round(prop), "%")) %>%
    rename(.,  lb = prop_low, ub = prop_upp) %>%
    drop_na(.)

  dta_mover_w = dff() %>%
    # filter(as_factor(wave) %in% input$wave) %>%
    # filter(pais_nam %in% input$pais) %>%
    group_by(vallabel = zap_missing(wealthf)) %>%
    summarise_at(vars(outcome()),
                 list(prop = ~survey_mean(between(., input$recode[1],
                                                  input$recode[2]),
                                          na.rm = TRUE,
                                          vartype = "ci") * 100)) %>%
    mutate(varlabel = "Wealth",
           proplabel = paste0(round(prop), "%")) %>%
    rename(.,  lb = prop_low, ub = prop_upp) %>%
    drop_na(.)

  dta_mover_ed = dff() %>%
    # filter(as_factor(wave) %in% input$wave) %>%
    # filter(pais_nam %in% input$pais) %>%
    group_by(vallabel = zap_missing(edrer)) %>%
    summarise_at(vars(outcome()),
                 list(prop = ~survey_mean(between(., input$recode[1],
                                                  input$recode[2]),
                                          na.rm = TRUE,
                                          vartype = "ci") * 100)) %>%
    mutate(varlabel = "Education",
           proplabel = paste0(round(prop), "%")) %>%
    rename(.,  lb = prop_low, ub = prop_upp) %>%
    drop_na(.)

  dta_mover_ag = dff() %>%
    # filter(as_factor(wave) %in% input$wave) %>%
    # filter(pais_nam %in% input$pais) %>%
    group_by(vallabel = haven::as_factor(zap_missing(edad))) %>%
    summarise_at(vars(outcome()),
                 list(prop = ~survey_mean(between(., input$recode[1],
                                                  input$recode[2]),
                                          na.rm = TRUE,
                                          vartype = "ci") * 100)) %>%
    mutate(varlabel = "Age",
           proplabel = paste0(round(prop), "%")) %>%
    rename(.,  lb = prop_low, ub = prop_upp) %>%
    drop_na(.)

  dta_mover_ur = dff() %>%
    # filter(as_factor(wave) %in% input$wave) %>%
    # filter(pais_nam %in% input$pais) %>%
    group_by(vallabel = haven::as_factor(zap_missing(ur))) %>%
    summarise_at(vars(outcome()),
                 list(prop = ~survey_mean(between(., input$recode[1],
                                                  input$recode[2]),
                                          na.rm = TRUE,
                                          vartype = "ci") * 100)) %>%
    mutate(varlabel = "Place of\nResidence",
           proplabel = paste0(round(prop), "%")) %>%
    rename(.,  lb = prop_low, ub = prop_upp) %>%
    drop_na(.)

  dta_mover <- rbind(dta_mover_ge, dta_mover_ag, dta_mover_w, dta_mover_ed, dta_mover_ur)
  dta_mover$vallabel <- as.character(dta_mover$vallabel)
  
  moverg <- lapop_mover(dta_mover, subtitle = "% in positive category",
              source_info = "LAPOP Lab, AmericasBarometer")
  return(moverg)
  })
  
  
  output$mover <- renderPlot({
    moverd()
  })
  
  # 
  # mover_secd <- eventReactive(input$go, {
  #   
  #   dta_mover_sec = dff() %>%
  #     group_by(vallabel = haven::as_factor(zap_missing(input$variable_sec))) %>%
  #     summarise_at(vars(outcome()),
  #                  list(prop = ~survey_mean(between(., input$recode[1], 
  #                                                   input$recode[2]), 
  #                                           na.rm = TRUE, 
  #                                           vartype = "ci") * 100)) %>%
  #     mutate(varlabel = "Secondary Variable",
  #            proplabel = paste0(round(prop), "%")) %>%
  #     rename(.,  lb = prop_low, ub = prop_upp) %>%
  #     drop_na(.)
  #   
  #   dta_mover_sec$vallabel <- as.character(dta_mover_sec$vallabel)
  #   
  #   dta_mover_secg <- lapop_mover(dta_mover_sec, subtitle = "% in positive category",
  #                         source_info = "LAPOP Lab, AmericasBarometer")
  #   return(dta_mover_secg)
  # })
  # 
  # 
  # output$mover_sec <- renderPlot({
  #   dta_mover_secd()
  # })
  # 
  
  
}


shinyApp(ui, server)

# runApp("C:/Users/plutowl/Documents/GitHub/lapop-shiny/shinyapp")
