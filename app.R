library(shiny)
library(lapop)
library(haven)
library(srvyr)
library(dplyr)
library(ggplot2)
library(lazyeval)
library(stringr)

library("shinyWidgets")
lapop_fonts_design()

# setwd("C:/Users/plutowl/Documents/GitHub/lapop-shiny")

# gm <- read_dta("C:/Users/plutowl/Desktop/gmr.dta")
# 
# dstrata <- gm %>%
#   as_survey(strata = strata, weights = weight1500)

dstrata <- readRDS("data/gmrstrata.rds")
labs <- readRDS("data/labs.rds")
vars_labels <- read.csv("data/new variables for data playground_mr_lap.csv", encoding = "latin1")

# Define UI for miles per gallon app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("AmericasBarometer Data Playground"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
                       
      
      # Input: Selector for variable to plot against mpg ----
      selectInput("variable", "Variable",
                  labs[order(names(labs))],
                  selected = "ing4"),
      
      # selectInput("pais","Countries",
      #             sort(levels(as_factor(dstrata$variables$pais)[!is.na(dstrata$variables$pais)])), 
      #             multiple = TRUE,
      #             selected = c("Argentina", "Bolivia", "Brazil", "Chile",
      #                          "Colombia", "Costa Rica", "Dominican Republic",
      #                          "Ecuador", "El Salvador", "Guatemala", "Haiti",
      #                          "Honduras", "Jamaica", "Mexico", "Nicaragua", 
      #                          "Panama", "Paraguay", "Peru", "Uruguay")),
      
      pickerInput(inputId = "pais", 
                  label = "Countries",
                  choices = sort(levels(as_factor(dstrata$variables$pais)[!is.na(dstrata$variables$pais)])),
                  selected = c("Argentina", "Bolivia", "Brazil", "Chile",
                               "Colombia", "Costa Rica", "Dominican Republic",
                               "Ecuador", "El Salvador", "Guatemala", "Haiti",
                               "Honduras", "Jamaica", "Mexico", "Nicaragua", 
                               "Panama", "Paraguay", "Peru", "Uruguay"),
                  options = list(`actions-box` = TRUE
                                 # `selected-text-format` = "count > 2",
                                 # `count-selected-text` = "{0}/{1} fruits"
                  ),
                  # options = list
                  multiple = TRUE), 
      
      
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
      
      # checkboxGroupInput("wave", "Survey Round(s)",
      #                    choices = c("2004" = "2004",
      #                      "2006" = "2006",
      #                      "2008" = "2008",
      #                      "2010" = "2010",
      #                      "2012" = "2012",
      #                      "2014" = "2014",
      #                      "2016/17" = "2016/17",
      #                      "2018/19" = "2018/19",
      #                      "2021" = "2021",
      #                      "2023" = "2023"),
      #                    selected = c("2006", "2008", "2010", "2012", "2014",
      #                                 "2016/17", "2018/19", "2021", "2023"),
      #                    inline = TRUE), 
      
      
      pickerInput(inputId = "wave", 
                  label = "Survey Rounds",
                  choices = c("2004" = "2004",
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
                  options = list(`actions-box` = TRUE
                                 # `selected-text-format` = "count > 2",
                                 # `count-selected-text` = "{0}/{1} fruits"
                                 ),
                  # options = list
                         multiple = TRUE), 
      
      # checkboxInput('all', "Select All/None", value = FALSE),
      
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
        'input.tabs == "Time Series" | input.tabs == "Cross-Country" | input.tabs == "Breakdown"',
        uiOutput("sliderUI"),
      ),
      
      
      # uiOutput("mychoices"),
      # c("None" = "None",
      #   "Crime Victimization" = "vic1ext",
      #   "Support for Democracy" = "ing4")
      conditionalPanel(
        'input.tabs == "Breakdown"',
        # uiOutput("sliderUI"),
        selectInput("variable_sec", "Secondary Variable",
                    c("None" = "None",
                      labs[order(names(labs))])),
        checkboxGroupInput("demog", "Demographic Variables",
                           c("Gender" = "gendermc",
                             "Age" = "edad",
                             "Wealth" = "wealth",
                             "Education" = "edre",
                             "Urban/Rural" = "ur"),
                           selected = c("gendermc", "edad", "edre"),
                           inline = TRUE)
      ),
    
      actionButton("go", "Generate")
      
      # actionButton("reset_input", "Reset inputs")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Formatted text for caption ----
      h3(textOutput("caption")),
      h5(textOutput("wording")),
      h5(textOutput("response")),
      
      tabsetPanel(id = "tabs",
        tabPanel("Histogram", plotOutput("hist")),
        
        tabPanel("Time Series", plotOutput("ts")),
        
        tabPanel("Cross-Country", plotOutput("cc")),
        
        tabPanel("Breakdown", plotOutput("mover"))
        ),
      br(),
      fluidRow(column(12, "",
                      downloadButton(outputId = "downloadPlot", label = "Download Figure"),
                      downloadButton(outputId = "downloadTable", label = "Download Table")))
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
  
  variable_sec <- reactive({
    input$variable_sec
  })
  
  variable_sec_lab <- reactive({
    vars_labels$question_short_en[which(vars_labels$column_name == paste(variable_sec()))]
  })
  
  sliderParams <- reactiveValues(valuex = c(1, 1))

  observeEvent(input$variable, {
    if (max(as.numeric(dstrata[['variables']][[formulaText()]]), na.rm=TRUE) == 7) {
      sliderParams$valuex <- c(5, 7)
    } else if (max(as.numeric(dstrata[['variables']][[formulaText()]]), na.rm=TRUE) == 2) {
      sliderParams$valuex <- c(2, 2)
    } else if (max(as.numeric(dstrata[['variables']][[formulaText()]]), na.rm=TRUE) == 4) {
      sliderParams$valuex <- c(3, 4)
    } else if (max(as.numeric(dstrata[['variables']][[formulaText()]]), na.rm=TRUE) == 5) {
      sliderParams$valuex <- c(4, 5)
    } else if (max(as.numeric(dstrata[['variables']][[formulaText()]]), na.rm=TRUE) == 10) {
      sliderParams$valuex <- c(7, 10)
    }
  })
  
  output$sliderUI <- renderUI({
    sliderInput(inputId = "recode",
                label = "Response values included in affirmative category?",
                min = min(as.numeric(dstrata[['variables']][[formulaText()]]), na.rm=TRUE), 
                max = max(as.numeric(dstrata[['variables']][[formulaText()]]), na.rm=TRUE), 
                value = sliderParams$valuex,
                step = 1)
  })
  
  
  # observeEvent(input$reset_input, {
  #   shinyjs::reset("side-panel")
  # })
  
  # observeEvent(input$reset_input, {
  #   updateSelectInput(session, "pais", selected = NA)
  #   updateSelectInput(session, "wave", selected = NA)
  #   updateSelectInput(session, "recode", selected = c(1, 1))
  #   
  #       
  #       # updateNumericInput(session, "y","Y", NA)
  # })
  # 
  # observe({
  #   updateCheckboxGroupInput(
  #     session, 'wave', choices = myChoices,
  #     selected = if(input$all) myChoices
  #   )
  # })
  
  dff <- eventReactive(input$go, ignoreNULL = FALSE, {
    dstrata %>%
      filter(as_factor(wave) %in% input$wave) %>%
      filter(pais_nam %in% input$pais)
  })
  
  # Return the formula text for printing as a caption ----
  cap <- renderText({
    vars_labels$question_short_en[which(vars_labels$column_name == formulaText())]
  })
  
  output$caption <- eventReactive(input$go, ignoreNULL = FALSE, {
    cap() 
  })
  
  word <- renderText({
    vars_labels$question_en[which(vars_labels$column_name == formulaText())]
  })
  
  output$wording <- eventReactive(input$go, ignoreNULL = FALSE, {
    word() 
  })
  
  resp <- renderText({
    vars_labels$responses_en_rec[which(vars_labels$column_name == formulaText())]
  })
  
  output$response <- eventReactive(input$go, ignoreNULL = FALSE, {
    resp() 
  })

    
#hist 
# must break into data event, graph event, and renderPlot to get download buttons to work
  histd <- eventReactive(input$go, ignoreNULL = FALSE, {
    hist_df = dff() %>%
      group_by(across(outcome())) %>%
      summarise(n = unweighted(n()))  %>%
      drop_na() %>%
      rename(cat = 1) %>%
      mutate(prop = prop.table(n) * 100,
             proplabel = paste(round(prop), "%", sep = ""),
             cat = str_wrap(as.character(haven::as_factor(cat)), width = 25))  
    return(hist_df)
    })

  histg <- eventReactive(input$go, ignoreNULL = FALSE, {
    histg <- lapop_hist(histd(), source_info = "LAPOP Lab, AmericasBarometer")
    return(histg)
  })
  
  output$hist <- renderPlot({
    return(histg())
  })
  
  
  #ts
  tsd <- eventReactive(input$go, ignoreNULL = FALSE, {
    dta_ts = dff() %>%
      group_by(as.character(as_factor(wave))) %>%
      summarise_at(vars(outcome()),
                   list(prop = ~survey_mean(between(., sliderParams$valuex[1],
                                                    sliderParams$valuex[2]),
                                            na.rm = TRUE,
                                            vartype = "ci") * 100)) %>%
      mutate(proplabel = paste0(round(prop), "%")) %>%
      rename(.,  wave = 1, lb = prop_low, ub = prop_upp) %>%
      filter(prop > 0)
    return(dta_ts)
  })
  
  # tsg <- eventReactive(input$go, ignoreNULL = FALSE, {
  #   dta_ts = dff() %>%
  #     group_by(as.character(as_factor(wave))) %>%
  #     summarise_at(vars(outcome()),
  #                  list(prop = ~survey_mean(between(., sliderParams$valuex[1],
  #                                                   sliderParams$valuex[2]),
  #                                           na.rm = TRUE,
  #                                           vartype = "ci") * 100)) %>%
  #     mutate(proplabel = paste0(round(prop), "%")) %>%
  #     rename(.,  wave = 1, lb = prop_low, ub = prop_upp) %>%
  #     filter(prop > 0)
  #   return(dta_ts)
  # })
  
  tsg <- eventReactive(input$go, ignoreNULL = FALSE, {
    tsg = lapop_ts(tsd(), source_info = "LAPOP Lab, AmericasBarometer",
                       subtitle = "% in affirmative category")
    return(tsg)
  })
  
  
  output$ts <- renderPlot({
    return(tsg())
  })

#cc 
  ccd <- eventReactive(input$go, ignoreNULL = FALSE, {
    dta_cc = dff() %>%
      group_by(vallabel = pais_lab) %>%
      summarise_at(vars(outcome()),
                   list(prop = ~survey_mean(between(., sliderParams$valuex[1],
                                                    sliderParams$valuex[2]), 
                                            na.rm = TRUE, 
                                            vartype = "ci") * 100)) %>%
      mutate(proplabel = paste0(round(prop), "%")) %>%
      rename(.,  lb = prop_low, ub = prop_upp)
    return(dta_cc)
  })
  
  ccg <- eventReactive(input$go, ignoreNULL = FALSE, {
    ccg = lapop_cc(ccd(), sort = "hi-lo", subtitle = "% in affirmative category",
             source_info = "LAPOP Lab, AmericasBarometer")
    return(ccg)
  })
  
  output$cc <- renderPlot({
    return(ccg())
  })
  
  
  #mover plot
  secdf <- eventReactive(input$go, ignoreNULL = FALSE, {
    if(input$variable_sec == "None") {
      dta_mover_sec = NULL
    } else {
      dta_mover_sec = dff() %>%
        group_by_at(input$variable_sec) %>%
        summarise_at(vars(outcome()),
                     list(prop = ~survey_mean(between(., sliderParams$valuex[1],
                                                      sliderParams$valuex[2]), 
                                              na.rm = TRUE, 
                                              vartype = "ci") * 100)) %>%
        rename(.,  vallabel = 1, lb = prop_low, ub = prop_upp) %>%
        mutate(vallabel = str_wrap(as.character(as_factor(zap_missing(vallabel))), width = 25),
               varlabel = str_wrap(variable_sec_lab(), width = 25),
               proplabel = paste0(round(prop), "%")) %>%
        drop_na(.)
    }
    return(dta_mover_sec)
  })
  
  
  genderdf <- eventReactive(input$go, ignoreNULL = FALSE, {
    if("gendermc" %in% input$demog) {
      dta_mover_ge = dff() %>%
        group_by(vallabel = haven::as_factor(zap_missing(gendermc))) %>%
        summarise_at(vars(outcome()),
                     list(prop = ~survey_mean(between(., sliderParams$valuex[1],
                                                      sliderParams$valuex[2]), 
                                              na.rm = TRUE, 
                                              vartype = "ci") * 100)) %>%
        mutate(varlabel = "Gender",
               proplabel = paste0(round(prop), "%")) %>%
        rename(.,  lb = prop_low, ub = prop_upp) %>%
        drop_na(.)
    }
    else {
      dta_mover_ge = NULL
    }
      return(dta_mover_ge)
    })
  
  wealthdf <- eventReactive(input$go, ignoreNULL = FALSE, {
    if("wealth" %in% input$demog) {
    dta_mover_w = dff() %>%
      group_by(vallabel = zap_missing(wealthf)) %>%
      summarise_at(vars(outcome()),
                   list(prop = ~survey_mean(between(., sliderParams$valuex[1],
                                                    sliderParams$valuex[2]),
                                            na.rm = TRUE,
                                            vartype = "ci") * 100)) %>%
      mutate(varlabel = "Wealth",
             proplabel = paste0(round(prop), "%")) %>%
      rename(.,  lb = prop_low, ub = prop_upp) %>%
      drop_na(.)
    }
    else {
      dta_mover_w = NULL
    }
    return(dta_mover_w)
  })
  
  
  eddf <- eventReactive(input$go, ignoreNULL = FALSE, {
    if("edre" %in% input$demog) {
    dta_mover_ed = dff() %>%
      group_by(vallabel = zap_missing(edrerf)) %>%
      summarise_at(vars(outcome()),
                   list(prop = ~survey_mean(between(., sliderParams$valuex[1],
                                                    sliderParams$valuex[2]),
                                            na.rm = TRUE,
                                            vartype = "ci") * 100)) %>%
      mutate(varlabel = "Education",
             proplabel = paste0(round(prop), "%")) %>%
      rename(.,  lb = prop_low, ub = prop_upp) %>%
      drop_na(.)
    }
    else {
      dta_mover_ed = NULL
    }
    return(dta_mover_ed)
    
  })

  edaddf <- eventReactive(input$go, ignoreNULL = FALSE, {
    if("edad" %in% input$demog) {
    dta_mover_ag = dff() %>%
      group_by(vallabel = haven::as_factor(zap_missing(edad))) %>%
      summarise_at(vars(outcome()),
                   list(prop = ~survey_mean(between(., sliderParams$valuex[1],
                                                    sliderParams$valuex[2]),
                                            na.rm = TRUE,
                                            vartype = "ci") * 100)) %>%
      mutate(varlabel = "Age",
             proplabel = paste0(round(prop), "%")) %>%
      rename(.,  lb = prop_low, ub = prop_upp) %>%
      drop_na(.)     
    }
    else {
      dta_mover_ag = NULL
    }
    return(dta_mover_ag)
  })
  
  urdf <- eventReactive(input$go, ignoreNULL = FALSE, {
    if("ur" %in% input$demog) {
      dta_mover_ur = dff() %>%
      group_by(vallabel = haven::as_factor(zap_missing(ur))) %>%
      summarise_at(vars(outcome()),
                   list(prop = ~survey_mean(between(., sliderParams$valuex[1],
                                                    sliderParams$valuex[2]),
                                            na.rm = TRUE,
                                            vartype = "ci") * 100)) %>%
      mutate(varlabel = "Place of\nResidence",
             proplabel = paste0(round(prop), "%")) %>%
      rename(.,  lb = prop_low, ub = prop_upp) %>%
      drop_na(.)
    }
    else {
      dta_mover_ur = NULL
    }
    return(dta_mover_ur)
  })
  
  
  
  
  moverd <- eventReactive(input$go, ignoreNULL = FALSE, { 
    dta_mover <- rbind(secdf(), genderdf(), edaddf(), wealthdf(), eddf(), urdf())
    dta_mover$vallabel <- as.character(dta_mover$vallabel)
    return(dta_mover)
  })
  
  moverg <- eventReactive(input$go, ignoreNULL = FALSE, { 
    moverg = lapop_mover(moverd(), subtitle = "% in affirmative category",
                source_info = "LAPOP Lab, AmericasBarometer")
    return(moverg)
  })
  
  output$mover <- renderPlot({
    return(moverg())
  })
  
  output$downloadPlot <- downloadHandler(
    filename = function(file) {
      ifelse(input$tabs == "Histogram", "hist.svg", 
             ifelse(input$tabs == "Time Series", "ts.svg", 
                    ifelse(input$tabs == "Cross-Country", "cc.svg", "mover.svg")))
    },
    content = function(file) {
      if(input$tabs == "Histogram") {
        lapop_save(histg(), file)
      } else if (input$tabs == "Time Series") {
        lapop_save(tsg(), file)
      } else if (input$tabs == "Cross Country") {
        lapop_save(ccg(), file)
      } else {
        lapop_save(moverg(), file)
      }
    }
  )
  
  
  output$downloadTable <- downloadHandler(
    filename = function(file) {
      ifelse(input$tabs == "Histogram", "hist.csv", 
             ifelse(input$tabs == "Time Series", "ts.csv", 
                    ifelse(input$tabs == "Cross-Country", "cc.csv", "mover.csv")))
    },
    content = function(file) {
      if(input$tabs == "Histogram") {
        write.csv(histd(), file)
      } else if (input$tabs == "Time Series") {
        write.csv(tsd(), file)
      } else if (input$tabs == "Cross Country") {
        write.csv(ccd(), file)
      } else {
        write.csv(moverd(), file)
      }
    }
  )
  
  

  
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
