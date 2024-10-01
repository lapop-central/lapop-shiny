library(shiny)
library(lapop)
library(haven)
library(srvyr)
library(dplyr)
library(ggplot2)
library(lazyeval)
library(stringr)
library(shinyWidgets)

lapop_fonts_design()

# setwd("C:/Users/plutowl/Documents/GitHub/lapop-shiny")

# gm <- read_dta("C:/Users/plutowl/Desktop/gmr.dta")
#
# dstrata <- gm %>%
#   as_survey(strata = strata, weights = weight1500)

# dstrata <- readRDS("C:/Users/plutowl/Downloads/gmrstrata.rds")


dstrata <- readRDS("data/gmrstrata.rds")
labs <- readRDS("data/labs.rds")
vars_labels <- read.csv("data/new variables for data playground_mr_lap.csv", encoding = "latin1")


Error<-function(x){
  tryCatch(x,error=function(e) return(FALSE))
}

waves_total = c("2004", "2006", "2008", "2010", "2012", "2014", "2016/17", "2018/19", "2021", "2023")


omit_na_edges <- function(df) {
  # Find which rows have NA values
  na_rows <- apply(df, 1, function(row) any(is.na(row)))
  
  # Find the first and last non-NA row
  first_non_na <- which(!na_rows)[1]
  last_non_na <- which(!na_rows)[length(which(!na_rows))]
  
  # Subset the dataframe to only include rows between the first and last non-NA rows
  df_clean <- df[first_non_na:last_non_na, ]
  
  return(df_clean)
}


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
      
      pickerInput(inputId = "pais", 
                  label = "Countries",
                  choices = sort(levels(as_factor(dstrata$variables$pais)[!is.na(dstrata$variables$pais)])),
                  selected = c("Argentina", "Bolivia", "Brazil", "Chile",
                               "Colombia", "Costa Rica", "Dominican Republic",
                               "Ecuador", "El Salvador", "Guatemala", "Haiti",
                               "Honduras", "Jamaica", "Mexico", "Nicaragua", 
                               "Panama", "Paraguay", "Peru", "Uruguay"),
                  options = list(`actions-box` = TRUE),
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
                  options = list(`actions-box` = TRUE),
                  # options = list
                         multiple = TRUE), 
      

      conditionalPanel(
        'input.tabs == "Time Series" | input.tabs == "Cross-Country" | input.tabs == "Breakdown"',
        uiOutput("sliderUI"),
      ),
      
      
      conditionalPanel(
        'input.tabs == "Breakdown"',
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




# Define server logic to plot various variables ----
server <- function(input, output, session) {
  
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
                label = "Response values included in percentage",
                min = min(as.numeric(dstrata[['variables']][[formulaText()]]), na.rm=TRUE), 
                max = max(as.numeric(dstrata[['variables']][[formulaText()]]), na.rm=TRUE), 
                value = sliderParams$valuex,
                step = 1)
  })
  
  

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
    hist_df = Error(
      dff() %>%
      group_by(across(outcome())) %>%
      summarise(n = unweighted(n()))  %>%
      drop_na() %>%
      rename(cat = 1) %>%
      mutate(prop = prop.table(n) * 100,
             proplabel = paste(round(prop), "%", sep = ""),
             cat = str_wrap(as.character(haven::as_factor(cat)), width = 25)))  

    validate(
      need(hist_df, "Error: question was not asked in this country/year combination" )
    )
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
    dta_ts = Error(
      dff() %>%
      group_by(as.character(as_factor(wave))) %>%
      summarise_at(vars(outcome()),
                   list(prop = ~survey_mean(between(., input$recode[1], 
                                                    input$recode[2]),
                                            na.rm = TRUE,
                                            vartype = "ci") * 100)) %>%
      mutate(proplabel = paste0(round(prop), "%")) %>%
      rename(.,  wave = 1, lb = prop_low, ub = prop_upp) %>%
      filter(prop > 0)
    )
    validate(
      need(dta_ts, "Error: question was not asked in this country/year combination" )
    )
    dta_ts = merge(dta_ts, data.frame(wave = as.character(waves_total), empty = 1), by = c("wave"), all.y = TRUE)
    return(omit_na_edges(dta_ts))
  })
  
  tsg <- eventReactive(input$go, ignoreNULL = FALSE, {
    tsg = lapop_ts(tsd(), source_info = "LAPOP Lab, AmericasBarometer",
                       subtitle = "% in selected category")
    return(tsg)
  })
  
  
  output$ts <- renderPlot({
    return(tsg())
  })

#cc 
  ccd <- eventReactive(input$go, ignoreNULL = FALSE, {
    dta_cc = Error(
      dff() %>%
      group_by(vallabel = pais_lab) %>%
      summarise_at(vars(outcome()),
                   list(prop = ~survey_mean(between(., input$recode[1], 
                                                    input$recode[2]), 
                                            na.rm = TRUE, 
                                            vartype = "ci") * 100)) %>%
      filter(prop != 0) %>%
      mutate(proplabel = paste0(round(prop), "%")) %>%
      rename(.,  lb = prop_low, ub = prop_upp)
    )
    validate(
      need(dta_cc, "Error: question was not asked in this country/year combination" )
    )
    return(dta_cc)
  })
  
  ccg <- eventReactive(input$go, ignoreNULL = FALSE, {
    ccg = lapop_cc(ccd(), sort = "hi-lo", subtitle = "% in selected category",
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
      dta_mover_sec = Error(
        dff() %>%
        group_by_at(input$variable_sec) %>%
        summarise_at(vars(outcome()),
                     list(prop = ~survey_mean(between(., input$recode[1], 
                                                      input$recode[2]), 
                                              na.rm = TRUE, 
                                              vartype = "ci") * 100)) %>%
        rename(.,  vallabel = 1, lb = prop_low, ub = prop_upp) %>%
        mutate(vallabel = str_wrap(as.character(as_factor(zap_missing(vallabel))), width = 25),
               varlabel = str_wrap(variable_sec_lab(), width = 25),
               proplabel = paste0(round(prop), "%")) %>%
        drop_na(.)
      )
    }
    return(dta_mover_sec)
  })
  
  
  genderdf <- eventReactive(input$go, ignoreNULL = FALSE, {
    if("gendermc" %in% input$demog) {
      dta_mover_ge = dff() %>%
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
                   list(prop = ~survey_mean(between(., input$recode[1], 
                                                    input$recode[2]),
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
                   list(prop = ~survey_mean(between(., input$recode[1], 
                                                    input$recode[2]),
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
                   list(prop = ~survey_mean(between(., input$recode[1], 
                                                    input$recode[2]),
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
                   list(prop = ~survey_mean(between(., input$recode[1], 
                                                    input$recode[2]),
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
  
  #combine all separate df for demographic variables into one mover df
  moverd <- eventReactive(input$go, ignoreNULL = FALSE, { 
    dta_mover <- Error(rbind(secdf(), genderdf(), edaddf(), wealthdf(), eddf(), urdf()))
    validate(
      need(dta_mover, "Error: question was not asked in this country/year combination" )
    )
    dta_mover$vallabel <- as.character(dta_mover$vallabel)
    return(dta_mover)
  })
  
  moverg <- eventReactive(input$go, ignoreNULL = FALSE, { 
    moverg = lapop_mover(moverd(), subtitle = "% in selected category",
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
}


shinyApp(ui, server)

# runApp("C:/Users/plutowl/Documents/GitHub/lapop-shiny/shinyapp")

