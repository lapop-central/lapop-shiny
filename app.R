library(lapop)
library(haven)
library(dplyr)
library(stringr)
library(shinyWidgets)
library(Hmisc)
library(tidyr)

lapop_fonts()

dstrata <- readRDS("gm_shiny_data.rds")
labs <- readRDS("labs.rds")
vars_labels <- read.csv("variable_labels_shiny.csv", encoding = "latin1")



Error<-function(x){
  tryCatch(x,error=function(e) return(FALSE))
}

waves_total = c("2004", "2006", "2008", "2010", "2012", "2014", "2016/17", "2018/19", "2021", "2023")


#helper function for cleaning ts -- handle missing values at end or middle of series
omit_na_edges <- function(df) {
  # Find which rows have NA values
  na_rows <- apply(df, 1, function(row) any(is.na(row)))
  
  # Find the first and last non-NA row
  first_non_na <- which(!na_rows)[1]
  last_non_na <- which(!na_rows)[length(which(!na_rows))]
  
  # Subset df to only include rows between the first and last non-NA rows
  df_clean <- df[first_non_na:last_non_na, ]
  
  return(df_clean)
}

#custom weighted averages and CIs, to speed up computational speed vs. survey_mean
weighted.ttest.ci <- function(x, weights) {
  nx <- length(x)
  vx <- Hmisc::wtd.var(x, weights, normwt = TRUE, na.rm = TRUE) ## From Hmisc
  mx <- weighted.mean(x, weights, na.rm = TRUE)
  stderr <- sqrt(vx/nx)
  tstat <- mx/stderr ## not mx - mu
  cint <- qt(1 - 0.05/2, nx - 1)
  cint <- tstat + c(-cint, cint)
  confint = cint * stderr
  result = data.frame(prop = mx, lb = confint[1], ub = confint[2])
  return(result)
} 

# helper function for mover
process_data <- function(data, outcome_var, recode_range, group_var, var_label, weight_var = "weight1500") {
  if (is.null(group_var)) {
    return(NULL)
  }
  processed_data <- data %>%
    drop_na(!!sym(outcome_var)) %>%
    mutate(outcome_rec = case_when(
      is.na(!!sym(outcome_var)) ~ NA_real_,
      !!sym(outcome_var) >= recode_range[1] & !!sym(outcome_var) <= recode_range[2] ~ 100,
      TRUE ~ 0
    )) %>%
    group_by(vallabel = haven::as_factor(zap_missing(!!sym(group_var)))) %>%
    summarise_at(vars("outcome_rec"), list(~weighted.ttest.ci(., !!sym(weight_var)))) %>%
    unnest_wider(col = "outcome_rec") %>%
    mutate(
      varlabel = var_label,
      proplabel = paste0(round(prop), "%")
    ) %>%
    drop_na(.)
  
  return(processed_data)
}

ui <- fluidPage(

  titlePanel("AmericasBarometer Data Playground"),
  
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
                       
      
      selectInput("variable", "Variable",
                  labs[order(names(labs))],
                  selected = "ing4"),
      
      pickerInput(inputId = "pais", 
                  label = "Countries",
                  choices = sort(levels(as_factor(dstrata$pais)[!is.na(dstrata$pais)])),
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
      
# show recode slider only for time series, cc, and breakdown (not hist)
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

  #set default slider values - 5-7 for 1-7 variable, 2 for 1-2 variable, 3-4 for 1-4 variable, etc.
  observeEvent(input$variable, {
    if (max(as.numeric(dstrata[[formulaText()]]), na.rm=TRUE) == 7) {
      sliderParams$valuex <- c(5, 7)
    } else if (max(as.numeric(dstrata[[formulaText()]]), na.rm=TRUE) == 2) {
      sliderParams$valuex <- c(2, 2)
    } else if (max(as.numeric(dstrata[[formulaText()]]), na.rm=TRUE) == 3) {
      sliderParams$valuex <- c(3, 3)
    } else if (max(as.numeric(dstrata[[formulaText()]]), na.rm=TRUE) == 4) {
      sliderParams$valuex <- c(3, 4)
    } else if (max(as.numeric(dstrata[[formulaText()]]), na.rm=TRUE) == 5) {
      sliderParams$valuex <- c(4, 5)
    } else if (max(as.numeric(dstrata[[formulaText()]]), na.rm=TRUE) == 10) {
      sliderParams$valuex <- c(7, 10)
    }
  })
  
  output$sliderUI <- renderUI({
    sliderInput(inputId = "recode",
                label = "Response values included in percentage",
                min = min(as.numeric(dstrata[[formulaText()]]), na.rm=TRUE), 
                max = max(as.numeric(dstrata[[formulaText()]]), na.rm=TRUE), 
                value = sliderParams$valuex,
                step = 1)
  })
  
  

  dff <- eventReactive(input$go, ignoreNULL = FALSE, {
    dstrata %>%
      filter(as_factor(wave) %in% input$wave) %>%
      filter(pais_nam %in% input$pais)
  })  

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
      summarise(n = n())  %>%
      drop_na() %>%
      rename(cat = 1) %>%
      mutate(prop = prop.table(n) * 100,
             proplabel = paste(round(prop), "%", sep = ""),
             cat = str_wrap(as.character(haven::as_factor(cat)), width = 25)))
  
  validate(
    need(hist_df, "Error: no data available. Please verify that this question was asked in this country/year combination")
  )
  return(hist_df)
  })

  
  histg <- eventReactive(input$go, ignoreNULL = FALSE, {
    histg <- lapop_hist(histd(), 
                        source_info = ", AmericasBarometer Data Playground")
    return(histg)
  })

  output$hist <- renderPlot({
    return(histg())
  })

  
  #ts
  tsd <- eventReactive(input$go, ignoreNULL = FALSE, {
    dta_ts = Error(
      dff() %>%
        drop_na(outcome()) %>%
        mutate(outcome_rec = case_when(
          is.na(!!sym(outcome())) ~ NA_real_,
          !!sym(outcome()) >= input$recode[1] &
            !!sym(outcome()) <= input$recode[2] ~ 100,
          TRUE ~ 0)) %>%
        group_by(as.character(as_factor(wave))) %>%
        summarise_at(vars("outcome_rec"),
                     list(~weighted.ttest.ci(., weight1500))) %>%
        unnest_wider(col = "outcome_rec") %>%
        mutate(proplabel = paste0(round(prop), "%")) %>%
        rename(.,  wave = 1) %>%
        filter(prop != 0) 
    )
    validate(
      need(dta_ts, "Error: no data available. Please verify that this question was asked in this country/year combination")
    )
    dta_ts = merge(dta_ts, data.frame(wave = as.character(waves_total), empty = 1), by = "wave", all.y = TRUE)
    return(omit_na_edges(dta_ts))
  })
  
  tsg <- eventReactive(input$go, ignoreNULL = FALSE, {
    tsg = lapop_ts(tsd(), source_info = ", AmericasBarometer Data Playground",
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
        drop_na(outcome()) %>%
        mutate(outcome_rec = case_when(
          is.na(!!sym(outcome())) ~ NA_real_,
          !!sym(outcome()) >= input$recode[1] &
          !!sym(outcome()) <= input$recode[2] ~ 100,
          TRUE ~ 0)) %>%
        group_by(vallabel = pais_lab) %>%
        summarise_at(vars("outcome_rec"),
                     list(~weighted.ttest.ci(., weight1500))) %>%
        unnest_wider(col = "outcome_rec") %>%
        filter(prop != 0) %>%
        mutate(proplabel = paste0(round(prop), "%"))
    )
    validate(
      need(dta_cc, "Error: no data available. Please verify that this question was asked in this country/year combination")
    )
    return(dta_cc)
  })
  
  ccg <- eventReactive(input$go, ignoreNULL = FALSE, {
    ccg = lapop_cc(ccd(), sort = "hi-lo", subtitle = "% in selected category",
             source_info = ", AmericasBarometer Data Playground")
    return(ccg)
  })

  output$cc <- renderPlot({
    return(ccg())
  })

  # Use function for each demographic breakdown variable
  secdf <- eventReactive(input$go, ignoreNULL = FALSE, {
    if (input$variable_sec == "None") {
      NULL
    } else {
      process_data(
        data = dff(),
        outcome_var = outcome(),
        recode_range = input$recode,
        group_var = input$variable_sec,
        var_label = str_wrap(variable_sec_lab(), width = 25)
      )
    }
  })
  
  genderdf <- eventReactive(input$go, ignoreNULL = FALSE, {
    if ("gendermc" %in% input$demog) {
      process_data(
        data = dff(),
        outcome_var = outcome(),
        recode_range = input$recode,
        group_var = "gendermc",
        var_label = "Gender"
      )
    } else {
      NULL
    }
  })
  
  wealthdf <- eventReactive(input$go, ignoreNULL = FALSE, {
    if ("wealth" %in% input$demog) {
      process_data(
        data = dff(),
        outcome_var = outcome(),
        recode_range = input$recode,
        group_var = "wealthf",
        var_label = "Wealth"
      )
    } else {
      NULL
    }
  })
  
  eddf <- eventReactive(input$go, ignoreNULL = FALSE, {
    if ("edre" %in% input$demog) {
      process_data(
        data = dff(),
        outcome_var = outcome(),
        recode_range = input$recode,
        group_var = "edrerf",
        var_label = "Education"
      )
    } else {
      NULL
    }
  })
  
  edaddf <- eventReactive(input$go, ignoreNULL = FALSE, {
    if ("edad" %in% input$demog) {
      process_data(
        data = dff(),
        outcome_var = outcome(),
        recode_range = input$recode,
        group_var = "edad",
        var_label = "Age"
      )
    } else {
      NULL
    }
  })
  
  urdf <- eventReactive(input$go, ignoreNULL = FALSE, {
    if ("ur" %in% input$demog) {
      process_data(
        data = dff(),
        outcome_var = outcome(),
        recode_range = input$recode,
        group_var = "ur",
        var_label = "Place of\nResidence"
      )
    } else {
      NULL
    }
  })
  
  # Combine =demographic data frames into one df
  moverd <- eventReactive(input$go, ignoreNULL = FALSE, {
    dta_mover <- Error(rbind(secdf(), genderdf(), edaddf(), wealthdf(), eddf(), urdf()))
    validate(
      need(dta_mover, "Error: no data available. Please verify that this question was asked in this country/year combination")
    )
    dta_mover$vallabel <- as.character(dta_mover$vallabel)
    return(dta_mover)
  })
  
  moverg <- eventReactive(input$go, ignoreNULL = FALSE, {
    moverg <- lapop_mover(moverd(), subtitle = "% in selected category", source_info = ", AmericasBarometer Data Playground")
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



