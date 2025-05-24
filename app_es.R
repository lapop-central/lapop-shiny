library(lapop)
library(haven)
library(dplyr)
library(tidyr)
library(shiny)
library(stringr)
library(shinyWidgets)
library(Hmisc)

lapop_fonts()

dstrata <- readRDS("gm_shiny_data_es.rds")
labs <- readRDS("labs_es.rds")
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
  
  titlePanel(""), # Leave it Empty
  
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      width = 3, # Reduce width (default is 4)
      
      selectInput("variable", "Variable de interés",
                  labs[order(names(labs))],
                  selected = "ing4"),
      
      pickerInput(inputId = "pais", 
                  label = "Países",
                  choices = sort(levels(as_factor(dstrata$pais)[!is.na(dstrata$pais)])),
                  selected = c("Argentina", "Bolivia", "Brasil", "Chile",
                               "Colombia", "Costa Rica", "República Dominicana",
                               "Ecuador", "El Salvador", "Guatemala", "Haití",
                               "Honduras", "Jamaica", "México", "Nicaragua", 
                               "Panamá", "Paraguay", "Perú", "Uruguay"),
                  options = list(`actions-box` = TRUE,
                                 `select-all-text` = "Seleccionar todos",
                                 `deselect-all-text` = "Deseleccionar todos",
                                 `none-selected-text` = "Nada seleccionado"),
                  multiple = TRUE), 
      
      # This fixes a formatting issue with checkboxGroupInput below
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
      
      # This triggers the "Generate" button
      tags$script(HTML("
      Shiny.addCustomMessageHandler('clickGenerateButton', function(message) {
    $('#go').click();
  });
")),
      
      
      # This makes slider input only integers
      tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),
      
      
      
      pickerInput(inputId = "wave", 
                  label = "Ronda de encuesta",
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
                  options = list(`actions-box` = TRUE,
                                 `select-all-text` = "Seleccionar todos",
                                 `deselect-all-text` = "Deseleccionar todos",
                                 `none-selected-text` = "Nada seleccionado"),
                  multiple = TRUE),
      
      # show recode slider only for time series, cc, and breakdown (not hist)
      conditionalPanel(
        'input.tabs == "Serie temporal" | input.tabs == "Comparativo" | input.tabs == "Desglose"',
        uiOutput("sliderUI"),
      ),
      
      
      conditionalPanel(
        'input.tabs == "Desglose"',
        selectInput("variable_sec", "Variable Secundaria",
                    c("Ninguna" = "None",
                      labs[order(names(labs))])),
        checkboxGroupInput("demog", "Variables Demográficas",
                           c("Género" = "gendermc",
                             "Edad" = "edad",
                             "Riqueza" = "wealth",
                             "Educación" = "edre",
                             "Urbano/Rural" = "ur"),
                           selected = c("gendermc", "edad", "edre"),
                           inline = TRUE)
      ),
      
      actionButton("go", "Generar")
      
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Formatted text for caption ----
      h3(textOutput("caption")),
      h5(textOutput("wording")),
      h5(textOutput("response")),
      
      tabsetPanel(id = "tabs",
                  tabPanel("Histograma", plotOutput("hist")),
                  
                  tabPanel("Serie temporal", plotOutput("ts")),
                  
                  tabPanel("Comparativo", plotOutput("cc")),
                  
                  tabPanel("Desglose", plotOutput("mover"))
      ),
      br(),
      fluidRow(column(12, "",
                      downloadButton(outputId = "downloadPlot", label = "Descargar gráfico"),
                      downloadButton(outputId = "downloadTable", label = "Descargar tabla")))
    )
  )
)




# Define server logic to plot various variables ----
server <- function(input, output, session) {
  
  # Triggers "go" between server and ui to generate default plots
  observe({
    if (!is.null(input$pais) && !is.null(input$wave)) {
      isolate({
        session$sendCustomMessage("clickGenerateButton", list())
      })
    }
  })
  
  # Triggers warning for mover plot
  observeEvent(input$demog, {
    # Check the number of selected variables
    if (length(input$demog) > 3 & input$variable_sec != "None") {
      # Show a warning message
      showNotification(HTML("Debes seleccionar un máximo de 3 variables demográficas para graficar."), type = "warning")
    }
  })
  
  # # -----------------------------------------------------------------------
  
  formulaText <- reactive({
    paste(input$variable)
  })
  
  outcome <- reactive({
    input$variable
  })
  
  outcome_code <- reactive({
    vars_labels$column_name[which(vars_labels$column_name == paste(outcome()))]
  })
  
  variable_sec <- reactive({
    input$variable_sec
  })
  
  variable_sec_lab <- reactive({
    vars_labels$question_short_es[which(vars_labels$column_name == paste(variable_sec()))]
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
                label = "Valores de respuesta de la variable de interés en porcentaje",
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
    vars_labels$question_short_es[which(vars_labels$column_name == formulaText())]
  })
  
  output$caption <- eventReactive(input$go, ignoreNULL = FALSE, {
    cap() 
  })
  
  word <- renderText({
    paste0(toupper(vars_labels$column_name[which(vars_labels$column_name == formulaText())]), ". ",
           vars_labels$question_es[which(vars_labels$column_name == formulaText())])
  })
  
  output$wording <- eventReactive(input$go, ignoreNULL = FALSE, {
    word() 
  })
  
  resp <- renderText({
    vars_labels$responses_es_rec[which(vars_labels$column_name == formulaText())]
  })
  
  output$response <- eventReactive(input$go, ignoreNULL = FALSE, {
    resp() 
  })
  
  slider_values <- renderText({
    if(input$recode[1] == input$recode[2]) {
      paste0("(valor: ", unique(input$recode), ")")
    } else {
      paste0("(rango: ", paste(input$recode, collapse = " a "), ")")
    }
  })
  
  output$selected_values <- eventReactive(input$go, ignoreNULL = FALSE, {
    slider_values()
  })
  
  
  # SOURCE INFO WITH PAIS and WAVE
  # # -----------------------------------------------------------------------
  source_info_both <- reactive({
    # Get country abbreviations that match selected country names
    pais_abbr <- dstrata %>%
      filter(pais_nam %in% input$pais) %>%
      distinct(pais_nam, pais_lab) %>%
      arrange(match(pais_nam, input$pais)) %>%  # preserve input order
      pull(pais_lab)
    
    pais_display <- paste(pais_abbr, collapse = ", ")
    wave_display <- paste(input$wave, collapse = ", ")
    
    if (nchar(pais_display) > 15) {
      paste0("Fuente: LAPOP Lab, Barómetro de las Américas Data Playground\n\nPaíses: ", pais_display, 
             "\nRondas: ", wave_display)
      
    } else {
      paste0("Fuente: LAPOP Lab, Barómetro de las Américas Data Playground\n\nPaíses: ", pais_display, 
             ". Rondas: ", wave_display)
    }
  })
  
  source_info_pais <- reactive({
    # Get country abbreviations that match selected country names
    pais_abbr <- dstrata %>%
      filter(pais_nam %in% input$pais) %>%
      distinct(pais_nam, pais_lab) %>%
      arrange(match(pais_nam, input$pais)) %>%  # preserve input order
      pull(pais_lab)
    
    pais_display <- paste(pais_abbr, collapse = ", ")
    wave_display <- paste(input$wave, collapse = ", ")
    
    paste0("Fuente: LAPOP Lab, Barómetro de las Américas Data Playground\n\nPaíses: ", pais_display)
  })
  
  source_info_wave <- reactive({
    # Get country abbreviations that match selected country names
    pais_abbr <- dstrata %>%
      filter(pais_nam %in% input$pais) %>%
      distinct(pais_nam, pais_lab) %>%
      arrange(match(pais_nam, input$pais)) %>%  # preserve input order
      pull(pais_lab)
    
    pais_display <- paste(pais_abbr, collapse = ", ")
    wave_display <- paste(input$wave, collapse = ", ")
    
    paste0("Fuente: LAPOP Lab, Barómetro de las Américas Data Playground\n\nRondas: ", wave_display)
  })
  
  # HISTOGRAM
  # # -----------------------------------------------------------------------
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
      need(hist_df, "Error: no hay datos disponibles. Verifique que esta pregunta se haya realizado en esta combinación de país/año.")
    )
    return(hist_df)
  })
  
  
  histg <- eventReactive(input$go, ignoreNULL = FALSE, {
    histg <- lapop_hist(histd(), 
                        ymax = ifelse(any(histd()$prop > 90), 110, 100), 
                        lang = "es",
                        source_info = source_info_both())
    return(histg)
  })
  
  output$hist <- renderPlot({
    return(histg())
  })
  
  
  # TIME SERIES
  # # -----------------------------------------------------------------------
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
      need(dta_ts, "Error: no hay datos disponibles. Verifique que esta pregunta se haya realizado en esta combinación de país/año.")
    )
    dta_ts = merge(dta_ts, data.frame(wave = waves_total), by = "wave", all.y = TRUE)
    return(omit_na_edges(dta_ts))
  })
  
  tsg <- eventReactive(input$go, ignoreNULL = FALSE, {
    tsg = lapop_ts(tsd(), 
                   ymax = ifelse(any(tsd()$prop > 88, na.rm = TRUE), 110, 100),
                   label_vjust = ifelse(any(tsd()$prop > 80, na.rm = TRUE), -1.1, -1.5),
                   source_info = source_info_pais(),
                   lang = "es",
                   subtitle = "% en la categoría seleccionada")
    return(tsg)
  })
  
  
  output$ts <- renderPlot({
    return(tsg())
  })
  
  # CROSS COUNTRY
  # # -----------------------------------------------------------------------
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
      need(dta_cc, "Error: no hay datos disponibles. Verifique que esta pregunta se haya realizado en esta combinación de país/año.")
    )
    return(dta_cc)
  })
  
  ccg <- eventReactive(input$go, ignoreNULL = FALSE, {
    ccg = lapop_cc(ccd(), sort = "hi-lo", 
                   subtitle = "% en la categoría seleccionada",
                   ymax = ifelse(any(ccd()$prop > 90, na.rm = TRUE), 110, 100),
                   lang = "es",
                   source_info = source_info_wave())
    return(ccg)
  })
  
  output$cc <- renderPlot({
    return(ccg())
  })
  
  # BREAK DOWN Use function for each demographic Desglose variable
  # # -----------------------------------------------------------------------
  secdf <- eventReactive(input$go, ignoreNULL = FALSE, {
    if (input$variable_sec == "None") {
      NULL
    }  else if (variable_sec() == outcome()) {
      showNotification("No se puede desglosar la variable resultado por sí misma", 
                       type = "error")
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
        var_label = "Género"
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
        var_label = "Riqueza"
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
        var_label = "Educación"
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
        var_label = "Edad"
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
        var_label = "Lugar de\nresidencia"
      )
    } else {
      NULL
    }
  })
  
  # Combine demographic data frames into one df
  moverd <- eventReactive(input$go, ignoreNULL = FALSE, {
    dta_mover <- Error(rbind(secdf(), genderdf(), edaddf(), wealthdf(), eddf(), urdf()))
    validate(
      need(dta_mover, "Error: no hay datos disponibles. Verifique que esta pregunta se haya realizado en esta combinación de país/año.")
    )
    dta_mover$vallabel <- as.character(dta_mover$vallabel)
    return(dta_mover)
  })
  
  moverg <- eventReactive(input$go, ignoreNULL = FALSE, {
    moverg <- lapop_mover(moverd(), 
                          subtitle = "% en la categoría seleccionada", 
                          ymax = ifelse(any(moverd()$prop > 90, na.rm = TRUE), 119,
                                        ifelse(any(moverd()$prop > 80, na.rm = TRUE), 109, 100)),
                          lang = "es",
                          source_info = source_info_both())
    return(moverg)
  })
  
  output$mover <- renderPlot({
    return(moverg())
  })
  
  # DOWNLOAD SECTION
  # -------------------------------------------------------------------------
  output$downloadPlot <- downloadHandler(
    filename = function(file) {
      ifelse(input$tabs == "Histograma",  paste0("hist_", outcome(),".svg"),
             ifelse(input$tabs == "Serie temporal",  paste0("ts_", outcome(),".svg"),
                    ifelse(input$tabs == "Comparativo",  paste0("cc_", outcome(),".svg"),  paste0("mover_", outcome(),".svg"))))
    },
    
    content = function(file) {
      if(input$tabs == "Histograma") {
        title_text <- isolate(cap())
        subtitle_text <- slider_values()
        
        hist_to_save <- lapop_hist(histd(),
                                   main_title = title_text,
                                   subtitle = "% en la categoría seleccionada ",
                                   ymax = ifelse(any(histd()$prop > 90), 110, 100), 
                                   lang = "es",
                                   source_info = paste0(source_info_both(), "\n\n", 
                                                        str_wrap(paste0(word(), " ", resp()), 125))
                                   )
        
        lapop_save(hist_to_save, file)
        showNotification(HTML("Descarga de figura completada ✓ "), type = "message")
        
      } else if (input$tabs == "Serie temporal") {
        title_text <- isolate(cap())
        subtitle_text <- slider_values()
        
        # Check for single time period
        #if(any(table(tsd()$wave) == 1)) {
          #showNotification(
           # "Advertencia: su selección incluye solo una serie temporal",
            #type = "warning",
            #duration = 5
          #)
        #}
        
        ts_to_save <-  lapop_ts(tsd(),
                                main_title = title_text,
                                subtitle = paste0("% en la categoría seleccionada ", subtitle_text),
                                ymax = ifelse(any(tsd()$prop > 88, na.rm = TRUE), 110, 100),
                                label_vjust = ifelse(any(tsd()$prop > 80, na.rm = TRUE), -1.1, -1.5),
                                lang = "es",
                                source_info = paste0(source_info_pais(), "\n\n", 
                                                     str_wrap(paste0(word(), " ", resp()), 125))
        )
        
        lapop_save(ts_to_save, file)
        showNotification(HTML("Descarga de figura completada ✓ "), type = "message")
        
      } else if (input$tabs == "Comparativo") {
        title_text <- isolate(cap())
        subtitle_text <- slider_values()
        
        cc_to_save <- lapop_cc(ccd(), sort = "hi-lo", 
                               main_title = title_text,
                               subtitle = paste0("% en la categoría seleccionada ", subtitle_text),
                               ymax = ifelse(any(ccd()$prop > 90, na.rm = TRUE), 110, 100),
                               lang = "es",
                               source_info = paste0(source_info_wave(), "\n\n", 
                                                    str_wrap(paste0(toupper(outcome_code()), ". ", word(), " ", resp()), 125))
        )
        
        lapop_save(cc_to_save, file)
        showNotification(HTML("Descarga de figura completada ✓ "), type = "message")
        
      } else {
        title_text <- isolate(cap())
        subtitle_text <- slider_values()
        
        mover_to_save <- lapop_mover(
          moverd(),
          main_title = title_text,
          subtitle = paste0("% en la categoría seleccionada ", subtitle_text),
          ymax = ifelse(any(moverd()$prop > 90, na.rm = TRUE), 119,
                        ifelse(any(moverd()$prop > 80, na.rm = TRUE), 109, 100)),
          lang = "es",
          source_info = paste0(source_info_both(), "\n\n", 
                               str_wrap(paste0(word(), " ", resp()), 125))
        )
        
        lapop_save(mover_to_save, file)
        showNotification(HTML("Descarga de figura completada ✓ "), type = "message")
      }
    }
  )
  
  
  output$downloadTable <- downloadHandler(
    filename = function(file) {
      ifelse(input$tabs == "Histograma",  paste0("hist_", outcome(),".csv"),
             ifelse(input$tabs == "Serie temporal",  paste0("ts_", outcome(),".csv"),
                    ifelse(input$tabs == "Comparativo",  paste0("cc_", outcome(),".csv"),  
                           paste0("mover_", outcome(),".csv"))))
    },
    content = function(file) {
      if(input$tabs == "Histograma") {
        write.csv(histd(), file, row.names=F)
        showNotification(HTML("Descarga de archivo completada ✓ "), type = "message")
      } else if (input$tabs == "Serie temporal") {
        write.csv(tsd(), file, row.names=F)
        showNotification(HTML("Descarga de archivo completada ✓ "), type = "message")
      } else if (input$tabs == "Comparativo") {
        write.csv(ccd(), file, row.names=F)
        showNotification(HTML("Descarga de archivo completada ✓ "), type = "message")
      } else {
        write.csv(moverd(), file, row.names=F)
        showNotification(HTML("Descarga de archivo completada ✓ "), type = "message")
      }
    }
  )
}

shinyApp(ui, server)