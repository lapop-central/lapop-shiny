suppressPackageStartupMessages({
  library(lapop)
  library(haven)
  library(dplyr)
  library(tidyr)
  library(shiny)
  library(stringr)
  library(shinyWidgets)
  library(bslib)
  library(Hmisc)
})

# # -----------------------------------------------------------------------
lapop_fonts()

dstrata <- readRDS("gm_shiny_data_en.rds")
labs <- readRDS("labs_en.rds")
vars_labels <- read.csv("variable_labels_shiny.csv", encoding = "latin1")

Error<-function(x){
  tryCatch(x,error=function(e) return(FALSE))
}

waves_total = c("2004", "2006", "2008", "2010", "2012", "2014", 
                "2016/17", "2018/19", "2021", "2023")

# helper for cleaning ts - handle missing values at end or middle of series
# # -----------------------------------------------------------------------

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

# custom weighted averages and CIs, to speed up computational speed vs. survey_mean
# # -----------------------------------------------------------------------

weighted.ttest.ci <- function(x, weights) {
  
  strata <- get("strata", envir = parent.frame())
  
  # Remove NAs
  valid <- !is.na(x) & !is.na(weights) & !is.na(strata)
  x       <- x[valid]
  weights <- weights[valid]
  strata  <- strata[valid]
  
  nx <- length(x)
  
  # Overall weighted mean
  mx <- weighted.mean(x, weights, na.rm = TRUE)
  
  # Stratified variance
  strata_list <- unique(strata)
  
  strat_var <- sapply(strata_list, function(s) {
    idx   <- strata == s
    x_s   <- x[idx]
    w_s   <- weights[idx]
    n_s   <- length(x_s)
    W_s   <- sum(w_s) / sum(weights)
    var_s <- Hmisc::wtd.var(x_s, w_s, normwt = TRUE, na.rm = TRUE)
    return(W_s^2 * var_s / n_s)
  })
  
  vx     <- sum(strat_var)
  stderr <- sqrt(vx)
  
  df    <- nx - length(strata_list)
  tstat <- mx / stderr
  cint  <- qt(1 - 0.05/2, df)
  cint  <- tstat + c(-cint, cint)
  confint <- cint * stderr
  
  result <- data.frame(prop = mx, lb = confint[1], ub = confint[2])
  return(result)
}


# helper function for mover
# # -----------------------------------------------------------------------

process_data <- function(data, outcome_var, recode_range, group_var, var_label, weight_var = "weight1500") {
  if (is.null(group_var)) {
    return(NULL)
  }  
  # Proceed with processing
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

# helper for missing country-year by outcome_var
# # -----------------------------------------------------------------------
get_missing_combinations <- function(data, outcome_var, wave_var,
                                     selected_waves, selected_countries) {
  # Convert wave values to string using haven labels
  data <- data %>%
    mutate(wave_str = as.character(haven::as_factor(.data[[wave_var]])))
  
  # Build the full country-wave grid
  all_combos <- expand.grid(
    pais_nam = selected_countries,
    wave = selected_waves,
    stringsAsFactors = FALSE
  )
  
  # Subset only relevant countries
  data <- data %>%
    filter(pais_nam %in% selected_countries)
  
  # Summarize: how many valid (non-NA and not 0) values exist per combo
  summary <- data %>%
    group_by(pais_nam, wave = wave_str) %>%
    summarise(
      n_valid = sum(!is.na(.data[[outcome_var]]) & .data[[outcome_var]] != 0),
      .groups = "drop"
    )
  
  # Merge and detect missing
  missing <- all_combos %>%
    left_join(summary, by = c("pais_nam", "wave")) %>%
    filter(is.na(n_valid) | n_valid == 0) %>%
    select(pais_nam, wave)
  
  return(missing)
}


# # -----------------------------------------------------------------------
# UI
# # -----------------------------------------------------------------------

ui <- fluidPage(
  
  titlePanel(""), # Leave it Empty
  
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      width = 3,  # Reduce width (default is 4)
      
      selectInput(inputId = "variable", 
                  label = "Variable",
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
                  multiple = TRUE), 
      
      # Show recode slider only for time series, cc, and breakdown (not hist)
      conditionalPanel(
        'input.tabs == "Time Series" | 
        input.tabs == "Cross Country" | 
        input.tabs == "Breakdown" | 
        input.tabs == "World Map"',
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
      
      #actionButton("go", "Generate") # Include button in UI
      
      tags$div(
        style = "display: none;",
        actionButton("go", "Generate")
      )
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      #width = 8,  # Adjust accordingly (default is 8)
      # Output: Formatted text for caption ----
      h3(textOutput("caption")),
      h5(textOutput("wording")),
      h5(textOutput("response")),
      
      tabsetPanel(id = "tabs",
                  tabPanel("Histogram", plotOutput("hist")),
                  
                  tabPanel("Time Series", plotOutput("ts")),
                  
                  tabPanel("Cross Country", plotOutput("cc")),
                  
                  tabPanel("Breakdown", plotOutput("mover")),

                  tabPanel("World Map", plotOutput("map"))
      ),
      br(),
      fluidRow(column(12, "",
                      uiOutput("missing_warning_card"),
                      downloadButton(outputId = "downloadPlot", label = "Download Figure"),
                      downloadButton(outputId = "downloadTable", label = "Download Table")))
    )
  )
)

# # -----------------------------------------------------------------------
# SERVER
# # -----------------------------------------------------------------------

server <- function(input, output, session) {
  
  # Triggers "go" between server and ui to generate default plots
  observe({
    if (!is.null(input$pais) && !is.null(input$wave)) {
      isolate({
        session$sendCustomMessage("clickGenerateButton", list())
      })
    }
  })
  
  # Check the number of selected variables for breakdown
  observeEvent(input$demog, {
    if (length(input$demog) > 3) {
      # Show a warning message
      showNotification(HTML("You should only select a maximum of 3 demographic variables to plot."), type = "warning")
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
                label = "Outcome variable response values shown as percentage",
                min = min(as.numeric(dstrata[[formulaText()]]), na.rm=TRUE), 
                max = max(as.numeric(dstrata[[formulaText()]]), na.rm=TRUE), 
                value = sliderParams$valuex,
                step = 1)
  })
  
  # Filtering data based on user's selection (dff)
  dff <- eventReactive(input$go, ignoreNULL = FALSE, {
    dstrata %>%
      filter(as_factor(wave) %in% input$wave) %>%
      filter(pais_nam %in% input$pais)
  })  
  
  # Rendering var caption based on user's var selection
  cap <- renderText({
    vars_labels$question_short_en[which(vars_labels$column_name == formulaText())]
  })
  
  output$caption <- renderText({
    cap() 
  })
  
  # Rendering variable code + wording based on user's var selection
  word <- renderText({
    paste0(toupper(vars_labels$column_name[which(vars_labels$column_name == formulaText())]), ". ",
    vars_labels$question_en[which(vars_labels$column_name == formulaText())])
  })
  
  output$wording <- renderText({
    word() 
  })
  
  # Rendering ROs based on user's var selection
  resp <- renderText({
    vars_labels$responses_en_rec[which(vars_labels$column_name == formulaText())]
  })
  
  output$response <- renderText({
    resp() 
  })
  
  # Rendering variable_sec ROs
  resp_sec <- renderText({
    vars_labels$responses_en_rec[which(vars_labels$column_name == input$variable_sec)]
  })
  
  output$response_sec <- renderText({
    resp_sec()
  })
  
  # Rendering User selected recode value(s)
  slider_values <- renderText({
    if(input$recode[1] == input$recode[2]) {
      paste0("(value: ", unique(input$recode), ")")
    } else {
      paste0("(range: ", paste(input$recode, collapse = " to "), ")")
    }
  })
  
  output$selected_values <- renderText({
    slider_values()
  })
  
  # WARNING FOR MISSING COMBOS
  # # -----------------------------------------------------------------------
  output$missing_warning_card <- renderUI({
    req(input$go > 0, input$wave, input$pais)
    
    # Normalize wave and country inputs
    selected_waves <- as.character(input$wave)
    selected_countries <- as.character(input$pais)
    
    # Step 1: Compute missing combinations
    missing <- get_missing_combinations(
      data = dff(),
      outcome_var = outcome(),
      wave_var = "wave",
      selected_waves = selected_waves,
      selected_countries = selected_countries
    )
    
    # Step 2: Skip if none missing
    if (nrow(missing) == 0) return(NULL)
    
    # Add country abbreviations
    missing <- missing %>%
      left_join(dstrata %>% distinct(pais_nam, pais_lab), by = "pais_nam")
    
    # Format message YEAR: COUNTRIES
    warning_text <- missing %>%
      group_by(wave) %>%
      summarise(
        country_list = paste(sort(unique(pais_lab)), collapse = ", "),
        .groups = "drop"
      ) %>%
      mutate(combo_label = paste0("<b>", wave, "</b>: ", country_list)) %>%
      pull(combo_label) %>%
      paste(collapse = "<br>")
    
    # Display warning card
    tags$div(
      style = "
      border: 2px solid #ffc107;
      border-radius: 8px;
      padding: 15px;
      background-color: #fff8e1;
      margin-bottom: 20px;
      max-height: 120px;
      overflow-y: auto;
      ",
      HTML(paste0(
        "<span style='font-size:16px; color: #856404;'>⚠️ <b>Warning:</b> The following country-years have no data for <b>",
        outcome(), "</b>:<br>", warning_text
      ))
    )
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
      paste0("Source: LAPOP Lab, AmericasBarometer Data Playground\n\nCountries selected: ", pais_display, 
             "\nSurvey rounds selected: ", wave_display)
      
    } else {
      paste0("Source: LAPOP Lab, AmericasBarometer Data Playground\n\nCountries selected: ", pais_display, 
             ". Survey rounds selected: ", wave_display)
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
    
    paste0("Source: LAPOP Lab, AmericasBarometer Data Playground\n\nCountries selected: ", pais_display)
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
    
    paste0("Source: LAPOP Lab, AmericasBarometer Data Playground\n\nSurvey rounds selected: ", wave_display)
  })
  
  # Histogram 
  # # -----------------------------------------------------------------------
  
  # must break into data event, graph event, and renderPlot to get download buttons to work
  histd <- reactive({
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
  
  
  histg <- reactive({
    histg <- lapop_hist(histd(), 
                        ymax = ifelse(any(histd()$prop > 90), 110, 100), 
                        source_info = source_info_both())
    return(histg)
  })
  
  output$hist <- renderPlot({
    return(histg())
  })
  
  
  # Time-series
  # # -----------------------------------------------------------------------
  tsd <- reactive({
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
  
  tsg <- reactive({
    tsg = lapop_ts(tsd(), 
                   ymax = ifelse(any(tsd()$prop > 88, na.rm = TRUE), 110, 100),
                   label_vjust = ifelse(any(tsd()$prop > 80, na.rm = TRUE), -1.1, -1.5),
                   source_info = source_info_pais(),
                   subtitle = "% in selected category")
    return(tsg)
  })
  
  
  output$ts <- renderPlot({
    return(tsg())
  })
  
  # Cross Country
  # # -----------------------------------------------------------------------
  ccd <- reactive({
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
  
  ccg <- reactive({
    ccg = lapop_cc(ccd(), sort = "hi-lo", 
                   subtitle = "% in selected category",
                   ymax = ifelse(any(ccd()$prop > 90, na.rm = TRUE), 110, 100),
                   source_info = source_info_wave())
    return(ccg)
  })
  
  output$cc <- renderPlot({
    return(ccg())
  })
  
  # Breakdown
  # # -----------------------------------------------------------------------
  # Use function for each demographic breakdown variable
  
  secdf <- reactive({
    if (input$variable_sec == "None") {
      NULL
    }  else if (variable_sec() == outcome()) {
      showNotification("❌ Error: You cannot break down the outcome variable by itself.", 
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
  
  genderdf <- reactive({
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
  
  wealthdf <- reactive({
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
  
  eddf <- reactive({
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
  
  edaddf <- reactive({
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
  
  urdf <- reactive({
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
  
  # Combine demographic data frames into one df
  moverd <- reactive({
    dta_mover <- Error(rbind(secdf(), genderdf(), edaddf(), wealthdf(), eddf(), urdf()))
    validate(
      need(dta_mover, "Error: no data available. Please verify that this question was asked in this country/year combination")
    )
    dta_mover$vallabel <- as.character(dta_mover$vallabel)
    return(dta_mover)
  })
  
  moverg <- reactive({
    moverg <- lapop_mover(moverd(), 
                          subtitle = "% in selected category", 
                          ymax = ifelse(any(moverd()$prop > 90, na.rm = TRUE), 119,
                                        ifelse(any(moverd()$prop > 80, na.rm = TRUE), 109, 100)),
                          lang = "en", source_info = source_info_both())
    return(moverg)
  })
  
  output$mover <- renderPlot({
    return(moverg())
  })
  
  # World Map
  # # -----------------------------------------------------------------------
  mapd <- reactive({
    
    req(input$wave)
    req(outcome())
    
    # Allow only one wave
    validate(
      need(
        length(input$wave) == 1,
        "Please select only ONE wave/year to display a map."
      )
    )
    
    var_sel <- outcome()
    rec_min <- input$recode[1]
    rec_max <- input$recode[2]
    
    dta_map <- dff() %>%
      
      # Create binary indicator (100 = within range, 0 = outside)
      mutate(
        outcome_rec = ifelse(
          .data[[var_sel]] >= rec_min &
            .data[[var_sel]] <= rec_max,
          100, 0
        )
      ) %>%
      
      # Aggregate by country
      group_by(pais_lab) %>%
      
      # Compute percent in range
      summarise(
        value = mean(outcome_rec, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      
      # Remove countries with no valid data
      filter(!is.na(value) & value > 0)
    
    validate(
      need(
        nrow(dta_map) > 0,
        "Error: no map data available for this country/year selection."
      )
    )
    
    return(dta_map)
  })
  
  
  
  mapg <- reactive({
    lapop_map(
      mapd(),
      survey = "AmericasBarometer",
      source_info = "\nSource: LAPOP Lab, AmericasBarometer Data Playground"
    )
  })
  
  
  output$map <- renderPlot({
    mapg()
  })
  
  # # -----------------------------------------------------------------------
  # DOWNLOAD SECTION
  # # -----------------------------------------------------------------------
  
  # Download Plot
  # # -----------------------------------------------------------------------
  output$downloadPlot <- downloadHandler(
    filename = function(file) {
      
      ifelse(input$tabs == "Histogram", paste0("hist_", outcome(), ".svg"),
             ifelse(input$tabs == "Time Series",  paste0("ts_", outcome(), ".svg"),
                    ifelse(input$tabs == "Cross Country",  paste0("cc_", outcome(), ".svg"),
                           ifelse(input$tabs == "World Map",  paste0("map_", outcome(), ".svg"),
                                  paste0("mover_", outcome(), ".svg"))))) # Add plot type to file export
    },
    
    content = function(file) {
      
      if(input$tabs == "Histogram") {
        title_text <- isolate(cap())
        word_text <- isolate(word())
        
        hist_to_save <- lapop_hist(histd(),
                                   main_title = title_text,
                                   subtitle = "% in selected category ",
                                   ymax = ifelse(any(histd()$prop > 90), 110, 100),
                                   source_info = source_info_both()
        )
        
        lapop_save(hist_to_save, file)
        showNotification(HTML("Histogram plot download complete ✓ "), type = "message")
        
      } else if (input$tabs == "Time Series") {
        title_text <- isolate(cap())
        subtitle_text <- slider_values()
        
        ts_to_save <-  lapop_ts(tsd(),
                                main_title = title_text,
                                subtitle = paste0("% in selected category ", subtitle_text),
                                ymax = ifelse(any(tsd()$prop > 88, na.rm = TRUE), 110, 100),
                                label_vjust = ifelse(any(tsd()$prop > 80, na.rm = TRUE), -1.1, -1.5),
                                source_info = source_info_pais()
        )
        
        lapop_save(ts_to_save, file)
        showNotification(HTML("Time series plot download complete ✓ "), type = "message")
        
      } else if (input$tabs == "Cross Country") {
        title_text <- isolate(cap())
        subtitle_text <- slider_values()
        
        cc_to_save <- lapop_cc(ccd(), sort = "hi-lo",
                               main_title = title_text,
                               subtitle = paste0("% in selected category ", subtitle_text),
                               ymax = ifelse(any(ccd()$prop > 90, na.rm = TRUE), 110, 100),
                               label_angle = 90,
                               source_info = source_info_wave()
        )
        
        lapop_save(cc_to_save, file)
        showNotification(HTML("Cross country plot download complete ✓ "), type = "message")
        
      } else if (input$tabs == "World Map") {
        title_text <- isolate(cap())
        subtitle_text <- slider_values()
        
        map_to_save <- lapop_map(mapd(),
                                 main_title = title_text,
                                 subtitle = paste0("% in selected category ", subtitle_text),
                                 source_info = paste0("\n", source_info_both()),
                                 survey = "AmericasBarometer"
        )
        
        lapop_save(map_to_save, file)
        showNotification(HTML("Map plot download complete ✓ "), type = "message")
        
      } else {
        title_text <- isolate(cap())
        subtitle_text <- slider_values()
        word_text <- isolate(word())
        
        mover_to_save <- lapop_mover(
          moverd(),
          main_title = title_text,
          subtitle = paste0("% in selected category ", subtitle_text),
          ymax = ifelse(any(moverd()$prop > 90, na.rm = TRUE), 119,
                        ifelse(any(moverd()$prop > 80, na.rm = TRUE), 109, 100)),
          source_info = source_info_both()
        )
        
        lapop_save(mover_to_save, file)
        showNotification(HTML("Break down plot download complete ✓ "), type = "message")
        
      }
    }
  )
  
  # Download Table
  # -----------------------------------------------------------------------
  output$downloadTable <- downloadHandler(
    filename = function(file) {
      
      ifelse(input$tabs == "Histogram", paste0("hist_", outcome(), ".csv"),
             ifelse(input$tabs == "Time Series",  paste0("ts_", outcome(), ".csv"),
                    ifelse(input$tabs == "Cross Country",  paste0("cc_", outcome(), ".csv"),
                           ifelse(input$tabs == "World Map",  paste0("map_", outcome(), ".csv"),
                                  paste0("mover_", outcome(), ".csv")))))
    },
    content = function(file) {
      if(input$tabs == "Histogram") {
        write.csv(histd(), file, row.names=F)
        showNotification(HTML("Histogram file download complete ✓ "),
                         type = "message")
        
      } else if (input$tabs == "Time Series") {
        write.csv(tsd(), file, row.names=F)
        showNotification(HTML("Time series file download complete ✓ "),
                         type = "message")
        
      } else if (input$tabs == "Cross Country") {
        write.csv(ccd(), file, row.names=F)
        showNotification(HTML("Cross country file download complete ✓ "),
                         type = "message")
        
      } else if (input$tabs == "World Map") {
        write.csv(mapd(), file, row.names=F)
        showNotification(HTML("Map file download complete ✓ "),
                         type = "message")
        
      } else {
        write.csv(moverd(), file, row.names=F)
        showNotification(HTML("Break down file download complete ✓ "),
                         type = "message")
        
      }
    }
  )
}

# RUN APP
# # -----------------------------------------------------------------------
shinyApp(ui, server)

# # -----------------------------------------------------------------------
# END
# # -----------------------------------------------------------------------