library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(shinycssloaders)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(DT)
library(ggstatsplot)
library(gt)
library(gtExtras)
library(treemap)
library(d3treeR)
library(tidyverse)
library(readr)
library(svglite)

# Data preparation
# Geo dataprepare
clean_water_data <- function(water_indicators) {
  water_indicators[water_indicators == ".."] <- NA
  water_indicators[water_indicators == "#N/A"] <- NA
  water_indicators[water_indicators == ""] <- NA
  numeric_cols <- names(water_indicators)[6:27]
  for(col in numeric_cols) {
    water_indicators[[col]] <- tryCatch(
      as.numeric(as.character(water_indicators[[col]])),
      warning = function(w) NA,
      error = function(e) NA
    )
  }
  return(water_indicators)
}
prepare_long_data <- function(water_indicators) {
  water_long <- water_indicators %>% 
    pivot_longer(
      cols = starts_with("X"),             
      names_to = "Year",
      values_to = "Value",
      values_transform = list(Value = as.numeric)
    ) %>%
    mutate(
      Year = as.numeric(gsub("X(\\d{4})\\.\\..*", "\\1", Year))
    ) %>%
    group_by(Country.Name, Region, Series.Name, Year) %>%
    summarise(
      Value = mean(Value, na.rm = TRUE),
      .groups = "drop"
    )
  return(water_long)
}

safe_max_value <- function(data, column) {
  if(is.list(data[[column]])) {
    values <- unlist(data[[column]])
  } else {
    values <- data[[column]]
  }
  max_val <- max(as.numeric(values), na.rm = TRUE)
  max_country <- data$Country[which.max(as.numeric(values))]
  return(list(value = max_val, country = max_country))
}


# ==================== UI ====================

ui <- dashboardPage(
  dashboardHeader(
    title = span("Blue Pulse", icon("tint"))  
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Geographic analysis", tabName = "treemap", icon = icon("map")),
      menuItem("Health Trend Forecasting ", tabName = "wash_trend", icon = icon("heartbeat")),
      menuItem("Confirmatory Data Analysis", icon = icon("chart-line"), startExpanded = TRUE,
               menuSubItem("Correlation Analysis", tabName = "cda_correlation"),
               menuSubItem("ANOVA Analysis", tabName = "cda_anova"),
               menuSubItem("Bullet Charts", tabName = "cda_bullet")
      )
    )
  ),
  dashboardBody(
    
#==================CSS===================
    tags$head(
      tags$style(HTML("
.select-pill .remove-btn {
  margin-left: 6px;
  cursor: pointer;
  color: #666;
  font-weight: bold;
}
.select-pill .remove-btn:hover {
  color: #dc3545;
}
")),
      tags$script(HTML("
      $(document).on('click', '.remove-btn', function() {
        var selectId = $(this).data('id');
        var value = $(this).data('value');
        var selectInput = $('#' + selectId)[0].selectize;
        selectInput.removeItem(value);
      });
      
      Shiny.addCustomMessageHandler('selectize-config', function(message) {
          message.ids.forEach(function(id) {
            var selectizeInput = $('#' + id).selectize(message.options)[0].selectize;
            if (selectizeInput) {
              selectizeInput.refreshOptions();
            }
          });
        });
    "))
    ),
 # theme style
    tags$head(
      tags$style(HTML("
         body { background-color: #e8f7ff; }
          .skin-blue .main-header .logo { background-color: #00acc1; color: white; }
          .skin-blue .main-header .navbar { background-color: #00acc1; }
          .content-wrapper, .right-side { background-color: #e8f7ff; }
          .box { border-top: 3px solid #0097a7; }
          .titlePanel, 
          .main-title,
          .analysis-panel > h3,
          .box-header,
          .panel-primary > .panel-heading {
            background-color: #0F2947 !important;
            color: white !important;
            padding: 10px 15px;
            margin-top: 0;
            border-radius: 4px 4px 0 0;
          }
          .nav-tabs > li > a,
          .tabbable > .nav > li > a {
          }
      ")),
    ),
    
    tabItems(
      # ---------- Treemap  ----------
      tabItem(tabName = "treemap",
              fluidRow(
                box(width = 12,
                    title = "Treemap visualisation geographic analysis",
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    sidebarLayout(
                      sidebarPanel(
                        wellPanel(
                          h4("Select Data Parameters"),
                          selectInput("indicator", 
                                      "Select Water Indicator:",
                                      choices = c(
                                        "Level of water stress" = "Level of water stress: freshwater withdrawal as a proportion of available freshwater resources",
                                        "Water productivity" = "Water productivity, total (constant 2015 US$ GDP per cubic meter of total freshwater withdrawal)",
                                        "Safely managed drinking water" = "People using safely managed drinking water services (% of population)",
                                        "Basic drinking water services" = "People using at least basic drinking water services (% of population)",
                                        "Basic handwashing facilities" = "People with basic handwashing facilities including soap and water (% of population)",
                                        "Basic sanitation services" = "People using at least basic sanitation services (% of population)",
                                        "Safely managed sanitation" = "People using safely managed sanitation services (% of population)",
                                        "Open defecation" = "People practicing open defecation (% of population)"
                                      ))
                        ),
                        wellPanel(
                          h4("Visualization Options"),
                          sliderInput("year", 
                                      "Select Year:",
                                      min = 2000, 
                                      max = 2021, 
                                      value = 2021,
                                      step = 1,
                                      sep = ""),
                          materialSwitch("interactive", "Interactive Treemap", TRUE),
                          selectInput("palette", 
                                      "Color Palette:",
                                      choices = c("RdYlBu", "Blues", "Greens", "Reds", "Purples", "Oranges"),
                                      selected = "RdYlBu")
                        ),
                        wellPanel(
                          h4("Export Options"),
                          downloadButton("downloadPlot", "Download Plot", class = "btn-primary btn-block"),
                          downloadButton("downloadTreemapData", "Download Data", class = "btn-info btn-block")
                        )
                      ),
                      mainPanel(
                        fluidRow(
                          valueBoxOutput("avgValueBox", width = 4),
                          valueBoxOutput("maxValueBox", width = 4),
                          valueBoxOutput("countriesBox", width = 4)
                        ),
                        fluidRow(
                          box(width = 12,
                              h3(textOutput("treemapTitle")),
                              conditionalPanel(
                                condition = "input.interactive == false",
                                withSpinner(plotOutput("staticTreemap", height = "600px"))
                              ),
                              conditionalPanel(
                                condition = "input.interactive == true",
                                withSpinner(d3treeOutput("interactiveTreemap", height = "600px"))
                              )
                          )
                        )
                      )
                    )
                )
              )
      ),
      
      # ---------- WASH-Based Health Trend Analysis  ----------
      tabItem(tabName = "wash_trend",
              fluidRow(
                box(width = 12,
                    title = "WASH-Based Health Trend Analysis",
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    sidebarLayout(
                      sidebarPanel(
                        radioButtons("variableType", 
                                     "Select Variable Type:",
                                     choices = c("WASH Variables", "Disease Rates", "Compare Both"),
                                     selected = "Compare Both"),
                        conditionalPanel(
                          condition = "input.variableType == 'WASH Variables' || input.variableType == 'Compare Both'",
                          selectInput("washVarsSelect", 
                                      "Select WASH Variables:",
                                      choices = c("WaterStress", "OpenDefecation", "BasicWaterServices", 
                                                  "BasicSanitationServices", "SafeManagedWaterServices", 
                                                  "SafeManagedSanitationServices", "PopulationDensity", 
                                                  "WaterProductivity"),
                                      selected = c("BasicWaterServices", "WaterStress"),
                                      multiple = TRUE)
                        ),
                        conditionalPanel(
                          condition = "input.variableType == 'Disease Rates' || input.variableType == 'Compare Both'",
                          selectInput("diseaseSelect", 
                                      "Select Disease Rates:",
                                      choices = c("TyphoidRate", "DiarrheaRate", "HepatitisRate", "UnsafeRisk"),
                                      selected = "TyphoidRate",
                                      multiple = TRUE)
                        ),
                        selectInput("regionSelect", 
                                    "Select Regions:",
                                    choices = NULL,  
                                    selected = NULL,
                                    multiple = TRUE),
                        selectInput("visualizationType", 
                                    "Visualization Type:",
                                    choices = c("Time Series", "Scatter Plot", "Bar Chart", "Heatmap"),
                                    selected = "Time Series"),
                        checkboxGroupInput("dataSourceSelect",
                                           "Data Source:",
                                           choices = c("Actual", "Forecast"),
                                           selected = c("Actual", "Forecast")),
                        sliderInput("yearSelect",
                                    "Select Years:",
                                    min = 2000,
                                    max = 2030,
                                    value = c(2000, 2030),
                                    step = 1, ticks = TRUE)
                      ),
                      mainPanel(
                        tabsetPanel(
                          tabPanel("Visualization", 
                                   withSpinner(plotlyOutput("mainPlot", height = "600px"))),
                          tabPanel("Correlation Analysis",
                                   withSpinner(plotlyOutput("correlationPlot", height = "600px"))),
                          tabPanel("Data Table", 
                                   DT::dataTableOutput("dataTable"))
                        )
                      )
                    )
                )
              )
      ),
      
      # ---------- Confirmatory Data Analysis  ----------
     
      tabItem(tabName = "cda_correlation",
              fluidRow(
                box(width = 12,
                    title = "Confirmatory Data Analysis - Correlation Analysis",
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    sidebarLayout(
                      sidebarPanel(
                        selectInput("cda_selectedDisease",
                                    "Select Disease:",
                                    choices = c("Typhoid Rate" = "TyphoidRate",
                                                "Diarrhea Rate" = "DiarrheaRate",
                                                "Hepatitis Rate" = "HepatitisRate",
                                                "Unsafe Water Risk" = "UnsafeRisk")),
                        selectInput("cda_selectedIndicators",
                                    "Select Water Indicator(s):",
                                    choices = NULL,
                                    selected = NULL,
                                    multiple = TRUE),
                        sliderInput("cda_selectedYear",
                                    "Select Year:",
                                    min = 2000, max = 2030,
                                    value = c(2000, 2030),
                                    sep = ""),
                        selectInput("cda_selectedRegions",
                                    "Select Region(s):",
                                    choices = NULL,
                                    selected = NULL,
                                    multiple = TRUE)
                      ),
                      mainPanel(
                        fluidRow(
                          valueBoxOutput("cda_countStat", width = 3),
                          valueBoxOutput("cda_waterValueStat", width = 3),
                          valueBoxOutput("cda_diseaseRateStat", width = 3),
                          valueBoxOutput("cda_correlationStat", width = 3)
                        ),
                        fluidRow(
                          tabsetPanel(
                            tabPanel("Correlation Visualization",
                                     withSpinner(plotOutput("cda_correlationPlot", height = "500px"))),
                            tabPanel("Data Table",
                                     div(class = "filtered-data-section",
                                         div(class = "section-title", "Filtered Data"),
                                         fluidRow(
                                           column(8, 
                                                  selectInput("cda_pageSize", "Rows per page:", 
                                                              choices = c(5, 10, 15, 20, 50), 
                                                              selected = 10)
                                           ),
                                           column(4,
                                                  textInput("cda_searchText", "Search:", "")
                                           )
                                         ),
                                         DT::dataTableOutput("cda_dataTable")
                                     ))
                          )
                        )
                      )
                    )
                )
              )
      ),
      tabItem(tabName = "cda_anova",
              fluidRow(
                box(width = 12,
                    title = "Confirmatory Data Analysis - ANOVA Analysis",
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    sidebarLayout(
                      sidebarPanel(
                        selectInput("cda_anovaAnalysisType", "Analysis Type:",
                                    choices = c(
                                      "Water Indicator Across Diseases" = "indicator_across_diseases",
                                      "Disease Rates by Region" = "disease_by_region",
                                      "Disease Rates by Water Indicator" = "disease_by_indicator",
                                      "Water Indicators by Region" = "indicator_by_region",
                                      "Compare All Diseases" = "all_diseases",
                                      "Compare All Water Indicators" = "all_indicators"
                                    ),
                                    selected = "indicator_across_diseases"),
                        uiOutput("cda_dynamicAnovaSelector1"),
                        uiOutput("cda_dynamicAnovaSelector2"),
                        selectInput("cda_anovaYearRange", "Year Range:",
                                    choices = c(
                                      "All Years" = "all",
                                      "Last 5 Years" = "last5",
                                      "Last 10 Years" = "last10",
                                      "Custom Range" = "custom"
                                    ),
                                    selected = "all"),
                        conditionalPanel(
                          condition = "input.cda_anovaYearRange == 'custom'",
                          sliderInput("cda_customAnovaYearRange", "Select Custom Year Range:",
                                      min = 2000, max = 2030,
                                      value = c(2000, 2030),
                                      sep = "")
                        ),
                        actionButton("cda_runAnovaBtn", "Run ANOVA Analysis", 
                                     class = "btn-primary", 
                                     style = "margin-top: 15px; width: 100%;")
                      ),
                      mainPanel(
                        withSpinner(plotOutput("cda_anovaPlot", height = "500px"))
                      )
                    )
                )
              )
      ),
      tabItem(tabName = "cda_bullet",
              fluidRow(
                box(width = 12,
                    title = "Confirmatory Data Analysis - Bullet Charts",
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    sidebarLayout(
                      sidebarPanel(
                        selectInput("cda_indicator_type_auto", "Choose Indicator Type:",
                                    choices = c("Water", "Disease")),
                        uiOutput("cda_dynamic_indicator_auto"),
                        sliderInput("cda_auto_year", "Select Year:", 
                                    min = 2000, max = 2030, 
                                    value = 2000)
                      ),
                      mainPanel(
                        gt::gt_output("cda_bullet_table_auto"),
                        verbatimTextOutput("cda_data_insights")
                      )
                    )
                )
              )
      )
    )
  )
)

# ==================== SERVER ====================
server <- function(input, output, session) {
  
  
  observe({
    shinyjs::runjs("
      $(document).ready(function() {
        var ids = ['washVarsSelect', 'diseaseSelect', 'regionSelect', 'cda_selectedIndicators', 'cda_selectedRegions'];
        ids.forEach(function(id) {
          var $select = $('#' + id);
          if ($select.length) {
            var selectize = $select.selectize({
              render: {
                item: function(data, escape) {
                  return '<div class=\"select-pill\">' + escape(data.text) + 
                         '<span class=\"remove-btn\" data-id=\"' + this.$input.attr('id') + '\" data-value=\"' + 
                         escape(data.value) + '\">×</span></div>';
                }
              }
            })[0].selectize;
            if (selectize) {
              selectize.refreshOptions();
            }
          }
        });
      });
    ")
    
    session$sendCustomMessage(type = "selectize-config", message = list(
      ids = c("washVarsSelect", "diseaseSelect", "regionSelect", "cda_selectedIndicators", "cda_selectedRegions"),
      options = list(
        render = list(
          item = htmlwidgets::JS("
          function(data, escape) {
            return '<div class=\"select-pill\">' + escape(data.text) + 
                   '<span class=\"remove-btn\" data-id=\"' + this.$input.attr('id') + '\" data-value=\"' + 
                   escape(data.value) + '\">×</span></div>'; 
          }
        ")
        )
      )
    ))
  })
  
  
  
  
  #### TREEMAP (Geo) 
  {
    water_data_raw <- reactive({
      req(input$year)
      tryCatch({
        water_indicators <- read.csv("WDICountryData.csv", 
                                     stringsAsFactors = FALSE, 
                                     na.strings = c("", "NA", "#N/A", ".."))
        water_indicators <- clean_water_data(water_indicators)
        water_long <- prepare_long_data(water_indicators)
        return(water_long)
      }, error = function(e) {
        showNotification(paste("Error loading data:", e$message), type = "error")
        return(NULL)
      })
    })
    
    water_data <- reactive({
      req(water_data_raw())
      water_long_selected <- water_data_raw() %>%
        filter(Year == input$year) %>%
        select(Country = Country.Name, Region, Variable = Series.Name, Value) %>%
        pivot_wider(names_from = Variable, values_from = Value) %>%
        drop_na("Region")
      water_long_selected$`Population, total` <- as.numeric(gsub(",", "", water_long_selected$`Population, total`))
      return(water_long_selected)
    })
    
    output$treemapTitle <- renderText({
      indicator_short_name <- names(which(c(
        "Level of water stress: freshwater withdrawal as a proportion of available freshwater resources" = "Level of water stress",
        "Water productivity, total (constant 2015 US$ GDP per cubic meter of total freshwater withdrawal)" = "Water productivity",
        "People using safely managed drinking water services (% of population)" = "Safely managed drinking water",
        "People using at least basic drinking water services (% of population)" = "Basic drinking water services",
        "People with basic handwashing facilities including soap and water (% of population)" = "Basic handwashing facilities",
        "People using at least basic sanitation services (% of population)" = "Basic sanitation services",
        "People using safely managed sanitation services (% of population)" = "Safely managed sanitation",
        "People practicing open defecation (% of population)" = "Open defecation"
      ) == input$indicator))
      if(length(indicator_short_name) == 0) {
        indicator_short_name <- input$indicator
      }
      paste(indicator_short_name, "by Region and Country (", input$year, ")")
    })
    
    treemap_filtered_data <- reactive({
      req(water_data(), input$indicator)
      water_data() %>% drop_na(input$indicator)
    })
    
    treemap_data <- reactive({
      req(treemap_filtered_data(), input$indicator, input$palette)
      indicator_short_name <- names(which(c(
        "Level of water stress: freshwater withdrawal as a proportion of available freshwater resources" = "Level of water stress",
        "Water productivity, total (constant 2015 US$ GDP per cubic meter of total freshwater withdrawal)" = "Water productivity",
        "People using safely managed drinking water services (% of population)" = "Safely managed drinking water",
        "People using at least basic drinking water services (% of population)" = "Basic drinking water services",
        "People with basic handwashing facilities including soap and water (% of population)" = "Basic handwashing facilities",
        "People using at least basic sanitation services (% of population)" = "Basic sanitation services",
        "People using safely managed sanitation services (% of population)" = "Safely managed sanitation",
        "People practicing open defecation (% of population)" = "Open defecation"
      ) == input$indicator))
      if(length(indicator_short_name) == 0) {
        indicator_short_name <- input$indicator
      }
      treemap(treemap_filtered_data(),
              index = c("Region", "Country"),
              vSize = "Population, total",
              vColor = input$indicator,
              type = "index",
              palette = input$palette, 
              title = paste(indicator_short_name, input$year),
              title.legend = indicator_short_name)
    })
    
    output$avgValueBox <- renderValueBox({
      req(treemap_filtered_data(), input$indicator)
      avg_val <- mean(treemap_filtered_data()[[input$indicator]], na.rm = TRUE)
      valueBox(round(avg_val, 1),
               "Global Average",
               icon = icon("globe"),
               color = "blue")
    })
    
    output$maxValueBox <- renderValueBox({
      req(treemap_filtered_data(), input$indicator)
      max_info <- safe_max_value(treemap_filtered_data(), input$indicator)
      valueBox(round(max_info$value, 1),
               paste("Highest:", max_info$country),
               icon = icon("arrow-up"),
               color = "green")
    })
    
    output$countriesBox <- renderValueBox({
      req(treemap_filtered_data())
      valueBox(nrow(treemap_filtered_data()),
               "Countries with Data",
               icon = icon("flag"),
               color = "purple")
    })
    
    output$staticTreemap <- renderPlot({
      req(treemap_data())
      treemap_data()
    })
    
    output$interactiveTreemap <- renderD3tree({
      req(treemap_data())
      d3tree(treemap_data(), rootname = "Worldwide")
    })
    
    output$downloadPlot <- downloadHandler(
      filename = function() {
        paste("treemap-", gsub(":", "", input$indicator), "-", input$year, ".png", sep = "")
      },
      content = function(file) {
        png(file, width = 1200, height = 800, res = 120)
        print(treemap_data())
        dev.off()
      }
    )
    
    output$downloadTreemapData <- downloadHandler(
      filename = function() {
        paste("treemap-data-", gsub(":", "", input$indicator), "-", input$year, ".csv", sep = "")
      },
      content = function(file) {
        write.csv(treemap_filtered_data(), file, row.names = FALSE)
      }
    )
  }
  
  #### WASH-Based Health Trend Analysis (Forecast) 
  {
    combined_df <- read.csv("combined_complete_data.csv", stringsAsFactors = FALSE)
    combined_df$Year <- as.numeric(combined_df$Year)
    combined_df$DataSource <- ifelse(combined_df$Year > 2020, "Forecast", "Actual")
    
    updateSelectInput(session, "regionSelect", choices = unique(combined_df$Region),
                      selected = unique(combined_df$Region)[1])
    updateSliderInput(session, "yearSelect", min = min(combined_df$Year, na.rm = TRUE),
                      max = max(combined_df$Year, na.rm = TRUE),
                      value = c(min(combined_df$Year, na.rm = TRUE), max(combined_df$Year, na.rm = TRUE)))
    
    filtered_data <- reactive({
      data <- combined_df %>% 
        filter(Year >= input$yearSelect[1] & Year <= input$yearSelect[2],
               Region %in% input$regionSelect,
               DataSource %in% input$dataSourceSelect)
      data
    })
    
    output$mainPlot <- renderPlotly({
      data <- filtered_data()
      if(nrow(data) == 0) {
        return(ggplotly(ggplot() + 
                          annotate("text", x = 0.5, y = 0.5, label = "No data to display") + 
                          theme_void()))
      }
      selected_vars <- if (input$variableType == "WASH Variables") {
        input$washVarsSelect
      } else if (input$variableType == "Disease Rates") {
        input$diseaseSelect
      } else {
        c(input$washVarsSelect, input$diseaseSelect)
      }
      plot_data <- data %>% 
        select(Region, Year, DataSource, all_of(selected_vars)) %>%
        pivot_longer(cols = all_of(selected_vars), names_to = "Variable", values_to = "Value")
      if(input$visualizationType == "Time Series") {
        p <- ggplot(plot_data, aes(x = Year, y = Value, color = Variable, linetype = DataSource)) +
          geom_line(size = 1, alpha = 0.6) +
          geom_point(size = 2) +
          labs(title = "Time Series of Selected Variables", y = "Value") +
          theme_minimal() +
          theme(legend.position = "bottom") +
          scale_linetype_manual(values = c("solid", "dashed"))
      } else if(input$visualizationType == "Scatter Plot") {
        p <- ggplot(plot_data, aes(x = Year, y = Value, color = Variable, shape = DataSource)) +
          geom_point(size = 3) +
          labs(title = "Scatter Plot of Selected Variables", x = "Year", y = "Value") +
          theme_minimal()
      } else if(input$visualizationType == "Bar Chart") {
        p <- ggplot(plot_data, aes(x = Year, y = Value, fill = Variable)) +
          geom_bar(stat = "identity", position = "dodge") +
          labs(title = "Bar Chart of Selected Variables", x = "Year", y = "Value") +
          theme_minimal()
      } else if(input$visualizationType == "Heatmap") {
        p <- ggplot(plot_data, aes(x = Year, y = Variable, fill = Value)) +
          geom_tile() +
          labs(title = "Heatmap of Selected Variables", x = "Year", y = "Variable") +
          scale_fill_viridis_c() +
          theme_minimal()
      }
      ggplotly(p)
    })
    
    output$correlationPlot <- renderPlotly({
      data <- filtered_data()
      if(nrow(data) == 0) {
        return(ggplotly(ggplot() + 
                          annotate("text", x = 0.5, y = 0.5, label = "Not enough data for correlation analysis") + 
                          theme_void()))
      }
      selected_vars <- if (input$variableType == "WASH Variables") {
        input$washVarsSelect
      } else if (input$variableType == "Disease Rates") {
        input$diseaseSelect
      } else {
        c(input$washVarsSelect, input$diseaseSelect)
      }
      cors <- cor(data[selected_vars], use = "complete.obs")
      p <- ggplot(as.data.frame(as.table(cors)), aes(Var1, Var2, fill = Freq)) +
        geom_tile() +
        scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
        labs(title = "Correlation Between WASH Variables and Disease Rates", x = "Variables", y = "Variables") +
        theme_minimal()
      ggplotly(p)
    })
    
    output$dataTable <- DT::renderDataTable({
      data <- filtered_data()
      selected_vars <- if (input$variableType == "WASH Variables") {
        input$washVarsSelect
      } else if (input$variableType == "Disease Rates") {
        input$diseaseSelect
      } else {
        c(input$washVarsSelect, input$diseaseSelect)
      }
      DT::datatable(
        data %>% select(Region, Year, DataSource, all_of(selected_vars)),
        options = list(pageLength = 15)
      )
    })
  }
  
  #### Confirmatory Data Analysis (CDA) 
  merged_data <- tryCatch({
    read.csv("merged_data.csv", stringsAsFactors = FALSE)
  }, error = function(e) {
    showNotification(paste("Error loading merged_data:", e$message), type = "error")
    data.frame() 
  })
  
 
  if(nrow(merged_data) == 0) {
    showNotification("No data loaded. Please check the CSV file.", type = "error")
    return() 
  }
  
 
  updateSelectInput(session, "cda_selectedIndicators", 
                    choices = unique(merged_data$`Series.Name`),
                    selected = unique(merged_data$`Series.Name`)[1])
  
  updateSelectInput(session, "cda_selectedRegions", 
                    choices = unique(merged_data$Region),
                    selected = unique(merged_data$Region)[1:2])
    
    # Correlation Analysis
    cda_filtered_data <- reactive({
      req(input$cda_selectedDisease)
      merged_data %>%
        filter(Year >= input$cda_selectedYear[1],
               Year <= input$cda_selectedYear[2],
               Region %in% input$cda_selectedRegions,
               `Series.Name` %in% input$cda_selectedIndicators) %>%
        mutate(DiseaseRate = .data[[input$cda_selectedDisease]],
               AvgValue = round(AvgValue, 2))
    })
    
    output$cda_correlationPlot <- renderPlot({
      req(cda_filtered_data())
      ggstatsplot::ggscatterstats(
        data = cda_filtered_data(),
        x = AvgValue,
        y = DiseaseRate,
        xlab = "Water Indicator Value",
        ylab = input$cda_selectedDisease,
        title = paste("Correlation between",
                      paste(input$cda_selectedIndicators, collapse = ", "),
                      "and", input$cda_selectedDisease),
        marginal = TRUE
      )
    })
    
    output$cda_dataTable <- DT::renderDataTable({
      df <- cda_filtered_data()
      if (!is.null(input$cda_searchText) && input$cda_searchText != "") {
        search_text <- tolower(input$cda_searchText)
        df <- df %>% filter_all(any_vars(grepl(search_text, tolower(as.character(.)))))
      }
      DT::datatable(
        df,
        options = list(
          pageLength = as.numeric(input$cda_pageSize),
          lengthMenu = c(5, 10, 15, 20, 50),
          searching = FALSE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel'),
          scrollX = TRUE,
          autoWidth = TRUE,
          columnDefs = list(list(className = 'dt-center', targets = "_all"))
        ),
        rownames = FALSE,
        class = 'cell-border stripe hover',
        filter = 'top',
        extensions = c('Buttons')
      ) %>%
        DT::formatRound(columns = c("AvgValue", "DiseaseRate"), digits = 2) %>%
        DT::formatStyle(
          columns = colnames(df),
          backgroundColor = DT::styleEqual(c(NA), c('rgba(255, 0, 0, 0.1)')),
          valueColumns = colnames(df)
        )
    })
    
    output$cda_countStat <- renderValueBox({
      valueBox(nrow(cda_filtered_data()), "Total Records", icon = icon("list"), color = "blue")
    })
    output$cda_waterValueStat <- renderValueBox({
      valueBox(round(mean(cda_filtered_data()$AvgValue, na.rm = TRUE), 2), "Avg Water Value", icon = icon("tint"), color = "aqua")
    })
    output$cda_diseaseRateStat <- renderValueBox({
      valueBox(round(mean(cda_filtered_data()$DiseaseRate, na.rm = TRUE), 2), "Avg Disease Rate", icon = icon("heartbeat"), color = "red")
    })
    output$cda_correlationStat <- renderValueBox({
      valueBox(round(cor(cda_filtered_data()$AvgValue, cda_filtered_data()$DiseaseRate, use = "complete.obs"), 2),
               "Correlation", icon = icon("chart-line"), color = "green")
    })
    
    # ANOVA Analysis
    cda_get_anova_years <- reactive({
      if (input$cda_anovaYearRange == "all") {
        c(min(merged_data$Year), max(merged_data$Year))
      } else if (input$cda_anovaYearRange == "last5") {
        max_year <- max(merged_data$Year)
        c(max_year - 4, max_year)
      } else if (input$cda_anovaYearRange == "last10") {
        max_year <- max(merged_data$Year)
        c(max_year - 9, max_year)
      } else if (input$cda_anovaYearRange == "custom") {
        input$cda_customAnovaYearRange
      }
    })
    
    cda_anova_data <- eventReactive(input$cda_runAnovaBtn, {
      year_range <- cda_get_anova_years()
      data <- merged_data %>% filter(Year >= year_range[1], Year <= year_range[2])
      switch(input$cda_anovaAnalysisType,
             "indicator_across_diseases" = {
               diseases <- c("TyphoidRate", "DiarrheaRate", "HepatitisRate", "UnsafeRisk")
               data <- data %>% filter(`Series.Name` == input$cda_anovaWaterIndicator)
               if (input$cda_anovaRegionForCrossDiseases != "all") {
                 data <- data %>% filter(Region == input$cda_anovaRegionForCrossDiseases)
               }
               data_long <- data %>% select(Year, Region, all_of(diseases)) %>%
                 pivot_longer(cols = all_of(diseases), names_to = "Disease", values_to = "Rate") %>%
                 filter(!is.na(Rate))
               list(data = data_long, x_var = "Disease", y_var = "Rate",
                    title = paste("ANOVA:", input$cda_anovaWaterIndicator, "Across Diseases"))
             },
             "disease_by_region" = {
               if (input$cda_anovaDisease != "all") {
                 data <- data %>% mutate(DiseaseRate = .data[[input$cda_anovaDisease]]) %>%
                   filter(!is.na(DiseaseRate))
                 if (!"all" %in% input$cda_anovaRegions) {
                   data <- data %>% filter(Region %in% input$cda_anovaRegions)
                 }
                 list(data = data, x_var = "Region", y_var = "DiseaseRate",
                      title = paste("ANOVA:", input$cda_anovaDisease, "across Regions"))
               } else {
                 diseases <- c("TyphoidRate", "DiarrheaRate", "HepatitisRate", "UnsafeRisk")
                 if (!"all" %in% input$cda_anovaRegions) {
                   data <- data %>% filter(Region %in% input$cda_anovaRegions)
                 }
                 data_long <- data %>% select(Region, Year, all_of(diseases)) %>%
                   pivot_longer(cols = all_of(diseases), names_to = "Disease", values_to = "Rate") %>%
                   filter(!is.na(Rate))
                 list(data = data_long, x_var = "Region", y_var = "Rate", facet_var = "Disease",
                      title = "ANOVA: All Diseases across Regions")
               }
             },
             "disease_by_indicator" = {
               if (input$cda_anovaDisease2 != "all") {
                 data <- data %>% mutate(DiseaseRate = .data[[input$cda_anovaDisease2]]) %>% filter(!is.na(DiseaseRate))
                 if (!"all" %in% input$cda_anovaIndicators) {
                   data <- data %>% filter(`Series.Name` %in% input$cda_anovaIndicators)
                 }
                 list(data = data, x_var = "Series.Name", y_var = "DiseaseRate",
                      title = paste("ANOVA:", input$cda_anovaDisease2, "across Water Indicators"))
               } else {
                 diseases <- c("TyphoidRate", "DiarrheaRate", "HepatitisRate", "UnsafeRisk")
                 if (!"all" %in% input$cda_anovaIndicators) {
                   data <- data %>% filter(`Series.Name` %in% input$cda_anovaIndicators)
                 }
                 data_long <- data %>% select(`Series.Name`, Year, all_of(diseases)) %>%
                   pivot_longer(cols = all_of(diseases), names_to = "Disease", values_to = "Rate") %>%
                   filter(!is.na(Rate))
                 list(data = data_long, x_var = "Series.Name", y_var = "Rate", facet_var = "Disease",
                      title = "ANOVA: All Diseases across Water Indicators")
               }
             },
             "indicator_by_region" = {
               if (input$cda_anovaIndicator != "all") {
                 data <- data %>% filter(`Series.Name` == input$cda_anovaIndicator, !is.na(AvgValue))
                 if (!"all" %in% input$cda_anovaRegions2) {
                   data <- data %>% filter(Region %in% input$cda_anovaRegions2)
                 }
                 list(data = data, x_var = "Region", y_var = "AvgValue",
                      title = paste("ANOVA:", input$cda_anovaIndicator, "across Regions"))
               } else {
                 if (!"all" %in% input$cda_anovaRegions2) {
                   data <- data %>% filter(Region %in% input$cda_anovaRegions2)
                 }
                 data <- data %>% filter(!is.na(AvgValue))
                 list(data = data, x_var = "Region", y_var = "AvgValue", facet_var = "Series.Name",
                      title = "ANOVA: All Water Indicators across Regions")
               }
             },
             "all_diseases" = {
               diseases <- c("TyphoidRate", "DiarrheaRate", "HepatitisRate", "UnsafeRisk")
               if (input$cda_anovaRegionForDiseases != "all") {
                 data <- data %>% filter(Region == input$cda_anovaRegionForDiseases)
               }
               data_long <- data %>% select(Year, all_of(diseases)) %>%
                 pivot_longer(cols = all_of(diseases), names_to = "Disease", values_to = "Rate") %>%
                 filter(!is.na(Rate))
               list(data = data_long, x_var = "Disease", y_var = "Rate",
                    title = "Comparison of All Disease Rates")
             },
             "all_indicators" = {
               if (input$cda_anovaRegionForIndicators != "all") {
                 data <- data %>% filter(Region == input$cda_anovaRegionForIndicators)
               }
               data <- data %>% filter(!is.na(AvgValue))
               list(data = data, x_var = "Series.Name", y_var = "AvgValue",
                    title = "Comparison of All Water Indicators")
             }
      )
    })
    
    output$cda_anovaSummary <- renderPrint({
      req(cda_anova_data())
      anova_result <- cda_anova_data()
      data <- anova_result$data
      x_var <- anova_result$x_var
      y_var <- anova_result$y_var
      formula <- as.formula(paste(y_var, "~", x_var))
      anova_test <- aov(formula, data = data)
      summary(anova_test)
    })
    
    output$cda_dynamicAnovaSelector1 <- renderUI({
      switch(input$cda_anovaAnalysisType,
             "indicator_across_diseases" = {
               selectInput("cda_anovaWaterIndicator", "Select Water Indicator:",
                           choices = unique(merged_data$`Series.Name`),
                           selected = unique(merged_data$`Series.Name`)[1])
             },
             "disease_by_region" = {
               selectInput("cda_anovaDisease", "Select Disease:",
                           choices = c("All Diseases" = "all",
                                       "Typhoid Rate" = "TyphoidRate",
                                       "Diarrhea Rate" = "DiarrheaRate",
                                       "Hepatitis Rate" = "HepatitisRate",
                                       "Unsafe Water Risk" = "UnsafeRisk"),
                           selected = "TyphoidRate")
             },
             "disease_by_indicator" = {
               selectInput("cda_anovaDisease2", "Select Disease:",
                           choices = c("All Diseases" = "all",
                                       "Typhoid Rate" = "TyphoidRate",
                                       "Diarrhea Rate" = "DiarrheaRate",
                                       "Hepatitis Rate" = "HepatitisRate",
                                       "Unsafe Water Risk" = "UnsafeRisk"),
                           selected = "TyphoidRate")
             },
             "indicator_by_region" = {
               selectInput("cda_anovaIndicator", "Select Water Indicator:",
                           choices = c("All Indicators" = "all", unique(merged_data$`Series.Name`)),
                           selected = unique(merged_data$`Series.Name`)[1])
             },
             "all_diseases" = {
               selectInput("cda_anovaRegionForDiseases", "Select Region:",
                           choices = c("All Regions" = "all", unique(merged_data$Region)),
                           selected = "all")
             },
             "all_indicators" = {
               selectInput("cda_anovaRegionForIndicators", "Select Region:",
                           choices = c("All Regions" = "all", unique(merged_data$Region)),
                           selected = "all")
             }
      )
    })
    
    output$cda_dynamicAnovaSelector2 <- renderUI({
      switch(input$cda_anovaAnalysisType,
             "indicator_across_diseases" = {
               selectInput("cda_anovaRegionForCrossDiseases", "Select Region:",
                           choices = c("All Regions" = "all", unique(merged_data$Region)),
                           selected = "all")
             },
             "disease_by_region" = {
               selectInput("cda_anovaRegions", "Select Regions:",
                           choices = c("All Regions" = "all", unique(merged_data$Region)),
                           selected = c("all"),
                           multiple = TRUE)
             },
             "disease_by_indicator" = {
               selectInput("cda_anovaIndicators", "Select Water Indicators:",
                           choices = c("All Indicators" = "all", unique(merged_data$`Series.Name`)),
                           selected = c("all"),
                           multiple = TRUE)
             },
             "indicator_by_region" = {
               selectInput("cda_anovaRegions2", "Select Regions:",
                           choices = c("All Regions" = "all", unique(merged_data$Region)),
                           selected = c("all"),
                           multiple = TRUE)
             }
      )
    })
    
    output$cda_anovaPlot <- renderPlot({
      req(cda_anova_data())
      anova_result <- cda_anova_data()
      data <- anova_result$data
      x_var <- anova_result$x_var
      y_var <- anova_result$y_var
      title <- anova_result$title
      validate(
        need(nrow(data) > 1, "No data available for the selected criteria."),
        need(x_var %in% colnames(data), "Invalid X variable."),
        need(y_var %in% colnames(data), "Invalid Y variable.")
      )
      y_min <- min(data[[y_var]], na.rm = TRUE)
      y_max <- max(data[[y_var]], na.rm = TRUE)
      y_breaks <- pretty(c(y_min, y_max), n = 10)
      custom_components <- list(
        theme_minimal(),
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
          axis.text.y = element_text(size = 10),
          axis.title = element_text(size = 12, face = "bold"),
          plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 12, hjust = 0.5),
          legend.position = "bottom",
          panel.grid.major = element_line(color = "gray90"),
          panel.grid.minor = element_line(color = "gray95")
        ),
        scale_y_continuous(limits = c(max(0, y_min - (y_max - y_min) * 0.1), y_max + (y_max - y_min) * 0.1),
                           breaks = y_breaks),
        scale_fill_brewer(palette = "Set2")
      )
      if (!is.null(anova_result$facet_var)) {
        ggstatsplot::grouped_ggbetweenstats(
          data = data,
          x = !!rlang::sym(x_var),
          y = !!rlang::sym(y_var),
          grouping.var = !!rlang::sym(anova_result$facet_var),
          xlab = x_var,
          ylab = y_var,
          title = title,
          type = "parametric",
          pairwise.comparisons = TRUE,
          pairwise.display = "significant",
          p.adjust.method = "fdr",
          bf.message = TRUE,
          results.subtitle = TRUE,
          violin.args = list(width = 0.8, alpha = 0.6),
          point.args = list(size = 2, alpha = 0.6),
          centrality.point.args = list(size = 4, color = "darkred"),
          ggplot.component = custom_components
        )
      } else {
        ggstatsplot::ggbetweenstats(
          data = data,
          x = !!rlang::sym(x_var),
          y = !!rlang::sym(y_var),
          xlab = x_var,
          ylab = y_var,
          title = title,
          type = "parametric",
          pairwise.comparisons = TRUE,
          pairwise.display = "significant",
          p.adjust.method = "fdr",
          bf.message = TRUE,
          results.subtitle = TRUE,
          violin.args = list(width = 0.8, alpha = 0.6),
          point.args = list(size = 2, alpha = 0.6),
          centrality.point.args = list(size = 4, color = "darkred"),
          ggplot.component = custom_components
        )
      }
    })
    
    # Bullet Charts
    output$cda_dynamic_indicator_auto <- renderUI({
      if (input$cda_indicator_type_auto == "Water") {
        excluded_indicators <- c("Population density (people per sq. km of land area)", "Population, total",
                                 "People with basic handwashing facilities including soap and water (% of population)")
        available_indicators <- unique(merged_data$`Series.Name`)[!unique(merged_data$`Series.Name`) %in% excluded_indicators]
        selectInput("cda_selected_indicator_auto", "Select Water Indicator:",
                    choices = available_indicators,
                    selected = available_indicators[1])
      } else {
        selectInput("cda_selected_disease_auto", "Select Disease:",
                    choices = c("Typhoid Rate" = "TyphoidRate",
                                "Diarrhea Rate" = "DiarrheaRate",
                                "Hepatitis Rate" = "HepatitisRate",
                                "Unsafe Water Risk" = "UnsafeRisk"),
                    selected = "TyphoidRate")
      }
    })
    
    cda_bullet_data <- reactive({
      req(input$cda_indicator_type_auto, input$cda_auto_year)
      if (input$cda_indicator_type_auto == "Water") {
        req(input$cda_selected_indicator_auto)
        target_value <- if(input$cda_selected_indicator_auto == "Water productivity, total (constant 2015 US$ GDP per cubic meter of total freshwater withdrawal)") {
          NA
        } else if(input$cda_selected_indicator_auto == "Level of water stress: freshwater withdrawal as a proportion of available freshwater resources") {
          0
        } else if(input$cda_selected_indicator_auto == "People practicing open defecation (% of population)") {
          0
        } else {
          100
        }
        water_data_auto <- merged_data %>% filter(`Series.Name` == input$cda_selected_indicator_auto,
                                                  !is.na(AvgValue),
                                                  Year <= input$cda_auto_year) %>% arrange(Year)
        water_data_auto %>% group_by(Region) %>% summarise(
          Min = min(AvgValue, na.rm = TRUE),
          Max = max(AvgValue, na.rm = TRUE),
          Average = round(mean(AvgValue, na.rm = TRUE), 1),
          Monthly = list(AvgValue),
          Actual = if(any(Year == input$cda_auto_year)) mean(AvgValue[Year == input$cda_auto_year], na.rm = TRUE) else NA,
          Target = if(is.na(target_value)) max(AvgValue, na.rm = TRUE) else target_value,
          .groups = "drop"
        )
      } else {
        req(input$cda_selected_disease_auto)
        disease_data_auto <- merged_data %>% filter(!is.na(.data[[input$cda_selected_disease_auto]]),
                                                    Year <= input$cda_auto_year) %>% arrange(Year)
        disease_data_auto %>% group_by(Region) %>% summarise(
          Min = min(.data[[input$cda_selected_disease_auto]], na.rm = TRUE),
          Max = max(.data[[input$cda_selected_disease_auto]], na.rm = TRUE),
          Average = round(mean(.data[[input$cda_selected_disease_auto]], na.rm = TRUE), 1),
          Monthly = list(.data[[input$cda_selected_disease_auto]]),
          Actual = if(any(Year == input$cda_auto_year)) mean(.data[[input$cda_selected_disease_auto]][Year == input$cda_auto_year], na.rm = TRUE) else NA,
          Target = if(input$cda_selected_disease_auto %in% c("UnsafeRisk", "DiarrheaRate","HepatitisRate","TyphoidRate")) 0 else max(.data[[input$cda_selected_disease_auto]], na.rm = TRUE),
          .groups = "drop"
        )
      }
    })
    
    output$cda_bullet_table_auto <- gt::render_gt({
      req(cda_bullet_data())
      validate(need(nrow(cda_bullet_data()) > 0, "No data available for the selected criteria."))
      show_target <- TRUE
      if(input$cda_indicator_type_auto == "Water" && 
         input$cda_selected_indicator_auto == "Water productivity, total (constant 2015 US$ GDP per cubic meter of total freshwater withdrawal)") {
        show_target <- FALSE
      }
      chart <- cda_bullet_data() %>% arrange(desc(Actual)) %>% head(10) %>% gt() %>%
        fmt_number(columns = c("Min", "Max", "Average", "Actual"), decimals = 1)
      chart <- chart %>% gtExtras::gt_plt_sparkline(column = "Monthly", type = "default")
      if(show_target) {
        chart <- chart %>% gtExtras::gt_plt_bullet(
          column = "Actual",
          target = "Target",
          width = 108,
          palette = if (input$cda_indicator_type_auto == "Water") c("lightblue", "black") else c("salmon", "black")
        )
      }
      chart <- chart %>% cols_label(
        Min = "Min",
        Max = "Max",
        Average = "Avg",
        Actual = "Value",
        Monthly = "Trend",
        Target = "Target"
      )
      chart %>% gtExtras::gt_theme_espn()
    })
  }


shinyApp(ui = ui, server = server)