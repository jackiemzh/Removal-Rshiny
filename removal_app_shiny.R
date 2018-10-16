# install the following packages

library(shiny)
library(xlsx)
library(markdown)
library(DT)
library(shinythemes)
library(shinycssloaders)
#


# UI function for managing the layout

ui <- fluidPage(theme = shinytheme("sandstone"),
                
                # logo
                titlePanel(img(height = 85, width = 250, src = "deplete_logo.png")),
                
                #titlePanel("DEPLETE"),
                
                # display loading when busy
                tags$head(tags$style(type="text/css", "
                                     #loadmessage {
                                     position: fixed;
                                     top: 1px;
                                     left: 0px;
                                     width: 100%;
                                     padding: 5px 0px 5px 0px;
                                     text-align: center;
                                     font-weight: bold;
                                     font-size: 100%;
                                     color: #000000;
                                     background-color: #f9f4b3;
                                     z-index: 105;
                                     }")),
                navbarPage("Menu",
                           
                           #################### classic model display tab
                           tabPanel("Classic model",
                                    
                                    tabsetPanel(type = "tabs",
                                                
                                                id = "classic_panels",
                                                
                                                tabPanel(h5("Datasets"), icon = icon("table"),
                                                         
                                                         h4("Upload data from Excel.
                                                            When you uploded your data file, please"),
                                                         
                                                         actionLink("link_to_tabpanel_set_tab", h4("continue to Settings")),
                                                         
                                                         # Input: upload dataset ----
                                                         fileInput("upload_data_classic", 
                                                                   h4("Input data file"),
                                                                   accept = c(
                                                                     'text/csv',
                                                                     'text/comma-separated-values,text/plain',
                                                                     '.csv',
                                                                     '.xlsx')
                                                                  ),
                                                         
                                                         # Input: Checkbox if file has header ----
                                                         checkboxInput("header", h4("Check if file has headers"), TRUE),
                                                         
                                                         # Output: preview data ++++
                                                         wellPanel(h4("Preview dataset"),
                                                                   DT::dataTableOutput("data1")
                                                                   ),
                                                         
                                                         tags$hr()
                                                         
                                                         ),
                                                
                                                tabPanel(h5("Settings"), icon = icon("cog", lib = "glyphicon"), 
                                                         
                                                         value="setting-panel",
                                                         
                                                         h4("Modify your model settings."),
                                                         
                                                         h4("When you finish with the settings, please"),
                                                         
                                                         actionLink("link_to_tabpanel_ana_tab", h4("continue to Analysing")),
                                                         
                                                         # Output: data summary ++++
                                                         wellPanel(h4("Data summary"),
                                                                   tableOutput("data_summary")
                                                         ),
                                                         
                                                         # potential models output
                                                         wellPanel(h4("A list of models to be fitted."),
                                                                    tableOutput("all_model_table")
                                                         ),
                                                         
                                                         
                                                         
                                                         # Input: data column number ----
                                                         numericInput("data_colnum", 
                                                                      h4("Indicate data column number"), 
                                                                      value = 2),
                                                         
                                                         # Input: covariates available or not? ----
                                                         selectInput("cov_yn", 
                                                                     h4("Covariates available?"), c("Yes","No")
                                                         ),
                                                         
                                                         
                                                         conditionalPanel( condition = "output.cov_yn_select",
                                                                           
                                                                           # Input: Specification of range within an interval ----
                                                                           sliderInput("cov_col_range", "Indicate covariate column numbers:",
                                                                                       min = 0, max = 20, # *** customise column range?
                                                                                       value = c(7,10)
                                                                           )
                                                                           
                                                                          #selectInput("cov_num", 
                                                                          #             h4("How many covariates considered?"), c("1", "2")
                                                                          # )
                                                                          
                                                                          # *** conditional options for classic model
                                                                           
                                                                           
                                                                          ),
                                                         
                                                         # reset inputs to default
                                                         
                                                         #uiOutput('resetable_input'),
                                                         #tags$hr(),
                                                  
                                                         actionButton("reset_input", "Reset to default",
                                                                      icon("refresh"), # (paper-plan, refresh)
                                                                      style="color: #fff; background-color: #74aad8; border-color: #2e6da4"
                                                                      ),
                                                         
                                                         tags$hr()
                                                         
                                                         # *** add more options in settings for classic model
                                                         
                                                         # submitButton("Apply")
                                                         ),
                                                
                                                tabPanel(h5("Analysing"),icon = icon("laptop", lib = "font-awesome"),
                                                         
                                                         #shinyjs::useShinyjs(), # from shinyjs library
                                                         value = "analysing-panel", # name this tab
                                                         
                                                         h4("Analyse your dataset."),
                                                         
                                                         h4("This may take several minutes depending on the size of your data, 
                                                            customised settings."),
                                                         
                                                         
                                                         h4("To avoid local maximum in the maximum likelihood estimation, we suggest run each model multiple times.
                                                             We suggest at least two maximum are found to make sure the algorithm is optimised.
                                                             Please specify the number of iterations used for each model (default is ten). 
                                                             "),
                                                         
                                                         # updating potential models situation (done, running or waiting) while once hit the run button
                                                         
                                                         wellPanel(h4("Model summary"),
                                                                   
                                                                   tableOutput("all_ana_model_table")
                                                                   
                                                         ),
                                                         
                                                         #wellPanel(h4("Check maximum log-likelihood outputs"),
                                                                   
                                                        #           verbatimTextOutput("check_mle")
                                                        # ),
                                                         
                                                         
                                                         # Test output
                                                         # tableOutput("out2"),
                                                         
                                                         # Input: data column number ----

                                                         numericInput("num_iteration", 
                                                                      h4("Indicate the number of iterations"), 
                                                                      value = 10),
                                                         
                                                         # notification icon exlamation-circle
                                                         
                                                         actionButton("run_ana_button","Run analysis",
                                                                      icon = icon("chevron-circle-right", lib = "font-awesome"),
                                                                      style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                                         
                                                         actionButton("reset_ana_button","Reset to default",
                                                                      icon = icon("refresh"),
                                                                      style="color: #fff; background-color: #74aad8; border-color: #2e6da4"),
                                                         
                                                         
                                                         h4("The dataset has been analysed, please"),
                                                         
                                                         actionLink("link_to_tabpanel_results_tab", h4("continue to Results")),
                                                         
                                                         tags$hr()
                                                        ),
                                                tabPanel(h5("Results"), icon = icon("stats", lib = "glyphicon"),
                                                         
                                                         
                                                         
                                                         # shinyjs::useShinyjs(), # from shinyjs library
                                                         value= "results-panel", # name this tab
                                                         
                                                         h4("Results are summarised."),
                                                         
                                                         # Classic model results tab ----
                                                         tabsetPanel(type = "pills",
                                                                     
                                                                     tabPanel("Plot results", icon = icon("bar-chart-o"),
                                                                              
                                                                              # Input: customise species name ----
                                                                              #textInput("species_name",h6("Species name"),value=("individuals")),
                                                                              
                                                                              # Output: classic model plot ++++
                                                                              withSpinner(plotOutput("plot")),
                                                                              
                                                                              # Button
                                                                              downloadButton("downloadPlot", "Download plot",
                                                                                             style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                                                              
                                                                              # Button
                                                                              downloadButton("downloadPredicted", "Download fitted dataset",
                                                                                             style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                                                              
                                                                              tags$hr()
                                                                              
                                                                              ),
                                                                     
                                                                     tabPanel("Model_comparison", icon = icon("sort-by-attributes", lib = "glyphicon"),
                                                                              #icon = icon("list-alt"),
                                                                              
                                                                              # Output: classic model comparison ++++
                                                                              tableOutput("fit_table"),
                                                                              
                                                                              
                                                                              # Button
                                                                              downloadButton("downloadModelCom", "Download results",
                                                                                             style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                                                              tags$hr()
                                                                              ),
                                                                     
                                                                     tabPanel("Estimates", icon = icon("th-list", lib = "font-awesome"),
                                                                              # "table"
                                                                              # Output: classic model estimates ++++
                                                                              tableOutput("estimates"),
                                                                              
                                                                              
                                                                              # Button
                                                                              downloadButton("downloadEstimates", "Download estiamtes",
                                                                                             style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                                                              tags$hr()
                                                                              )
                                                                     
                                                                     #tabPanel("Predicted data", 
                                                                              
                                                                              # Output: predicted values from a classic model ++++
                                                                    #          tableOutput("predicted_table")
                                                                              
                                                                              
                                                                     #         ) 
                                                                     )
                                                        )
                                                  ),      
                                     
                                    conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                                     tags$div("Loading...",id="loadmessage")
                                                    )   
                                    ),

                           #################### RD model display tab
                           tabPanel("Robust design model",
                                    
                                    tabsetPanel(type = "tabs",
                                                
                                                id = "RD_panels",
                                                
                                                tabPanel(h5("Datasets"), icon = icon("table"),
                                                         
                                                         h4("Upload data from Excel.
                                                            When you uploded your data file, please"),
                                                         
                                                         actionLink("link_to_tabpanel_set_tab_RD", h4("continue to Settings.")),
                                                         
                                                         # Input: upload dataset ----
                                                         fileInput("upload_data_RD", 
                                                                   h4("Input data file"),
                                                                   accept = c(
                                                                     'text/csv',
                                                                     'text/comma-separated-values,text/plain',
                                                                     '.csv',
                                                                     '.xlsx')
                                                         ),
                                                         
                                                         # Input: Checkbox if file has header ----
                                                         checkboxInput("header", h4("Check if file has headers"), TRUE),
                                                         
                                                         # Output: preview (RD) data ++++
                                                         wellPanel(h4("Preview dataset"),
                                                                   DT::dataTableOutput("data1_RD")
                                                         ),
                                                         
                                                         tags$hr()
                                                         
                                                         ),
                                                
                                                tabPanel(h5("Settings"), icon = icon("cog", lib = "glyphicon"), 
                                                         
                                                         value="setting_RD-panel",
                                                         
                                                         h4("Modify your model settings.
                                                            When you finish with the settings, please"),
                                                         
                                                         actionLink("link_to_tabpanel_ana_tab_RD", h4("continue to Analysing")),
                                                         
                                                         # Output: data summary ++++
                                                         wellPanel(h4("Data summary"),
                                                                   tableOutput("data_summary_RD")
                                                         ),
                                                         
                                                         
                                                         
                                                         # potential models output
                                                         wellPanel(h4("A list of models to be fitted."),
                                                                   tableOutput("all_model_table_RD")
                                                                   ),
                                                        
                                                         
                                                         fluidRow(
                                                           column(3,
                                                                  
                                                                  numericInput("data_colnum_RD", 
                                                                               h5("Indicate data column number"), 
                                                                               value = 7),
                                                                  
                                                                  # Input: does the dataset have equal no. of secondary samples? ----
                                                                  
                                                                  selectInput("equ_secondary_num_yn", 
                                                                              h5("Equal number of secondary samples withinn each primary period?"), c("","Yes", "No")
                                                                              ),
                                                                  
                                                                  conditionalPanel( condition = "output.equ_secondary_num_null",
                              
                                                                                    NULL
                                                                                    
                                                                                   ),
                                                                  
                                                                  conditionalPanel( condition = "output.equ_secondary_num_yes",
                                                                                    
                                                                                    # Input: give k_i if is equal across the study ----
                                                                                    
                                                                                    numericInput("no_secondary_occasion", 
                                                                                                 h6("  Indicate the number of secondary samples"), 
                                                                                                 value = 2)
                                                                                    
                                                                                   ),
                                                                  
                                                                  conditionalPanel( condition = "output.equ_secondary_num_no",
                                                                                    
                                                                                    # Input: give k_i index if not equal ----
                                                                                    
                                                                                    numericInput("RD_index", 
                                                                                                 h6("  Robust design index column number"), 
                                                                                                 value = 2)
                                                                                   )
                                                                                    
                                                                                    ## *** end of secondary sample conditional options for RD model
                                                                  ),
                                                                  
                                                                  
                                                                  
                                                           
                                                           column(4, offset = 1,
                                                                  
                                                                  # Input: covariates available or not? ----
                                                                  selectInput("cov_yn_RD", 
                                                                              h4("Covariates available?"), c("Yes", "No")
                                                                  ),
                                                                  
                                                                  conditionalPanel( condition = "output.cov_yn_select_RD",
                                                                                    
                                                                                    # Input: Specification of range within an interval ----
                                                                                    sliderInput("cov_col_range_RD", "Indicate covariate column numbers:",
                                                                                                min = 0, max = 20, # *** customise column range?
                                                                                                value = c(12,18)
                                                                                                )
                                                                                    
                                                                                    ## end of conditional options for RD model
                                                                                   )
                                                                  
                                                                  
                                                                  # *** add more options in settings for RD model
                                                                  
                                                                  # submitButton("Apply")
                                                                  
                                                                  ),
                                                           
                                                           column(4,
                                                                  
                                                                  # h4("options for p and phi?"),
                                                                  
                                                                  # Input: phi type ---- options conditional cov_yn
                                                                  selectInput("phi_type_RD", 
                                                                              h4("Define transition probability type"), 
                                                                              c("","All","Constant", "Covariates","Time-varying")
                                                                              ),
                                                                  
                                                                  # Input: p type  ----  options conditional cov_yn
                                                                  selectInput("p_type_RD", 
                                                                              h4("Define capture probability type"), 
                                                                              c("","All","Constant", "Covariates")
                                                                              )
                                                                  
                                                                  
                                                                   )
                                                           
                                                           # end of three column page
                                                         ),
                                                         
                                                         # reset inputs to default
                                                         
                                                         #uiOutput('resetable_input'),
                                                         #tags$hr(),
                                                         
                                                         
                                                         
                                                         actionButton("reset_input_RD", "Reset to default",
                                                                      icon("refresh"), # (paper-plan, refresh)
                                                                      style="color: #fff; background-color: #74aad8; border-color: #2e6da4"
                                                                      ),
                                                         
                                                        
                                                         tags$hr()
                                                         
                                                         ),
                                                
                                                tabPanel(h5("Analysing"),icon = icon("laptop", lib = "font-awesome"),
                                                         
                                                         #shinyjs::useShinyjs(), # from shinyjs library
                                                       value = "analysis_RD-panel", # name this tab
                                                         
                                                         h4("Analyse your dataset."),
                                                         
                                                         h4("This may take several minutes depending on the size of your data, 
                                                            customised settings."),
                                                         
                                                         
                                                         h4("To avoid local maximum in the maximum likelihood estimation, we suggest run each model multiple times.
                                                             We suggest at least two maximum are found to make sure the algorithm is optimised.
                                                             Please specify the number of iterations used for each model (default is ten). 
                                                             "),
                                                         
                                                       
                                                         # updating potential models situation (done, running or waiting) while once hit the run button
                                                         
                                                         wellPanel(h4("Model summary"),
                                                                   
                                                                   tableOutput("all_ana_model_table_RD")
                                                                   ),
                                                                   
                                                         
                                                         # Test output
                                                         # textOutput("out3"),
                                                         
                                                         
                                                        # wellPanel(h4("Check maximum log-likelihood outputs"),
                                                                   
                                                                   # MLE outputs
                                                                   
                                                        #           verbatimTextOutput("check_mle_RD")
                                                        # ),
                                                                  
                                                        

                                                         # Input: (RD) data column number ----
                                                         
                                                         numericInput("num_iteration_RD", 
                                                                      h4("Indicate the number of iterations"), 
                                                                      value = 10),
                                                         
                                                         # notification icon exlamation-circle
                                                         
                                                         actionButton("run_ana_button_RD","Run analysis",
                                                                      icon = icon("chevron-circle-right", lib = "font-awesome"),
                                                                      style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                                         
                                                         actionButton("reset_ana_button_RD","Reset to default",
                                                                      icon = icon("refresh"),
                                                                      style="color: #fff; background-color: #74aad8; border-color: #2e6da4"),
                                                         
                                                         hr(),
                                                         
                                                         
                                                         # current process
                                                         
                                                         h4("The dataset has been analysed, please"),
                                                         
                                                         actionLink("link_to_tabpanel_results_tab_RD", h4("continue to Results.")),
                                                         
                                                         tags$hr()
                                                         
                                                         ),
                                                
                                                tabPanel(h5("Results"), icon = icon("stats", lib = "glyphicon"),
                                                         
                                                         #shinyjs::useShinyjs(), # from shinyjs library
                                                         value= "results_RD-panel", # name this tab
                                                         
                                                         h4("Results are summarised."),
                                                         
                                                         # (RD) model results tab ----
                                                         tabsetPanel(type = "pills",
                                                                     
                                                                     tabPanel("Plot results", icon = icon("bar-chart-o"),
                                                                              
                                                                              # Input: customise species name ----
                                                                              textInput("species_name_RD",h6("Species name"),value=("individuals")),
                                                                              
                                                                              # Output: (RD) model plot ++++
                                                                              withSpinner(plotOutput("plot_RD")),
                                                                              
                                                                              # Button
                                                                              downloadButton("downloadPlot_RD", "Download plot",
                                                                                             style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                                                              
                                                                              # Button
                                                                              downloadButton("downloadPredicted_RD", "Download fitted dataset",
                                                                                             style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                                                              tags$hr()
                                                                              
                                                                     ),
                                                                     
                                                                     tabPanel("Model_comparison", icon = icon("sort-by-attributes", lib = "glyphicon"),
                                                                              #icon = icon("list-alt"),
                                                                              
                                                                              # Output: (RD) model comparison ++++
                                                                              tableOutput("fit_table_RD"),
                                                                              
                                                                              
                                                                              # Button
                                                                              downloadButton("downloadModelCom_RD", "Download results",
                                                                                             style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                                                              tags$hr()
                                                                     ),
                                                                     
                                                                     tabPanel("Estimates", icon = icon("th-list", lib = "font-awesome"),
                                                                              # "table"
                                                                              # Output: (RD) model estimates ++++
                                                                              tableOutput("estimates_RD"),
                                                                              
                                                                              
                                                                              # Button
                                                                              downloadButton("downloadEstimates_RD", "Download estiamtes",
                                                                                             style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                                                              tags$hr()
                                                                     )
                                                                     
                                                                     #tabPanel("Predicted data", 
                                                                     
                                                                     # Output: predicted values from a RD model ++++
                                                                     #          tableOutput("predicted_table")
                                                                     
                                                                     
                                                                     #         ) 
                                                         )
                                                )
                                                ),      
                                    
                                    conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                                     tags$div("Loading...",id="loadmessage")
                                    )   
                           ),        
                           
                           
                           
                           
                           
                           
                           
                           
                           
                           ################### Help and about us
                           navbarMenu("More",
                                      tabPanel("Help!",
                                               h3("Coming soon..")
                                               ),
                                      tabPanel("About us",
                                               h3("Add infom..")
                                               )
                                      ),
                           
                           # add navbarPage() options
                           fluid = TRUE
                            )
               # add fluidPage() options
                 )          


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Define server logic ----
server <- function(input, output, session) {
  
  
  # reactive UI to reset (classic) inputs
  # if observe action button, reset to default
  observe({
    
    input$reset_input
    
    # reset data_colnum
    updateNumericInput(session, "data_colnum", value = 2)
    
    # Can also set the label and select items
    updateSelectInput(session,
                      "cov_yn", # input name
                      label = paste("Covariates available?"),
                      choices = c("Yes","No"),
                      selected = c("Yes")
                      )
  })

  # reset UI in analysis tab

  observe({
    
    input$reset_ana_button
    
    # update action buttion to NULL (never clicked before)
    # updateActionButton(session,"run_ana_button",
    #                   label = "Run analysis again", 
    #                   icon = icon("chevron-circle-right", lib = "font-awesome")
                       #style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
    #                   )
    
    # reset data_colnum
    updateNumericInput(session, "num_iteration", value = 10)
    
  })
  
  
  
  # links in classic panel
  observeEvent(input$link_to_tabpanel_set_tab, {
    newvalue <- "setting-panel"
    updateTabItems(session, "classic_panels", newvalue)
  })
  
  observeEvent(input$link_to_tabpanel_ana_tab, {
    newvalue <- "analysing-panel"
    updateTabItems(session, "classic_panels", newvalue)
  })
  
  observeEvent(input$link_to_tabpanel_results_tab, {
    newvalue <- "results-panel"
    updateTabItems(session, "classic_panels", newvalue)
  })
  
  
  
  # links in RD panel
  observeEvent(input$link_to_tabpanel_set_tab_RD, {
    newvalue <- "setting_RD-panel"
    updateTabItems(session, "RD_panels", newvalue)
  })
  

  observeEvent(input$link_to_tabpanel_ana_tab_RD, {
    newvalue <- "analysis_RD-panel"
    updateTabItems(session, "RD_panels", newvalue)
  })
  
  observeEvent(input$link_to_tabpanel_results_tab_RD, {
    newvalue <- "results_RD-panel"
    updateTabItems(session, "RD_panels", newvalue)
  })
  
  
  # reactive UI to reset (RD) inputs
  
  # if observe action button, reset to (RD) default settings
  observe({
    
    input$reset_input_RD
    
    # reset data_colnum
    updateNumericInput(session, "data_colnum_RD", value = 7)
    
    # reset covariable or not options
    updateSelectInput(session,
                      "cov_yn_RD", # input name
                      label = paste("Covariates available?"),
                      choices = c("No","Yes"),
                      selected = c("No")
                      )
    # reset equ_secondary_num_yn to null
    updateSelectInput(session,
                      "equ_secondary_num_yn", # input name
                      label = paste("Equal number of seconary samples within each primary period?"),
                      choices = c("", "No","Yes"),
                      selected = c("")
                      )
    
    # reset p type options to null
    updateSelectInput(session,
                      "p_type_RD", # input name
                      label = paste("Define capture probability type"),
                      choices = c("","All","Constant", "Covariates"),
                      selected = c("")
                      )
    
    # reset phi type options to null
    updateSelectInput(session,
                      "phi_type_RD", # input name
                      label = paste("Define transition probability type"),
                      choices = c("","All","Constant", "Covariates","Time-varying"),
                      selected = c("")
                      )
    
  })
  
  # reset UI in (RD) analysis tab
  
  observe({
    
    input$reset_ana_button_RD
    
    # update action buttion to NULL (never clicked before)
    # updateActionButton(session,"run_ana_button",
    #                   label = "Run analysis again", 
    #                   icon = icon("chevron-circle-right", lib = "font-awesome")
    #style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
    #                   )
    
    # reset data_colnum
    updateNumericInput(session, "num_iteration_RD", value = 10)
    
  })
  
  

  
  
  
  # not working
  # observeEvent(
  #  input$reset_ana_button, {
  #  shinyjs::reset("analysis-panel") # reset funtion in analysis tab
  # })
  
  # reset UI in results tab 
  # observe({
  #  input$reset_ana_button
    
  # shinyjs::reset("results-panel")
    
  #})
  
  #observe
  
  
 # analysis-panel
  
  
#  run_analysis_buttion
  
  
  
  #output$resetable_input <- renderUI({
  #  times <- input$reset_input
  #  div(id=letters[(times %% length(letters)) + 1],
        
        # add default settings
  #      )
  # })
  
  
  # functions
  
  logit <- function(x){ log(x/(1-x)) }
  expit <- function(x){ 1/(1+exp(-x)) }
  
  # use reactive function to update input values
  
  
  # target data
  #data1<- reactive({
  
  # data_all<- dataInput()
  #data_all[,input$data_colnum]
  
  #})
  
  ########  prep for geometric removal data
  dataInput <- reactive({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$upload_data_classic)
    
    infile <- input$upload_data_classic
    
    read.xlsx(infile$datapath, sheetIndex=1,header = input$header)
    
    
  })
  
  
  # read covariates column names - are used for covariate names later
  data_cov_names<- reactive({
    
    if (input$cov_yn=="Yes") {
      
    data_all<- dataInput()
    
    column_names<- colnames(data_all)
    cov_names<- column_names[input$cov_col_range[1]:input$cov_col_range[2]]
  
    }
    
    return(cov_names)
  })
  
  dataTotalcol<- reactive({
    # total number of columns in the uploaded data
    # to limite the number of options in the input selections.
    
    data_all<- dataInput()
    Totalcol = ncol(data_all)
    Totalcol
  })
  
  D<- reactive({
    data_all<- dataInput()
    data1=data_all[,input$data_colnum]
    
    sum(data1[which( !is.na(data1), arr.ind=TRUE)])
    
  })
  
  T<-reactive({
    data_all<- dataInput()
    data1=data_all[,input$data_colnum]
    length(data1)})
  
  
  x<-reactive({
    
    data_all<- dataInput()
    data1=data_all[,input$data_colnum]
    
    x0<-rep(0,T()+1)
    for( k in 2:(T()+1)){x0[k]=sum(data1[1:(k-1)])}
    x0
  })
  
  x.d<-reactive({
    data_all<- dataInput()
    data1=data_all[,input$data_colnum]
    
    x.d0<-rep(0,T()-1)
    for (k in 1:(T()-1)){x.d0[k]=x()[T()+1]-x()[k+1]}
    x.d0
  })
  
  
  # data plot
  classic_data_plot<-reactive({
    data_all<- dataInput()
    data_ploints<-data_all[,input$data_colnum]
    
    plot(data_ploints,
         pch=10,
         xlab="sampling occasions",
         ylab="counts",
         main=paste("Predicted counts of individuals removed.")
         )
  })
  
  
  # print_plot function

  print_plot<-reactive({
    
    if (run_button_counts$counts>0) {

      # name the fitted counts results
      fitted.counts.tab<- isolate(fitted.results.geo()$estimated.counts)
      
      # isolate this to avoid reactive plot function
      data_plot<- plot(c(1:T()),fitted.counts.tab$observed_data,
                              pch="+",
                              col = "black",
                              lwd=2,#pch=10,
                              xlab="sampling occasions",
                              ylab="counts",
                              main=paste("Predicted counts of individuals removed.")
                              )

      
      lines(c(1:T()), fitted.counts.tab$observed_data,col = "black")
      
      # fitted data
      points(c(1:T()), fitted.counts.tab$estimated_data,pch=2, col = "red") # estimated data, red triangle
      lines(c(1:T()), fitted.counts.tab$estimated_data,col = "red")
      
      # 95% CI
      #lines(c(1:T()), fitted.counts.tab$lower_CI, col = "red", lwd=2,lty=3) # lower CI, dashed gray line
      #lines(c(1:T()), fitted.counts.tab$upper_CI, col = "red", lwd=2,lty=3) # upper CI, dashed gray line
      

  #  data_all<- isolate(dataInput())
  #  data_ploints<-data_all[,input$data_colnum]
    
    # plot data points
    # pfs[[1]]()
    
    
    # predicted data *** change to the best model
    
   # est_data<- hh.geo.pc.prediction(hh=1)$est.count.results$est.num
  #  points(c(1:T()), est_data,pch=13)
    #lines(c(1:T()), est_data, col = 'black', lwd=2,lty=3)
    
    #*lower<- hh.geo.pc.prediction(hh=1)$est.count.results$LCI
    #lines(c(1:T()), lower, col = 'black', lwd=2,lty=3) 
    
    #*upper<- hh.geo.pc.prediction(hh=1)$est.count.results$UCI
    #lines(c(1:T()), upper, col = 'black', lwd=2,lty=3) 
    
    
    } else {NULL}
    
    return(data_plot)
    
  })
  
  
  ## conditional panel geo
  
  # display hidden content if input$cov_yn=="Yes"
  output$cov_yn_select <- reactive({
    input$cov_yn=="Yes"
  })
  
  outputOptions(output, "cov_yn_select", suspendWhenHidden = FALSE) 
  
  
  
  
  ########  prep for RD data
  
  dataInput_RD <- reactive({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$upload_data_RD)
    
    infile <- input$upload_data_RD
    
    read.xlsx(infile$datapath, sheetIndex=1,header = input$header)
    
  })
  
  
  # read RD covariates column names - are used for covariate names later
  data_cov_names_RD<- reactive({
    
    if (input$cov_yn_RD=="Yes") {
      
      # read RD data
      data_all<- dataInput_RD()
      
      column_names<- colnames(data_all)
      cov_names_RD<- column_names[input$cov_col_range_RD[1]:input$cov_col_range_RD[2]]
      
    }
    
    return(cov_names_RD)
  })
  
  
  # total no. of observed individuals
  D_RD<- reactive({
    data_all<- dataInput_RD()
    data1_RD=data_all[,input$data_colnum_RD]
    
    sum_RD<-sum(data1_RD[which(!is.na(data1_RD), arr.ind=TRUE)])
    
    return(sum_RD)
  })

  
  # length of study
  T_RD<-reactive({
    data_all<- dataInput_RD()
    data1_RD=data_all[,input$data_colnum_RD]
    length(data1_RD)})
  
  
  no_k<-reactive({
    input$no_secondary_occasion
  })
  
  # missing occasions
  miss_occ_RD<-reactive({
    data_all<- dataInput_RD()
    data1_RD=data_all[,input$data_colnum_RD]
    
    miss_records<-which(is.na(data1_RD), arr.ind=TRUE)
    miss_records
  })
  
  
  # download plot
  download_plot_RD<-reactive({
    
    # Take a dependency on input$goButton. This will run once initially,
    # because the value changes from NULL to 0.
    input$run_ana_button_RD
    
    data_all<- isolate(dataInput_RD())
    data_ploints<-data_all[,input$data_colnum]
    
    plot(data_ploints,
         pch=10,
         xlab="sampling occasions",
         ylab="counts",
         main=paste("Predicted counts of individuals removed.")
    )
    
    
  })
  
  
  ## display hidden content if input$cov_yn_RD=="Yes"
  output$cov_yn_select_RD <- reactive({
    input$cov_yn_RD=="Yes"
  })
  
  outputOptions(output, "cov_yn_select_RD", suspendWhenHidden = FALSE) 
  
  
  
  
  ## display hidden content if input$equ_secondary_num_yn=="Yes"
  output$equ_secondary_num_yes <- reactive({
    input$equ_secondary_num_yn=="Yes"
  })
  
  outputOptions(output, "equ_secondary_num_yes", suspendWhenHidden = FALSE) 
  
  ## display hidden content if input$equ_secondary_num_yn=="No"
  output$equ_secondary_num_no <- reactive({
    input$equ_secondary_num_yn=="No"
  })
  
  outputOptions(output, "equ_secondary_num_no", suspendWhenHidden = FALSE) 
  
  
  ## display hidden content if input$equ_secondary_num_yn==""
  output$equ_secondary_num_null <- reactive({
    input$equ_secondary_num_yn==""
  })
  
  outputOptions(output, "equ_secondary_num_null", suspendWhenHidden = FALSE) 
  
  
  
  
  
  ################################  fit the geo model
  ################################  optimisation for geo
  
  geo.prediction <- reactive({
    
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    data_all<- dataInput()
    data1=data_all[,input$data_colnum]
    
    geo.ll <- function (param,data,x,T,D){
      p=expit(param[1]);n0=exp(param[2]);N=n0+D
      
      logL=lgamma(N+1)-sum(lgamma(data+1))-lgamma(n0+1)+
        D*log(p)+(T*N-sum(x))*log(1-p)
      return(-logL)
    }
    
    param=c(logit(0.3),log(10)) # random starting number
    fit.geo<-optim(par=param,
                   fn=geo.ll,method="BFGS",hessian=TRUE,
                   data=data1,x=x(),T=T(),D=D())
    
    #return(ept.data)
    return(fit.geo)
    
  })
  
  
  ################################  estimates from geo
  
  ept.p.geo<- reactive({ expit(geo.prediction()$par[1])})
  
  ept.N.geo<- reactive({ exp(geo.prediction()$par[2])+D()})
  
  ################################  predicted numbers from geo
  
  #ept.num.geo<- reactive({ 
  
  #  ept.data<-rep(0,T()); ept.data[1]<-ept.N.geo()*ept.p.geo()
  # for(i in 2:T()) {ept.data[i]<-ept.N.geo()*ept.p.geo()*(1-ept.p.geo())^(i-1)}
  
  #ept.data
  
  #  })
  
  ################################  SE from geo  
  
  
  se.p.geo<- reactive({ 
    se<-sqrt(diag(solve(geo.prediction()$hessian)))
    se<-expit(se[1])
    se
  })
  
  se.n0.geo<- reactive({ 
    se<-sqrt(diag(solve(geo.prediction()$hessian)))
    se<-exp(se[2])
    se
  })
  
  
  ################################  estimate table results from geo  
  geo.est.tab<-reactive({ 
    
    
    #geo.aic.models()$model.names.ordered[1]
    #geo.aic.models()$p_cov_used_ordered[1]
    # "p_average"
    # Need some words for explainations of results here
    
    # *** customise the length of p estimates
    estimates.tab.results<-data.frame(Notation=c("p_int","p_slope1","p_slope2","n_0"),
                                      Estimates=c("","","",""),
                                      SE=c("","","",""),
                                      LowerCI=c("","","",""),
                                      UpperCI=c("","","","")
    )

    estimates.tab.results
  })
  
  
  
  
  ################################  fit the geo.pt model
  ################################  optimisation for geo.pt
  
  no_covariate_available<-reactive({
    
    # calculate the number of covarates available
    
    if (input$cov_yn=="Yes") {
      no_cov <- input$cov_col_range[2]-input$cov_col_range[1]+1
    } else {
      no_cov<-0
    }
    
    return(no_cov)
  })
  
  
  # define the total no. of potential models to fit
  total_no_fits<-reactive({
    
  if (input$cov_yn=="Yes") {

    if (no_covariate_available()==1) {
      
      # if there is one cov
      no_fits = 2 # i.e. p(c) and p(cov) 
      
      no_one_cov_cases = 1
      
      no_two_cov_cases = 0
      
    } else {
      
      # no. of one cov cases
      no_one_cov_cases = no_covariate_available()
      
      # no. of two cov cases
      no_two_cov_cases = choose(no_covariate_available(),2)*2 # addictive cases + addicative_interaction cases 
      
      # constant model + p(one_cov) cases + p(two_cov) cases
      no_fits= 1 + no_one_cov_cases + no_two_cov_cases 
    }
  } else {
    
    # input$cov_yn=="No"
    # fit geometric model only
    
    no_fits<-1
    no_one_cov_cases<-0
    no_two_cov_cases<-0
  }
    
    return(list(no_fits=no_fits,
                no_one_cov_cases=no_one_cov_cases,
                no_two_cov_cases=no_two_cov_cases))
  })
  

  
  two_cov_combin_index_vec <-reactive({
    
    # all unique combinations of 2 covariates
    # note: dim(two_cov_combin_index_vec)[1] = total_no_fits()
    
    orignal_index_vec<-t(combn(no_covariate_available(),2))
    
    # adjust to covariate index in the original dataset
    
    first_cov_index<- as.numeric(orignal_index_vec[,1] + (input$cov_col_range[1]-1))
    second_cov_index<- as.numeric(orignal_index_vec[,2] + (input$cov_col_range[1]-1))
    
    return(list(orignal_index_vec=orignal_index_vec,
                first_cov_index=first_cov_index,
                second_cov_index=second_cov_index))
  })
    
    
  
  
  # define cov (max is 2) considered in the model
  zz<-reactive({

    
    if (input$cov_yn=="Yes") {
      
      data_all<- dataInput()
      
      if (no_covariate_available()==1) {
        
        # one cov case
        
        cov1= data_all[,input$cov_col_11]; covariate_1=cov1-mean(cov1)
        cov= data.frame(cov1=covariate_1)
        
      } else {
        
        # two cov case
         
        num_one_unique_cov_case<-total_no_fits()$no_one_cov_cases
        
        num_two_unique_cov_case<-choose(no_covariate_available(),2)
        
        # set up a covariate array with length total_no_fits() elements
        # cov1= data_all[,input$cov_col_range[1]]; covariate_1=cov1-mean(cov1)
        # cov2= data_all[,input$cov_col_range[1]+1]; covariate_2=cov2-mean(cov2)
        
        # column.names=c("covariate1","covariate2")
        
        # initialise covariate arrary, first empty arrary is for p(c), the rest do have covariate information
        cov= array( rep(NA,total_no_fits()$no_two_cov_case), 
                    dim=c(T(),2, 1 + num_one_unique_cov_case + num_two_unique_cov_case ) )
        
        
        ## one cov used in sub models within two cov cases
        for (jj in 1:num_one_unique_cov_case) {
          
             cov_ind <- input$cov_col_range[1] + (jj-1)
             cov[,1,jj+1]<- data_all[,cov_ind] -   mean( data_all[,cov_ind] )
             
        }
        
        
        ## two cov used in two cov models
        # update two unique combination of covs
        for (ii in 1:num_two_unique_cov_case ) {
          
          # read index vectors
          first_cov_ind<- two_cov_combin_index_vec()$first_cov_index[ii]
          second_cov_ind<- two_cov_combin_index_vec()$second_cov_index[ii]
          
          # standarlise and update covariate array
          cov[,1,ii+1+num_one_unique_cov_case]<- data_all[,first_cov_ind] -   mean( data_all[,first_cov_ind] )
          cov[,2,ii+1+num_one_unique_cov_case]<- data_all[,second_cov_ind] - mean( data_all[,second_cov_ind] )

        }
 
      }
      
    } else {
      # no cov case
      NULL
    }
    
    return(cov)
  })
  
  
  
  ### run the constant geometric model p(c) ONLY
  #   hh is an index ------ 
  hh.geo.pc.prediction<- function(hh=1){
    
    # kk index is an parameter
    force(hh)
    
    # read data
    data_all<- dataInput()
    data1=data_all[,input$data_colnum]
    
    # read loglikelihood function
    geo.ll <- function (param,data,x,T,D){
      p=expit(param[1]);n0=exp(param[2]);N=n0+D
      
      logL=lgamma(N+1)-sum(lgamma(data+1))-lgamma(n0+1)+
        D*log(p)+(T*N-sum(x))*log(1-p)
      return(-logL)
    }
    
    # read estimated counts function
    est.geo.ll <- function (param, T, D) {
      
      ept.p=param[1];ept.p=rep(ept.p,T)
      ept.N=param[2]
      
      ept.data<-rep(0,T); ept.data[1]<-ept.N*ept.p[1]
      
      for(i in 2:T)
      {ept.data[i]<- ept.N*prod(1-ept.p[1:i-1])*ept.p[i]
      }
      
      return(ept.data)
    }
    
    
    
    ## constant geometric model ONLY.
    
    # function to run the model
        
        fit.model.geo.ll<- function(param) {
          try(optim(par=param,
                    fn=geo.ll, # make sure it's using geo.ll function for optimisation
                    method="BFGS",hessian=TRUE,
                    data=data1,x=x(),T=T(),D=D()),silent=TRUE)}
        
        # make sure hessian is available
        inv <- function(m) {class(try(solve(m),silent=TRUE))=="matrix"} 
        
        # *** custermise the number of iterations
        n.rep= isolate(input$num_iteration)
        
        # initialise numeric and matrix 
        p.test<-N0.test<-loglike.test<-numeric()
        hessians.test <- list(matrix(rep(0,2*2), ncol=2)) # 2 is the length(param)
        hessians.test <- rep(hessians.test,n.rep)
        
        
        # *** update this for all models
        time_spent<-numeric()
        
        # initialise iteration numbers
        j=1
        k=1
        
        # Start the clock for optimisation =-=-=-=-
        ptm <- proc.time()
        
        # run iterations
        for (j in 1:n.rep) { 
          
          # random starting values
          param=c(logit(runif(1,0.2,0.8)), log(runif(1,1,20)) )
          
          # run the model
          fit <- fit.model.geo.ll(param)
          
          if (inv(fit$hessian)==TRUE) { # check if hessian is available
            if (k<(n.rep+1)) {
              
              # record estimates for each iteration
              p.test[k] <- expit(fit$par[1])
              N0.test[k] <- exp(fit$par[3])
              
              # record hessians for each iteration
              hessians.test[[k]]=fit$hessian
              
              
              # record loglike for each iteration
              # these will be double checked by users
              loglike.test[k]<- -fit$value
              
              k=k+1      
            } }
          #j=j+1
          
          # the end of iterations
        }
        
        # Stop the clock =-=-=-=-
        t.record<-proc.time() - ptm
        
        # Time spent
        time_spent<- as.numeric(names(table(t.record[1])))
        
        
        # put together results for all iterations
        AIC.test <- - 2*loglike.test + (2*length(param))
        
        mle.test<- data.frame(p=p.test,
                              N0=N0.test,
                              loglike=loglike.test,
                              AIC= AIC.test)
        
        # observe the number of times hitting the MLEs
        loglike.test<-signif(loglike.test, 7)
        num.max.location<- length(which(loglike.test==max(loglike.test)))
        
        
        # observe which iteration has MLEs
        max.location<-which(loglike.test==max(loglike.test))[1]
        
        # record MLEs corresponding to the max.location
        fit.p <- p.test[max.location]
        fit.N <- N0.test[max.location]+D()

        # record the hessian，loglike and AIC corresponding to the max.location
        fit.hessians <- hessians.test[[max.location]]
        fit.loglike <- max(loglike.test)
        fit.AIC <- - 2*fit.loglike + (2*length(param))
        
        # *** SE and 95% CI
        # p
        SE<-sqrt(diag(solve(hessians.test[[max.location]]))) 
        SE.p<- SE[1]*fit.p*(1-fit.p)
        lowerCI.p<- fit.p-1.96*SE.p
        upperCI.p<- fit.p+1.96*SE.p
        
        # ******** N
        SE.N<- exp(SE[2])
        lowerCI.N<- fit.N-1.96*SE.N
        upperCI.N<- fit.N+1.96*SE.N
        
        
        
        
        # ***put together MLE results as well as loglike and AIC
        # first row shows fit.p, fit.N, fit.loglike and AIC
        # second row shows SE of fit.p, fit.N and NA, NA
        # third row shows lower 95% CI of fit.p, fit.N and NA, NA
        # forth row shows upper 95% CI of fit.p, fit.N and NA, NA
        
        fit.mle.results <-     data.frame(p_int=c(fit.p,SE.p,lowerCI.p,upperCI.p),
                                          N=c(fit.N,SE.N,lowerCI.N,upperCI.N),
                                          loglike=c(fit.loglike, NA,NA,NA),
                                          AIC=c(fit.AIC, NA,NA,NA)
                                          )
        
        # *** estimated counts of individuals and 95% CI
        
        est.numbers<- est.geo.ll(param=c(fit.p,fit.N), T=T(), D=D())
        
        lowerCI.numbers<- est.geo.ll(param=c(lowerCI.p,lowerCI.N), T=T(), D=D())
        upperCI.numbers<- est.geo.ll(param=c(upperCI.p,upperCI.N), T=T(), D=D())
          
        est.count.results<-data.frame(est.num=est.numbers,
                                      LCI=lowerCI.numbers,
                                      UCI=upperCI.numbers)
          
          
        
        ## --- the end of running p(c) model
   
    return(list(mle.test=mle.test, # results for all iteration
                hessians.test=hessians.test, # results for all iteration
                fit.mle.results=fit.mle.results, # results for MLEs with SE and 95% CI
                fit.hessians=fit.hessians, # hessian for MLEs
                time_spent=time_spent, # time spent
                num.max.location=num.max.location, # the number of times hitting the MLEs,
                est.count.results=est.count.results # estimated counts of individuals and 95% CI
    ))
    
  }
  
  
  
  
  ### run (gg)th one covaraite geometric model ONLY (could be sub model in two cov cases)
  ### e.g. p(z1), p(z2), p(z3) ... models
  #   gg is an index ------
  gg.geo.pt.1cov.prediction<- function(gg){
    
    # gg index is an parameter
    force(gg)
    
    # read data
    data_all<- dataInput()
    data1=data_all[,input$data_colnum]
    
    # # read pt loglikelihood function for one cov
    geo.pt.cov.ll <- function (param,data,x.d,T,D,z){
      
      c1<-param[1]; c2<-param[2];n0<-exp(param[3])
      p=expit(c1+c2*z);N=n0+D
      
      q<-numeric(); for (i in 1:(T-1)){q[i]=1-p[i]}
      
      logL=lgamma(N+1)-sum(lgamma(data+1))-lgamma(n0+1)+
        sum(x.d*log(q)) +sum(data*log(p))+ n0*sum(log(q))
      
      return(-logL)
    }
    
    # read estimated counts function
    est.geo.pt.cov.ll <- function (param, T, D,z) {
      
      ept.p=expit(param[1]+param[1]*z)
      ept.N=param[2] # estimated population size
      
      ept.data<-rep(0,T); ept.data[1]<-ept.N*ept.p[1]
      
      for(i in 2:T)
      {ept.data[i]<- ept.N*prod(1-ept.p[1:i-1])*ept.p[i]
      }
      
      return(ept.data)
    }
    
    
    
    ## the (kk)th one covaraite geometric model (sub model in two cov cases) ONLY.
    
        # choose one covariate for the (gg)th covariate
        z <- zz()[,1,gg+1]  # +1 because the first covaraiete arrary is empty for p(c) model
        
        
        ## ---- pt (z1)  model
        
        # function to run the model
        
        fit.model.geo.pt.cov.ll<- function(param) {
          try(optim(par=param,
                    fn=geo.pt.cov.ll, # make sure it's using geo.pt.cov.ll function for optimisation
                    method="BFGS",hessian=TRUE,
                    data=data1,x.d=x.d(),T=T(),D=D(),z=z),silent=TRUE)}
        
        # make sure hessian is available
        inv <- function(m) {class(try(solve(m),silent=TRUE))=="matrix"} 
        
        # custermise the number of iterations
        n.rep= isolate(input$num_iteration)
        
        # initialise numeric and matrix 
        p.int.1cov.test<-p.slope.1cov.test1<-N0.1cov.test<-loglike.1cov.test<-numeric()
        hessians.1cov.test <- list(matrix(rep(0,3*3), ncol=3)) # 3 is the length(param)
        hessians.1cov.test <- rep(hessians.1cov.test,n.rep)
        
        
        # *** update this for all models
        time_spent.1cov<-numeric()
        
        # initialise iteration numbers
        j=1
        k=1
        
        # Start the clock for optimisation =-=-=-=-
        ptm <- proc.time()
        
        # run iterations
        for (j in 1:n.rep) { 
          
          # random starting values
          param=c(runif(2,-0.1,0.1), log(runif(1,1,20)) )
          
          # run the model
          fit <- fit.model.geo.pt.cov.ll(param)
          
          if (inv(fit$hessian)==TRUE) { # check if hessian is available
            if (k<(n.rep+1)) {
              
              # record estimates for each iteration
              p.int.1cov.test[k] <- fit$par[1]
              p.slope.1cov.test1[k] <- fit$par[2]
              N0.1cov.test[k] <- exp(fit$par[3])
              
              # record hessians for each iteration
              hessians.1cov.test[[k]]=fit$hessian
              
              
              # record loglike for each iteration
              # these will be double checked by users
              loglike.1cov.test[k]<- -fit$value
              
              k=k+1      
            } }
          #j=j+1
          
          # the end of iterations
        }
        
        # Stop the clock =-=-=-=-
        t.record<-proc.time() - ptm
        
        # Time spent
        time_spent.1cov<- as.numeric(names(table(t.record[1])))
        
        
        # put together results for all iterations
        AIC.1cov.test <- - 2*loglike.1cov.test + (2*length(param))
        
        mle.test.1cov<- data.frame(p_int=p.int.1cov.test,
                                   p_slope1=p.slope.1cov.test1,
                                   N0=N0.1cov.test,
                                   loglike=loglike.1cov.test,
                                   AIC= AIC.1cov.test)
        
        # observe the number of times hitting the MLEs
        loglike.1cov.test<-signif(loglike.1cov.test, 7)
        num.max.location.1cov<- length(which(loglike.1cov.test==max(loglike.1cov.test)))
        
        
        # observe which iteration has MLEs
        max.location.1cov<-which(loglike.1cov.test==max(loglike.1cov.test))[1]
        
        # record MLEs corresponding to the max.location.2cov
        fit.p.int.1cov <- p.int.1cov.test[max.location.1cov]
        fit.p.slope.1cov.1 <- p.slope.1cov.test1[max.location.1cov]
        fit.N.1cov <- N0.1cov.test[max.location.1cov]+D()
        
        # record the hessian corresponding to the max.location.2cov
        fit.hessians.1cov <- hessians.1cov.test[[max.location.1cov]]
        fit.loglike.1cov <- max(loglike.1cov.test)
        fit.AIC.1cov<- - 2*fit.loglike.1cov + (2*length(param))
        
        # *** SE and 95% CI
        # SE
        SE<-sqrt(diag(solve(hessians.1cov.test[[max.location.1cov]]))) 
        
        # p.int
        SE.p.int.1cov<- SE[1]
        lowerCI.p.int.1cov<- fit.p.int.1cov-1.96*fit.p.int.1cov
        upperCI.p.int.1cov<- fit.p.int.1cov+1.96*fit.p.int.1cov

        # p slope 1
        SE.p.slope.1cov.1<- SE[2]
        lowerCI.p.slope.1cov.1<- fit.p.slope.1cov.1-1.96*fit.p.slope.1cov.1
        upperCI.p.slope.1cov.1<- fit.p.slope.1cov.1+1.96*fit.p.slope.1cov.1
        
        
        # ******** N
        SE.N.1cov<- exp(SE[3])
        lowerCI.N.1cov<- fit.N.1cov-1.96*fit.N.1cov
        upperCI.N.1cov<- fit.N.1cov+1.96*fit.N.1cov
        
        
        
        
        # ***put together MLE results as well as loglike and AIC
        # first row shows fit.p, fit.N, fit.loglike and AIC
        # second row shows SE of fit.p, fit.N and NA, NA
        # third row shows lower 95% CI of fit.p, fit.N and NA, NA
        # forth row shows upper 95% CI of fit.p, fit.N and NA, NA
        
        fit.mle.results.1cov <-     data.frame(p_int=c(fit.p.int.1cov,SE.p.int.1cov,lowerCI.p.int.1cov,upperCI.p.int.1cov),
                                          p_slope1=c(fit.p.slope.1cov.1,SE.p.slope.1cov.1,lowerCI.p.slope.1cov.1,upperCI.p.slope.1cov.1),
                                          N=c(fit.N.1cov,SE.N.1cov,lowerCI.N.1cov,upperCI.N.1cov),
                                          loglike=c(fit.loglike.1cov, NA,NA,NA),
                                          AIC=c(fit.AIC.1cov, NA,NA,NA)
        )
        
        # *** estimated counts of individuals and 95% CI
        
        est.numbers<- est.geo.pt.cov.ll(param=c(fit.p.int.1cov,
                                                fit.p.slope.1cov.1,
                                                fit.N.1cov), 
                                        T=T(), D=D(), z=z)
        
        lowerCI.numbers<- est.geo.pt.cov.ll(param=c(lowerCI.p.int.1cov,lowerCI.p.slope.1cov.1,lowerCI.N.1cov), T=T(), D=D(), z=z)
        upperCI.numbers<- est.geo.pt.cov.ll(param=c(upperCI.p.int.1cov,upperCI.p.slope.1cov.1,upperCI.N.1cov), T=T(), D=D(), z=z)
        
        est.count.results.1cov<-data.frame(est.num=est.numbers,
                                      LCI=lowerCI.numbers,
                                      UCI=upperCI.numbers)

        
        
        ## --- the end of running p(z1) model

    return(list(mle.test.1cov=mle.test.1cov, # results for all iteration
                hessians.1cov.test=hessians.1cov.test, # results for all iteration
                fit.mle.results.1cov=fit.mle.results.1cov, # results for MLEs
                fit.hessians.1cov=fit.hessians.1cov, # hessian for MLEs
                time_spent.1cov=time_spent.1cov, # time spent
                num.max.location.1cov=num.max.location.1cov, # the number of times hitting the MLEs
                est.count.results.1cov=est.count.results.1cov # estimated counts of individuals and 95% CI
    ))
    
  }
  
  
  
  
  
  
  
  
  
  
  
  
  
  ### run (kk)th two cov model ONLY, i.e. p(z1+z2) and p(z1+z2+z1*z2) models
  #   kk is an index ------
  kk.geo.pt.2cov.prediction<- function(kk){
    
    # kk index is an parameter
    force(kk)
    
    data_all<- dataInput()
    data1=data_all[,input$data_colnum]

    # pt loglikelihood function for two covs
    geo.pt.2cov.ll <- function (param,data,x.d,T,D,z1,z2){
      
      c1<-param[1]; c2<-param[2];c3<-param[3]
      p=expit(c1+c2*z1+c3*z2)
      n0<-exp(param[4]); N=n0+D
      
      q<-numeric(); for (i in 1:(T-1)){q[i]=1-p[i]}
      
      logL=lgamma(N+1)-sum(lgamma(data+1))-lgamma(n0+1)+
        sum(x.d*log(q)) +sum(data*log(p))+ n0*sum(log(q))
      
      return(-logL)
    }
    
    
    # read estimated counts function
    est.geo.pt.2cov.ll<-function (param,T,z1,z2) {
      
      
      ept.p<- expit(param[1]+param[2]*z1+param[3]*z2)
      ept.N<- param[4] # estimated population size
      
      ept.data<-numeric(); ept.data[1]<-ept.N*ept.p[1]
      
      for(i in 2:T) {ept.data[i]<- ept.N*prod(1-ept.p[1:i-1])*ept.p[i]}
      
      return(ept.data)
    }
    
    
    
    # pt function for two covs
    geo.pt.2cov.times.ll <- function (param,data,x.d,T,D,z1,z2){
      
      c1<-param[1]; c2<-param[2];c3<-param[3];c4<-param[4]
      p=expit(c1+c2*z1+c3*z2+c4*z1*z2)
      n0<-exp(param[5]); N=n0+D
      
      q<-numeric(); for (i in 1:(T-1)){q[i]=1-p[i]}
      
      logL=lgamma(N+1)-sum(lgamma(data+1))-lgamma(n0+1)+
        sum(x.d*log(q)) +sum(data*log(p))+ n0*sum(log(q))
      
      return(-logL)
    }
    
    # read estimated counts function
    est.geo.pt.2cov.times.ll<-function (param, T, z1, z2) {
      
      
      ept.p<- expit(param[1]+param[2]*z1+param[3]*z2+param[4]*z1*z2)
      ept.N<- param[5] # estimated population size
      
      ept.data<-numeric(); ept.data[1]<-ept.N*ept.p[1]
      
      for(i in 2:T) {ept.data[i]<- ept.N*prod(1-ept.p[1:i-1])*ept.p[i]}
      
      return(ept.data)
    }
    
    
    
    ## the (kk)th combination of two covaraites geo model ONLY.
    
    #kk_results<-reactive({
      
      # run the model according to the no. of cov
      
      if (input$cov_yn=="Yes") {
        
        if (no_covariate_available()>1) {
          
          # this function deals with two cov cases ONLY.
          
          # choose covariates for the (kk)th combination of covariates
          z1=zz()[,1,kk+1+total_no_fits()$no_one_cov_cases] 
          z2=zz()[,2,kk+1+total_no_fits()$no_one_cov_cases] 
          
          
          ## ---- pt (z1+z2) additive model
          
          # function to run the model
          
          fit.model.geo.pt.2cov.ll<- function(param) {
                      try(optim(par=param,
                      fn=geo.pt.2cov.ll, # make sure it's using geo.pt.2cov.ll function for optimisation
                      method="BFGS",hessian=TRUE,
                      data=data1,x.d=x.d(),T=T(),D=D(),z1=z1,z2=z2),silent=TRUE)}
          
          # make sure hessian is available
          inv <- function(m) {class(try(solve(m),silent=TRUE))=="matrix"} 
          
          # custermise the number of iterations
          n.rep= isolate(input$num_iteration)

          # initialise numeric and matrix 
          p.int.2cov.test<-p.slope.2cov.test1<-p.slope.2cov.test2<-N0.2cov.test<-loglike.2cov.test<-numeric()
          hessians.2cov.test <- list(matrix(rep(0,4*4), ncol=4)) # 4 is the length(param)
          hessians.2cov.test <- rep(hessians.2cov.test,n.rep)
          
          
          # update this for all models
          time_spent.2cov<-numeric()
          
          # initialise iteration numbers
          j=1
          k=1
          
          # Start the clock for optimisation =-=-=-=-
          ptm <- proc.time()
          
          # run iterations
          for (j in 1:n.rep) { 
          
            # random starting values
            param=c(runif(3,-0.1,0.1), log(runif(1,1,20)) )
            
            # run the model
            fit <- fit.model.geo.pt.2cov.ll(param)
            
            if (inv(fit$hessian)==TRUE) { # check if hessian is available
              if (k<(n.rep+1)) {
                
                # record estimates for each iteration
                p.int.2cov.test[k] <- fit$par[1]
                p.slope.2cov.test1[k] <- fit$par[2]
                p.slope.2cov.test2[k] <- fit$par[3]
                N0.2cov.test[k] <- exp(fit$par[4])
                
                # record hessians for each iteration
                hessians.2cov.test[[k]]=fit$hessian
                
                
                # record loglike for each iteration
                # these will be double checked by users
                loglike.2cov.test[k]<- -fit$value
                
                k=k+1      
              } }
            #j=j+1
            
            # the end of iterations
          }
          
          # Stop the clock =-=-=-=-
          t.record<-proc.time() - ptm
          
          # Time spent
          time_spent.2cov<- as.numeric(names(table(t.record[1])))
         
          
          # put together results for all iterations
          AIC.2cov.test <- - 2*loglike.2cov.test + (2*length(param))
          
                mle.test.2cov<- data.frame(p_int=p.int.2cov.test,
                                           p_slope1=p.slope.2cov.test1,
                                           p_slope2=p.slope.2cov.test2,
                                           N0=N0.2cov.test,
                                           loglike=loglike.2cov.test,
                                           AIC= AIC.2cov.test)
          
          # observe the number of times hitting the MLEs
          loglike.2cov.test<-signif(loglike.2cov.test, 7)
          num.max.location.2cov<- length(which(loglike.2cov.test==max(loglike.2cov.test)))
                
           
          # observe which iteration has MLEs
          max.location.2cov<-which(loglike.2cov.test==max(loglike.2cov.test))[1]
          
          # record MLEs corresponding to the max.location.2cov
          fit.p.int.2cov <- p.int.2cov.test[max.location.2cov]
          fit.p.slope.2cov.1 <- p.slope.2cov.test1[max.location.2cov]
          fit.p.slope.2cov.2 <- p.slope.2cov.test2[max.location.2cov]
          fit.N.2cov <- N0.2cov.test[max.location.2cov]+D()
          
          # record the hessian corresponding to the max.location.2cov
          fit.hessians.2cov <- hessians.2cov.test[[max.location.2cov]]
          fit.loglike.2cov <- max(loglike.2cov.test)
          fit.AIC.2cov<- - 2*fit.loglike.2cov + (2*length(param))

          
          # *** SE and 95% CI
          # 
          SE<-sqrt(diag(solve(fit.hessians.2cov[[max.location.2cov]]))) 
          
          # p int
          SE.p.int.2cov<- SE[1]
          lowerCI.p.int.2cov<- fit.p.int.2cov-1.96*SE.p.int.2cov
          upperCI.p.int.2cov<- fit.p.int.2cov+1.96*SE.p.int.2cov
          
          # p slope 1
          SE.p.slope.2cov.1<- SE[2]
          lowerCI.p.slope.2cov.1<- fit.p.slope.2cov.1-1.96*SE.p.slope.2cov.1
          upperCI.p.slope.2cov.1<- fit.p.slope.2cov.1+1.96*SE.p.slope.2cov.1
          
          # p slope 2
          SE.p.slope.2cov.2<- SE[3]
          lowerCI.p.slope.2cov.2<- fit.p.slope.2cov.2-1.96*SE.p.slope.2cov.2
          upperCI.p.slope.2cov.2<- fit.p.slope.2cov.2+1.96*SE.p.slope.2cov.2
          
          
          # ******** N
          SE.N.2cov<- exp(SE[4])
          lowerCI.N.2cov<- fit.N.2cov-1.96*SE.N.2cov
          upperCI.N.2cov<- fit.N.2cov+1.96*SE.N.2cov
          
          
                # ***put together MLE results as well as loglike and AIC
                # first row shows fit.p, fit.N, fit.loglike and AIC
                # second row shows SE of fit.p, fit.N and NA, NA
                # third row shows lower 95% CI of fit.p, fit.N and NA, NA
                # forth row shows upper 95% CI of fit.p, fit.N and NA, NA
                
                fit.mle.results.2cov<- data.frame(p=c(fit.p.int.2cov,
                                                      SE.p.int.2cov,
                                                      lowerCI.p.int.2cov,
                                                      upperCI.p.int.2cov),
                                                  
                                                  p_slope1=c(fit.p.slope.2cov.1,
                                                             SE.p.slope.2cov.1,
                                                             lowerCI.p.slope.2cov.1,
                                                             upperCI.p.slope.2cov.1),
                                                  
                                                  p_slope2=c(fit.p.slope.2cov.2,
                                                             SE.p.slope.2cov.2,
                                                             lowerCI.p.slope.2cov.2,
                                                             upperCI.p.slope.2cov.1),
                                                  
                                                  N=c(fit.N.2cov,
                                                      SE.N.2cov,
                                                      lowerCI.N.2cov,
                                                      upperCI.N.2cov),
                                                  
                                                  loglike=c(fit.loglike.2cov, NA,NA,NA),
                                                  AIC=c(fit.AIC.2cov, NA,NA,NA)
                )
                
                # *** estimated counts of individuals and 95% CI
                
                est.numbers<- est.geo.pt.2cov.ll(param=c(fit.p.int.2cov,
                                                         fit.p.slope.2cov.1,
                                                         fit.p.slope.2cov.2,
                                                         fit.N.2cov), 
                                                 T=T(),z1=z1,z2=z2)
                
                lowerCI.numbers<- est.geo.pt.2cov.ll(param=c(lowerCI.p.int.2cov,
                                                             lowerCI.p.slope.2cov.1,
                                                             lowerCI.p.slope.2cov.2,
                                                             lowerCI.N.2cov), 
                                                     T=T(),z1=z1,z2=z2)
                
                upperCI.numbers<- est.geo.pt.2cov.ll(param=c(upperCI.p.int.2cov,
                                                             upperCI.p.slope.2cov.1,
                                                             upperCI.p.slope.2cov.2,
                                                             upperCI.N.2cov), 
                                                     T=T(),z1=z1,z2=z2)
                
                est.count.results.2cov<-data.frame(est.num=est.numbers,
                                                   LCI=lowerCI.numbers,
                                                   UCI=upperCI.numbers)
                

           
                
                         
          ## --- the end of pt (z1+z2) additive model
          
          
          
          ## ---- pt (z1+z2+z1*z2) interaction model
          
          # function to run the model
          
          fit.model.geo.pt.2cov.times.ll<- function(param) {
            try(optim(par=param,
                      fn=geo.pt.2cov.times.ll, # make sure it's using geo.pt.2cov.times.ll function for optimisation
                      method="BFGS",hessian=TRUE,
                      data=data1,x.d=x.d(),T=T(),D=D(),z1=z1,z2=z2),TRUE)}
          
          # make sure hessian is available
          inv <- function(m) {class(try(solve(m),silent=T))=="matrix"} 
          
          # custermise the number of iterations
          n.rep= isolate(input$num_iteration)
          
          # initialise numeric and matrix 
          p.int.2cov.times.test<-p.slope.2cov.times.test1<-p.slope.2cov.times.test2<-p.slope.2cov.times.test3<-N0.2cov.times.test<-loglike.2cov.times.test<-numeric()
          hessians.2cov.times.test <- list(matrix(rep(0,5*5), ncol=5)) # 5 is the length(param)
          hessians.2cov.times.test <- rep(hessians.2cov.times.test,n.rep)
          
          
          # update this for all models
          time_spent.2cov.times<-numeric()
          
          # initialise iteration numbers
          j=1
          k=1
          
          # Start the clock for optimisation =-=-=-=-
          ptm <- proc.time()
          
          # run iterations
          for (j in 1:n.rep) { 
            
            # random starting values
            param=c(runif(4,-0.1,0.1), log(runif(1,1,20)) )
            
            # run the model
            fit <- fit.model.geo.pt.2cov.times.ll(param)
            
            if (inv(fit$hessian)==TRUE) { # check if hessian is available
              if (k<(n.rep+1)) {
                
                # record estimates for each iteration
                p.int.2cov.times.test[k] <- fit$par[1]
                p.slope.2cov.times.test1[k] <- fit$par[2]
                p.slope.2cov.times.test2[k] <- fit$par[3]
                p.slope.2cov.times.test3[k] <- fit$par[4]
                N0.2cov.times.test[k] <- exp(fit$par[5])
                
                # record hessians for each iteration
                hessians.2cov.times.test[[k]]=fit$hessian
                
                
                # record loglike for each iteration
                # these will be double checked by users
                loglike.2cov.times.test[k]<- -fit$value
                
                k=k+1      
              } }
            #j=j+1
          }
            
          # Stop the clock =-=-=-=-
          t.record<-proc.time() - ptm
          
          # Time spent
          time_spent.2cov.times<- as.numeric(names(table(t.record[1])))
          
          
          # put together results for all iterations
          AIC.2cov.times.test <- - 2*loglike.2cov.times.test + (2*length(param))
          
          mle.test.2cov.times<- data.frame(p_int=p.int.2cov.times.test,
                                                  p_slope1=p.slope.2cov.times.test1,
                                                  p_slope2=p.slope.2cov.times.test2,
                                                  p_slope3=p.slope.2cov.times.test3,
                                                  N0=N0.2cov.times.test,
                                                  loglike=loglike.2cov.times.test,
                                                  AIC= AIC.2cov.times.test)
          
          
          # observe the number of times hitting the MLEs
          loglike.2cov.times.test<-signif(loglike.2cov.times.test, 7)
          num.max.location.2cov.times<- length(which(loglike.2cov.times.test==max(loglike.2cov.times.test)))
          
          # observe which iteration has MLEs
          max.location.2cov.times<-which(loglike.2cov.times.test==max(loglike.2cov.times.test))[1]
          
          # record MLEs corresponding to the max.location.2cov.times
          fit.p.int.2cov.times <- p.int.2cov.times.test[max.location.2cov.times]
          fit.p.slope.2cov.times.1 <- p.slope.2cov.times.test1[max.location.2cov.times]
          fit.p.slope.2cov.times.2 <- p.slope.2cov.times.test2[max.location.2cov.times]
          fit.p.slope.2cov.times.3 <- p.slope.2cov.times.test3[max.location.2cov.times]
          fit.N.2cov.times <- N0.2cov.times.test[max.location.2cov.times] + D()
          
          
          # record the hessian corresponding to the max.location.2cov.times
          fit.hessians.2cov.times <- hessians.2cov.times.test[[max.location.2cov.times]]
          fit.loglike.2cov.times <- max(loglike.2cov.times.test)
          fit.AIC.2cov.times <- - 2*fit.loglike.2cov.times + (2*length(param))
          
          
          # *** SE and 95% CI
          # 
          SE<-sqrt(diag(solve(hessians.2cov.times.test[[max.location.2cov.times]])))
          
          # p int
          SE.p.int.2cov.time<- SE[1]
          lowerCI.p.int.2cov.time<- fit.p.int.2cov.times-1.96*SE.p.int.2cov.time
          upperCI.p.int.2cov.time<- fit.p.int.2cov.times+1.96*SE.p.int.2cov.time
          
          # p slope 1
          SE.p.slope.2cov.times.1<- SE[2]
          lowerCI.p.slope.2cov.time.1<- fit.p.slope.2cov.times.1-1.96*SE.p.slope.2cov.times.1
          upperCI.p.slope.2cov.time.1<- fit.p.slope.2cov.times.1+1.96*SE.p.slope.2cov.times.1
          
          # p slope 2
          SE.p.slope.2cov.times.2<- SE[3]
          lowerCI.p.slope.2cov.time.2<- fit.p.slope.2cov.times.2-1.96*SE.p.slope.2cov.times.2
          upperCI.p.slope.2cov.time.2<- fit.p.slope.2cov.times.2+1.96*SE.p.slope.2cov.times.2
          
          # p slope 3
          SE.p.slope.2cov.times.3<- SE[4]
          lowerCI.p.slope.2cov.time.3<- fit.p.slope.2cov.times.3-1.96*SE.p.slope.2cov.times.3
          upperCI.p.slope.2cov.time.3<- fit.p.slope.2cov.times.3+1.96*SE.p.slope.2cov.times.3
          
          
          # ******** N
          SE.N.2cov.times<- exp(SE[5])
          lowerCI.N.2cov.times<- fit.N.2cov.times-1.96*SE.N.2cov.times
          upperCI.N.2cov.times<- fit.N.2cov.times+1.96*SE.N.2cov.times
          
          
          
          # ***put together MLE results as well as loglike and AIC
          # first row shows fit.p, fit.N, fit.loglike and AIC
          # second row shows SE of fit.p, fit.N and NA, NA
          # third row shows lower 95% CI of fit.p, fit.N and NA, NA
          # forth row shows upper 95% CI of fit.p, fit.N and NA, NA
          
          fit.mle.results.2cov.times<- data.frame(p_int=c(fit.p.int.2cov.times,
                                                          SE.p.int.2cov.time,
                                                          lowerCI.p.int.2cov.time,
                                                          upperCI.p.int.2cov.time),
                                                  
                                                  p_slope1=c(fit.p.slope.2cov.times.1,
                                                             SE.p.slope.2cov.times.1,
                                                             lowerCI.p.slope.2cov.time.1,
                                                             upperCI.p.slope.2cov.time.1),
                                                  
                                                  p_slope2=c(fit.p.slope.2cov.times.2,
                                                             SE.p.slope.2cov.times.2,
                                                             lowerCI.p.slope.2cov.time.2,
                                                             upperCI.p.slope.2cov.time.2),
                                                  
                                                  p_slope3=c(fit.p.slope.2cov.times.3,
                                                             SE.p.slope.2cov.times.3,
                                                             lowerCI.p.slope.2cov.time.3,
                                                             upperCI.p.slope.2cov.time.3),
                                                  
                                                  N=c(fit.N.2cov.times,
                                                      SE.N.2cov.times,
                                                      lowerCI.N.2cov.times,
                                                      upperCI.N.2cov.times),
                                                  
                                                  loglike=c(fit.loglike.2cov.times, NA,NA,NA),
                                                  AIC=c(fit.AIC.2cov.times, NA,NA,NA)
                                                  )
          
          
          # *** estimated counts of individuals and 95% CI
          # fitted param
          fit_param<-c(fit.p.int.2cov.times,
                       fit.p.slope.2cov.times.1,
                       fit.p.slope.2cov.times.2,
                       fit.p.slope.2cov.times.3,
                       fit.N.2cov.times)
          
          est.numbers <- est.geo.pt.2cov.times.ll(param=fit_param,T=T(),z1=z1,z2=z2)

          lowerCI.numbers<- est.geo.pt.2cov.times.ll(param=c(lowerCI.p.int.2cov.time,
                                                             lowerCI.p.slope.2cov.time.1,
                                                             lowerCI.p.slope.2cov.time.2,
                                                             lowerCI.p.slope.2cov.time.3,
                                                             lowerCI.N.2cov.times), 
                                                     T=T(), z1=z1,z2=z2)
          upperCI.numbers<- est.geo.pt.2cov.times.ll(param=c(upperCI.p.int.2cov.time,
                                                             upperCI.p.slope.2cov.time.1,
                                                             upperCI.p.slope.2cov.time.2,
                                                             upperCI.p.slope.2cov.time.3,
                                                             upperCI.N.2cov.times),
                                                     T=T(),  z1=z1,z2=z2)
          
          est.count.results.2cov.times<-data.frame(est.num=est.numbers,
                                                   LCI=lowerCI.numbers,
                                                   UCI=upperCI.numbers)
          

          
          
          
          
          ## --- pt (z1+z2+z1*z2) interaction model
            
        }
      } 
 
    return(list(mle.test.2cov=mle.test.2cov, # results for all iteration
                mle.test.2cov.times=mle.test.2cov.times, # results for all iteration
                hessians.2cov.test=hessians.2cov.test,
                hessians.2cov.times.test,hessians.2cov.times.test,
                fit.mle.results.2cov=fit.mle.results.2cov, # results for MLEs
                fit.mle.results.2cov.times=fit.mle.results.2cov.times, # results for MLEs
                fit.hessians.2cov=fit.hessians.2cov,
                fit.hessians.2cov.times=fit.hessians.2cov.times,
                time_spent.2cov=time_spent.2cov, # time spent
                time_spent.2cov.times=time_spent.2cov.times, # time spent,
                num.max.location.2cov=num.max.location.2cov, # the number of times hitting the MLEs
                num.max.location.2cov.times=num.max.location.2cov.times, # # the number of times hitting the MLEs 
                est.count.results.2cov=est.count.results.2cov,
                est.count.results.2cov.times=est.count.results.2cov.times
                ))
    
  }
  
  
  
  
  
  # 
  geo.pt.prediction <- reactive({
    
    
  })
  
  
  
  # run all possible models
  all.geo.prediction<- reactive({
    
    if (run_button_counts$counts!=0) {
      
      data_all<- dataInput()
      
      # always run constant model
      pc.prediction <-hh.geo.pc.prediction(hh=1)
    
      
      if (input$cov_yn=="Yes") {
        
        if (no_covariate_available()==1) {
          
          # one cov case
          
          pt.1cov.prediction <- gg.geo.pt.1cov.prediction(gg=1) 
          
          # as there is only one covariate in the data
          
          pt.2cov.prediction <- NULL
          
        } else {
          
          ### two cov case
          
          # the number of one and two cov cases 
          num_one_unique_cov_case<-total_no_fits()$no_one_cov_cases
          num_two_unique_cov_case<-choose(no_covariate_available(),2)
          
          
          ## one cov used as sub models within two cov cases
          
          # initialise pt.1cov.prediction as a list to store results for each model with one cov
          pt.1cov.prediction<-list()
          
          for (gg in 1:num_one_unique_cov_case) {
            
            pt.1cov.prediction[[gg]] <- gg.geo.pt.1cov.prediction(gg)
            
          }
          
          
          ## two cov used in two cov models
          
          # initialise pt.1cov.prediction as a list to store results for each model with one cov
          pt.2cov.prediction<-list()
          
          for (kk in 1:num_two_unique_cov_case) {
            
            pt.2cov.prediction[[kk]] <- kk.geo.pt.2cov.prediction(kk)
            
          }
          
          
        }
        
      } 
      
      
    } else {
      
      # if reset or did not hit the action button
      
      pc.prediction<- NULL
      pt.1cov.prediction<-NULL
      pt.2cov.prediction<-NULL
      
    }
    
    # --- the end of action button condition 
        
    return(list(pc.prediction=pc.prediction,
                pt.1cov.prediction=pt.1cov.prediction,
                pt.2cov.prediction=pt.2cov.prediction
                ))
    
    })
  
  
  
  
  
  
  
  
  ################################  model comparison from geo.pt for one combination of two cov
  
  
  geo.aic.models<-reactive({
    
    if (run_button_counts$counts!=0) {
      
      #*** add structure of no. of cov
      
      ## >= two cov available
      # AIC values
      geo.pt.aic<- numeric()
      
      geo.pt.aic[1]<-all.geo.prediction()$pc.prediction$fit.mle.results$AIC[1]
      
      for (ii in 1:total_no_fits()$no_one_cov_cases ) {
        geo.pt.aic[ii+1]<- all.geo.prediction()$pt.1cov.prediction[[ii]]$fit.mle.results.1cov$AIC[1] + 2*3
      }
      
      for (jj in 1:choose(no_covariate_available(),2) ) {
        geo.pt.aic[jj*2-1+(1+total_no_fits()$no_one_cov_cases)] <- all.geo.prediction()$pt.2cov.prediction[[jj]]$fit.mle.results.2cov$AIC[1]
        geo.pt.aic[jj*2 +(1+total_no_fits()$no_one_cov_cases)] <- all.geo.prediction()$pt.2cov.prediction[[jj]]$fit.mle.results.2cov.times$AIC[1]
      }
      
      
      # ordered index vector
      aic.order.index<-order(geo.pt.aic)
      
      # ordered aic
      aic.ordered<-geo.pt.aic[aic.order.index]
      
      # delta aic
      delta.aic.ordered<-geo.pt.aic[aic.order.index]- geo.pt.aic[aic.order.index[1]]
      
      
      # ordered model_name_vec
      
      model.names<-all_list_model_tab()$Model
      model.names.ordered<-model.names[aic.order.index]
      
      
      # ordered cov_used_vec
      
      p_cov_used<-all_list_model_tab()$p_cov_used
      p_cov_used_ordered<-p_cov_used[aic.order.index]
      
      # ordered no. param
      geo.pt.no.param<- c(2,
                          rep(3,total_no_fits()$no_one_cov_cases),
                          rep(c(4,5), choose(no_covariate_available(),2))
      )
      
      geo.pt.no.param.ordered<- geo.pt.no.param[aic.order.index]
      
      
      # ordered maximised loglikelihood
      geo.pt.maxlogl<- numeric()
      
      geo.pt.maxlogl[1]<- all.geo.prediction()$pc.prediction$fit.mle.results$loglike[1]
      
      for (ii in 1:total_no_fits()$no_one_cov_cases ) {
        geo.pt.maxlogl[ii+1]<- all.geo.prediction()$pt.1cov.prediction[[ii]]$fit.mle.results.1cov$loglike[1]
      }
      
      for (jj in 1:choose(no_covariate_available(),2) ) {
        
        geo.pt.maxlogl[jj*2-1+(1+total_no_fits()$no_one_cov_cases)] <- all.geo.prediction()$pt.2cov.prediction[[jj]]$fit.mle.results.2cov$loglike[1]
        geo.pt.maxlogl[jj*2 +(1+total_no_fits()$no_one_cov_cases)] <- all.geo.prediction()$pt.2cov.prediction[[jj]]$fit.mle.results.2cov.times$loglike[1]
      }
      
      geo.pt.maxlogl.ordered<-geo.pt.maxlogl[aic.order.index]
      
      
      
      
      # the end of action button
    }

    
    return(list(geo.pt.no.param=geo.pt.no.param,
                geo.pt.maxlogl=geo.pt.maxlogl,
                geo.pt.aic=geo.pt.aic,
                aic.order.index=aic.order.index,
                model.names.ordered=model.names.ordered, # ordered
                p_cov_used_ordered=p_cov_used_ordered,  # ordered
                geo.pt.no.param.ordered=geo.pt.no.param.ordered, # ordered
                geo.pt.maxlogl.ordered=geo.pt.maxlogl.ordered, # ordered
                aic.ordered=aic.ordered, # ordered
                delta.aic.ordered=delta.aic.ordered # ordered

    ))
    
  })
  
  
  
  # model comparison results table for one combination of two cov
  geo.model.tab<-reactive({ 
    
  if (run_button_counts$counts!=0) {
      
    model.tab.results<-data.frame(
      Model = geo.aic.models()$model.names.ordered,
      p_cov_used = geo.aic.models()$p_cov_used_ordered,
      No_param=geo.aic.models()$geo.pt.no.param.ordered,
      max_logL=geo.aic.models()$geo.pt.maxlogl.ordered,
      AIC=geo.aic.models()$aic.ordered,
      delta.AIC= geo.aic.models()$delta.aic.ordered
    )
    } else {
      
      model.tab.results=NULL
      
    }
    model.tab.results
  })
  
  
  
  # fitted results otbained from the model with the lowest AIC score
  # estimated params, SEs and 95% CI
  # estimates counts and 95% CI
  
  fitted.results.geo<- reactive ({
    
    if (run_button_counts$counts!=0) {
      

      
    if (geo.aic.models()$geo.pt.no.param.ordered[1]==5){
      # if z1+z2+z1*z2 model
    
      # choose which model
      original.index<- which(order(geo.aic.models()$geo.pt.aic)==1)
      adjust.index<- (original.index - 1 - total_no_fits()$no_one_cov_cases)/2 # which combinations of covs in c(1:choose(no_covariate_available(),2))
      
      est.tab <- all.geo.prediction()$pt.2cov.prediction[[adjust.index]]$fit.mle.results.2cov.times
      
      estimates.tab.results<-data.frame(Notation=c("p_int","p_slope_i","p_slope_j","p_slope_k","N"),
                                        
                                        Estimates=c(est.tab$p_int[1],
                                                    est.tab$p_slope1[1],
                                                    est.tab$p_slope2[1],
                                                    est.tab$p_slope3[1],
                                                    est.tab$N[1]),
                                        
                                        SE=c(est.tab$p_int[2],
                                             est.tab$p_slope1[2],
                                             est.tab$p_slope2[2],
                                             est.tab$p_slope3[2],
                                             est.tab$N[2]),
                                        
                                        LowerCI=c(est.tab$p_int[3],
                                                  est.tab$p_slope1[3],
                                                  est.tab$p_slope2[3],
                                                  est.tab$p_slope3[3],
                                                  est.tab$N[3]),
                                        
                                        UpperCI=c(est.tab$p_int[4],
                                                  est.tab$p_slope1[4],
                                                  est.tab$p_slope2[4],
                                                  est.tab$p_slope3[4],
                                                  est.tab$N[4])
                                        )
      
      est.counts<- all.geo.prediction()$pt.2cov.prediction[[adjust.index]]$est.count.results.2cov.times
      
      estimated.counts<-data.frame(occasions=c(1:T()),
                                    observed_data=dataInput()[,input$data_colnum],
                                    estimated_data=est.counts$est.num,
                                    lower_CI=est.counts$LCI,
                                    upper_CI=est.counts$UCI
                                    )
      
      
    }
      
    if (geo.aic.models()$geo.pt.no.param.ordered[1]==4){
        # if z1+z2 model
      
      # choose which model
      original.index<- which(order(geo.aic.models()$geo.pt.aic)==1)
      adjust.index<- (original.index - 1 - total_no_fits()$no_one_cov_cases +1 )/2 # which combinations of covs in c(1:choose(no_covariate_available(),2))
      
      est.tab <- all.geo.prediction()$pt.2cov.prediction[[adjust.index]]$fit.mle.results.2cov
      
      estimates.tab.results<-data.frame(Notation=c("p_int","p_slope_i","p_slope_j","N"),
                                        
                                        Estimates=c(est.tab$p_int[1],
                                                    est.tab$p_slope1[1],
                                                    est.tab$p_slope2[1],
                                                    est.tab$N[1]),
                                        
                                        SE=c(est.tab$p_int[2],
                                             est.tab$p_slope1[2],
                                             est.tab$p_slope2[2],
                                             est.tab$N[2]),
                                        
                                        LowerCI=c(est.tab$p_int[3],
                                                  est.tab$p_slope1[3],
                                                  est.tab$p_slope2[3],
                                                  est.tab$N[3]),
                                        
                                        UpperCI=c(est.tab$p_int[4],
                                                  est.tab$p_slope1[4],
                                                  est.tab$p_slope2[4],
                                                  est.tab$N[4])
                                        )
      
      est.counts<- all.geo.prediction()$pt.2cov.prediction[[adjust.index]]$est.count.results.2cov
      
      estimated.counts<-data.frame(occasions=c(1:T()),
                                    observed_data=dataInput()[,input$data_colnum],
                                    estimated_data=est.counts$est.num,
                                    lower_CI=est.counts$LCI,
                                    upper_CI=est.counts$UCI
                                    )
      
        
    }       
    
    if (geo.aic.models()$geo.pt.no.param.ordered[1]==3){
      # if z1 model
      
      # choose which model
      original.index<- which(order(geo.aic.models()$geo.pt.aic)==1)
      adjust.index<- original.index - 1 # which combinations of covs in c(1:choose(no_covariate_available(),2))
      
      est.tab <- all.geo.prediction()$pt.1cov.prediction[[adjust.index]]$fit.mle.results.1cov
      
      estimates.tab.results<-data.frame(Notation=c("p_int","p_slope_i","N"),
                                        
                                        Estimates=c(est.tab$p_int[1],
                                                    est.tab$p_slope1[1],
                                                    est.tab$N[1]),
                                        
                                        SE=c(est.tab$p_int[2],
                                             est.tab$p_slope1[2],
                                             est.tab$N[2]),
                                        
                                        LowerCI=c(est.tab$p_int[3],
                                                  est.tab$p_slope1[3],
                                                  est.tab$N[3]),
                                        
                                        UpperCI=c(est.tab$p_int[4],
                                                  est.tab$p_slope1[4],
                                                  est.tab$N[4])
      )
      
      est.counts<- all.geo.prediction()$pt.1cov.prediction[[adjust.index]]$fit.mle.results.1cov
      
      estimated.counts<-data.frame(occasions=c(1:T()),
                                    observed_data=dataInput()[,input$data_colnum],
                                    estimated_data=est.counts$est.num,
                                    lower_CI=est.counts$LCI,
                                    upper_CI=est.counts$UCI
                                    )
      
    }
      
    if (geo.aic.models()$geo.pt.no.param.ordered[1]==2) {
      # if pc model
      
      est.tab <- all.geo.prediction()$pc.prediction$fit.mle.results
      
      estimates.tab.results<-data.frame(Notation=c("p","N"),
                                        Estimates=c(est.tab$p[1],
                                                    est.tab$N[1]),
                                        SE=c(est.tab$p[2],
                                             est.tab$N[2]),
                                        LowerCI=c(est.tab$p[3],
                                                  est.tab$N[3]),
                                        UpperCI=c(est.tab$p[4],
                                                  est.tab$N[4])
                                        )
      
      est.counts<- all.geo.prediction()$pc.prediction$est.count.results
      
      estimated.counts<-data.frame(occasions=c(1:T()),
                                    observed_data=dataInput()[,input$data_colnum],
                                    estimated_data=est.counts$est.num,
                                    lower_CI=est.counts$LCI,
                                    upper_CI=est.counts$UCI
                                    )
                                    
        
      
    }
     
    } else {
      
      estimates.tab.results=data.frame(Notation="",Estimates="",SE="",LowerCI="",UpperCI="")
                                       
    }
    
    return(list(estimates.tab.results=estimates.tab.results,
                estimated.counts=estimated.counts))
  })
  
  

  
  #============================  #============================ RD prediction #============================ #============================ 
  # =================================================== #============================ #============================ 
  # ###############################    RD models
  
  # RD no. of cov available
  no_covariate_available_RD<-reactive({
    
    if (input$cov_yn_RD=="Yes") {
    no_cov <- input$cov_col_range_RD[2]-input$cov_col_range_RD[1]+1
    } else {no_cov<- 0}
    
    return(no_cov)
  })
  
  # define the total no. of potential models to fit
  total_no_fits_RD<-reactive({
    
    input_not_null <- isTRUE(input$p_type_RD!="" & input$phi_type_RD!="")
    
    if ( input_not_null == TRUE ) {
      
    if (input$cov_yn_RD == "Yes") {
     
      if (input$phi_type_RD=="All" & input$p_type_RD=="All") {
        no_fits<- (2+no_covariate_available_RD()) * (1+no_covariate_available_RD())
      }
      
      
      if (input$phi_type_RD=="Constant" & input$p_type_RD=="Constant") {
        no_fits<- 1
      }
      
      if (input$phi_type_RD=="Constant" & input$p_type_RD=="Covariates") {
        no_fits<- 1+no_covariate_available_RD()
      }
      
      
      if (input$phi_type_RD=="Covariates" & input$p_type_RD=="Constant") {
        no_fits<- length(c(1,(no_covariate_available_RD()+2):(2*no_covariate_available_RD()+1)))
      }
      
      if (input$phi_type_RD=="Covariates" & input$p_type_RD=="Covariates") {
        no_fits<- length(1:(2*no_covariate_available_RD()+2+no_covariate_available_RD()*no_covariate_available_RD()-1))
      }
      
      
      if (input$phi_type_RD=="Time-varying" & input$p_type_RD=="Constant") {
        no_fits<- 2
      }
      
      if (input$phi_type_RD=="Time-varying" & input$p_type_RD=="Covariates") {
        no_fits<- length(c(1,
                           (2*no_covariate_available_RD()+2+no_covariate_available_RD()*no_covariate_available_RD()):(2*no_covariate_available_RD()+2+no_covariate_available_RD()*no_covariate_available_RD()+no_covariate_available_RD())))
      }

    } else {
      
      # input$cov_yn_RD == "No"
      if (input$phi_type_RD=="All" & input$p_type_RD=="All") { no_fits<- 2 }
      if (input$phi_type_RD=="Constant" & input$p_type_RD=="Constant") { no_fits<- 1 }
      if (input$phi_type_RD=="Time-varying" & input$p_type_RD=="Constant") { no_fits<- 2 }
      
    }
  }

    
    if ( input_not_null == FALSE ) {
      no_fits<-1
}
    
    
    # RD no_fits
    return(no_fits)
  })
  

  
  
  
  # define cov (max is 1) considered in the model
  zz_RD<-reactive({

    if (input$cov_yn_RD=="Yes") {
      
      data_all<- dataInput_RD()
      
      if (no_covariate_available_RD()==1) {
        
        # one cov case
        
        cov1= data_all[,input$cov_col_range_RD[1]]; covariate_1=cov1-mean(cov1)
        cov= data.frame(phi_cov=covariate_1,p_cov=covariate_1)
        
      } else {
        
        
        all_cov_index_vec <-1:no_covariate_available_RD()
        
        # set up a covariate array with length total_no_fits() elements
        # phi_cov= data_all[,input$cov_col_range_RD[1]]; phi_covariate=phi_cov-mean(phi_cov)
        #  p_cov= data_all[,input$cov_col_range_RD[1]+1]; p_covariate=p_cov-mean(p_cov)
        
        # column.names=c("phi_cov","p_cov")
        # first and second columns corresponds to phi and p respectively
         cov= array(NA, dim=c(T_RD(),2,total_no_fits_RD()) )
        
        
        # update phi_cov and p_cov column for each of potential models
        
        if (input$phi_type_RD=="All" & input$p_type_RD=="All") {
          
          for (ii in 1:no_covariate_available_RD()) {
            
            # ======= # ======= # ======= # ======= # ======= 
            # ======= phic, pcov case 
            ## define covariates for p from the 2nd model to the no_covariate_available_RD()+1 model in the table in the Settings section
            # adjust to p covariate index in the original dataset
            p_cov_index <-  ii + (input$cov_col_range_RD[1]-1)
            p_cov_vec<-data_all[,p_cov_index]
            not_na_index<-which(!is.na(p_cov_vec))
            
            # start from the 2nd arrary as the first model in the table in the Settings section is a constant model without any covaraites.
            # standarlise and update covariate array
            cov[,2,ii+1][not_na_index]<- p_cov_vec[not_na_index]- mean(p_cov_vec[not_na_index]) 
            
            
            # ======= # ======= # ======= # ======= # ======= 
            # ======= phicov, pc case 
            
            # adjust to phi covariate index in the original dataset
            phi_cov_index<- ii + (input$cov_col_range_RD[1]-1)
            phi_cov_vec<-data_all[,phi_cov_index]
            not_na_index<-which(!is.na(phi_cov_vec))
            
            ## define covariates for phi from the no_covariate_available_RD()+2 model to the no_covariate_available_RD()*2+1 model
            # standarlise and update covariate array
            cov[,1,ii+no_covariate_available_RD()+1][not_na_index]<- phi_cov_vec[not_na_index]- mean(phi_cov_vec[not_na_index])
            
            
            # ======= # ======= # ======= # ======= # ======= 
            # ======= phicov, pcov case 
            
            ## define covariates for p 
            ## from the no_covariate_available_RD()*2+2 model to the no_covariate_available_RD()^2+no_covariate_available_RD()*2+1 model
            # standarlise and update covariate array
            
            for (jj in 1:no_covariate_available_RD()) {
              
              cov[,2,ii+(jj+1)*no_covariate_available_RD()+1][not_na_index]<- p_cov_vec[not_na_index]- mean(p_cov_vec[not_na_index]) 
              
            }
            
            #cov[,2,ii+no_covariate_available_RD()*2+1][not_na_index]<- p_cov_vec[not_na_index]- mean(p_cov_vec[not_na_index]) 
            
          #  cov[,2,ii+no_covariate_available_RD()*3+1][not_na_index]<- p_cov_vec[not_na_index]- mean(p_cov_vec[not_na_index]) 
            
           # cov[,2,ii+no_covariate_available_RD()*4+1][not_na_index]<- p_cov_vec[not_na_index]- mean(p_cov_vec[not_na_index]) 
            
          #  cov[,2,ii+no_covariate_available_RD()*5+1][not_na_index]<- p_cov_vec[not_na_index]- mean(p_cov_vec[not_na_index]) 
            
            
            ## define covariates for phi
            ## from the no_covariate_available_RD()*2+2 model to the no_covariate_available_RD()^2+no_covariate_available_RD()*2+1 model
            # standarlise and update covariate array
            
            for (jj in 1:no_covariate_available_RD()) {
              
              cov[,1,jj+no_covariate_available_RD()*(ii-1)+no_covariate_available_RD()*2+1][not_na_index]<- phi_cov_vec[not_na_index]- mean(phi_cov_vec[not_na_index]) 
              
            }
            
            
            # ======= # ======= # ======= # ======= # ======= 
            # ======= phit, pc case -- No covariates - the no_covariate_available_RD()^2+no_covariate_available_RD()*2+2 model
            
            
            # ======= # ======= # ======= # ======= # ======= 
            # ======= phit, pcov case 
            
            ## define covariates for p
            ## from the no_covariate_available_RD()^2+no_covariate_available_RD()*2+3 model to the end
            # standarlise and update covariate array
            cov[,2,ii+no_covariate_available_RD()*(no_covariate_available_RD()+2)+2][not_na_index]<- 
              p_cov_vec[not_na_index]- mean(p_cov_vec[not_na_index])
            

          }
          
          # the end of case input$phi_type_RD=="All" & input$p_type_RD=="All"
        }
        
          
        if (input$phi_type_RD=="Constant" & input$p_type_RD=="Covariates") {
          
          for (ii in 1:no_covariate_available_RD()) {
            
            # ======= # ======= # ======= # ======= # ======= 
            # ======= phic, pcov case 
            ## define covariates for p from the 2nd model to the no_covariate_available_RD()+1 model in the table in the Settings section
            # adjust to p covariate index in the original dataset
            p_cov_index <-  ii + (input$cov_col_range_RD[1]-1)
            p_cov_vec<-data_all[,p_cov_index]
            not_na_index<-which(!is.na(p_cov_vec))
            
            # start from the 2nd arrary as the first model in the table in the Settings section is a constant model without any covaraites.
            # standarlise and update covariate array
            cov[,2,ii+1][not_na_index]<- p_cov_vec[not_na_index]- mean(p_cov_vec[not_na_index]) 
          }
          
          # the end of case input$phi_type_RD=="constant" & input$p_type_RD=="cov"
        }
        
        if (input$phi_type_RD=="Covariates" & input$p_type_RD=="Constant") {
          
          for (ii in 1:no_covariate_available_RD()) {
            
            
            # ======= # ======= # ======= # ======= # ======= 
            # ======= phicov, pc case 
            
            # adjust to phi covariate index in the original dataset
            phi_cov_index<- ii + (input$cov_col_range_RD[1]-1)
            phi_cov_vec<-data_all[,phi_cov_index]
            not_na_index<-which(!is.na(phi_cov_vec))
            
            ## define covariates for phi from the no_covariate_available_RD()+2 model to the no_covariate_available_RD()*2+1 model
            # standarlise and update covariate array
            cov[,1,ii+1][not_na_index]<- phi_cov_vec[not_na_index]- mean(phi_cov_vec[not_na_index])
            
          }
          
          # the end of case input$phi_type_RD=="All" & input$p_type_RD=="All"
        }
        
        if (input$phi_type_RD=="Covariates" & input$p_type_RD=="Covariates") {
          
          for (ii in 1:no_covariate_available_RD()) {
            
            # ======= # ======= # ======= # ======= # ======= 
            # ======= phic, pcov case 
            ## define covariates for p from the 2nd model to the no_covariate_available_RD()+1 model in the table in the Settings section
            # adjust to p covariate index in the original dataset
            p_cov_index <-  ii + (input$cov_col_range_RD[1]-1)
            p_cov_vec<-data_all[,p_cov_index]
            not_na_index<-which(!is.na(p_cov_vec))
            
            # start from the 2nd arrary as the first model in the table in the Settings section is a constant model without any covaraites.
            # standarlise and update covariate array
            cov[,2,ii+1][not_na_index]<- p_cov_vec[not_na_index]- mean(p_cov_vec[not_na_index]) 
            
            
            # ======= # ======= # ======= # ======= # ======= 
            # ======= phicov, pc case 
            
            # adjust to phi covariate index in the original dataset
            phi_cov_index<- ii + (input$cov_col_range_RD[1]-1)
            phi_cov_vec<-data_all[,phi_cov_index]
            not_na_index<-which(!is.na(phi_cov_vec))
            
            ## define covariates for phi from the no_covariate_available_RD()+2 model to the no_covariate_available_RD()*2+1 model
            # standarlise and update covariate array
            cov[,1,ii+no_covariate_available_RD()+1][not_na_index]<- phi_cov_vec[not_na_index]- mean(phi_cov_vec[not_na_index])
            
            
            # ======= # ======= # ======= # ======= # ======= 
            # ======= phicov, pcov case 
            
            ## define covariates for p 
            ## from the no_covariate_available_RD()*2+2 model to the no_covariate_available_RD()^2+no_covariate_available_RD()*2+1 model
            # standarlise and update covariate array
            
            for (jj in 1:no_covariate_available_RD()) {
              
              cov[,2,ii+(jj+1)*no_covariate_available_RD()+1][not_na_index]<- p_cov_vec[not_na_index]- mean(p_cov_vec[not_na_index]) 
              
            }
            
            ## define covariates for phi
            ## from the no_covariate_available_RD()*2+2 model to the no_covariate_available_RD()^2+no_covariate_available_RD()*2+1 model
            # standarlise and update covariate array
            
            for (jj in 1:no_covariate_available_RD()) {
              
              cov[,1,jj+no_covariate_available_RD()*(ii-1)+no_covariate_available_RD()*2+1][not_na_index]<- phi_cov_vec[not_na_index]- mean(phi_cov_vec[not_na_index]) 
              
            }
            
            
          }
          
          # the end of case input$phi_type_RD=="All" & input$p_type_RD=="All"
        }

        if (input$phi_type_RD=="Time-varying" & input$p_type_RD=="Covariates") {
          
           for (ii in 1:no_covariate_available_RD()) {
           # ======= # ======= # ======= # ======= # ======= 
           # ======= phit, pcov case 
           
           p_cov_index <-  ii + (input$cov_col_range_RD[1]-1)
           p_cov_vec<-data_all[,p_cov_index]
           not_na_index<-which(!is.na(p_cov_vec))
           
           ## define covariates for p
           ## from the 3rd model to the end
           # standarlise and update covariate array
           cov[,2,ii+2][not_na_index]<- p_cov_vec[not_na_index]- mean(p_cov_vec[not_na_index])
           
           }
           
         }
         
        
      }
      
    } else {
      # no cov case
      NULL
    }
    
    # RD covariate(s)
    return(cov)
  })
  
  
  
  ###
  
  
  
  
  
  
  
  # define the (phic,pc) RD model, cc is an index ------

  cc.RD.prediction<- function(cc=1){
    
    # kk index is an parameter
    force(cc)
    
    data_all<- dataInput_RD()
    data1=data_all[,input$data_colnum_RD]
    
    
    ## read functions 
    inv <- function(m) {class(try(solve(m),silent=T))=="matrix"}
    
    # phi(c), p(c) with 3 params
    RD.phic.pc.logl <- function (param, data, T, D, k, missed){
      
      data[missed]=0
      
      sim.K=T  #  total no. of occasions
      sim.k= k # as.integer(exp( param(1) ) )   # no. of secondary occasisons (param)
      sim.T=sim.K/sim.k  # no. of primary occasions
      
      # param
      phi12<-expit(param[1]); phi12<-rep(phi12,sim.T-1); phi21 <- 1-phi12; pi<-c(mean(phi21),1-mean(phi21))
      p <- expit(param[2]); p<-rep(p,sim.K)
      n01<-exp(param[3]);N=n01+D
      
      
      phi <-  list(matrix(rep(c(0,1),2), ncol=2,byrow=TRUE)); phi <- rep(phi,(sim.T)-1)
      for (j in 1:((sim.T)-1)) {phi[[j]]<- matrix(c(1-phi12[j],phi12[j],phi21[j],1-phi21[j]), ncol=2,byrow=TRUE)}
      
      
      # prob of being removed, primary periods in rows (1,), secondary in cols (1,1)
      alpha<-matrix(NA,ncol=sim.K, nrow=sim.K)
      
      # prob of being in state 1 (observable) & not being removed
      beta1<-matrix(NA,ncol=sim.K, nrow=sim.K)
      
      # prob of being in state 2 (unobservable) & not being removed
      beta2<-matrix(NA,ncol=sim.K, nrow=sim.K)
      
      
      # jj=1, ii=1
      alpha[1,1]<- pi[1]*p[1]
      beta1[1,1]<- (alpha[1,1]/p[1])*(1-p[1]) # state 1 & not being removed 
      beta2[1,1]<- 1-(alpha[1,1]/p[1])                # state 2 & not being removed
      
      # jj=2:sim.k, ii=1
      
      for (jj in 2:sim.k){
        
        alpha[1,jj]<- (alpha[1,jj-1]/p[jj-1]*(1-p[jj-1])*p[jj])
        beta1[1,jj]<- (beta1[1,jj-1]*(1-p[jj]))
        beta2[1,jj]<- (beta2[1,jj-1])
      }
      
      
      #   #   #   #   #   #   #   #   #   from 2nd primary period
      for (ii in 2:sim.T) {
        
        phi11=phi[[ii-1]][1,1]; phi12=phi[[ii-1]][1,2];
        phi21=phi[[ii-1]][2,1]; phi22=phi[[ii-1]][2,2];       
        
        for (jj in 1:sim.k) {
          
          if (jj==1) {
            alpha[ii,1]<- (beta1[ii-1,sim.k]*phi11 + beta2[ii-1,sim.k]*phi21)*p[ii]
            beta1[ii,1]<- (alpha[ii,1]/p[ii])*(1-p[ii])  # state 1 & not being removed 
            beta2[ii,1]<-  beta1[ii-1,sim.k]*phi12 + beta2[ii-1,sim.k]*phi22    # state 2 & not being removed
          }
          else {
            alpha[ii,jj]<- (alpha[ii,jj-1]/p[jj-1]*(1-p[jj-1])*p[jj])
            beta1[ii,jj]<- beta1[ii,jj-1]*(1-p[ii])
            beta2[ii,jj]<- beta2[ii,jj-1]
          }
          
        }
      }
      
      
      L=numeric()
      
      o=1
      for (ii in 1:sim.T) {
        
        for (jj in 1:sim.k) {
          
          L[o]=alpha[ii,jj]
          o=o+1
        }}
      
      L=L[1:sim.K]  
      
      L[which(L==0)]<-10^(-10)
      L[missed]<-10^(-10)
      
      L0=1-sum(L[-missed])
      
      
      logL=lgamma(N+1)-sum(lgamma(data[-missed]+1))-lgamma(n01+1)+sum(data[-missed]*log(L[-missed])) + n01*log(L0)
      
      return(-logL)
    }
    
    # read estimated counts function
    est.RD.phic.pc.logl <- function (param, T, D, k, missed) {
      
      sim.K=T  #  total no. of occasions
      sim.k= k # as.integer(exp( param(1) ) )   # no. of secondary occasisons (param)
      sim.T=sim.K/sim.k  # no. of primary occasions
      
      # param
      phi12<-param[1]; phi12<-rep(phi12,sim.T-1); phi21 <- 1-phi12; pi<-c(mean(phi21),1-mean(phi21))
      p <- param[2]; p<-rep(p,sim.K)
      N<-param[3] # estiamted population size

      
      
      phi <-  list(matrix(rep(c(0,1),2), ncol=2,byrow=TRUE)); phi <- rep(phi,(sim.T)-1)
      for (j in 1:((sim.T)-1)) {phi[[j]]<- matrix(c(1-phi12[j],phi12[j],phi21[j],1-phi21[j]), ncol=2,byrow=TRUE)}
      
      
      # prob of being removed, primary periods in rows (1,), secondary in cols (1,1)
      alpha<-matrix(NA,ncol=sim.K, nrow=sim.K)
      
      # prob of being in state 1 (observable) & not being removed
      beta1<-matrix(NA,ncol=sim.K, nrow=sim.K)
      
      # prob of being in state 2 (unobservable) & not being removed
      beta2<-matrix(NA,ncol=sim.K, nrow=sim.K)
      
      
      # jj=1, ii=1
      alpha[1,1]<- pi[1]*p[1]
      beta1[1,1]<- (alpha[1,1]/p[1])*(1-p[1]) # state 1 & not being removed 
      beta2[1,1]<- 1-(alpha[1,1]/p[1])                # state 2 & not being removed
      
      # jj=2:sim.k, ii=1
      
      for (jj in 2:sim.k){
        
        alpha[1,jj]<- (alpha[1,jj-1]/p[jj-1]*(1-p[jj-1])*p[jj])
        beta1[1,jj]<- (beta1[1,jj-1]*(1-p[jj]))
        beta2[1,jj]<- (beta2[1,jj-1])
      }
      
      
      #   #   #   #   #   #   #   #   #   from 2nd primary period
      for (ii in 2:sim.T) {
        
        phi11=phi[[ii-1]][1,1]; phi12=phi[[ii-1]][1,2];
        phi21=phi[[ii-1]][2,1]; phi22=phi[[ii-1]][2,2];       
        
        for (jj in 1:sim.k) {
          
          if (jj==1) {
            alpha[ii,1]<- (beta1[ii-1,sim.k]*phi11 + beta2[ii-1,sim.k]*phi21)*p[ii]
            beta1[ii,1]<- (alpha[ii,1]/p[ii])*(1-p[ii])  # state 1 & not being removed 
            beta2[ii,1]<-  beta1[ii-1,sim.k]*phi12 + beta2[ii-1,sim.k]*phi22    # state 2 & not being removed
          }
          else {
            alpha[ii,jj]<- (alpha[ii,jj-1]/p[jj-1]*(1-p[jj-1])*p[jj])
            beta1[ii,jj]<- beta1[ii,jj-1]*(1-p[ii])
            beta2[ii,jj]<- beta2[ii,jj-1]
          }
          
        }
      }
      
      
      L=numeric()
      
      o=1
      for (ii in 1:sim.T) {
        
        for (jj in 1:sim.k) {
          
          L[o]=alpha[ii,jj]
          o=o+1
        }}
      
      L=L[1:sim.K]  
      
      L[missed]<-NA
      
      # predicted counts
      ept.data<-numeric()
      
      times<-c(1:sim.K)[-missed]
      
      for(i in times)
      {ept.data[i]<- N*L[i]
      }
      
      return(ept.data)
    }
    
    
    
    # run (fit.phic.pc.optim) function
    fit.phic.pc.optim<- function(param) {
      try(optim(par=param,
                fn=RD.phic.pc.logl,
                method="BFGS",
                #method="L-BFGS-B",
                #lower = c(rep(logit(10^{-5}),2),log(10^{-5})), 
                #upper = c(rep(logit(0.99999),2),log(10^{5})),
                hessian=TRUE,
                data=data1,
                T=T_RD(),
                D=D_RD(),
                k=no_k(),
                missed=miss_occ_RD()),silent =TRUE)}

    # make sure hessian is available
    inv <- function(m) {class(try(solve(m),silent=TRUE))=="matrix"} 
          
          # input iteration number
          n.rep= isolate(input$num_iteration_RD)
          
          # initial estiamte elements
          phi12.test<-p.test<-N0.test<-loglike.test<-numeric()
          hessians.test <- list(matrix(rep(0,3*3), ncol=3)) # 3 is the length(param)
          hessians.test <- rep(hessians.test,n.rep)
          
          #phi12.test<-matrix(NA,ncol=T_RD()/no_k()-1,nrow=n.rep)
          
          # update this for all models
          time_spent=numeric()
          
          # initialise iteration numbers
          j=1
          k=1
          
          # Start the clock for optimisation =-=-=-=-
          ptm <- proc.time()
          
          # run iterations
          for (j in 1:n.rep) { 
            
            # initial starting param
            param=c(logit(runif(1,0.1,0.9)), #phi12
                    logit(runif(1,0.1,0.9)),# p
                    log(runif(1,1,50)) ) # N0
            
            # run the model
            fit <- fit.phic.pc.optim(param=param)
                  
            if (inv(fit$hessian)==TRUE) { # check if hessian is available
              if (k<(n.rep+1)) {
                
                # record estimates for each iteration
                phi12.test[k] <- expit(fit$par[1])
                p.test[k] <- expit(fit$par[2])
                N0.test[k] <- exp(fit$par[3])
                
                # record hessians for each iteration
                 hessians.test[[k]]=fit$hessian
              
                # record loglike for each iteration
                # these will be double checked by users
                loglike.test[k]<- -fit$value
                
                k=k+1      
              } }
            # j=j+1
            
            # the end of iterations
          }
          

          # Stop the clock =-=-=-=-
          t.record<-proc.time() - ptm
          
          # Time spent
          time_spent <- as.numeric(names(table(t.record[1])))
          
          # put together results for all iterations
          AIC.test <- - 2*loglike.test + (2*length(param))
          
          mle.test<-      data.frame(phi12=phi12.test,
                                     p_int=p.test,
                                     N0=N0.test,
                                     loglike=loglike.test,
                                     AIC= AIC.test
                                     )
          
          # observe the number of times hitting the MLEs
          loglike.test<-signif(loglike.test, 7)
          num.max.location <- length(which(loglike.test==max(loglike.test)))
          
          
          # observe which iteration has MLEs
          max.location <-which(loglike.test==max(loglike.test))[1]
          
          # record MLEs corresponding to the max.location.2cov
          fit.phi12 <- phi12.test[max.location]
          fit.p <- p.test[max.location]
          fit.N <- N0.test[max.location]+ D_RD() # + RD observed data
          
          # record the hessian corresponding to the max.location.2cov
          fit.hessians <- hessians.test[[max.location]]
          fit.loglike <- max(loglike.test)
          fit.AIC<- - 2*max(loglike.test) + (2*length(param))
          
          # *** SE and 95% CI
  
          SE <- sqrt(diag(solve(hessians.test[[max.location]])))
          
          
          # phi12
          SE.phi12 <- SE[1]*fit.phi12*(1-fit.phi12)
          lowerCI.phi12 <- fit.phi12-1.96*SE.phi12
          upperCI.phi12 <- fit.phi12+1.96*SE.phi12
          
          # p
              SE.p <- SE[2]*fit.p*(1-fit.p)
              lowerCI.p <- fit.p-1.96*SE.p
              upperCI.p <- fit.p+1.96*SE.p
          
          # ******** N
              SE.N <- exp(SE[3])
              lowerCI.N <- fit.N -1.96*SE.N
              upperCI.N <- fit.N +1.96*SE.N
          
          
          # ***put together MLE results as well as loglike and AIC
          # first row shows fit.phi12, fit.p, fit.N, fit.loglike and AIC
          # second row shows SE of fit.phi12, fit.p, fit.N and NA, NA
          # third row shows lower 95% CI of fit.phi12, fit.p, fit.N and NA, NA
          # forth row shows upper 95% CI of fit.phi12, fit.p, fit.N and NA, NA
          
          fit.mle.results    <-  data.frame(phi12=c(fit.phi12),#%,SE.phi12,lowerCI.phi12,upperCI.phi12),
                                            p=c(fit.p),#%SE.p,lowerCI.p,upperCI.p),
                                            N=c(fit.N),#%,SE.N,lowerCI.N,upperCI.N),
                                            loglike=c(fit.loglike),#% , NA,NA,NA),
                                            AIC=c(fit.AIC) #% , NA,NA,NA)
          )
          
          # *** estimated counts of individuals and 95% CI
          
          est.numbers<- est.RD.phic.pc.logl(param=c(fit.phi12,
                                                    fit.p,
                                                    fit.N), 
                                            T=T_RD(), 
                                            D=D_RD(), 
                                            k=no_k(), 
                                            missed=miss_occ_RD()
                                            )
          
          #% lowerCI.numbers<- est.RD.phic.pc.logl(param=c(lowerCI.phi12,lowerCI.p,lowerCI.N),
          #%                                       T=T_RD(), 
          #%                                      D=D_RD(), 
          #%                                      k=no_k(), 
          #%                                     missed=miss_occ_RD()
          #%                                     )
          
          #%upperCI.numbers<- est.RD.phic.pc.logl(param=c(upperCI.phi12,upperCI.p,upperCI.N), 
          #%                                      T=T_RD(), 
          #%                                      D=D_RD(), 
          #%                                      k=no_k(), 
          #%                                      missed=miss_occ_RD()
          #%                                      )
          
          est.counts<-data.frame(est.num=est.numbers)#%,
                                 #%LCI=lowerCI.numbers,
                                 #%UCI=upperCI.numbers)
          
          ## --- the end of (phic, pc) RD model

      
    
    return(list(mle.test=mle.test, # results for all iteration
                hessians.test=hessians.test, # results for all iteration
                fit.mle.results=fit.mle.results, # results for MLEs
                fit.hessians=fit.hessians, # hessian for MLEs
                time_spent=time_spent, # time spent
                num.max.location=num.max.location, # the number of times hitting the MLEs
                est.counts=est.counts # estimated counts and 95% CI
    ))
    
  }
 # --- the end of cc model 
 
  # define the (phic,pcov) RD model, ccov is an index ------
  
  ccov.RD.prediction<- function(ccov){
    
    # ccov index is an parameter
    force(ccov)
    
    data_all<- dataInput_RD()
    data1=data_all[,input$data_colnum_RD]
    
  
    # phi(c), p(cov) with 4 params
    RD.phic.pcov.logl <- function (param,data, T, D, k, missed, z){
      
      data[missed]=0
      
      sim.K=T  #  total no. of occasions
      sim.k= k # as.integer(exp( param(1) ) )   # no. of secondary occasisons (param)
      sim.T=sim.K/sim.k  # no. of primary occasions
      
      # param
      phi12<-expit(param[1]); phi12<-rep(phi12,sim.T-1); phi21 <- 1-phi12; pi<-c(mean(phi21),1-mean(phi21))
      p <- expit(param[2]+param[3]*z); p[missed]<-10^{-10}
      n01<-exp(param[4]);N=n01+D
      
      
      phi <-  list(matrix(rep(c(0,1),2), ncol=2,byrow=TRUE)); phi <- rep(phi,(sim.T)-1)
      for (j in 1:((sim.T)-1)) {phi[[j]]<- matrix(c(1-phi12[j],phi12[j],phi21[j],1-phi21[j]), ncol=2,byrow=TRUE)}
      
      
      # prob of being removed, primary periods in rows (1,), secondary in cols (1,1)
      alpha<-matrix(NA,ncol=sim.k, nrow=sim.T)
      
      # prob of being in state 1 (observable) & not being removed
      beta1<-matrix(NA,ncol=sim.k, nrow=sim.T)
      
      # prob of being in state 2 (unobservable) & not being removed
      beta2<-matrix(NA,ncol=sim.k, nrow=sim.T)
      
      
      # jj=1, ii=1
      alpha[1,1]<- pi[1]*p[1]
      beta1[1,1]<- (alpha[1,1]/p[1])*(1-p[1]) # state 1 & not being removed 
      beta2[1,1]<- 1-(alpha[1,1]/p[1])                # state 2 & not being removed
      
      # jj=2:sim.k, ii=1
      
      for (jj in 2:sim.k){
        
        alpha[1,jj]<- (alpha[1,jj-1]/p[jj-1]*(1-p[jj-1])*p[jj])
        beta1[1,jj]<- (beta1[1,jj-1]*(1-p[jj]))
        beta2[1,jj]<- (beta2[1,jj-1])
      }
      
      
      #   #   #   #   #   #   #   #   #   from 2nd primary period
      for (ii in 2:sim.T) {
        
        phi11=phi[[ii-1]][1,1]; phi12=phi[[ii-1]][1,2];
        phi21=phi[[ii-1]][2,1]; phi22=phi[[ii-1]][2,2];       
        
        for (jj in 1:sim.k) {
          
          if (jj==1) {
            alpha[ii,1]<- (beta1[ii-1,sim.k]*phi11 + beta2[ii-1,sim.k]*phi21)*p[ii]
            beta1[ii,1]<- (alpha[ii,1]/p[ii])*(1-p[ii])  # state 1 & not being removed 
            beta2[ii,1]<-  beta1[ii-1,sim.k]*phi12 + beta2[ii-1,sim.k]*phi22    # state 2 & not being removed
          }
          else {
            alpha[ii,jj]<- (alpha[ii,jj-1]/p[jj-1]*(1-p[jj-1])*p[jj])
            beta1[ii,jj]<- beta1[ii,jj-1]*(1-p[ii])
            beta2[ii,jj]<- beta2[ii,jj-1]
          }
          
        }
      }
      
      
      L=numeric()
      
      o=1
      for (ii in 1:sim.T) {
        
        for (jj in 1:sim.k) {
          
          L[o]=alpha[ii,jj]
          o=o+1
        }}
      
      L=L[1:sim.K]  
      
      L[which(L==0)]<-10^(-10)
      L[missed]<-10^(-10)
      
      L0= 1-sum(L[-missed])
      
      logL=lgamma(N+1)-sum(lgamma(data[-missed]+1))-lgamma(n01+1)+sum(data[-missed]*log(L[-missed])) + n01*log(L0)
      
      return(-logL)
    }

    # read estimated counts function
    est.RD.phic.pcov.logl <- function (param, T, D, k, missed , z) {
      
      sim.K=T  #  total no. of occasions
      sim.k= k # as.integer(exp( param(1) ) )   # no. of secondary occasisons (param)
      sim.T=sim.K/sim.k  # no. of primary occasions
      
      # param
      phi12<-param[1]; phi12<-rep(phi12,sim.T-1); phi21 <- 1-phi12; pi<-c(mean(phi21),1-mean(phi21))
      p <- expit(param[2]+param[3]*z); p[missed]<-10^{-10}
      N<-param[4] # estiamted population size
      
      
      
      phi <-  list(matrix(rep(c(0,1),2), ncol=2,byrow=TRUE)); phi <- rep(phi,(sim.T)-1)
      for (j in 1:((sim.T)-1)) {phi[[j]]<- matrix(c(1-phi12[j],phi12[j],phi21[j],1-phi21[j]), ncol=2,byrow=TRUE)}
      
      
      # prob of being removed, primary periods in rows (1,), secondary in cols (1,1)
      alpha<-matrix(NA,ncol=sim.K, nrow=sim.K)
      
      # prob of being in state 1 (observable) & not being removed
      beta1<-matrix(NA,ncol=sim.K, nrow=sim.K)
      
      # prob of being in state 2 (unobservable) & not being removed
      beta2<-matrix(NA,ncol=sim.K, nrow=sim.K)
      
      
      # jj=1, ii=1
      alpha[1,1]<- pi[1]*p[1]
      beta1[1,1]<- (alpha[1,1]/p[1])*(1-p[1]) # state 1 & not being removed 
      beta2[1,1]<- 1-(alpha[1,1]/p[1])                # state 2 & not being removed
      
      # jj=2:sim.k, ii=1
      
      for (jj in 2:sim.k){
        
        alpha[1,jj]<- (alpha[1,jj-1]/p[jj-1]*(1-p[jj-1])*p[jj])
        beta1[1,jj]<- (beta1[1,jj-1]*(1-p[jj]))
        beta2[1,jj]<- (beta2[1,jj-1])
      }
      
      
      #   #   #   #   #   #   #   #   #   from 2nd primary period
      for (ii in 2:sim.T) {
        
        phi11=phi[[ii-1]][1,1]; phi12=phi[[ii-1]][1,2];
        phi21=phi[[ii-1]][2,1]; phi22=phi[[ii-1]][2,2];       
        
        for (jj in 1:sim.k) {
          
          if (jj==1) {
            alpha[ii,1]<- (beta1[ii-1,sim.k]*phi11 + beta2[ii-1,sim.k]*phi21)*p[ii]
            beta1[ii,1]<- (alpha[ii,1]/p[ii])*(1-p[ii])  # state 1 & not being removed 
            beta2[ii,1]<-  beta1[ii-1,sim.k]*phi12 + beta2[ii-1,sim.k]*phi22    # state 2 & not being removed
          }
          else {
            alpha[ii,jj]<- (alpha[ii,jj-1]/p[jj-1]*(1-p[jj-1])*p[jj])
            beta1[ii,jj]<- beta1[ii,jj-1]*(1-p[ii])
            beta2[ii,jj]<- beta2[ii,jj-1]
          }
          
        }
      }
      
      
      L=numeric()
      
      o=1
      for (ii in 1:sim.T) {
        
        for (jj in 1:sim.k) {
          
          L[o]=alpha[ii,jj]
          o=o+1
        }}
      
      L=L[1:sim.K]  
      
      L[missed]<-NA
      
      # predicted counts
      ept.data<-numeric()
      
      times<-c(1:sim.K)[-missed]
      
      for(i in times)
      {ept.data[i]<- N*L[i]
      }
      
      return(ept.data)
    }
    
    
    # choose one covariate for the model
    z_p <- zz_RD()[,2,ccov+1]  # +1 because the first covaraiete arrary is empty for p(c) model
                          # 2nd column is because we need to choose covariate for the capture probabilities (p)
                          # see settings in zz_RD() function
    
    # define the (fit.phic.pcov.optim) function for optimisation
    fit.phic.pcov.optim<- function(param) {
      try(optim(par=param,
                fn=RD.phic.pcov.logl, # define the loglikelihood function
                method="BFGS",
                #method="L-BFGS-B",
                #lower = c(rep(logit(10^{-5}),2),log(10^{-5})), 
                #upper = c(rep(logit(0.99999),2),log(10^{5})),
                hessian=TRUE,
                data=data1,
                T=T_RD(),
                D=D_RD(),
                k=no_k(),
                z=z_p,
                missed=miss_occ_RD()),silent =TRUE)}
    
    # make sure hessian is available
    inv <- function(m) {class(try(solve(m),silent=TRUE))=="matrix"} 
    
    # input iteration number
    n.rep= isolate(input$num_iteration_RD)
    
    # initial estiamte elements
    phi12.ccov.test<-p.int.ccov.test<-p.slope1.ccov.test<-N0.ccov.test<-loglike.ccov.test<-numeric()
    hessians.ccov.test <- list(matrix(rep(0,4*4), ncol=4)) # 4 is the length(param)
    hessians.ccov.test <- rep(hessians.ccov.test,n.rep)
    
    #phi12.test<-matrix(NA,ncol=T_RD()/no_k()-1,nrow=n.rep)
    
    # update this for all models
    time_spent=numeric()
    
    # initialise iteration numbers
    j=1
    k=1
    
    # Start the clock for optimisation =-=-=-=-
    ptm <- proc.time()
    
    # run iterations
    for (j in 1:n.rep) { 
      
      # initial starting param
      param=c(logit(runif(1,0.1,0.9)), #phi12
              runif(2,-1,1),# p.int and p.slope1
              log(runif(1,1,50)) ) # N0
      
      # run the model
      fit <- fit.phic.pcov.optim(param=param)
      
      
      if (inv(fit$hessian)==TRUE) { # check if hessian is available
        if (k<(n.rep+1)) {
          
          # record estimates for each iteration
          phi12.ccov.test[k] <- expit(fit$par[1])
          p.int.ccov.test[k] <- fit$par[2]
          p.slope1.ccov.test[k] <- fit$par[3]
          N0.ccov.test[k] <- exp(fit$par[4])
          
          # record hessians for each iteration
          hessians.ccov.test[[k]]=fit$hessian
          
          # record loglike for each iteration
          # these will be double checked by users
          loglike.ccov.test[k]= -fit$value
          
          k=k+1      
        } }
      # j=j+1
      
      # the end of iterations
    }
    
    
    # Stop the clock =-=-=-=-
    t.record<-proc.time() - ptm
    
    # Time spent
    time_spent.ccov<- as.numeric(names(table(t.record[1])))
    
    # put together results for all iterations
    AIC.ccov.test <- - 2*loglike.ccov.test + (2*length(param))
    
    mle.test.ccov<- data.frame(phi12=phi12.ccov.test,
                               p_int=p.int.ccov.test,
                               p_slope=p.slope1.ccov.test,
                               N0=N0.ccov.test,
                               loglike=loglike.ccov.test,
                               AIC= AIC.ccov.test
    )

    # observe the number of times hitting the MLEs
    loglike.ccov.test<-signif(loglike.ccov.test, 7)
    num.max.location.ccov<- length(which(loglike.ccov.test==max(loglike.ccov.test)))

    
    # observe which iteration has MLEs
    max.location.ccov<-which(loglike.ccov.test==max(loglike.ccov.test))[1]

    # record MLEs corresponding to the max.location.2cov
    fit.phi12.ccov <- phi12.ccov.test[max.location.ccov]
    fit.p.int.ccov <- p.int.ccov.test[max.location.ccov]
    fit.p.slope1.ccov <- p.slope1.ccov.test[max.location.ccov]
    fit.N.ccov <- N0.ccov.test[max.location.ccov]+ D_RD() # + RD observed data

    # record the hessian corresponding to the max.location.2cov
    fit.hessians.ccov <- hessians.ccov.test[[max.location.ccov]]
    fit.loglike.ccov <- max(loglike.ccov.test)
    fit.AIC.ccov<- - 2*max(loglike.ccov.test) + (2*length(param))

    # *** SE and 95% CI
    
    SE <- sqrt(diag(solve(hessians.ccov.test[[max.location.ccov]])))

    # phi12
    SE.phi12.ccov <- SE[1]*fit.phi12.ccov*(1-fit.phi12.ccov)
    lowerCI.phi12.ccov <- ifelse(fit.phi12.ccov-1.96*SE.phi12.ccov<0, 10^{-10},fit.phi12.ccov-1.96*SE.phi12.ccov)
    upperCI.phi12.ccov <- fit.phi12.ccov+1.96*SE.phi12.ccov
    
    # p.int
    SE.p.int.ccov<- SE[2]
    lowerCI.p.int.ccov<- fit.p.int.ccov-1.96*SE.p.int.ccov
    upperCI.p.int.ccov<- fit.p.int.ccov+1.96*SE.p.int.ccov
    
    # p.slope1
    SE.p.slope1.ccov<- SE[3]
    lowerCI.p.slope1.ccov<- fit.p.slope1.ccov-1.96*SE.p.slope1.ccov
    upperCI.p.slope1.ccov<- fit.p.slope1.ccov+1.96*SE.p.slope1.ccov
    
    # ******** N
    SE.N.ccov<- exp(SE[4])
    lowerCI.N.ccov<- fit.N.ccov-1.96*SE.N.ccov
    upperCI.N.ccov<- fit.N.ccov+1.96*SE.N.ccov
    
    
    # ***put together MLE results as well as loglike and AIC
    # first row shows fit.phi12, fit.p.int, fit.p.slope1, fit.N, fit.loglike and AIC
    # second row shows SE 
    # third row shows lower 95% CI 
    # forth row shows upper 95% CI 

    fit.mle.results.ccov <-data.frame(phi12=c(fit.phi12.ccov,
                                              SE.phi12.ccov,
                                              lowerCI.phi12.ccov,
                                              upperCI.phi12.ccov),
                                      
                                      p_int=c(fit.p.int.ccov,
                                              SE.p.int.ccov,
                                              lowerCI.p.int.ccov,
                                              upperCI.p.int.ccov),
                                      
                                      p_slope=c(fit.p.slope1.ccov,
                                                 SE.p.slope1.ccov,
                                                 lowerCI.p.slope1.ccov,
                                                 upperCI.p.slope1.ccov),
                                      
                                      N=c(fit.N.ccov,
                                          SE.N.ccov,
                                          lowerCI.N.ccov,
                                          upperCI.N.ccov),
                                      
                                      loglike=c(fit.loglike.ccov, NA,NA,NA),
                                      AIC=c(fit.AIC.ccov, NA,NA,NA)
    )
    
    # *** estimated counts of individuals and 95% CI
    
    est.numbers<- est.RD.phic.pcov.logl(param=c(fit.phi12.ccov,
                                                fit.p.int.ccov,
                                                fit.p.slope1.ccov,
                                                fit.N.ccov), 
                                        T=T_RD(), 
                                        D=D_RD(), 
                                        k=no_k(), 
                                        missed=miss_occ_RD(),
                                        z=z_p)
    
    lowerCI.numbers<- est.RD.phic.pcov.logl(param=c(lowerCI.phi12.ccov,
                                                    lowerCI.p.int.ccov,
                                                    lowerCI.p.slope1.ccov,
                                                    lowerCI.N.ccov),
                                            T=T_RD(), 
                                            D=D_RD(), 
                                            k=no_k(), 
                                            missed=miss_occ_RD(),
                                            z=z_p #
                                            )
    
    upperCI.numbers<- est.RD.phic.pcov.logl(param=c(upperCI.phi12.ccov,
                                                    upperCI.p.int.ccov,
                                                    upperCI.p.slope1.ccov,
                                                    upperCI.N.ccov), 
                                          T=T_RD(), 
                                          D=D_RD(), 
                                          k=no_k(), 
                                          missed=miss_occ_RD(),
                                          z=z_p #
                                          )
    
    est.counts<-data.frame(est.num=est.numbers,
                           LCI=lowerCI.numbers,
                           UCI=upperCI.numbers)

    ## --- the end of (phic, pcov) RD model
    
    
    
    return(list(mle.test=mle.test.ccov, # results for all iteration
                hessians.test=hessians.ccov.test, # results for all iteration
                fit.mle.results=fit.mle.results.ccov, # results for MLEs
                fit.hessians=fit.hessians.ccov, # hessian for MLEs
                time_spent=time_spent.ccov, # time spent
                num.max.location=num.max.location.ccov, # the number of times hitting the MLEs
                est.counts=est.counts # estimated counts and 95% CI
    ))
    
  }
# --- the end of ccov model  
  
  
  # define the (phicov, pc) RD model, covc is an index ------
  
  covc.RD.prediction<- function(covc){
    
    # kk index is an parameter
    force(covc)
    
    data_all<- dataInput_RD()
    data1=data_all[,input$data_colnum_RD]
    
    
    # phi(cov), p(c) with 4 params
    RD.phicov.pc.logl <- function (param,data, T, D, k, missed, z){
      
      data[missed]=0
      
      sim.K=T  #  total no. of occasions
      sim.k= k # as.integer(exp( param(1) ) )   # no. of secondary occasisons (param)
      sim.T=sim.K/sim.k  # no. of primary occasions
      
      
      #preprocess z
      z_mat<- matrix(z,ncol=sim.k,nrow=sim.T,byrow=TRUE)
      
      # param
      phi12<-expit(param[1]+param[2]*z_mat[2:sim.T,1]); na.index<-which(is.na(phi12)); phi12[na.index]<-0
      phi21 <- 1-phi12; phi21[na.index]<-0
      pi<-c(mean(phi21),1-mean(phi21))
      
      p <- expit(param[3]); p<-rep(p,sim.K)
      n01<-exp(param[4]);N=n01+D
      
      
      phi <-  list(matrix(rep(c(0,1),2), ncol=2,byrow=TRUE)); phi <- rep(phi,(sim.T)-1)
      for (j in 1:((sim.T)-1)) {phi[[j]]<- matrix(c(1-phi12[j],phi12[j],phi21[j],1-phi21[j]), ncol=2,byrow=TRUE)}
      
      
      # prob of being removed, primary periods in rows (1,), secondary in cols (1,1)
      alpha<-matrix(NA,ncol=sim.k, nrow=sim.T)
      
      # prob of being in state 1 (observable) & not being removed
      beta1<-matrix(NA,ncol=sim.k, nrow=sim.T)
      
      # prob of being in state 2 (unobservable) & not being removed
      beta2<-matrix(NA,ncol=sim.k, nrow=sim.T)
      
      
      # jj=1, ii=1
      alpha[1,1]<- pi[1]*p[1]
      beta1[1,1]<- (alpha[1,1]/p[1])*(1-p[1]) # state 1 & not being removed 
      beta2[1,1]<- 1-(alpha[1,1]/p[1])                # state 2 & not being removed
      
      # jj=2:sim.k, ii=1
      
      for (jj in 2:sim.k){
        
        alpha[1,jj]<- (alpha[1,jj-1]/p[jj-1]*(1-p[jj-1])*p[jj])
        beta1[1,jj]<- (beta1[1,jj-1]*(1-p[jj]))
        beta2[1,jj]<- (beta2[1,jj-1])
      }
      
      
      #   #   #   #   #   #   #   #   #   from 2nd primary period
      for (ii in 2:sim.T) {
        
        phi11=phi[[ii-1]][1,1]; phi12=phi[[ii-1]][1,2];
        phi21=phi[[ii-1]][2,1]; phi22=phi[[ii-1]][2,2];       
        
        for (jj in 1:sim.k) {
          
          if (jj==1) {
            alpha[ii,1]<- (beta1[ii-1,sim.k]*phi11 + beta2[ii-1,sim.k]*phi21)*p[ii]
            beta1[ii,1]<- (alpha[ii,1]/p[ii])*(1-p[ii])  # state 1 & not being removed 
            beta2[ii,1]<-  beta1[ii-1,sim.k]*phi12 + beta2[ii-1,sim.k]*phi22    # state 2 & not being removed
          }
          else {
            alpha[ii,jj]<- (alpha[ii,jj-1]/p[jj-1]*(1-p[jj-1])*p[jj])
            beta1[ii,jj]<- beta1[ii,jj-1]*(1-p[ii])
            beta2[ii,jj]<- beta2[ii,jj-1]
          }
          
        }
      }
      
      L=numeric()
      
      o=1
      for (ii in 1:sim.T) {
        
        for (jj in 1:sim.k) {
          
          L[o]=alpha[ii,jj]
          o=o+1
        }}
      
      L=L[1:sim.K]  
      
      L[which(L==0)]<-10^(-10)
      L[missed]<-10^(-10)
      L0=1-sum(L[-missed])
      
      
      logL=lgamma(N+1)-sum(lgamma(data[-missed]+1))-lgamma(n01+1)+sum(data[-missed]*log(L[-missed])) + n01*log(L0)
      
      return(-logL)
    }
    
    
    # read estimated counts function
    est.RD.phicov.pc.logl <- function (param, T, D, k, missed, z) {
      
      sim.K=T  #  total no. of occasions
      sim.k= k # as.integer(exp( param(1) ) )   # no. of secondary occasisons (param)
      sim.T=sim.K/sim.k  # no. of primary occasions
      
      
      #preprocess z
      z_mat<- matrix(z,ncol=sim.k,nrow=sim.T,byrow=TRUE)
      
      # param
      phi12<-expit(param[1]+param[2]*z_mat[2:sim.T,1]); na.index<-which(is.na(phi12)); phi12[na.index]<-0
      phi21 <- 1-phi12; phi21[na.index]<-0
      pi<-c(mean(phi21),1-mean(phi21))
      
      p <- param[3]; p<-rep(p,sim.K)
      N<-param[4] # estiamted population size
      
      
      
      phi <-  list(matrix(rep(c(0,1),2), ncol=2,byrow=TRUE)); phi <- rep(phi,(sim.T)-1)
      for (j in 1:((sim.T)-1)) {phi[[j]]<- matrix(c(1-phi12[j],phi12[j],phi21[j],1-phi21[j]), ncol=2,byrow=TRUE)}
      
      
      # prob of being removed, primary periods in rows (1,), secondary in cols (1,1)
      alpha<-matrix(NA,ncol=sim.K, nrow=sim.K)
      
      # prob of being in state 1 (observable) & not being removed
      beta1<-matrix(NA,ncol=sim.K, nrow=sim.K)
      
      # prob of being in state 2 (unobservable) & not being removed
      beta2<-matrix(NA,ncol=sim.K, nrow=sim.K)
      
      
      # jj=1, ii=1
      alpha[1,1]<- pi[1]*p[1]
      beta1[1,1]<- (alpha[1,1]/p[1])*(1-p[1]) # state 1 & not being removed 
      beta2[1,1]<- 1-(alpha[1,1]/p[1])                # state 2 & not being removed
      
      # jj=2:sim.k, ii=1
      
      for (jj in 2:sim.k){
        
        alpha[1,jj]<- (alpha[1,jj-1]/p[jj-1]*(1-p[jj-1])*p[jj])
        beta1[1,jj]<- (beta1[1,jj-1]*(1-p[jj]))
        beta2[1,jj]<- (beta2[1,jj-1])
      }
      
      
      #   #   #   #   #   #   #   #   #   from 2nd primary period
      for (ii in 2:sim.T) {
        
        phi11=phi[[ii-1]][1,1]; phi12=phi[[ii-1]][1,2];
        phi21=phi[[ii-1]][2,1]; phi22=phi[[ii-1]][2,2];       
        
        for (jj in 1:sim.k) {
          
          if (jj==1) {
            alpha[ii,1]<- (beta1[ii-1,sim.k]*phi11 + beta2[ii-1,sim.k]*phi21)*p[ii]
            beta1[ii,1]<- (alpha[ii,1]/p[ii])*(1-p[ii])  # state 1 & not being removed 
            beta2[ii,1]<-  beta1[ii-1,sim.k]*phi12 + beta2[ii-1,sim.k]*phi22    # state 2 & not being removed
          }
          else {
            alpha[ii,jj]<- (alpha[ii,jj-1]/p[jj-1]*(1-p[jj-1])*p[jj])
            beta1[ii,jj]<- beta1[ii,jj-1]*(1-p[ii])
            beta2[ii,jj]<- beta2[ii,jj-1]
          }
          
        }
      }
      
      
      L=numeric()
      
      o=1
      for (ii in 1:sim.T) {
        
        for (jj in 1:sim.k) {
          
          L[o]=alpha[ii,jj]
          o=o+1
        }}
      
      L=L[1:sim.K]  
      
      L[missed]<-NA
      
      # predicted counts
      ept.data<-numeric()
      
      times<-c(1:sim.K)[-missed]
      
      for(i in times)
      {ept.data[i]<- N*L[i]
      }
      
      return(ept.data)
    }
 
    #*** choose one covariate for the model
    # 1st column is because we need to choose covariate for the phi12
    # +1+no_covariate_available_RD() to choose the right covariate vector
    # see settings in zz_RD() function
    
    if (input$phi_type_RD=="Covariates" & input$p_type_RD=="Constant") {
        z_phi12 <- zz_RD()[,1,covc+1]  
        }
    
    if (input$phi_type_RD=="Covariates" & input$p_type_RD=="Covariates") {
        z_phi12 <- zz_RD()[,1,covc+1+no_covariate_available_RD()]
    }
    

    # run (fit.phicov.pc.optim) function
    fit.phicov.pc.optim<- function(param) {
      try(optim(par=param,
                fn=RD.phicov.pc.logl,
                method="BFGS",
                #method="L-BFGS-B",
                #lower = c(rep(logit(10^{-5}),2),log(10^{-5})), 
                #upper = c(rep(logit(0.99999),2),log(10^{5})),
                hessian=TRUE,
                data=data1,
                T=T_RD(),
                D=D_RD(),
                k=no_k(),
                missed=miss_occ_RD(),
                z=z_phi12),silent=TRUE)}
    
    # make sure hessian is available
    inv <- function(m) {class(try(solve(m),silent=TRUE))=="matrix"} 
    
    # input iteration number
    n.rep= isolate(input$num_iteration_RD)
    
    # initial estiamte elements
    phi12.int.test<-phi12.slope.test <- p.test<-N0.test<-loglike.test<-numeric()
    hessians.test <- list(matrix(rep(0,4*4), ncol=4)) # 4 is the length(param)
    hessians.test <- rep(hessians.test,n.rep)
    
    #phi12.test<-matrix(NA,ncol=T_RD()/no_k()-1,nrow=n.rep)
    
    # update this for all models
    time_spent=numeric()
    
    # initialise iteration numbers
    j=1
    k=1
    
    # Start the clock for optimisation =-=-=-=-
    ptm <- proc.time()
    
    # run iterations
    for (j in 1:n.rep) { 
      
      # initial starting param
      param=c(runif(2,-1,1), #phi12.int and phi12.slope
              logit(runif(1,0.1,0.9)),# p
              log(runif(1,10,200)) ) # N0
      
      # run the model
      fit <- fit.phicov.pc.optim(param=param)
      
      
      if (inv(fit$hessian)==TRUE) { # check if hessian is available
        if (k<(n.rep+1)) {
          
          
          # record estimates for each iteration
          phi12.int.test[k] <- fit$par[1]
          phi12.slope.test[k] <- fit$par[2]
          p.test[k] <- expit(fit$par[3])
          N0.test[k] <- exp(fit$par[4])
          
          # record hessians for each iteration
          hessians.test[[k]]<-fit$hessian
          
          # record loglike for each iteration
          # these will be double checked by users
          loglike.test[k]<- -fit$value
          
          k=k+1      
        } }
      
      # the end of iterations
    }
    
    
    # Stop the clock =-=-=-=-
    t.record<-proc.time() - ptm
    
    # Time spent
    time_spent <- as.numeric(names(table(t.record[1])))
    
    # put together results for all iterations
    AIC.test <- - 2*loglike.test + (2*length(param))
    
    mle.test<-      data.frame(phi12_int=phi12.int.test,
                               phi12_slope=phi12.slope.test,
                               p=p.test,
                               N0=N0.test,
                               loglike=loglike.test,
                               AIC= AIC.test
    )
    
    # observe the number of times hitting the MLEs
    loglike.test<-signif(loglike.test, 7)
    num.max.location <- length(which(loglike.test==max(loglike.test)))
    
    # observe which iteration has MLEs
    max.location <-which(loglike.test==max(loglike.test))[1]
    
    # record MLEs corresponding to the max.location.2cov
    fit.phi12.int <- phi12.int.test[max.location]
    fit.phi12.slope <- phi12.slope.test[max.location]
    fit.p <- p.test[max.location]
    fit.N <- N0.test[max.location]+ D_RD() # + RD observed data
    
    # record the hessian corresponding to the max.location.2cov
    fit.hessians <- hessians.test[[max.location]]
    fit.loglike <- max(loglike.test)
    fit.AIC<- - 2*max(loglike.test) + (2*length(param))

    # *** SE and 95% CI
    
    SE <- sqrt(diag(solve(hessians.test[[max.location]])))

    # phi12.int
    SE.phi12.int <- SE[1]
    lowerCI.phi12.int <- fit.phi12.int-1.96*SE.phi12.int
    upperCI.phi12.int <- fit.phi12.int+1.96*SE.phi12.int

    # phi12.slope
    SE.phi12.slope <- SE[2]
    lowerCI.phi12.slope <- fit.phi12.slope-1.96*SE.phi12.slope
    upperCI.phi12.slope <- fit.phi12.slope+1.96*SE.phi12.slope
    
    # p
    SE.p <- SE[3]*fit.p*(1-fit.p)
    lowerCI.p <- ifelse(fit.p-1.96*SE.p<0, 0, fit.p-1.96*SE.p)
    upperCI.p <- fit.p+1.96*SE.p
    
    # ******** N
    SE.N <- exp(SE[4])
    lowerCI.N <- fit.N -1.96*SE.N
    upperCI.N <- fit.N +1.96*SE.N

    
    # ***put together MLE results as well as loglike and AIC
    # first row shows fit.phi12, fit.p, fit.N, fit.loglike and AIC
    # second row shows SE of fit.phi12, fit.p, fit.N and NA, NA
    # third row shows lower 95% CI of fit.phi12, fit.p, fit.N and NA, NA
    # forth row shows upper 95% CI of fit.phi12, fit.p, fit.N and NA, NA
    
    fit.mle.results    <-  data.frame(phi12_int=c(fit.phi12.int,
                                                  SE.phi12.int,
                                                  lowerCI.phi12.int,
                                                  upperCI.phi12.int),
                                      phi12_slope=c(fit.phi12.slope,
                                                    SE.phi12.slope,
                                                    lowerCI.phi12.slope,
                                                    upperCI.phi12.slope),
                                      p=c(fit.p,
                                          SE.p,
                                          lowerCI.p,
                                          upperCI.p),
                                      N=c(fit.N,
                                          SE.N,
                                          lowerCI.N,
                                          upperCI.N),
                                      loglike=c(fit.loglike , NA,NA,NA),
                                      AIC=c(fit.AIC , NA,NA,NA)
    )

    # *** estimated counts of individuals and 95% CI
    
    est.numbers<- est.RD.phicov.pc.logl(param=c(fit.phi12.int,
                                              fit.phi12.slope,
                                              fit.p,
                                              fit.N), 
                                      T=T_RD(), 
                                      D=D_RD(), 
                                      k=no_k(), 
                                      missed=miss_occ_RD(),
                                      z=z_phi12
                                      )
    
    lowerCI.numbers<- est.RD.phicov.pc.logl(param=c(lowerCI.phi12.int,
                                                  lowerCI.phi12.slope,
                                                  lowerCI.p,
                                                  lowerCI.N),
                                          T=T_RD(), 
                                          D=D_RD(), 
                                          k=no_k(), 
                                          missed=miss_occ_RD(),
                                          z=z_phi12)

    
    upperCI.numbers<- est.RD.phicov.pc.logl(param=c(upperCI.phi12.int,
                                                  upperCI.phi12.slope,
                                                  upperCI.p,
                                                  upperCI.N), 
                                          T=T_RD(), 
                                          D=D_RD(), 
                                          k=no_k(), 
                                          missed=miss_occ_RD(),
                                          z=z_phi12)

    est.counts<-data.frame(est.num=est.numbers,
                           LCI=lowerCI.numbers,
                           UCI=upperCI.numbers)
    
    ## --- the end of (phic, pc) RD model
    
    
    
    return(list(mle.test=mle.test, # results for all iteration
                hessians.test=hessians.test, # results for all iteration
                fit.mle.results=fit.mle.results, # results for MLEs
                fit.hessians=fit.hessians, # hessian for MLEs
                time_spent=time_spent, # time spent
                num.max.location=num.max.location, # the number of times hitting the MLEs
                est.counts=est.counts # estimated counts and 95% CI
    ))
    
  }
  # --- the end of covc model 
  
  # define the (phicov, pcov) RD model, covcov is an index ------
  
  covcov.RD.prediction<- function(covcov_phi,covcov_p){
    
    # covcov_phi and covcov_p indexes are parameters
    force(covcov_phi)
    force(covcov_p)
    
    data_all<- dataInput_RD()
    data1=data_all[,input$data_colnum_RD]
    
    
    # phi(cov), p(cov) with 5 params
    RD.phicov.pcov.logl <- function (param,data, T, D, k, missed, z1, z2){
      
      data[missed]=0
      
      sim.K=T  #  total no. of occasions
      sim.k= k # as.integer(exp( param(1) ) )   # no. of secondary occasisons (param)
      sim.T=sim.K/sim.k  # no. of primary occasions
      
      #preprocess z1
      z1_mat<- matrix(z1,ncol=sim.k,nrow=sim.T,byrow=TRUE)
      
      # param
      phi12<-expit(param[1]+param[2]*z1_mat[2:sim.T,1]); na.index<-which(is.na(phi12)); phi12[na.index]<-0
      phi21 <- 1-phi12; phi21[na.index]<-0
      pi<-c(mean(phi21),1-mean(phi21))
      
      p <- expit(param[3]+param[4]*z2); p[missed]<-10^{-10}
      n01<-exp(param[5]);N=n01+D
      
      
      phi <-  list(matrix(rep(c(0,1),2), ncol=2,byrow=TRUE)); phi <- rep(phi,(sim.T)-1)
      for (j in 1:((sim.T)-1)) {phi[[j]]<- matrix(c(1-phi12[j],phi12[j],phi21[j],1-phi21[j]), ncol=2,byrow=TRUE)}
      
      
      # prob of being removed, primary periods in rows (1,), secondary in cols (1,1)
      alpha<-matrix(NA,ncol=sim.k, nrow=sim.T)
      
      # prob of being in state 1 (observable) & not being removed
      beta1<-matrix(NA,ncol=sim.k, nrow=sim.T)
      
      # prob of being in state 2 (unobservable) & not being removed
      beta2<-matrix(NA,ncol=sim.k, nrow=sim.T)
      
      
      # jj=1, ii=1
      alpha[1,1]<- pi[1]*p[1]
      beta1[1,1]<- (alpha[1,1]/p[1])*(1-p[1]) # state 1 & not being removed 
      beta2[1,1]<- 1-(alpha[1,1]/p[1])                # state 2 & not being removed
      
      # jj=2:sim.k, ii=1
      
      for (jj in 2:sim.k){
        
        alpha[1,jj]<- (alpha[1,jj-1]/p[jj-1]*(1-p[jj-1])*p[jj])
        beta1[1,jj]<- (beta1[1,jj-1]*(1-p[jj]))
        beta2[1,jj]<- (beta2[1,jj-1])
      }
      
      
      #   #   #   #   #   #   #   #   #   from 2nd primary period
      for (ii in 2:sim.T) {
        
        phi11=phi[[ii-1]][1,1]; phi12=phi[[ii-1]][1,2];
        phi21=phi[[ii-1]][2,1]; phi22=phi[[ii-1]][2,2];       
        
        for (jj in 1:sim.k) {
          
          if (jj==1) {
            alpha[ii,1]<- (beta1[ii-1,sim.k]*phi11 + beta2[ii-1,sim.k]*phi21)*p[ii]
            beta1[ii,1]<- (alpha[ii,1]/p[ii])*(1-p[ii])  # state 1 & not being removed 
            beta2[ii,1]<-  beta1[ii-1,sim.k]*phi12 + beta2[ii-1,sim.k]*phi22    # state 2 & not being removed
          }
          else {
            alpha[ii,jj]<- (alpha[ii,jj-1]/p[jj-1]*(1-p[jj-1])*p[jj])
            beta1[ii,jj]<- beta1[ii,jj-1]*(1-p[ii])
            beta2[ii,jj]<- beta2[ii,jj-1]
          }
          
        }
      }
      
      
      L=numeric()
      
      o=1
      for (ii in 1:sim.T) {
        
        for (jj in 1:sim.k) {
          
          L[o]=alpha[ii,jj]
          o=o+1
        }}
      
      L=L[1:sim.K]  
      
      L[which(L==0)]<-10^(-10)
      L[missed]<-10^(-10)
      
      L0=1-sum(L[-missed])
      
      logL=lgamma(N+1)-sum(lgamma(data[-missed]+1))-lgamma(n01+1)+sum(data[-missed]*log(L[-missed])) + n01*log(L0)
      
      return(-logL)
    }
    
    
    # read estimated counts function
    est.RD.phicov.pcov.logl <- function (param, T, D, k, missed, z1, z2) {
      
      sim.K=T  #  total no. of occasions
      sim.k= k # as.integer(exp( param(1) ) )   # no. of secondary occasisons (param)
      sim.T=sim.K/sim.k  # no. of primary occasions
      
      #preprocess z1
      z1_mat<- matrix(z1,ncol=sim.k,nrow=sim.T,byrow=TRUE)
      
      # param
      phi12<-expit(param[1]+param[2]*z1_mat[2:sim.T,1]); na.index<-which(is.na(phi12)); phi12[na.index]<-0
      phi21 <- 1-phi12; phi21[na.index]<-0
      pi<-c(mean(phi21),1-mean(phi21))
      
      p <- expit(param[3]+param[4]*z2); p[missed]<-10^{-10}
      N<-param[5] # estiamted population size
      
      
      
      phi <-  list(matrix(rep(c(0,1),2), ncol=2,byrow=TRUE)); phi <- rep(phi,(sim.T)-1)
      for (j in 1:((sim.T)-1)) {phi[[j]]<- matrix(c(1-phi12[j],phi12[j],phi21[j],1-phi21[j]), ncol=2,byrow=TRUE)}
      
      
      # prob of being removed, primary periods in rows (1,), secondary in cols (1,1)
      alpha<-matrix(NA,ncol=sim.k, nrow=sim.T)
      
      # prob of being in state 1 (observable) & not being removed
      beta1<-matrix(NA,ncol=sim.k, nrow=sim.T)
      
      # prob of being in state 2 (unobservable) & not being removed
      beta2<-matrix(NA,ncol=sim.k, nrow=sim.T)
      
      
      # jj=1, ii=1
      alpha[1,1]<- pi[1]*p[1]
      beta1[1,1]<- (alpha[1,1]/p[1])*(1-p[1]) # state 1 & not being removed 
      beta2[1,1]<- 1-(alpha[1,1]/p[1])                # state 2 & not being removed
      
      # jj=2:sim.k, ii=1
      
      for (jj in 2:sim.k){
        
        alpha[1,jj]<- (alpha[1,jj-1]/p[jj-1]*(1-p[jj-1])*p[jj])
        beta1[1,jj]<- (beta1[1,jj-1]*(1-p[jj]))
        beta2[1,jj]<- (beta2[1,jj-1])
      }
      
      
      #   #   #   #   #   #   #   #   #   from 2nd primary period
      for (ii in 2:sim.T) {
        
        phi11=phi[[ii-1]][1,1]; phi12=phi[[ii-1]][1,2];
        phi21=phi[[ii-1]][2,1]; phi22=phi[[ii-1]][2,2];       
        
        for (jj in 1:sim.k) {
          
          if (jj==1) {
            alpha[ii,1]<- (beta1[ii-1,sim.k]*phi11 + beta2[ii-1,sim.k]*phi21)*p[ii]
            beta1[ii,1]<- (alpha[ii,1]/p[ii])*(1-p[ii])  # state 1 & not being removed 
            beta2[ii,1]<-  beta1[ii-1,sim.k]*phi12 + beta2[ii-1,sim.k]*phi22    # state 2 & not being removed
          }
          else {
            alpha[ii,jj]<- (alpha[ii,jj-1]/p[jj-1]*(1-p[jj-1])*p[jj])
            beta1[ii,jj]<- beta1[ii,jj-1]*(1-p[ii])
            beta2[ii,jj]<- beta2[ii,jj-1]
          }
          
        }
      }
      
      
      L=numeric()
      
      o=1
      for (ii in 1:sim.T) {
        
        for (jj in 1:sim.k) {
          
          L[o]=alpha[ii,jj]
          o=o+1
        }}
      
      L=L[1:sim.K]  
      
      L[missed]<-NA
      
      # predicted counts
      ept.data<-numeric()
      
      times<-c(1:sim.K)[-missed]
      
      for(i in times)
      {ept.data[i]<- N*L[i]
      }
      
      return(ept.data)
    }
    
    
    #*** choose one covariate for the model
    z_p     <- zz_RD()[,2,covcov_p+(covcov_phi+1)*no_covariate_available_RD()+1] 
    z_phi12 <- zz_RD()[,1,covcov_phi+no_covariate_available_RD()*(covcov_p-1)+no_covariate_available_RD()*2+1]
    # see settings in zz_RD() function
    
    
    # run (fit.phicov.pc.optim) function
    fit.phicov.pcov.optim<- function(param) {
      try(optim(par=param,
                fn=RD.phicov.pcov.logl,
                method="BFGS",
                #method="L-BFGS-B",
                #lower = c(rep(logit(10^{-5}),2),log(10^{-5})), 
                #upper = c(rep(logit(0.99999),2),log(10^{5})),
                hessian=TRUE,
                data=data1,
                T=T_RD(),
                D=D_RD(),
                k=no_k(),
                missed=miss_occ_RD(),
                z1=z_phi12,
                z2=z_p),silent=TRUE)}
    
    # make sure hessian is available
    inv <- function(m) {class(try(solve(m),silent=TRUE))=="matrix"} 
    
    # input iteration number
    n.rep= isolate(input$num_iteration_RD)
    
    # initial estiamte elements
    phi12.int.test<-phi12.slope.test <- p.int.test<-p.slope.test<-N0.test<-loglike.test<-numeric()
    hessians.test <- list(matrix(rep(0,5*5), ncol=5)) # 4 is the length(param)
    hessians.test <- rep(hessians.test,n.rep)
    
    #phi12.test<-matrix(NA,ncol=T_RD()/no_k()-1,nrow=n.rep)
    
    # update this for all models
    time_spent=numeric()
    
    # initialise iteration numbers
    j=1
    k=1
    
    # Start the clock for optimisation =-=-=-=-
    ptm <- proc.time()
    
    # run iterations
    for (j in 1:n.rep) { 
      
      # initial starting param
      param=c(runif(4,-1,1), #phi12.int, phi12.slope, p.int and p.slope
              log(runif(1,1,50)) ) # N0
      
      # run the model
      fit <- fit.phicov.pcov.optim(param=param)
      
      
      if (inv(fit$hessian)==TRUE) { # check if hessian is available
        if (k<(n.rep+1)) {
          
          # record estimates for each iteration
          phi12.int.test[k] <- fit$par[1]
          phi12.slope.test[k] <- fit$par[2]
          p.int.test[k] <- fit$par[3]
          p.slope.test[k] <- fit$par[3]
          N0.test[k] <- exp(fit$par[4])
          
          # record hessians for each iteration
          hessians.test[[k]]=fit$hessian
          
          # record loglike for each iteration
          # these will be double checked by users
          loglike.test[k]<- -fit$value
          
          k=k+1      
        } }
      #j=j+1
      
      # the end of iterations
    }
    
    
    # Stop the clock =-=-=-=-
    t.record<-proc.time() - ptm
    
    # Time spent
    time_spent <- as.numeric(names(table(t.record[1])))
    
    # put together results for all iterations
    AIC.test <- - 2*loglike.test + (2*length(param))
    
    mle.test<-      data.frame(phi12_int=phi12.int.test,
                               phi12_slope=phi12.slope.test,
                               p_int=p.int.test,
                               p_slope=p.slope.test,
                               N0=N0.test,
                               loglike=loglike.test,
                               AIC= AIC.test
    )
    
    # observe the number of times hitting the MLEs
    loglike.test<-signif(loglike.test, 7)
    num.max.location <- length(which(loglike.test==max(loglike.test)))
    
    
    # observe which iteration has MLEs
    max.location <-which(loglike.test==max(loglike.test))[1]
    
    # record MLEs corresponding to the max.location.2cov
    fit.phi12.int <- phi12.int.test[max.location]
    fit.phi12.slope <- phi12.slope.test[max.location]
    fit.p.int <- p.int.test[max.location]
    fit.p.slope <- p.slope.test[max.location]
    fit.N <- N0.test[max.location]+ D_RD() # + RD observed data
    
    # record the hessian corresponding to the max.location.2cov
    fit.hessians <- hessians.test[[max.location]]
    fit.loglike <- max(loglike.test)
    fit.AIC<- - 2*max(loglike.test) + (2*length(param))
    
    # *** SE and 95% CI
    
    SE <- sqrt(diag(solve(hessians.test[[max.location]])))
    
    # phi12.int
    SE.phi12.int <- SE[1]
    lowerCI.phi12.int <- fit.phi12.int-1.96*SE.phi12.int
    upperCI.phi12.int <- fit.phi12.int+1.96*SE.phi12.int
    
    # phi12.slope
    SE.phi12.slope <- SE[2]
    lowerCI.phi12.slope <- fit.phi12.slope-1.96*SE.phi12.slope
    upperCI.phi12.slope <- fit.phi12.slope+1.96*SE.phi12.slope
    
    # p.int
    SE.p.int <- SE[3]
    lowerCI.p.int <- fit.p.int-1.96*SE.p.int
    upperCI.p.int <- fit.p.int+1.96*SE.p.int
    
    # p.slope
    SE.p.slope <- SE[4]
    lowerCI.p.slope <- fit.p.slope-1.96*SE.p.slope
    upperCI.p.slope <- fit.p.slope+1.96*SE.p.slope
    
    # ******** N
    SE.N <- exp(SE[5])
    lowerCI.N <- fit.N -1.96*SE.N
    upperCI.N <- fit.N +1.96*SE.N
    
    
    # ***put together MLE results as well as loglike and AIC
    # first row shows fit.phi12, fit.p, fit.N, fit.loglike and AIC
    # second row shows SE of fit.phi12, fit.p, fit.N and NA, NA
    # third row shows lower 95% CI of fit.phi12, fit.p, fit.N and NA, NA
    # forth row shows upper 95% CI of fit.phi12, fit.p, fit.N and NA, NA
    
    fit.mle.results    <-  data.frame(phi12_int=c(fit.phi12.int,
                                                  SE.phi12.int,
                                                  lowerCI.phi12.int,
                                                  upperCI.phi12.int),
                                      phi12_slope=c(fit.phi12.slope,
                                                    SE.phi12.slope,
                                                    lowerCI.phi12.slope,
                                                    upperCI.phi12.slope),
                                      p_int=c(fit.p.int,
                                          SE.p.int,
                                          lowerCI.p.int,
                                          upperCI.p.int),
                                      p_slope=c(fit.p.slope,
                                          SE.p.slope,
                                          lowerCI.p.slope,
                                          upperCI.p.slope),
                                      N=c(fit.N,
                                          SE.N,
                                          lowerCI.N,
                                          upperCI.N),
                                      loglike=c(fit.loglike , NA,NA,NA),
                                      AIC=c(fit.AIC , NA,NA,NA)
    )
    
    # *** estimated counts of individuals and 95% CI
    
    est.numbers<- est.RD.phicov.pcov.logl(param=c(fit.phi12.int,
                                              fit.phi12.slope,
                                              fit.p.int,
                                              fit.p.slope,
                                              fit.N), 
                                      T=T_RD(), 
                                      D=D_RD(), 
                                      k=no_k(), 
                                      missed=miss_occ_RD(),
                                      z1=z_phi12,
                                      z2=z_p
    )
    
    lowerCI.numbers<- est.RD.phicov.pcov.logl(param=c(lowerCI.phi12.int,
                                                  lowerCI.phi12.slope,
                                                  lowerCI.p.int,
                                                  lowerCI.p.slope,
                                                  lowerCI.N),
                                          T=T_RD(), 
                                          D=D_RD(), 
                                          k=no_k(), 
                                          missed=miss_occ_RD(),
                                          z1=z_phi12,
                                          z2=z_p
    )
    
    upperCI.numbers<- est.RD.phicov.pcov.logl(param=c(upperCI.phi12.int,
                                                  upperCI.phi12.slope,
                                                  upperCI.p.int,
                                                  upperCI.p.slope,
                                                  upperCI.N), 
                                          T=T_RD(), 
                                          D=D_RD(), 
                                          k=no_k(), 
                                          missed=miss_occ_RD(),
                                          z1=z_phi12,
                                          z2=z_p
    )
    
    est.counts<-data.frame(est.num=est.numbers,
                           LCI=lowerCI.numbers,
                           UCI=upperCI.numbers)
    
    ## --- the end of (phicov, pcov) RD model
    
    
    
    return(list(mle.test=mle.test, # results for all iteration
                hessians.test=hessians.test, # results for all iteration
                fit.mle.results=fit.mle.results, # results for MLEs
                fit.hessians=fit.hessians, # hessian for MLEs
                time_spent=time_spent, # time spent
                num.max.location=num.max.location, # the number of times hitting the MLEs
                est.counts=est.counts # estimated counts and 95% CI
    ))
    
  }
  # --- the end of covcov model 
  
  
  # define the (phit,pc) RD model, cc is an index ------
  
  tc.RD.prediction<- function(tc=1){
    
    # kk index is an parameter
    force(tc)
    
    data_all<- dataInput_RD()
    data1=data_all[,input$data_colnum_RD]
    
    
    # phi(t), p(c) with K/k+1 params
    RD.phit.pc.logl <- function (param,data, T, D, k,missed){
      
      data[missed]=0
      
      sim.K=T  #  total no. of occasions
      sim.k= k # as.integer(exp( param(1) ) )   # no. of secondary occasisons (param)
      sim.T=sim.K/sim.k  # no. of primary occasions
      
      # param
      phi12<-expit(param[1:((sim.T)-1)]); phi21 <- 1-phi12; pi<-c(mean(phi21),1-mean(phi21))
      p <- expit(param[(sim.T)]); p<-rep(p,sim.K)
      n01<-exp(param[(sim.T)+1]);N=n01+D
      
      
      phi <-  list(matrix(rep(c(0,1),2), ncol=2,byrow=TRUE)); phi <- rep(phi,(sim.T)-1)
      for (j in 1:((sim.T)-1)) {phi[[j]]<- matrix(c(1-phi12[j],phi12[j],phi21[j],1-phi21[j]), ncol=2,byrow=TRUE)}
      
      
      # prob of being removed, primary periods in rows (1,), secondary in cols (1,1)
      alpha<-matrix(NA,ncol=sim.K, nrow=sim.K)
      
      # prob of being in state 1 (observable) & not being removed
      beta1<-matrix(NA,ncol=sim.K, nrow=sim.K)
      
      # prob of being in state 2 (unobservable) & not being removed
      beta2<-matrix(NA,ncol=sim.K, nrow=sim.K)
      
      
      # jj=1, ii=1
      alpha[1,1]<- pi[1]*p[1]
      beta1[1,1]<- (alpha[1,1]/p[1])*(1-p[1]) # state 1 & not being removed 
      beta2[1,1]<- 1-(alpha[1,1]/p[1])                # state 2 & not being removed
      
      # jj=2:sim.k, ii=1
      
      for (jj in 2:sim.k){
        
        alpha[1,jj]<- (alpha[1,jj-1]/p[jj-1]*(1-p[jj-1])*p[jj])
        beta1[1,jj]<- (beta1[1,jj-1]*(1-p[jj]))
        beta2[1,jj]<- (beta2[1,jj-1])
      }
      
      
      #   #   #   #   #   #   #   #   #   from 2nd primary period
      for (ii in 2:sim.T) {
        
        phi11=phi[[ii-1]][1,1]; phi12=phi[[ii-1]][1,2];
        phi21=phi[[ii-1]][2,1]; phi22=phi[[ii-1]][2,2];       
        
        for (jj in 1:sim.k) {
          
          if (jj==1) {
            alpha[ii,1]<- (beta1[ii-1,sim.k]*phi11 + beta2[ii-1,sim.k]*phi21)*p[ii]
            beta1[ii,1]<- (alpha[ii,1]/p[ii])*(1-p[ii])  # state 1 & not being removed 
            beta2[ii,1]<-  beta1[ii-1,sim.k]*phi12 + beta2[ii-1,sim.k]*phi22    # state 2 & not being removed
          }
          else {
            alpha[ii,jj]<- (alpha[ii,jj-1]/p[jj-1]*(1-p[jj-1])*p[jj])
            beta1[ii,jj]<- beta1[ii,jj-1]*(1-p[ii])
            beta2[ii,jj]<- beta2[ii,jj-1]
          }
          
        }
      }
      
      L0=1-sum(alpha[1:sim.T,1:sim.k])
      
      L=numeric()
      
      o=1
      for (ii in 1:sim.T) {
        
        for (jj in 1:sim.k) {
          
          L[o]=alpha[ii,jj]
          o=o+1
        }}
      
      L=L[1:sim.K]  
      
      L[which(L==0)]<-10^(-10)
      L[missed]<-10^(-10)
      
      logL=lgamma(N+1)-sum(lgamma(data[-missed]+1))-lgamma(n01+1)+sum(data[-missed]*log(L[-missed])) + n01*log(L0)
      
      return(-logL)
    }
    
    
    # read estimated counts function
    est.RD.phit.pc.logl <- function (param, T, D, k, missed) {
      
      sim.K=T  #  total no. of occasions
      sim.k= k # as.integer(exp( param(1) ) )   # no. of secondary occasisons (param)
      sim.T=sim.K/sim.k  # no. of primary occasions
      
      # param
      phi12<-param[1:sim.T-1];  phi21 <- 1-phi12; pi<-c(mean(phi21),1-mean(phi21))
      p <- param[sim.T]; p<-rep(p,sim.K)
      N<-param[(sim.T)+1] # estiamted population size
      
      phi <-  list(matrix(rep(c(0,1),2), ncol=2,byrow=TRUE)); phi <- rep(phi,(sim.T)-1)
      for (j in 1:((sim.T)-1)) {phi[[j]]<- matrix(c(1-phi12[j],phi12[j],phi21[j],1-phi21[j]), ncol=2,byrow=TRUE)}
      
      
      # prob of being removed, primary periods in rows (1,), secondary in cols (1,1)
      alpha<-matrix(NA,ncol=sim.K, nrow=sim.K)
      
      # prob of being in state 1 (observable) & not being removed
      beta1<-matrix(NA,ncol=sim.K, nrow=sim.K)
      
      # prob of being in state 2 (unobservable) & not being removed
      beta2<-matrix(NA,ncol=sim.K, nrow=sim.K)
      
      
      # jj=1, ii=1
      alpha[1,1]<- pi[1]*p[1]
      beta1[1,1]<- (alpha[1,1]/p[1])*(1-p[1]) # state 1 & not being removed 
      beta2[1,1]<- 1-(alpha[1,1]/p[1])                # state 2 & not being removed
      
      # jj=2:sim.k, ii=1
      
      for (jj in 2:sim.k){
        
        alpha[1,jj]<- (alpha[1,jj-1]/p[jj-1]*(1-p[jj-1])*p[jj])
        beta1[1,jj]<- (beta1[1,jj-1]*(1-p[jj]))
        beta2[1,jj]<- (beta2[1,jj-1])
      }
      
      
      #   #   #   #   #   #   #   #   #   from 2nd primary period
      for (ii in 2:sim.T) {
        
        phi11=phi[[ii-1]][1,1]; phi12=phi[[ii-1]][1,2];
        phi21=phi[[ii-1]][2,1]; phi22=phi[[ii-1]][2,2];       
        
        for (jj in 1:sim.k) {
          
          if (jj==1) {
            alpha[ii,1]<- (beta1[ii-1,sim.k]*phi11 + beta2[ii-1,sim.k]*phi21)*p[ii]
            beta1[ii,1]<- (alpha[ii,1]/p[ii])*(1-p[ii])  # state 1 & not being removed 
            beta2[ii,1]<-  beta1[ii-1,sim.k]*phi12 + beta2[ii-1,sim.k]*phi22    # state 2 & not being removed
          }
          else {
            alpha[ii,jj]<- (alpha[ii,jj-1]/p[jj-1]*(1-p[jj-1])*p[jj])
            beta1[ii,jj]<- beta1[ii,jj-1]*(1-p[ii])
            beta2[ii,jj]<- beta2[ii,jj-1]
          }
          
        }
      }
      
      
      L=numeric()
      
      o=1
      for (ii in 1:sim.T) {
        
        for (jj in 1:sim.k) {
          
          L[o]=alpha[ii,jj]
          o=o+1
        }}
      
      L=L[1:sim.K]  
      
      L[missed]<-NA
      
      # predicted counts
      ept.data<-numeric()
      
      times<-c(1:sim.K)[-missed]
      
      for(i in times)
      {ept.data[i]<- N*L[i]
      }
      
      return(ept.data)
    }
    
    
    # run (fit.phit.pc.optim) function
    fit.phit.pc.optim<- function(param) {
      try(optim(par=param,
                fn=RD.phit.pc.logl,
                method="BFGS",
                #method="L-BFGS-B",
                #lower = c(rep(logit(10^{-5}),2),log(10^{-5})), 
                #upper = c(rep(logit(0.99999),2),log(10^{5})),
                hessian=TRUE,
                data=data1,
                T=T_RD(),
                D=D_RD(),
                k=no_k(),
                missed=miss_occ_RD()),silent=TRUE)}
    
    # make sure hessian is available
    inv <- function(m) {class(try(solve(m),silent=TRUE))=="matrix"} 
    
    # input iteration number
    n.rep= isolate(input$num_iteration_RD)
    
    # initial estiamte elements
    p.test<-N0.test<-loglike.test<-numeric()
    phi12.test<-matrix(NA,ncol=T_RD()/no_k()-1,nrow=n.rep)
    
    hessians.test <- list(matrix(rep(0,(T_RD()/no_k()+1)*(T_RD()/no_k()+1)), ncol=(T_RD()/no_k()+1))) # 3 is the length(param)
    hessians.test <- rep(hessians.test,n.rep)

    
    # update this for all models
    time_spent=numeric()
    
    # initialise iteration numbers
    j=1
    k=1
    
    # Start the clock for optimisation =-=-=-=-
    ptm <- proc.time()
    
    # run iterations
    for (j in 1:n.rep) { 
      
      # initial starting param
      param=c(logit(runif(T_RD()/no_k()-1,0.1,0.9)), #phi12
              logit(runif(1,0.1,0.9)),# p
              log(runif(1,10,200)) ) # N0
      
      # run the model
      fit <- fit.phit.pc.optim(param=param)
      
      
      if (inv(fit$hessian)==TRUE) { # check if hessian is available
        if (k<(n.rep+1)) {
          
          # record estimates for each iteration
          phi12.test[k,] <- expit(fit$par[T_RD()/no_k()-1])
          p.test[k] <- expit(fit$par[T_RD()/no_k()])
          N0.test[k] <- exp(fit$par[T_RD()/no_k()+1])
          
          # record hessians for each iteration
          hessians.test[[k]]=fit$hessian
          
          # record loglike for each iteration
          # these will be double checked by users
          loglike.test[k]<- -fit$value
          
          k=k+1      
        } }
      # j=j+1
      
      # the end of iterations
    }
    
    
    # Stop the clock =-=-=-=-
    t.record<-proc.time() - ptm
    
    # Time spent
    time_spent <- as.numeric(names(table(t.record[1])))
    
    # put together results for all iterations
    AIC.test <- - 2*loglike.test + (2*length(param))
    
    mle.test<-      data.frame(p=p.test,
                               N0=N0.test,
                               loglike=loglike.test,
                               AIC= AIC.test
    )
    
    # observe the number of times hitting the MLEs
    loglike.test<-signif(loglike.test, 7)
    num.max.location <- length(which(loglike.test==max(loglike.test)))
    
    
    # observe which iteration has MLEs
    max.location <-which(loglike.test==max(loglike.test))[1]
    
    # record MLEs corresponding to the max.location.2cov
    fit.phi12 <- phi12.test[max.location,]
    fit.p <- p.test[max.location]
    fit.N <- N0.test[max.location]+ D_RD() # + RD observed data
    
    # record the hessian corresponding to the max.location.2cov
    fit.hessians <- hessians.test[[max.location]]
    fit.loglike <- max(loglike.test)
    fit.AIC<- - 2*max(loglike.test) + (2*length(param))
    
    # *** SE and 95% CI
    
    SE <- sqrt(diag(solve(hessians.test[[max.location]])))
    
    # phi12
    SE.phi12 <- SE[T_RD()/no_k()-1]*fit.phi12*(1-fit.phi12)
    lowerCI.phi12 <- fit.phi12-1.96*SE.phi12
    upperCI.phi12 <- fit.phi12+1.96*SE.phi12
    
    # p
    SE.p <- SE[T_RD()/no_k()]*fit.p*(1-fit.p)
    lowerCI.p <- fit.p-1.96*SE.p
    upperCI.p <- fit.p+1.96*SE.p
    
    # ******** N
    SE.N <- exp(SE[T_RD()/no_k()+1])
    lowerCI.N <- fit.N -1.96*SE.N
    upperCI.N <- fit.N +1.96*SE.N
    
    
    # ***put together MLE results as well as loglike and AIC
    
    
    fit.mle.results.phi12<- c(fit.phi12[1],SE.phi12[1],lowerCI.phi12[1],upperCI.phi12[1])
    
    for (jj in 2:(T_RD()/no_k()-1)) {
      fit.mle.results.phi12<- cbind(fit.mle.results.phi12, 
                                    c(fit.phi12[jj],SE.phi12[jj],lowerCI.phi12[jj],upperCI.phi12[jj])
                                    )
    }
    
      
    fit.mle.results.p.N.logl.aic<-  data.frame(p=c(fit.p,SE.p,lowerCI.p,upperCI.p),
                                      N=c(fit.N,SE.N,lowerCI.N,upperCI.N),
                                      loglike=c(fit.loglike , NA,NA,NA),
                                      AIC=c(fit.AIC , NA,NA,NA)
    )
    
    
    # *** estimated counts of individuals and 95% CI
    
    est.numbers<- est.RD.phit.pc.logl(param=c(fit.phi12,
                                              fit.p,
                                              fit.N), 
                                      T=T_RD(), 
                                      D=D_RD(), 
                                      k=no_k(), 
                                      missed=miss_occ_RD()
    )
    
    lowerCI.numbers<- est.RD.phit.pc.logl(param=c(lowerCI.phi12,lowerCI.p,lowerCI.N),
                                          T=T_RD(), 
                                          D=D_RD(), 
                                          k=no_k(), 
                                          missed=miss_occ_RD()
    )
    
    upperCI.numbers<- est.RD.phit.pc.logl(param=c(upperCI.phi12,upperCI.p,upperCI.N), 
                                          T=T_RD(), 
                                          D=D_RD(), 
                                          k=no_k(), 
                                          missed=miss_occ_RD()
    )
    
    est.counts<-data.frame(est.num=est.numbers,
                           LCI=lowerCI.numbers,
                           UCI=upperCI.numbers)
    
    ## --- the end of (phic, pc) RD model
    
    
    
    return(list(mle.test=mle.test, # results for all iteration
                hessians.test=hessians.test, # results for all iteration
                fit.mle.results.phi12=fit.mle.results.phi12, # results for phi12 MLEs
                fit.mle.results.p.N.logl.aic=fit.mle.results.p.N.logl.aic,
                fit.hessians=fit.hessians, # hessian for MLEs
                time_spent=time_spent, # time spent
                num.max.location=num.max.location, # the number of times hitting the MLEs
                est.counts=est.counts # estimated counts and 95% CI
    ))
    
  }
  # --- the end of tc model 
  
  
  # define the (phit,pcov) RD model, cc is an index ------
  
  tcov.RD.prediction<- function(tcov){
    
    # kk index is an parameter
    force(tcov)
    
    data_all<- dataInput_RD()
    data1=data_all[,input$data_colnum_RD]
    
    
    # phi(t), p(cov) with K+2 params
    RD.phit.pcov.logl <- function (param,data, T, D, k,missed, z){
      
      data[missed]=0
      
      sim.K=T  #  total no. of occasions
      sim.k= k # as.integer(exp( param(1) ) )   # no. of secondary occasisons (param)
      sim.T=sim.K/sim.k  # no. of primary occasions
      
      # param
      phi12<-expit(param[1:((sim.T)-1)]); phi21 <- 1-phi12; pi<-c(mean(phi21),1-mean(phi21))
      p <- expit(param[sim.T]+param[(sim.T)+1]*z); p[missed]<-10^{-10}
      n01<-exp(param[(sim.T)+2]);N=n01+D
      
      
      phi <-  list(matrix(rep(c(0,1),2), ncol=2,byrow=TRUE)); phi <- rep(phi,(sim.T)-1)
      for (j in 1:((sim.T)-1)) {phi[[j]]<- matrix(c(1-phi12[j],phi12[j],phi21[j],1-phi21[j]), ncol=2,byrow=TRUE)}
      
      
      # prob of being removed, primary periods in rows (1,), secondary in cols (1,1)
      alpha<-matrix(NA,ncol=sim.K, nrow=sim.K)
      
      # prob of being in state 1 (observable) & not being removed
      beta1<-matrix(NA,ncol=sim.K, nrow=sim.K)
      
      # prob of being in state 2 (unobservable) & not being removed
      beta2<-matrix(NA,ncol=sim.K, nrow=sim.K)
      
      
      # jj=1, ii=1
      alpha[1,1]<- pi[1]*p[1]
      beta1[1,1]<- (alpha[1,1]/p[1])*(1-p[1]) # state 1 & not being removed 
      beta2[1,1]<- 1-(alpha[1,1]/p[1])                # state 2 & not being removed
      
      # jj=2:sim.k, ii=1
      
      for (jj in 2:sim.k){
        
        alpha[1,jj]<- (alpha[1,jj-1]/p[jj-1]*(1-p[jj-1])*p[jj])
        beta1[1,jj]<- (beta1[1,jj-1]*(1-p[jj]))
        beta2[1,jj]<- (beta2[1,jj-1])
      }
      
      
      #   #   #   #   #   #   #   #   #   from 2nd primary period
      for (ii in 2:sim.T) {
        
        phi11=phi[[ii-1]][1,1]; phi12=phi[[ii-1]][1,2];
        phi21=phi[[ii-1]][2,1]; phi22=phi[[ii-1]][2,2];       
        
        for (jj in 1:sim.k) {
          
          if (jj==1) {
            alpha[ii,1]<- (beta1[ii-1,sim.k]*phi11 + beta2[ii-1,sim.k]*phi21)*p[ii]
            beta1[ii,1]<- (alpha[ii,1]/p[ii])*(1-p[ii])  # state 1 & not being removed 
            beta2[ii,1]<-  beta1[ii-1,sim.k]*phi12 + beta2[ii-1,sim.k]*phi22    # state 2 & not being removed
          }
          else {
            alpha[ii,jj]<- (alpha[ii,jj-1]/p[jj-1]*(1-p[jj-1])*p[jj])
            beta1[ii,jj]<- beta1[ii,jj-1]*(1-p[ii])
            beta2[ii,jj]<- beta2[ii,jj-1]
          }
          
        }
      }
      
      
      L=numeric()
      
      o=1
      for (ii in 1:sim.T) {
        
        for (jj in 1:sim.k) {
          
          L[o]=alpha[ii,jj]
          o=o+1
        }}
      
      L=L[1:sim.K]  
      
      L[which(L==0)]<-10^(-10)
      L[missed]<-10^(-10)
      L0<- 1- sum(L[-missed])
      
      logL=lgamma(N+1)-sum(lgamma(data[-missed]+1))-lgamma(n01+1)+sum(data[-missed]*log(L[-missed])) + n01*log(L0)
      
      return(-logL)
    }
    
    
    # read estimated counts function
    est.RD.phit.pcov.logl <- function (param, T, D, k, missed, z) {
      
      sim.K=T  #  total no. of occasions
      sim.k= k # as.integer(exp( param(1) ) )   # no. of secondary occasisons (param)
      sim.T=sim.K/sim.k  # no. of primary occasions
      
      # param
      phi12<-param[1:sim.T-1];  phi21 <- 1-phi12; pi<-c(mean(phi21),1-mean(phi21))
      p <- expit(param[sim.T]+param[(sim.T)+1]*z); p[missed]<-10^{-10}
      N<- param[(sim.T)+2] # estiamted population size
      
      phi <-  list(matrix(rep(c(0,1),2), ncol=2,byrow=TRUE)); phi <- rep(phi,(sim.T)-1)
      for (j in 1:((sim.T)-1)) {phi[[j]]<- matrix(c(1-phi12[j],phi12[j],phi21[j],1-phi21[j]), ncol=2,byrow=TRUE)}
      
      
      # prob of being removed, primary periods in rows (1,), secondary in cols (1,1)
      alpha<-matrix(NA,ncol=sim.K, nrow=sim.K)
      
      # prob of being in state 1 (observable) & not being removed
      beta1<-matrix(NA,ncol=sim.K, nrow=sim.K)
      
      # prob of being in state 2 (unobservable) & not being removed
      beta2<-matrix(NA,ncol=sim.K, nrow=sim.K)
      
      
      # jj=1, ii=1
      alpha[1,1]<- pi[1]*p[1]
      beta1[1,1]<- (alpha[1,1]/p[1])*(1-p[1]) # state 1 & not being removed 
      beta2[1,1]<- 1-(alpha[1,1]/p[1])                # state 2 & not being removed
      
      # jj=2:sim.k, ii=1
      
      for (jj in 2:sim.k){
        
        alpha[1,jj]<- (alpha[1,jj-1]/p[jj-1]*(1-p[jj-1])*p[jj])
        beta1[1,jj]<- (beta1[1,jj-1]*(1-p[jj]))
        beta2[1,jj]<- (beta2[1,jj-1])
      }
      
      
      #   #   #   #   #   #   #   #   #   from 2nd primary period
      for (ii in 2:sim.T) {
        
        phi11=phi[[ii-1]][1,1]; phi12=phi[[ii-1]][1,2];
        phi21=phi[[ii-1]][2,1]; phi22=phi[[ii-1]][2,2];       
        
        for (jj in 1:sim.k) {
          
          if (jj==1) {
            alpha[ii,1]<- (beta1[ii-1,sim.k]*phi11 + beta2[ii-1,sim.k]*phi21)*p[ii]
            beta1[ii,1]<- (alpha[ii,1]/p[ii])*(1-p[ii])  # state 1 & not being removed 
            beta2[ii,1]<-  beta1[ii-1,sim.k]*phi12 + beta2[ii-1,sim.k]*phi22    # state 2 & not being removed
          }
          else {
            alpha[ii,jj]<- (alpha[ii,jj-1]/p[jj-1]*(1-p[jj-1])*p[jj])
            beta1[ii,jj]<- beta1[ii,jj-1]*(1-p[ii])
            beta2[ii,jj]<- beta2[ii,jj-1]
          }
          
        }
      }
      
      
      L=numeric()
      
      o=1
      for (ii in 1:sim.T) {
        
        for (jj in 1:sim.k) {
          
          L[o]=alpha[ii,jj]
          o=o+1
        }}
      
      L=L[1:sim.K]  
      
      L[missed]<-NA
      
      # predicted counts
      ept.data<-numeric()
      
      times<-c(1:sim.K)[-missed]
      
      for(i in times)
      {ept.data[i]<- N*L[i]
      }
      
      return(ept.data)
    }
    
    
    #*** choose one covariate for the model
    z <- zz_RD()[,2,tcov+2] 
    
    # run (fit.phit.pc.optim) function
    fit.phit.pcov.optim<- function(param) {
      try(optim(par=param,
                fn=RD.phit.pcov.logl,
                method="BFGS",
                #method="L-BFGS-B",
                #lower = c(rep(logit(10^{-5}),2),log(10^{-5})), 
                #upper = c(rep(logit(0.99999),2),log(10^{5})),
                hessian=TRUE,
                data=data1,
                T=T_RD(),
                D=D_RD(),
                k=no_k(),
                missed=miss_occ_RD(),
                z=z),
          silent=TRUE)}
    
    # make sure hessian is available
    inv <- function(m) {class(try(solve(m),silent=TRUE))=="matrix"} 
    
    # input iteration number
    n.rep= isolate(input$num_iteration_RD)
    
    # initial estiamte elements
    p.int.test<-p.slope.test<-N0.test<-loglike.test<-numeric()
    phi12.test<-matrix(NA,ncol=T_RD()/no_k()-1,nrow=n.rep)
    
    hessians.test <- list(matrix(rep(0,(T_RD()/no_k()+1)*(T_RD()/no_k()+1)), ncol=(T_RD()/no_k()+1))) # 3 is the length(param)
    hessians.test <- rep(hessians.test,n.rep)
    
    
    # update this for all models
    time_spent=numeric()
    
    # initialise iteration numbers
    j=1
    k=1
    
    # Start the clock for optimisation =-=-=-=-
    ptm <- proc.time()
    
    # run iterations
    for (j in 1:n.rep) { 
      
      # initial starting param
      param=c(logit(runif(T_RD()/no_k()-1,0.1,0.9)), #phi12
              runif(2,-1,1),# p.int, p.slope
              log(runif(1,10,200)) ) # N0
      
      # run the model
      fit <- fit.phit.pcov.optim(param=param)
      
      
      if (inv(fit$hessian)==TRUE) { # check if hessian is available
        if (k<(n.rep+1)) {
          
          # record estimates for each iteration
          phi12.test[k,] <- expit(fit$par[T_RD()/no_k()-1])
          p.int.test[k] <- fit$par[T_RD()/no_k()]
          p.slope.test[k] <- fit$par[T_RD()/no_k()+1]
          N0.test[k] <- exp(fit$par[T_RD()/no_k()+2])
          
          # record hessians for each iteration
          hessians.test[[k]]=fit$hessian
          
          # record loglike for each iteration
          # these will be double checked by users
          loglike.test[k]<- -fit$value
          
          k=k+1      
        } }
      # j=j+1
      
      # the end of iterations
    }
    
    
    # Stop the clock =-=-=-=-
    t.record<-proc.time() - ptm
    
    # Time spent
    time_spent <- as.numeric(names(table(t.record[1])))
    
    # put together results for all iterations
    AIC.test <- - 2*loglike.test + (2*length(param))
    
    mle.test<-      data.frame(p_int=p.int.test,
                               p_slope=p.slope.test,
                               N0=N0.test,
                               loglike=loglike.test,
                               AIC= AIC.test
    )
    
    # observe the number of times hitting the MLEs
    loglike.test<-signif(loglike.test, 7)
    num.max.location <- length(which(loglike.test==max(loglike.test)))
    
    
    # observe which iteration has MLEs
    max.location <-which(loglike.test==max(loglike.test))[1]
    
    # record MLEs corresponding to the max.location.2cov
    fit.phi12 <- phi12.test[max.location,]
    fit.p.int <- p.int.test[max.location]
    fit.p.slope <- p.slope.test[max.location]
    fit.N <- N0.test[max.location]+ D_RD() # + RD observed data
    
    # record the hessian corresponding to the max.location.2cov
    fit.hessians <- hessians.test[[max.location]]
    fit.loglike <- max(loglike.test)
    fit.AIC<- - 2*max(loglike.test) + (2*length(param))
    
    # *** SE and 95% CI
    
    SE <- sqrt(diag(solve(hessians.test[[max.location]])))
    
    # phi12
    SE.phi12 <- SE[T_RD()/no_k()-1]*fit.phi12*(1-fit.phi12)
    lowerCI.phi12 <- fit.phi12-1.96*SE.phi12
    upperCI.phi12 <- fit.phi12+1.96*SE.phi12
    
    # p.int
    SE.p.int <- SE[T_RD()/no_k()]
    lowerCI.p.int <- fit.p.int-1.96*SE.p.int
    upperCI.p.int <- fit.p.int+1.96*SE.p.int
    
    # p.slope
    SE.p.slope <- SE[T_RD()/no_k()+1]
    lowerCI.p.slope <- fit.p.slope-1.96*SE.p.slope
    upperCI.p.slope <- fit.p.slope+1.96*SE.p.slope
    
    # ******** N
    SE.N <- exp(SE[T_RD()/no_k()+2])
    lowerCI.N <- fit.N -1.96*SE.N
    upperCI.N <- fit.N +1.96*SE.N
    
    
    # ***put together MLE results as well as loglike and AIC
    
    
    fit.mle.results.phi12<- c(fit.phi12[1],SE.phi12[1],lowerCI.phi12[1],upperCI.phi12[1])
    
    for (jj in 2:(T_RD()/no_k()-1)) {
      fit.mle.results.phi12<- cbind(fit.mle.results.phi12, 
                                    c(fit.phi12[jj],SE.phi12[jj],lowerCI.phi12[jj],upperCI.phi12[jj])
      )
    }
    
    
    fit.mle.results.p.N.logl.aic<-  data.frame(p_int=c(fit.p.int,SE.p.int,lowerCI.p.int,upperCI.p.int),
                                               p_slope=c(fit.p.int,SE.p.slope,lowerCI.p.slope,upperCI.p.slope),
                                               N=c(fit.N,SE.N,lowerCI.N,upperCI.N),
                                               loglike=c(fit.loglike , NA,NA,NA),
                                               AIC=c(fit.AIC , NA,NA,NA)
    )
    
    
    # *** estimated counts of individuals and 95% CI
    
    est.numbers<- est.RD.phit.pcov.logl(param=c(fit.phi12,
                                              fit.p.int,
                                              fit.p.slope,
                                              fit.N), 
                                      T=T_RD(), 
                                      D=D_RD(), 
                                      k=no_k(), 
                                      missed=miss_occ_RD(),
                                      z=z
    )
    
    lowerCI.numbers<- est.RD.phit.pcov.logl(param=c(lowerCI.phi12,
                                                    lowerCI.p.int,
                                                    lowerCI.p.slope,
                                                    lowerCI.N),
                                          T=T_RD(), 
                                          D=D_RD(), 
                                          k=no_k(), 
                                          missed=miss_occ_RD(),
                                          z=z
    )
    
    upperCI.numbers<- est.RD.phit.pcov.logl(param=c(upperCI.phi12,
                                                    upperCI.p.int,
                                                    upperCI.p.slope,
                                                    upperCI.N), 
                                          T=T_RD(), 
                                          D=D_RD(), 
                                          k=no_k(), 
                                          missed=miss_occ_RD(),
                                          z=z
    )
    
    est.counts<-data.frame(est.num=est.numbers,
                           LCI=lowerCI.numbers,
                           UCI=upperCI.numbers)
    
    ## --- the end of (phic, pc) RD model
    
    
    
    return(list(mle.test=mle.test, # results for all iteration
                hessians.test=hessians.test, # results for all iteration
                fit.mle.results.phi12=fit.mle.results.phi12, # results for phi12 MLEs
                fit.mle.results.p.N.logl.aic=fit.mle.results.p.N.logl.aic,
                fit.hessians=fit.hessians, # hessian for MLEs
                time_spent=time_spent, # time spent
                num.max.location=num.max.location, # the number of times hitting the MLEs
                est.counts=est.counts # estimated counts and 95% CI
    ))
    
  }
  # --- the end of tcov model 
  
  
  # testing
  #   output$out3 <-  renderText({
  #     out3<- test.RD.prediction()$ccov.prediction$time_spent
    
  #    return(out3)
  # })
  
  #****
  # the inputs created are in input$car1, input$car2, ...
  
  output$check_mle_RD <- renderText({
    
    input_not_null <- isTRUE(input$p_type_RD!="" & input$phi_type_RD!="")
    
    
    
    if ( input_not_null == TRUE ) {
      
      if (run_button_counts_RD$counts!=0) {
        
        #b<- test.RD.prediction()$ccov.prediction$time_spent
        
        b<- ccov.RD.prediction(2)$time_spent
        
        
      }}
    
    return(b)
    #  flex_Num<-1:total_no_fits_RD()
    
    
    # results
    #    res <- unlist(lapply(1:total_no_fits_RD(), function(i) input[[paste0("flex_Num", i)]]))
    #    print(res)
    
    #    if (any(res)) {
    #     print(flex_Num[res])
    #    }
    
  })
  
  test.RD.prediction<- reactive({
    
    input_not_null <- isTRUE(input$p_type_RD!="" & input$phi_type_RD!="")
    
    if (run_button_counts_RD$counts!=0 & input_not_null == TRUE ) {
     
      data_all<- dataInput_RD()
      
      # always run the (phic, pc) model, i.e. input$phi_type_RD=="Constant" & input$p_type_RD=="Constant"
      #cc.prediction <- cc.RD.prediction(cc=1)
       
      # number of covariates available
      num.cov<- no_covariate_available_RD()
      
      if (input$cov_yn_RD=="Yes") {
        
        # initialise ccov.prediction as a list to store results for each model with one cov
        ccov.prediction<- list()
        
       if (input$phi_type_RD=="Constant" & input$p_type_RD=="Covariates") {
        
         for (ii in 1:no_covariate_available_RD()) {
          ccov.prediction[[ii]] <- ccov.RD.prediction(ccov=ii) }
        }
        
        
      } else {
        
        
      }
      
      
    }
    
    return(ccov.prediction=ccov.prediction)
  })
  
  
  
   
  
  #**** run all possible RD models
  all.RD.prediction<- reactive({
    
    input_not_null <- isTRUE(input$p_type_RD!="" & input$phi_type_RD!="")
    
    if (run_button_counts_RD$counts!=0 & input_not_null == TRUE ) {
     # if the action button is clicked and inputs are not null
      
      data_all<- dataInput_RD()
      
      # always run the (phic, pc) model, i.e. input$phi_type_RD=="Constant" & input$p_type_RD=="Constant"
      cc.prediction <- try(cc.RD.prediction(cc=1))
      
      # number of covariates available
      num.cov<- no_covariate_available_RD()
      
      if (input$cov_yn_RD=="Yes") {
        
        
          if (input$phi_type_RD=="Constant" & input$p_type_RD=="Constant") {
        
               ccov.prediction=NULL
               covc.prediction=NULL
               covcov.prediction=NULL
               tc.prediction=NULL
               tcov.prediction=NULL
               
          }
          
          if (input$phi_type_RD=="Constant" & input$p_type_RD=="Covariates") {
            
            #no_fits<- 1+no_covariate_available_RD()
            
            # initialise ccov.prediction as a list to store results for each model with one cov
            ccov.prediction<- list()
            
            for (ii in 1:num.cov) {
              
              ccov.prediction[[ii]] <- ccov.RD.prediction(ccov=ii)
              
            }
            
            covc.prediction=NULL
            covcov.prediction=NULL
            tc.prediction=NULL
            tcov.prediction=NULL
            
            #--- the end of ccov case
          }
        
            
            
          if (input$phi_type_RD=="Covariates" & input$p_type_RD=="Constant") {  
            #no_fits<- length(c(1,(no_covariate_available_RD()+2):(2*no_covariate_available_RD()+1)))
            
            # initialise ccov.prediction as a list to store results for each model with one cov
            covc.prediction<- list()
            
            for (ii in 1:num.cov) {
              
              covc.prediction[[ii]] <- covc.RD.prediction(covc=ii)
              
            }
            
            #
            ccov.prediction=NULL
            
            covcov.prediction=NULL
            tc.prediction=NULL
            tcov.prediction=NULL
            
            }
          
         # ***   
          if (input$phi_type_RD=="Covariates" & input$p_type_RD=="Covariates") {
           # no_fits<- length(1:(2*no_covariate_available_RD()+2+no_covariate_available_RD()*no_covariate_available_RD()-1))
            
            tc.prediction=NULL
            tcov.prediction=NULL
            
            
            # initialise ccov.prediction as a list to store results for each model with one cov
            ccov.prediction<- list()
            
            for (kk in 1:num.cov) {
              
              ccov.prediction[[kk]] <- ccov.RD.prediction(ccov=kk)
              
            }
            
            
            # initialise covc.prediction as a list to store results for each model with one cov
            covc.prediction<- list()
            
            for (ii in 1:num.cov) {
              
              covc.prediction[[ii]] <- covc.RD.prediction(covc=ii)
              
            }
            
            # initialise covcov.prediction as a list to store results for each model with one cov
            covcov.prediction<- list()
            
            
            for (gg in 1:num.cov) { #covcov_phi
              
              for (uu in 1:num.cov) { #covcov_p  
                
                # 1+2*num.cov+(gg-1)*num.cov+uu
              covcov.prediction[[(gg-1)*num.cov+uu]] <- covcov.RD.prediction(covcov_phi = gg,covcov_p=uu)
              
            }}
            
            
            
          }
            
            
         #***
          if (input$phi_type_RD=="Time-varying" & input$p_type_RD=="Constant") {
            #no_fits<- 2
            
            ccov.prediction=NULL
            covc.prediction=NULL
            covcov.prediction=NULL
            tcov.prediction=NULL
            
            #
            tc.prediction<- list()
            
            tc.prediction[[1]] <- tc.RD.prediction(tc=1)

            
          }
            
            
         #*** 
          if (input$phi_type_RD=="Time-varying" & input$p_type_RD=="Covariates") {
              
          # no_fits<- length(c(1,
          #                     (2*no_covariate_available_RD()+2+no_covariate_available_RD()*no_covariate_available_RD()):(2*no_covariate_available_RD()+2+no_covariate_available_RD()*no_covariate_available_RD()+no_covariate_available_RD())))
          
            ccov.prediction=NULL
            covc.prediction=NULL
            covcov.prediction=NULL
            
            
            
            #
            tc.prediction<- list()
            
            tc.prediction[[1]] <- tc.RD.prediction(tc=1)
            
            #
            tcov.prediction<- list()
            
            for (kk in 1:no_covariate_available_RD()) {
              
              tcov.prediction[[kk]] <- tcov.RD.prediction(tcov=kk)
              
            }
            
            
          }


        } else {
          
          # no covariates available, input$cov_yn_RD == "No", no_covariate_available_RD()==0
          
          #***
          if (input$phi_type_RD=="All" & input$p_type_RD=="All") { 
            ccov.prediction=NULL
            covc.prediction=NULL
            covcov.prediction=NULL
            tc.prediction=NULL
            tcov.prediction=NULL
            
          }
          
          #***
          if (input$phi_type_RD=="Constant" & input$p_type_RD=="Constant") { 
            ccov.prediction=NULL
            covc.prediction=NULL
            covcov.prediction=NULL
            tc.prediction=NULL
            tcov.prediction=NULL
          }
          
          #***
          if (input$phi_type_RD=="Time-varying" & input$p_type_RD=="Constant") { 
            ccov.prediction=NULL
            covc.prediction=NULL
            covcov.prediction=NULL
            tcov.prediction=NULL
            
            #
            tc.prediction<- list()
            
            tc.prediction[[1]] <- tc.RD.prediction(tc=1)
            }
          
          
        }
      
      
    } else {
      
      # if reset or did not hit the action button
      cc.prediction=NULL
      ccov.prediction=NULL
      covc.prediction=NULL
      covcov.prediction=NULL
      tc.prediction=NULL
      tcov.prediction=NULL
      
    }
    
    # --- the end of action button condition 
    
    return(list(cc.prediction=cc.prediction,
                ccov.prediction=ccov.prediction,
                covc.prediction=covc.prediction,
                covcov.prediction=covcov.prediction,
                tc.prediction=tc.prediction,
                tcov.prediction=tcov.prediction
                ))
    
  })
  
  
  #----------------------------------------
  
  RD.aic.models<-reactive({
    
    if (run_button_counts_RD$counts!=0) {
      
      # number of covariates available
      num.cov<- no_covariate_available_RD()
      
      
      # AIC values
      RD.aic<- numeric()
      
      # aic
      RD.aic[1]<-all.RD.prediction()$cc.prediction$fit.mle.results$AIC[1]
      
      # no.param
      RD.no.param<- c(3)
      
      # maximised loglikelihood vector
      RD.maxlogl<- numeric()
      RD.maxlogl[1]<-all.RD.prediction()$cc.prediction$fit.mle.results$loglike[1] 
      
      # ==
      if (input$phi_type_RD=="Constant" & input$p_type_RD=="Covariates") {
        
        #aic
        for (ii in 1:num.cov) {
          
          RD.aic[ii+1] <- all.RD.prediction()$ccov.prediction[[ii]]$fit.mle.results$AIC[1]
          RD.maxlogl[ii+1]<-all.RD.prediction()$ccov.prediction[[ii]]$fit.mle.results$loglike[1]
        }
        
        # no.param
        RD.no.param<- c(RD.no.param,rep(4,no_covariate_available_RD()))
        
      }
      
      # ==
      if (input$phi_type_RD=="Covariates" & input$p_type_RD=="Constant") {  
        #no_fits<- length(c(1,(no_covariate_available_RD()+2):(2*no_covariate_available_RD()+1)))
        
        for (ii in 1:num.cov) {
          
          RD.aic[ii+1] <- all.RD.prediction()$covc.prediction[[ii]]$fit.mle.results$AIC[1]
          RD.maxlogl[ii+1]<-all.RD.prediction()$covc.prediction[[ii]]$fit.mle.results$loglike[1]
        }
        
        # no.param
        RD.no.param<- c(RD.no.param,rep(4,no_covariate_available_RD()))
        
      }
      
      # ==
      
      if (input$phi_type_RD=="Covariates" & input$p_type_RD=="Covariates") {
        # no_fits<- length(1:(2*no_covariate_available_RD()+2+no_covariate_available_RD()*no_covariate_available_RD()-1))
        
        # ccov.prediction 
        for (kk in 1:num.cov) {
          
          RD.aic[kk+1] <- all.RD.prediction()$ccov.prediction[[kk]]$fit.mle.results$AIC[1]
          RD.maxlogl[kk+1]<-all.RD.prediction()$covc.prediction[[kk]]$fit.mle.results$loglike[1]
        }
        
        
        # covc.prediction
        for (ii in 1:num.cov) {
          
          RD.aic[ii+1+num.cov] <- all.RD.prediction()$covc.prediction[[ii]]$fit.mle.results$AIC[1]
          RD.maxlogl[ii+1+num.cov]<-all.RD.prediction()$covc.prediction[[ii]]$fit.mle.results$loglike[1]
        }
        
        # covcov.prediction
        
        for (gg in 1:num.cov) { #covcov_phi
          
          for (uu in 1:num.cov) { #covcov_p  
            
            RD.aic[1+2*num.cov+(gg-1)*num.cov+uu] <- all.RD.prediction()$covcov.prediction[[(gg-1)*num.cov+uu]]$fit.mle.results$AIC[1]
            RD.maxlogl[1+2*num.cov+(gg-1)*num.cov+uu]<-all.RD.prediction()$covcov.prediction[[(gg-1)*num.cov+uu]]$fit.mle.results$loglike[1]       
                   # 1+2*num.cov+(gg-1)*num.cov+uu
                   # covcov.prediction[[(gg-1)*num.cov+uu]] <- covcov.RD.prediction(covcov_phi = gg,covcov_p=uu)
                   
          }}
        
        
        # no.param
        RD.no.param<- c(RD.no.param,
                        rep(4,2*no_covariate_available_RD()),
                        rep(5,no_covariate_available_RD()*no_covariate_available_RD()))
        
      }
      
      # ==
      if (input$phi_type_RD=="Time-varying" & input$p_type_RD=="Constant") {
        #no_fits<- 2
        
        RD.aic[2] <- all.RD.prediction()$tc.prediction[[1]]$fit.mle.results$AIC[1]
        RD.maxlogl[2]<-all.RD.prediction()$tc.prediction[[1]]$fit.mle.results$loglike[1]       
        
        # no.param
        RD.no.param<- c(RD.no.param,
                        T_RD()/no_k()+1)
        
      }
      
      # ==
      if (input$phi_type_RD=="Time-varying" & input$p_type_RD=="Covariates") {
        
        
        # tc.prediction
        RD.aic[2] <- all.RD.prediction()$tc.prediction[[1]]$fit.mle.results$AIC[1]
        RD.maxlogl[2]<-all.RD.prediction()$tc.prediction[[1]]$fit.mle.results$loglike[1]
        
        # tcov.prediction
        
        for (kk in 1:num.cov) {
          
          RD.aic[2+num.cov] <- all.RD.prediction()$tcov.prediction[[kk]]$fit.mle.results$AIC[1]
          #tcov.prediction[[kk]] <- tcov.RD.prediction(tcov=kk)
          RD.maxlogl[2+num.cov]<-all.RD.prediction()$tcov.prediction[[kk]]$fit.mle.results$loglike[1]
        }
        
        # no.param
        RD.no.param<- c(RD.no.param,
                        T_RD()/no_k()+1,
                        rep(T_RD()/no_k()+2,num.cov))
        
        
      }
      
      
      
      # ordered index vector
      aic.order.index<-order(RD.aic)
      
      # ordered aic
      aic.ordered<-RD.aic[aic.order.index]
      
      # delta aic
      delta.aic.ordered<-RD.aic[aic.order.index]- RD.aic[aic.order.index[1]]
      
      
      # ordered model_name_vec
      
      model.names<-all_list_model_tab_RD()$Model
      model.names.ordered<-model.names[aic.order.index]
      
      
      # ordered phi12 cov_used_vec
      
      phi_cov_used<-all_list_model_tab_RD()$phi_cov_used
      phi_cov_used_ordered<-phi_cov_used[aic.order.index]
      
      # ordered cov_used_vec
      
      p_cov_used<-all_list_model_tab_RD()$p_cov_used
      p_cov_used_ordered<-p_cov_used[aic.order.index]
      
      # ordered no. param
      
      RD.no.param.ordered<- RD.no.param[aic.order.index]
      
      
      # ordered maximised loglikelihood
      
      RD.maxlogl.ordered<-RD.maxlogl[aic.order.index]
      
      
      
      
      # the end of action button
    }
    
    
    return(list(RD.no.param=RD.no.param,
      RD.maxlogl=RD.maxlogl,
      RD.aic=RD.aic,
      aic.order.index=aic.order.index,
      model.names.ordered=model.names.ordered, # ordered
      phi_cov_used_ordered=phi_cov_used_ordered,
      p_cov_used_ordered=p_cov_used_ordered,  # ordered
      RD.no.param.ordered=RD.no.param.ordered, # ordered
      RD.maxlogl.ordered=RD.maxlogl.ordered, # ordered
      aic.ordered=aic.ordered, # ordered
      delta.aic.ordered=delta.aic.ordered # ordered
      
    ))
    
  })
  
  
  
  
  
  
  # model comparison results table for one combination of two cov
  RD.model.tab<-reactive({ 
    
    if (run_button_counts_RD$counts!=0) {
      
      model.tab.results<-data.frame(
        Model = RD.aic.models()$model.names.ordered,
        phi_cov_used = RD.aic.models()$phi_cov_used_ordered,
        p_cov_used = RD.aic.models()$p_cov_used_ordered,
        No_param=RD.aic.models()$RD.no.param.ordered,
        max_logL=RD.aic.models()$RD.maxlogl.ordered,
        AIC=RD.aic.models()$aic.ordered,
        delta.AIC= RD.aic.models()$delta.aic.ordered
      )
    } else {
      
      model.tab.results=NULL
      
    }
    model.tab.results
  })
  
  

  # fitted results otbained from the model with the lowest AIC score
  # estimated params, SEs and 95% CI
  # estimates counts and 95% CI
  
  
  # cc 3
  # ccov 4 & p=cov
  # covc 4 & phi=cov
  # covcov 5
  # tc > 5 $ p=constant
  # tcov >5 & p=cov
  
  fitted.results.RD<- reactive ({
    
    if (run_button_counts_RD$counts!=0) {
      
      
      if (RD.aic.models()$RD.no.param.ordered[1]>5 & input$p_type_RD=="Covariates" ){
        # if  tcov >5 & p=cov
        
        # choose which model
        original.index<- which(order(RD.aic.models()$RD.aic)==1)
        adjust.index<- (original.index - 2) 
        
        est.tab.phi <- all.RD.prediction()$tcov.prediction[[adjust.index]]$fit.mle.results.phi12
        est.tab.p.N <- all.RD.prediction()$tcov.prediction[[adjust.index]]$fit.mle.results.p.N.logl.aic
        
        phi.notation<-paste0("phi",1:(T_RD()/no_k()-1))
        
        
        estimates.tab.results<-data.frame(Notation=c(phi.notation,
                                                     "p_int","p_slope","N"),
                                          
                                          Estimates=c(est.tab.phi[1,],
                                                      est.tab.p.N$p_int[1],
                                                      est.tab.p.N$p_slope[1],
                                                      est.tab.p.N$N[1]),
                                          
                                          SE=c(est.tab.phi[2,],
                                               est.tab.p.N$p_int[2],
                                               est.tab.p.N$p_slope[2],
                                               est.tab.p.N$N[2]),
                                          
                                          LowerCI=c(est.tab.phi[3,],
                                                    est.tab.p.N$p_int[3],
                                                    est.tab.p.N$p_slope[3],
                                                    est.tab.p.N$N[3]),
                                          
                                          UpperCI=c(est.tab.phi[4,],
                                                    est.tab.p.N$p_int[4],
                                                    est.tab.p.N$p_slope[4],
                                                    est.tab.p.N$N[4])
        )
        
        est.counts.tab<- all.RD.prediction()$tcov.prediction[[adjust.index]]$est.counts
        
        estimated.counts<-data.frame(occasions=c(1:T_RD()),
                                     observed_data=dataInput_RD()[,input$data_colnum_RD],
                                     estimated_data=est.counts.tab$est.num,
                                     lower_CI=est.counts.tab$LCI,
                                     upper_CI=est.counts.tab$UCI
        )
        
        
      }
      
      if (RD.aic.models()$RD.no.param.ordered[1]>5 & input$p_type_RD=="Constant"){
        # if  tc > 5 & p constant
        
        
        est.tab.phi <- all.RD.prediction()$tc.prediction[[1]]$fit.mle.results.phi12
        est.tab.p.N <- all.RD.prediction()$tc.prediction[[1]]$fit.mle.results.p.N.logl.aic
        
        phi.notation<-paste0("phi",1:(T_RD()/no_k()-1))
        
        estimates.tab.results<-data.frame(Notation=c(phi.notation,
                                                     "p","N"),
                                          
                                          Estimates=c(est.tab.phi[1,],
                                                      est.tab.p.N$p[1],
                                                      est.tab.p.N$N[1]),
                                          
                                          SE=c(est.tab.phi[2,],
                                               est.tab.p.N$p[2],
                                               est.tab.p.N$N[2]),
                                          
                                          LowerCI=c(est.tab.phi[3,],
                                                    est.tab.p.N$p[3],
                                                    est.tab.p.N$N[3]),
                                          
                                          UpperCI=c(est.tab.phi[4,],
                                                    est.tab.p.N$p[4],
                                                    est.tab.p.N$N[4])
        )
        
        est.counts.tab<- all.RD.prediction()$tc.prediction[[1]]$est.counts
        
        estimated.counts<-data.frame(occasions=c(1:T_RD()),
                                     observed_data=dataInput_RD()[,input$data_colnum_RD],
                                     estimated_data=est.counts.tab$est.num,
                                     lower_CI=est.counts.tab$LCI,
                                     upper_CI=est.counts.tab$UCI
        )
        
        
      }       
      
      if (RD.aic.models()$RD.no.param.ordered[1]==5){
        # if covcov 5
        
        # choose which model
        #original.index<- which(order(RD.aic.models()$RD.aic)==1)
        #adjust.index<- original.index - 1 
        
        phi_index<- which(data_cov_names_RD()==RD.aic.models()$phi_cov_used_ordered[1])
        p_index<- which(data_cov_names_RD()==RD.aic.models()$p_cov_used_ordered[1])
        
        gg = phi_index
        uu = p_index
        
        adjust.index<- (gg-1)*no_covariate_available_RD()+uu
        
        est.tab <- all.RD.prediction()$covcov.prediction[[adjust.index]]$fit.mle.results
        
        
        estimates.tab.results<-data.frame(Notation=c("phi_int","phi_slope","p_int","p_slope","N"),
                                          
                                          Estimates=c(est.tab$phi12_int[1],
                                                      est.tab$phi12_slope[1],
                                                      est.tab$p_int[1],
                                                      est.tab$p_slope[1],
                                                      est.tab$N[1]),
                                          
                                          SE=c(est.tab$phi12_int[2],
                                               est.tab$phi12_slope[2],
                                               est.tab$p_int[2],
                                               est.tab$p_slope[2],
                                               est.tab$N[2]),
                                          
                                          LowerCI=c(est.tab$phi12_int[3],
                                                    est.tab$phi12_slope[3],
                                                    est.tab$p_int[3],
                                                    est.tab$p_slope[3],
                                                    est.tab$N[3]),
                                          
                                          UpperCI=c(est.tab$phi12_int[4],
                                                    est.tab$phi12_slope[4],
                                                    est.tab$p_int[4],
                                                    est.tab$p_slope[4],
                                                    est.tab$N[4])
        )
        
        est.counts.tab<- all.RD.prediction()$covcov.prediction[[adjust.index]]$est.counts
        
        estimated.counts<-data.frame(occasions=c(1:T_RD()),
                                     observed_data=dataInput_RD()[,input$data_colnum_RD],
                                     estimated_data=est.counts.tab$est.num,
                                     lower_CI=est.counts.tab$LCI,
                                     upper_CI=est.counts.tab$UCI
        )
        
      }
      
      if (RD.aic.models()$RD.no.param.ordered[1]==4 & input$phi_type_RD=="Covariates" ) {
        # if covc 4 & phi=cov
        
        original.index<- which(order(RD.aic.models()$RD.aic)==1)
        
        if (input$p_type_RD=="Constant") {
          adjust.index<- original.index - 1
        } else {
          # if input$p_type_RD=="Covariates" which includes cc, ccov, (covc), covcov models
          adjust.index<- original.index - 1 - no_covariate_available_RD()
        }
         
        
        est.tab <- all.RD.prediction()$covc.prediction[[adjust.index]]$fit.mle.results
        
        estimates.tab.results<-data.frame(Notation=c("phi_int","phi_slope","p","N"),
                                          
                                          Estimates=c(est.tab$phi12_int[1],
                                                      est.tab$phi12_slope[1],
                                                      est.tab$p[1],
                                                      est.tab$N[1]),
                                          
                                          SE=c(est.tab$phi12_int[2],
                                               est.tab$phi12_slope[2],
                                               est.tab$p[2],
                                               est.tab$N[2]),
                                          
                                          LowerCI=c(est.tab$phi12_int[3],
                                                    est.tab$phi12_slope[3],
                                                    est.tab$p[3],
                                                    est.tab$N[3]),
                                          
                                          UpperCI=c(est.tab$phi12_int[4],
                                                    est.tab$phi12_slope[4],
                                                    est.tab$p[4],
                                                    est.tab$N[4])
        )
        
        est.counts.tab<- all.RD.prediction()$covc.prediction[[adjust.index]]$est.counts
        
        estimated.counts<-data.frame(occasions=c(1:T_RD()),
                                     observed_data=dataInput_RD()[,input$data_colnum_RD],
                                     estimated_data=est.counts.tab$est.num,
                                     lower_CI=est.counts.tab$LCI,
                                     upper_CI=est.counts.tab$UCI
        )
        
        
        
      }
      
      
      if (RD.aic.models()$RD.no.param.ordered[1]==4 & input$p_type_RD=="Covariates") {
        # if ccov 4 & p=cov, phic
        
        original.index<- which(order(RD.aic.models()$RD.aic)==1)
        adjust.index<- original.index - 1 
        
        est.tab <- all.RD.prediction()$ccov.prediction[[adjust.index]]$fit.mle.results
        
        
        estimates.tab.results<-data.frame(Notation=c("phi","p_int","p_slope","N"),
                                          
                                          Estimates=c(est.tab$phi12[1],
                                                      est.tab$p_int[1],
                                                      est.tab$p_slope[1],
                                                      est.tab$N[1]),
                                          
                                          SE=c(est.tab$phi12[2],
                                               est.tab$p_int[2],
                                               est.tab$p_slope[2],
                                               est.tab$N[2]),
                                          
                                          LowerCI=c(est.tab$phi12[3],
                                                    est.tab$p_int[3],
                                                    est.tab$p_slope[3],
                                                    est.tab$N[3]),
                                          
                                          UpperCI=c(est.tab$phi12[4],
                                                    est.tab$p_int[4],
                                                    est.tab$p_slope[4],
                                                    est.tab$N[4])
        )
        
        est.counts.tab<- all.RD.prediction()$ccov.prediction[[adjust.index]]$est.counts
        
        
        estimated.counts<-data.frame(occasions=c(1:T_RD()),
                                     observed_data=dataInput_RD()[,input$data_colnum_RD],
                                     estimated_data=est.counts.tab$est.num,
                                     lower_CI=est.counts.tab$LCI,
                                     upper_CI=est.counts.tab$UCI
        )
       
      }
      
      
      if (RD.aic.models()$RD.no.param.ordered[1]==3) {
        # if cc 3
        
        est.tab <- all.RD.prediction()$cc.prediction$fit.mle.results
        
        estimates.tab.results<-data.frame(Notation=c("phi","p","N"),
                                          
                                          Estimates=c(est.tab$phi12[1],
                                                      est.tab$p[1],
                                                      est.tab$N[1]),
                                          
                                          SE=c(est.tab$phi12[2],
                                               est.tab$p[2],
                                               est.tab$N[2]),
                                          
                                          LowerCI=c(est.tab$phi12[3],
                                                    est.tab$p[3],
                                                    est.tab$N[3]),
                                          
                                          UpperCI=c(est.tab$phi12[4],
                                                    est.tab$p[4],
                                                    est.tab$N[4])
        )
        
        est.counts.tab<- all.RD.prediction()$ccov.prediction[[adjust.index]]$est.counts
        
        estimated.counts<-data.frame(occasions=c(1:T_RD()),
                                     observed_data=dataInput_RD()[,input$data_colnum_RD],
                                     estimated_data=est.counts.tab$est.num,
                                     lower_CI=est.counts.tab$LCI,
                                     upper_CI=est.counts.tab$UCI
        )
       
      }
      
      
      
      
    } else {
      
      estimates.tab.results=data.frame(Notation="",Estimates="",SE="",LowerCI="",UpperCI="")
      
    }
    
    return(list(estimates.tab.results=estimates.tab.results,
                estimated.counts=estimated.counts))
  })
  
  
  
  
  
   
  ################################  estimate table results from RD  
  est.tab_RD<-reactive({ 
    
    
    #geo.aic.models()$model.names.ordered[1]
    #geo.aic.models()$p_cov_used_ordered[1]
    # "p_average"
    # Need some words for explainations of results here
    
    # *** customise the length of p estimates
    estimates.tab.results_RD<-data.frame(
                                      Notation=c("p_int","p_slope1","p_slope2","n_0"),
                                      Estimates=c("","","",""),
                                      SE=c("","","",""),
                                      LowerCI=c("","","",""),
                                      UpperCI=c("","","","")
    )
    
    
    est.tab.results<-data.frame(
      Param_name = c("Capture probability", "Population size"),
      Notation = c("p", "N"),
      Estimate= c(ept.p.RD(),ept.N.RD()),
      Standard_error= c(se.p.RD(),se.n01.RD()),
      Confidence_interval= c("","")
    )
    
    return(estimates.tab.results_RD)
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ################################  
  # outputs
  ################################  preview data head
  #output$data1 <- renderTable({
  
  #  data_all<- dataInput()
  #  return(head(data_all))
  #})
  
  #============================ classic model
  output$data1 <-DT::renderDataTable({
    data_all<- dataInput()
    DT::datatable(data_all)
  })
  
  ################################ predicted plot
  
  # read the number of times hitted run analysis button
  
 #tags$script("$(document).on('click', '#reset_ana_button', 
#            function () {
#              Shiny.onInputChange('run_ana_button',0);
#             }
#            );"
#            )
  
  # set new reactive values of the number of hits of actionButton
  #  as impossible to reset value of an actionButton == 0
  
  run_button_counts <- reactiveValues(counts = 0)
  
  # Each time (run) button is clicked, add 1 to reactive value of run_button_counts$counts
  observeEvent(input$run_ana_button,
               {run_button_counts$counts =  isolate(run_button_counts$counts) +1 }
               )
  
  observeEvent(input$reset_ana_button,
               {run_button_counts$counts = 0}
               )
 # run_button_counts<-reactive({isolate(input$run_ana_button)
 #    })
  
 # default_plot <- eventReactive(input$reset_ana_button, {
#    head(cars)
 # })
  
  # reset_run_button_counts<-observeEvent(input$reset_ana_button,
  #                                      isolate(run_ana_button)=0)

  
  #############################
  
  all_list_model_tab<-reactive({
    
    no_cov<-no_covariate_available()
    
     if (input$cov_yn=="Yes" && no_cov>1){
        
        ## when there are more than 1 covaraiete available
        
        # pcov name vector 
        one_cov_vec<- paste0("p(cov",1:no_cov,")") # when one cov is considered 
        
        # read index vectors
        first_cov_index<-two_cov_combin_index_vec()$orignal_index_vec[,1]
        second_cov_index<-two_cov_combin_index_vec()$orignal_index_vec[,2]
        
        two_cov_vec_plus<- paste0("p(cov",first_cov_index,
                                  "+cov",second_cov_index,")"
                                  )
        
        two_cov_vec_plus_times<- paste0("p(cov",first_cov_index,
                                        "+cov",second_cov_index,
                                        "+cov",first_cov_index,
                                        "*cov",second_cov_index,")"
                                        )
        
        two_cov_vec<-character(total_no_fits()$no_two_cov_cases)
        for (i in 1:(length(two_cov_vec)/2)) {
          two_cov_vec[i*2-1]<-two_cov_vec_plus[i]
          two_cov_vec[i*2]<-two_cov_vec_plus_times[i]
        }
        
        # model_name_vec
        model_name_vec<-c(paste("p(c)"),
                          one_cov_vec,
                          two_cov_vec)
        
        
        # p_cov_used_vec when more than one covs are considered
  
        # initialise two_cov_names_vec
        two_cov_names_vec<- character()
        
        for (ii in 1:choose(no_covariate_available(),2)) {
          
          cov1_name<-data_cov_names()[two_cov_combin_index_vec()$orignal_index_vec[ii,1]]
          cov2_name<- data_cov_names()[two_cov_combin_index_vec()$orignal_index_vec[ii,2]]
          
          two_cov_names_vec[ii*2-1] <- paste0("  ",cov1_name, " & ",cov2_name)
          two_cov_names_vec[ii*2] <- paste0("  ",cov1_name," & ",cov2_name)
        }
        
        
        p_cov_used_vec<-c(paste("  -"),
                          paste0("  ",data_cov_names()),
                          two_cov_names_vec)
        
     } 
    
    if (input$cov_yn=="Yes" && no_cov==1){
        
        # pcov vector when one cov is considered 
        model_name_vec<- paste0("p(c)"," p(cov",1:no_cov,")")
        
        # p_cov_used_vec
        p_cov_used_vec<-c(paste("-"), 
                          paste("cov"))
        
      }  
    
    if (input$cov_yn=="No"){
      
      # input$cov_yn=="No"
      # fit geometric model only
      
      model_name_vec<-paste0("p(c)")
      p_cov_used_vec<- paste0("-")
      
    }
    
    
    Num<- seq_along(model_name_vec)
    
    all_model_tab<-data.frame(Num= Num,
                              Model=model_name_vec,
                              p_cov_used=p_cov_used_vec
                              )
    
    return(list(all_model_tab=all_model_tab,
                Num=Num,
                Model=model_name_vec,
                p_cov_used=p_cov_used_vec))
    
  })
  
  
  
  
  
  # all_model_table in the Settings section.
  output$all_model_table<-renderTable({
    
    print_all_list_model_tab<-all_list_model_tab()$all_model_tab
    
    #print_all_list_model_tab<-vanilla.table(print_all_list_model_tab)
    
    #print_all_list_model_tab[, c("Num","Model"), to ="header"] <- parLeft() # left text
    #print_all_list_model_tab[, c("Num","Model")] <- parLeft() # left align header
    
    return(print_all_list_model_tab)
  })
  
  
  
  #########################
  # *** all_ana_model_table in the Analysing section
  #
  all_ana_model_table<-reactive({
    
    
    if (run_button_counts$counts!=0) {
      
      # num_max vector, the number of times a model hitting the maximum logl
      
      Num<-all_list_model_tab()$Num
      
      # model_name_vec
      
      model_name_vec_RD<-all_list_model_tab()$Model
      
      # cov_used_vec
      
      p_cov_used<-all_list_model_tab()$p_cov_used
      
      
      #
      
     # time_spent<-time_all_ana_model_table()
      
      time_spent<- numeric()
      time_spent[1]<-all.geo.prediction()$pc.prediction$time_spent
      #time_spent<-rep(time_spent,length(all_list_model_tab()$Num))
      
      for (ii in 1:total_no_fits()$no_one_cov_cases ) {
        
        time_spent[ii+1]<- all.geo.prediction()$pt.1cov.prediction[[ii]]$time_spent.1cov
      }
      #time_spent[2:(1+total_no_fits()$no_one_cov_cases)]<- 
      #  all.geo.prediction()$pt.1cov.prediction[[1:total_no_fits()$no_one_cov_cases]]$time_spent.1cov
      
      
      for (jj in 1:choose(no_covariate_available(),2) ) {
        
        time_spent[jj*2-1+(1+total_no_fits()$no_one_cov_cases)] <- all.geo.prediction()$pt.2cov.prediction[[jj]]$time_spent.2cov
        
        time_spent[jj*2 +(1+total_no_fits()$no_one_cov_cases)] <- all.geo.prediction()$pt.2cov.prediction[[jj]]$time_spent.2cov.times
      }
      
      
      #num_max<-all_list_model_tab()$Num
      
      num_max<- numeric()
      
      num_max[1]<- all.geo.prediction()$pc.prediction$num.max.location
      
      for (jj in 1:total_no_fits()$no_one_cov_cases ) {
        num_max[jj+1]<- all.geo.prediction()$pt.1cov.prediction[[jj]]$num.max.location.1cov
        
      }
      
      
      for (jj in 1:choose(no_covariate_available(),2) ) {
        
        num_max[jj*2-1+(1+total_no_fits()$no_one_cov_cases)] <- all.geo.prediction()$pt.2cov.prediction[[jj]]$num.max.location.2cov
        
        num_max[jj*2+(1+total_no_fits()$no_one_cov_cases)] <- all.geo.prediction()$pt.2cov.prediction[[jj]]$num.max.location.2cov.times
        
      }
      
      
      #num_max<- numeric()
      #num_max[1]<- all.geo.prediction()$pt.2cov.prediction[[2]]$num.max.location.2cov.times
      #num_max<- rep(num_max,length(all_list_model_tab()$Num))
        
      
      # chechinput to print out the max_logls 
      # rep("", 6)
      
      print_logLs<-rep(c(""),total_no_fits()$no_fits)
      
      
      # Table to print
      draft_all_model_ana_tab_RD<-data.frame(Num= Num,
                                             Model=model_name_vec_RD,
                                             p_cov_used=p_cov_used,
                                             time_spent=time_spent,
                                             num_max= num_max
                                             #print_logLs=print_logLs
                                             )
      
    } else {
      
      # if reset
      
      
      # num_max vector, the number of times a model hitting the maximum logl
      
      Num<-all_list_model_tab()$Num
      
      # model_name_vec_RD
      
      model_name_vec_RD<-all_list_model_tab()$Model
      
      # cov_used_vec_RD
      
      p_cov_used<-all_list_model_tab()$p_cov_used
      
      
      # time_spent vector, two cov case
       time_spent<-rep(c(""),total_no_fits()$no_fits)
      
      #time_spent<-numeric()
      #time_spent[1]<- hh.geo.pc.prediction(hh=1)$time_spent
      #time_spent[2:(1+total_no_fits()$no_one_cov_cases)]<- hh.geo.pc.prediction(hh=1)$time_spent
      
      
      # model_condition, waiting (0/num_iteration, blue), 
      # running(c(1:(num_iteration-1))/num_iteration, yellow), done (num_iteration/num_iteration, green)
      # no. of times hitting the MLE  
      
      num_max<- rep(c(""),total_no_fits()$no_fits)
      
     
      # chechinput to print out the max_logls 
      
      print_logLs<-rep(c(""),total_no_fits()$no_fits)
      
      
      # Table to print
      draft_all_model_ana_tab_RD<-data.frame(Num= Num,
                                             Model=model_name_vec_RD,
                                             p_cov_used=p_cov_used,
                                             time_spent=time_spent,
                                             num_max= num_max
                                             #print_logLs=print_logLs
                                             )
      
    }

   
    # returns one table element
    return(draft_all_model_ana_tab_RD)
    
  })
  
  
  
  
  output$all_ana_model_table<-renderTable({
    
    # Create checkboxes
    #all_ana_model_table_RD()$all_ana_model_table_RD$print_max_logLs <- 
    #                        paste0('<label><input type="checkbox" id="car', 
    #                        1:6, '"> <span>') # 1:total_no_models()
    #rownames(mymtcars), '</span></label>')
    
    # flex_table <- vanilla.table(d_all_ana_model_table_RD()$draft_all_model_ana_tab_RD) # convert to FlexTable objet
    
    # flex_table[, draft_all_model_ana_tab_RD$print_max_logLs(), to = "header"] <- parLeft() # left align checkboxes
    
    #  flex_table[, draft_all_model_ana_tab_RD$print_max_logLs()] <- parLeft() # left align header
    
    # all_model_ana_tab_RD<- flex_table
    
    flex_tab<-all_ana_model_table()
    
    flex_Num<-flex_tab$Num
    
    null_values<-flex_tab$print_logLs
    
    #flex_tab$print_logLs<-  paste0('<label><input type="checkbox" id="flex_Num', 
    #                               1:total_no_fits()$no_fits, '"> <span>', # 1:total_no_models()
    #                               null_values, '</span></label>')
    
    # convert to FlexTable objet
    #flex_table<- vanilla.table(flex_tab)
    
    #flex_table[, c("Num","Model"), to = "header"] <- parLeft() # left align the column "Num"
    #flex_table[, c("Num","Model")] <- parLeft() # left align header

    
    return(flex_tab)
  })
  
  
  
  # the inputs created are in input$car1, input$car2, ...
  output$check_mle <- renderPrint({
    
    #gg.geo.pt.1cov.prediction(gg=1)$num.max.location.1cov$est.numbers
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ################################3
  # show predicted data plot
  output$plot <- renderPlot({

    # plot if hit run button 
    
    if (run_button_counts$counts!=0) {
    
      data_plot<-print_plot()

    } else {data_plot<- NULL}
    
    return(data_plot)
    
  })
  
  
  
  # download plot

  output$downloadPlot <- downloadHandler(
    
    filename <- function() {
      paste('plot', 'png', sep = ".")
    },
    
    content <- function(file) {
      
      png(file) # add more arguments for pic
      
      # name the fitted counts results
      fitted.counts.tab<- isolate(fitted.results.geo()$estimated.counts)
      
      # isolate this to avoid reactive plot function
      data_plot<- plot(c(1:T()),fitted.counts.tab$observed_data,
                       pch="+",
                       col = "black",
                       lwd=2,#pch=10,
                       xlab="sampling occasions",
                       ylab="counts",
                       main=paste("Predicted counts of individuals removed.")
      )
      
      
      lines(c(1:T()), fitted.counts.tab$observed_data,col = "black")
      
      # fitted data
      points(c(1:T()), fitted.counts.tab$estimated_data,pch=2, col = "red") # estimated data, red triangle
      lines(c(1:T()), fitted.counts.tab$estimated_data,col = "red")
      
      # 95% CI
      #lines(c(1:T()), fitted.counts.tab$lower_CI, col = "red", lwd=2,lty=3) # lower CI, dashed gray line
      #lines(c(1:T()), fitted.counts.tab$upper_CI, col = "red", lwd=2,lty=3) # upper CI, dashed gray line
      
      
      #print(print_plot())
      
      dev.off()
    },
    contentType = "image/png"
    
  )
  

  
  # predicted data and CI as a table
  
  download_predicted_data<-reactive({
    
    # name the fitted counts results
    fitted.counts.tab<-fitted.results.geo()$estimated.counts
    
    predicted_data_CI_table<-data.frame(
      Sampling_occasion = c(1:T()),
      Observed_data = fitted.counts.tab$observed_data,
      Predicted.counts.tab=fitted.counts.tab$estimated_data # add predicted column
      #Lower_CI=fitted.counts.tab$lower_CI, # lower 95% CI
      #Upper_CI=fitted.counts.tab$upper_CI
      )
    
    return(predicted_data_CI_table)
    
  })
  
  
  # Output: download predicted data and confidence interval
  output$downloadPredicted <- downloadHandler(
    filename = function() {
      paste("predicted_counts",download_predicted_data(), ".csv", sep = "")
    },
    content = function(file) {
      
      write.csv(download_predicted_data(), file, row.names = FALSE)
    }
  )
  
  
  
  ################################  model comparison according to AIC
  output$fit_table <- renderTable({
    
    data_all<- dataInput()
    data1=data_all[,input$data_colnum]
    
    if (input$cov_yn=="Yes"){ 
      
      output<-geo.model.tab()
      
    } else {
      output<-geo.model.tab()
      
      }
 
    return(output)
  }) 
  
  
  ## download model comparison
  output$downloadModelCom <- downloadHandler(
    filename = function() {
      paste("predicted_counts",geo.model.tab(), ".csv", sep = "")
    },
    content = function(file) {
      
      write.csv(geo.model.tab(), file, row.names = FALSE)
    }
  )
  
  ################################  estimates, SE and 95% CI results
  output$estimates <- renderTable({
    
    data_all<- dataInput()
    data1=data_all[,input$data_colnum]

    print<- fitted.results.geo()$estimates.tab.results
    
    return(print)
  })  
  
  
  ## download estimates
  output$downloadEstimates <- downloadHandler(
    filename = function() {
      paste("predicted_counts",fitted.results.geo()$estimates.tab.results, ".csv", sep = "")
    },
    content = function(file) {
      
      write.csv(fitted.results.geo()$estimates.tab.results, file, row.names = FALSE)
    }
  )
  

  ################################ data summary

  
  data_summary_tab<- reactive({
    
    data_all<- dataInput()
    data1=data_all[,input$data_colnum]
    
    if (input$cov_yn=="Yes") {
      num_cov<- ceiling(no_covariate_available())
    } else {
      num_cov<-c("Not Selected")
    }
    
    data_summary<-data.frame(
      Length_of_study = T(),
      Total_removed_number = ceiling(D()),
      Number_covariate_available= num_cov
    )
    
    return(data_summary)
    
  })
  
  # Output: data_summary
  output$data_summary <- renderTable({
    
    # output depends on the reactive function data_summary_tab()
    output<-data_summary_tab()
    
    return(output)
    
  }) 
  
  ### test 
  output$out2 <-  renderTable({
    
    # Take a dependency on input$goButton. This will run once initially,
    # because the value changes from NULL to 0.
    # input$run_ana_button
    
    # Use isolate() to avoid dependency on other inputs
    
    data_all<- isolate(dataInput())
    data1=data_all[,input$data_colnum]
    
    #kk.geo.pt.2cov.prediction(kk=1)
    #pp<-all.geo.prediction()$pc.prediction
    
    #num_one_unique_cov_case<-total_no_fits()$no_one_cov_cases
    
    # initialise pt.1cov.prediction as a list to store results for each model with one cov
    
    #ooo<-all.geo.prediction$pt.2cov.prediction[[2]]
    # gg.geo.pt.1cov.prediction(gg=gg)
    
    #ppp<- all.geo.prediction()$pc.prediction
    #pt.1cov.prediction[[2]]$time_spent.1cov
    #re=ooo$time_spent.2cov.times
    
    #test.list<-list()
    #test.list[[2]]<-all.geo.prediction()$pt.2cov.prediction[[2]]
    
    
    return( )
  })
  
  
  
  
  

  
  
  #============================  #============================  #============================  #============================  
  #============================ RD model outputs
  
  # Output: preview data
  output$data1_RD <-DT::renderDataTable({
    data_all<- dataInput_RD()
    DT::datatable(data_all)
  })  
 
  # Output: RD data summary
  
  data_summary_tab_RD<- reactive({
    
    data_all<- dataInput_RD()
    data1=data_all[,input$data_colnum_RD]
    
    if (input$cov_yn_RD=="Yes") {
      num_cov<- ceiling(no_covariate_available_RD())
    } else {
      num_cov<-"Not Selected"
    }
    
    data_summary<-data.frame(
      Length_of_study = T_RD(),
      Total_removed_number = ceiling(D_RD()),
      Number_covariate_available= num_cov
    )
    
    return(data_summary)
    
  })
  
  
  # Output: data summary table in RD Settings section
  output$data_summary_RD <- renderTable({
    
    # depends on the reactive function data_summary_tab_RD()
    output<-data_summary_tab_RD()
    
    return(output)
    
  }) 
  
  

  ## all_list_model_tab_RD() function
  all_list_model_tab_RD<-reactive({
  
  input_not_null <- isTRUE(input$p_type_RD!="" & input$phi_type_RD!="")
  
  if ( input_not_null == TRUE ) {
    
    #model_name_vec_RD
    
    if (no_covariate_available_RD()>0) {
      
    no_cov<-no_covariate_available_RD()

    # phic + pcov vector
    phic_pcov_vec<- paste0("phi(c), p(cov",1:no_cov,")")
      
    # phicov + pc + pcov vector
    
      # phicov + pc
    phicov_pc0<- paste0("phi(cov",1:no_cov,")")
    phicov_pc<- paste0(phicov_pc0,", p(c)")
    
      #phicov + pcov
    pcov_vec<- paste0(", p(cov",1:no_cov,")")

    phicov_pcov<- c()
    
    for (i in 1:no_cov) {
      phicov_pcov<- c(phicov_pcov,paste0(phicov_pc0[i], pcov_vec))
    }

    # *** change phicov + pc case order
    phicov_pc_pcov<-c(phicov_pc,phicov_pcov)
    
    
    # phit + pcov
    phit_pcov<- paste("phi(t)", pcov_vec)
    
      
    # model_name_vec_RD
    model_name_vec_RD<-c(paste("phi(c), p(c)"),
                         phic_pcov_vec,
                         phicov_pc_pcov,
                         paste("phi(t), p(c)"),
                         phit_pcov
    )
    
    
    
    ####### ** covariate names vectors
    
    #** phi_cov_used_vec_RD according to covariate names
    
    # (phicov) + pcov
    phicov_pcov_phiname <- character()
    for (ii in 1:no_cov) {
      phicov_pcov_phiname[(ii*no_cov-(no_cov-1)):(ii*no_cov)] <- data_cov_names_RD()[ii]
      
    }
    
    # final phi_cov_used_vec_RD
    phi_cov_used_vec_RD<-c(paste("-"),
                           rep(paste("-"),no_cov), # (phic) + pcov
                           data_cov_names_RD(), # (phicov) + pc
                           phicov_pcov_phiname, # (phicov) + pcov
                           paste("-"),  # (phit) + pc
                           rep(paste("-"),no_cov) # (phit) + pcov
    )
    
    
    
    #** p_cov_used_vec_RD according to covariate names
    
    p_cov_used_vec_RD<-c(paste("-"),
                         data_cov_names_RD(), # phic + (pcov)
                         rep(paste("-"),no_cov), # phicov + (pc)
                         rep(data_cov_names_RD(),no_cov), # phicov + (pcov)
                         paste("-"), # phit + (pc)
                         data_cov_names_RD() # phit + (pcov)
                         )
    
    } else {
      
      # no cov 
      
      # model_name_vec_RD
      model_name_vec_RD<-c(paste("phi(c), p(c)"),
                           paste("phi(t), p(c)")
      )
      
      # phi_cov_used_vec_RD
      phi_cov_used_vec_RD<-c(paste("-"),
                             paste("-")  # (phit) + pc
      )
      
      # p_cov_used_vec_RD
      p_cov_used_vec_RD<-c(paste("-"),
                           paste("-") # phit + (pc)
      )
    }
    
    
    
    
    
    if (input$phi_type_RD=="All" & input$p_type_RD=="All" ) {
      
      all_model_tab_RD<-data.frame(Num= seq_along(model_name_vec_RD),
                                   Model=model_name_vec_RD,
                                   phi_cov_used=phi_cov_used_vec_RD, # p cov if length(cov)=1
                                   p_cov_used=p_cov_used_vec_RD # phi cov
      )
      
      
    } 
    
    #
    if (input$phi_type_RD=="Constant" & input$p_type_RD=="Constant") {
      
      all_model_tab_RD<-data.frame(Num= seq_along(model_name_vec_RD[1]),
                                   Model=model_name_vec_RD[1],
                                   phi_cov_used=phi_cov_used_vec_RD[1], # p cov if length(cov)=1
                                   p_cov_used=p_cov_used_vec_RD[1] # phi cov
      )
    }
    
    #
    if (input$phi_type_RD=="Constant" & input$p_type_RD=="Covariates") {
      
      
      all_model_tab_RD<-data.frame(Num= seq_along(model_name_vec_RD[1:(no_cov+1)]),
                                   Model=model_name_vec_RD[1:(no_cov+1)],
                                   phi_cov_used=phi_cov_used_vec_RD[1:(no_cov+1)], 
                                   p_cov_used=p_cov_used_vec_RD[1:(no_cov+1)] 
      )
      
    }
    
    #
    if (input$phi_type_RD=="Covariates" & input$p_type_RD=="Constant") {
      
      
      all_model_tab_RD<-data.frame(Num= seq_along(c(model_name_vec_RD[1],model_name_vec_RD[(no_cov+2):(2*no_cov+1)])),
                                   Model=c(model_name_vec_RD[1],model_name_vec_RD[(no_cov+2):(2*no_cov+1)]),
                                   phi_cov_used=c(phi_cov_used_vec_RD[1],phi_cov_used_vec_RD[(no_cov+2):(2*no_cov+1)]), 
                                   p_cov_used=c(p_cov_used_vec_RD[1] ,p_cov_used_vec_RD[(no_cov+2):(2*no_cov+1)])
      )
      
    }
    
    # 
    if (input$phi_type_RD=="Covariates" & input$p_type_RD=="Covariates") {
      
      
      all_model_tab_RD<-data.frame(Num= seq_along(model_name_vec_RD[1:(2*no_cov+2+no_cov*no_cov-1)]),
                                   Model=c(model_name_vec_RD[1:(2*no_cov+2+no_cov*no_cov-1)]),
                                   phi_cov_used=c(phi_cov_used_vec_RD[1:(2*no_cov+2+no_cov*no_cov-1)]), 
                                   p_cov_used=c(p_cov_used_vec_RD[1:(2*no_cov+2+no_cov*no_cov-1)]) 
      )
      
    }
    
    #
    if (input$phi_type_RD=="Time-varying" & input$p_type_RD=="Constant") {
      
      
      all_model_tab_RD<-data.frame(Num= seq_along(c(model_name_vec_RD[1],model_name_vec_RD[(2*no_cov+2+no_cov*no_cov)])),
                                   Model=c(model_name_vec_RD[1],model_name_vec_RD[(2*no_cov+2+no_cov*no_cov)]),
                                   phi_cov_used=c(phi_cov_used_vec_RD[1],phi_cov_used_vec_RD[(2*no_cov+2+no_cov*no_cov)]), 
                                   p_cov_used=c(p_cov_used_vec_RD[1],p_cov_used_vec_RD[(2*no_cov+2+no_cov*no_cov)])
      )
      
    }
    
    #
    if (input$phi_type_RD=="Time-varying" & input$p_type_RD=="Covariates") {
      
      
      all_model_tab_RD<-data.frame(Num= seq_along(c(model_name_vec_RD[1],model_name_vec_RD[(2*no_cov+2+no_cov*no_cov):(2*no_cov+2+no_cov*no_cov+no_cov)])),
                                   Model=c(model_name_vec_RD[1],model_name_vec_RD[(2*no_cov+2+no_cov*no_cov):(2*no_cov+2+no_cov*no_cov+no_cov)]),
                                   phi_cov_used=c(phi_cov_used_vec_RD[1],phi_cov_used_vec_RD[(2*no_cov+2+no_cov*no_cov):(2*no_cov+2+no_cov*no_cov+no_cov)]), 
                                   p_cov_used=c(p_cov_used_vec_RD[1],p_cov_used_vec_RD[(2*no_cov+2+no_cov*no_cov):(2*no_cov+2+no_cov*no_cov+no_cov)])
      )
      
    }
    
  } else { all_model_tab_RD<- data.frame(Num= "",
                                         Model= "",
                                         phi_cov_used= "", # p cov if length(cov)=1
                                         p_cov_used="" # phi cov
  ) }
    
  return(all_model_tab_RD)
  
  })
    
    
    
    
  
  
  
  
  # Output: all_model_table_RD in the Settings section
  output$all_model_table_RD<-renderTable({
    
     print_all_list_model_tab_RD<-all_list_model_tab_RD()
     
     #print_all_list_model_tab_RD<-vanilla.table(print_all_list_model_tab_RD)
     
     #print_all_list_model_tab_RD[, c("Num","Model"), to = "header"] <- parLeft() # left align the column "Num"
     #print_all_list_model_tab_RD[, c("Num","Model")] <- parLeft() # left align header
     
    return(print_all_list_model_tab_RD)
  })
  
  
  
  
  # *** all_ana_model_table_RD in the Analysing section
  
all_ana_model_table_RD<-reactive({
    
  input_not_null <- isTRUE(input$p_type_RD!="" & input$phi_type_RD!="")
    
  if ( input_not_null == TRUE ) {
    
    if (run_button_counts_RD$counts!=0) {

      # num_max vector, the number of times a model hitting the maximum logl
      
      Num<-all_list_model_tab_RD()$Num
      
      # model_name_vec_RD
      
      model_name_vec_RD<-all_list_model_tab_RD()$Model
      
      # cov_used_vec_RD
      phi_cov_used<-all_list_model_tab_RD()$phi_cov_used
      p_cov_used<-all_list_model_tab_RD()$p_cov_used
      
      
      
      #*** time_spent vector, two cov case
      time_spent<- numeric()
      time_spent[1]<- all.RD.prediction()$cc.prediction$time_spent
      
      #*** no. of times hitting the MLE
      num_max<- numeric()
      num_max[1]<- all.RD.prediction()$cc.prediction$num.max.location
      
      
      # adding more time and num_max elements if possible
      if (input$phi_type_RD=="Constant" & input$p_type_RD=="Covariates") {
        
        for (ii in 1:no_covariate_available_RD() ) {
          time_spent[ii+1]<- all.RD.prediction()$ccov.prediction[[ii]]$time_spent
          num_max[ii+1]<- all.RD.prediction()$ccov.prediction[[ii]]$num.max.location
        }
        
      }
      
      
      if (input$phi_type_RD=="Covariates" & input$p_type_RD=="Constant") {
        
        for (ii in 1:no_covariate_available_RD() ) {
          time_spent[ii+1]<- all.RD.prediction()$covc.prediction[[ii]]$time_spent
          num_max[ii+1]<- all.RD.prediction()$covc.prediction[[ii]]$num.max.location
        }
        
      }
      
      
      if (input$phi_type_RD=="Covariates" & input$p_type_RD=="Covariates") {
        
        for (ii in 1:no_covariate_available_RD() ) {
          time_spent[ii+1]<- all.RD.prediction()$ccov.prediction[[ii]]$time_spent
          num_max[ii+1]<- all.RD.prediction()$ccov.prediction[[ii]]$num.max.location
        }
        
        for (ii in 1:no_covariate_available_RD() ) {
          time_spent[ii+1+no_covariate_available_RD()]<- all.RD.prediction()$covc.prediction[[ii]]$time_spent
          num_max[ii+1+no_covariate_available_RD()]<- all.RD.prediction()$covc.prediction[[ii]]$num.max.location
        }
        
        for (gg in 1:no_covariate_available_RD() ) {
          for (uu in 1:no_covariate_available_RD() ) {
            
          index<- (gg-1)*no_covariate_available_RD()+uu
          
          time_spent[index+1+2*no_covariate_available_RD()]<- all.RD.prediction()$covcov.prediction[[index]]$time_spent
          num_max[index+1+2*no_covariate_available_RD()]<- all.RD.prediction()$covcov.prediction[[index]]$num.max.location
        }}
       
        # recall
        # 1+2*num.cov+(gg-1)*num.cov+uu
        # covcov.prediction[[(gg-1)*num.cov+uu]] <- covcov.RD.prediction(covcov_phi = gg,covcov_p=uu)
         
      }
      
      if (input$phi_type_RD=="Time-varying" & input$p_type_RD=="Constant") {
        
        time_spent[2]<- all.RD.prediction()$tc.prediction[[1]]$time_spent
        num_max[2]<- all.RD.prediction()$tc.prediction[[1]]$num.max.location
        
      }


      if (input$phi_type_RD=="Time-varying" & input$p_type_RD=="Covariates") {
        
        #tc
        time_spent[2]<- all.RD.prediction()$tc.prediction[[1]]$time_spent
        num_max[2]<- all.RD.prediction()$tc.prediction[[1]]$num.max.location
        
        # cc, tc, tcov
        for (ii in 1:no_covariate_available_RD() ) {
          time_spent[2+ii]<- all.RD.prediction()$tcov.prediction[[ii]]$time_spent
          num_max[2+ii]<- all.RD.prediction()$tcov.prediction[[ii]]$num.max.location
        }
        
      }
      

      # chechinput to print out the max_logls 
      
      # print_logLs<-rep(c(""),length(Num))

      
    } else {
      
      # num_max vector, the number of times a model hitting the maximum logl
      
      Num<-all_list_model_tab_RD()$Num
      
      # model_name_vec_RD
      
      model_name_vec_RD<-all_list_model_tab_RD()$Model
      
      # cov_used_vec_RD
      
      p_cov_used<-all_list_model_tab_RD()$p_cov_used
      phi_cov_used<-all_list_model_tab_RD()$phi_cov_used
      
      # if reset or did not hit the run button
      time_spent<-rep(c(""),length(Num))
      
      num_max<- rep(c(""),length(Num))
      
      print_logLs<-rep(c(""),length(Num))
    }
    
    
    # Table to print out *** all_list_
    draft_all_model_ana_tab_RD<-data.frame(Num= Num,
                                           Model=model_name_vec_RD,
                                           phi_cov_used=phi_cov_used,
                                           p_cov_used=p_cov_used,
                                           time_spent=time_spent,
                                           num_max= num_max
                                           #print_logLs=print_logLs
                                           )
    
    } else { 
      
   draft_all_model_ana_tab_RD<- data.frame(Num= "",
                                           Model= "",
                                           phi_cov_used= "", 
                                           p_cov_used= "", 
                                           time_spent="",
                                           num_max= ""
                                          # print_logLs=""
                                           ) 
    }
    
    # returns one table element
    return(draft_all_model_ana_tab_RD)
    
  })
  
  
  
  output$all_ana_model_table_RD<-renderTable({
    
    # Create checkboxes
    #all_ana_model_table_RD()$all_ana_model_table_RD$print_max_logLs <- 
    #                        paste0('<label><input type="checkbox" id="car', 
    #                        1:6, '"> <span>') # 1:total_no_models()
                            #rownames(mymtcars), '</span></label>')
    
    # flex_table <- vanilla.table(d_all_ana_model_table_RD()$draft_all_model_ana_tab_RD) # convert to FlexTable objet
    
    # flex_table[, draft_all_model_ana_tab_RD$print_max_logLs(), to = "header"] <- parLeft() # left align checkboxes
    
    #  flex_table[, draft_all_model_ana_tab_RD$print_max_logLs()] <- parLeft() # left align header
    
    # all_model_ana_tab_RD<- flex_table
    
    flex_tab<-all_ana_model_table_RD()
    
    #flex_Num<-flex_tab$Num
    
    #null_values<-flex_tab$print_logLs
    
    #flex_tab$print_logLs<-  paste0('<label><input type="checkbox" id="flex_Num', 
    #                        1:total_no_fits_RD(), '"> <span>', # 1:total_no_models()
    #                        null_values, '</span></label>')
    
    #flex_table<- vanilla.table(flex_tab)

    #flex_table[, c("Num","Model"), to = "header"] <- parLeft() # left align the column "Num"
    #flex_table[, c("Num","Model")] <- parLeft() # left align header
    
    return(flex_tab)
  })
  
  
  



  ################################ (RD) predicted plot
  
  # set new reactive values of the number of hits of actionButton
  #  as impossible to reset value of an actionButton == 0
  
  run_button_counts_RD <- reactiveValues(counts = 0)
  
  # Each time (RD run) button is clicked, add 1 to reactive value of run_button_counts$counts
  observeEvent(input$run_ana_button_RD,
               {run_button_counts_RD$counts =  isolate(run_button_counts_RD$counts) +1 }
  )
  
  observeEvent(input$reset_ana_button_RD,
               {run_button_counts_RD$counts = 0}
  )
  # run_button_counts<-reactive({isolate(input$run_ana_button)
  #    })
  
  # default_plot <- eventReactive(input$reset_ana_button, {
  #    head(cars)
  # })
  
  # reset_run_button_counts<-observeEvent(input$reset_ana_button,
  #                                      isolate(run_ana_button)=0)
  
  ##### plot RD
  
  # show predicted data plot
  output$plot_RD <- renderPlot({
    
    # plot if hit run button 
    
    if (run_button_counts_RD$counts!=0) {
      
      data_plot<-print_plot_RD()
      
    } else {data_plot<- NULL}
    
    return(data_plot)
    
  })
  
  
  # print_plot_RD function
  
  print_plot_RD<-reactive({
    
    if (run_button_counts$counts>0) {
      
      # name the fitted counts results
      fitted.counts.tab<- isolate(fitted.results.RD()$estimated.counts)
      
      # isolate this to avoid reactive plot function
      data_plot<- plot(c(1:T_RD()),fitted.counts.tab$observed_data,
                       pch="+",
                       col = "black",
                       lwd=2,#pch=10,
                       xlab="sampling occasions",
                       ylab="counts",
                       main=paste("Predicted counts of individuals removed.")
      )
      
      
      lines(c(1:T_RD()), fitted.counts.tab$observed_data,col = "black")
      
      # fitted data
      points(c(1:T_RD()), fitted.counts.tab$estimated_data,pch=2, col = "red") # estimated data, red triangle
      lines(c(1:T_RD()), fitted.counts.tab$estimated_data,col = "red")
      
      # 95% CI
      #lines(c(1:T_RD()), fitted.counts.tab$estimated_data+fitted.counts.tab$lower_CI, col = "red", lwd=2,lty=3) # lower CI, dashed gray line
      #lines(c(1:T_RD()), fitted.counts.tab$estimated_data+fitted.counts.tab$upper_CI, col = "red", lwd=2,lty=3) # upper CI, dashed gray line
      
    } else {NULL}
    
    return(data_plot)
    
  })
  
  # download plot RD
  
  # download plot RD
  
  output$downloadPlot_RD <- downloadHandler(
    
    filename <- function() {
      paste('plot', 'png', sep = ".")
    },
    
    content <- function(file) {
      
      png(file) # add more arguments for pic
      
      # name the fitted counts results
      fitted.counts.tab<- isolate(fitted.results.RD()$estimated.counts)
      
      # isolate this to avoid reactive plot function
      data_plot<- plot(c(1:T_RD()),fitted.counts.tab$observed_data,
                       pch="+",
                       col = "black",
                       lwd=2,#pch=10,
                       xlab="sampling occasions",
                       ylab="counts",
                       main=paste("Predicted counts of individuals removed.")
      )
      
      
      lines(c(1:T_RD()), fitted.counts.tab$observed_data,col = "black")
      
      # fitted data
      points(c(1:T_RD()), fitted.counts.tab$estimated_data,pch=2, col = "red") # estimated data, red triangle
      lines(c(1:T_RD()), fitted.counts.tab$estimated_data,col = "red")
      
      # 95% CI
      #lines(c(1:T_RD()), fitted.counts.tab$estimated_data+fitted.counts.tab$lower_CI, col = "red", lwd=2,lty=3) # lower CI, dashed gray line
      #lines(c(1:T_RD()), fitted.counts.tab$estimated_data+fitted.counts.tab$upper_CI, col = "red", lwd=2,lty=3) # upper CI, dashed gray line
      
      
      #print(print_plot())
      
      dev.off()
    },
    contentType = "image/png"
    
  )
  
  
##  
  
  # predicted RD data and CI as a table
  
  download_predicted_data_RD<-reactive({
    
    # name the fitted RD counts results
    fitted.counts.tab<-fitted.results.RD()$estimated.counts
    
    predicted_data_CI_table<-data.frame(
      Sampling_occasion = c(1:T_RD()),
      Observed_data = fitted.counts.tab$observed_data,
      Predicted.counts.tab=fitted.counts.tab$estimated_data # add predicted column
      #Lower_CI=fitted.counts.tab$estimated_data+fitted.counts.tab$lower_CI, # lower 95% CI
      #Upper_CI=fitted.counts.tab$estimated_data+fitted.counts.tab$upper_CI
    )
    
    return(predicted_data_CI_table)
    
  })
  
  
  # Output: download predicted RD data and confidence interval
  output$downloadPredicted_RD <- downloadHandler(
    filename = function() {
      paste("predicted_counts",download_predicted_data_RD(), ".csv", sep = "")
    },
    content = function(file) {
      
      write.csv(download_predicted_data_RD(), file, row.names = FALSE)
    }
  )
  
  
  
  
  ################################  model comparison according to AIC
  output$fit_table_RD <- renderTable({
    
    data_all<- dataInput_RD()
    data1=data_all[,input$data_colnum_RD]
    
    output<- RD.model.tab()
    
    return(output)
  }) 
  
  
  ## download model comparison
  output$downloadModelCom_RD <- downloadHandler(
    filename = function() {
      paste("predicted_counts",RD.model.tab(), ".csv", sep = "")
    },
    content = function(file) {
      
      write.csv(RD.model.tab(), file, row.names = FALSE)
    }
  )
  
  
  ################################  estimates results
  output$estimates_RD <- renderTable({
    
    data_all<- dataInput_RD()
    data1=data_all[,input$data_colnum_RD]
    
    print<- fitted.results.RD()$estimates.tab.results
    
    return(print)
  })  

  ## download estimates_RD
  output$downloadEstimates_RD <- downloadHandler(
    filename = function() {
      paste("predicted_counts",fitted.results.RD()$estimates.tab.results, ".csv", sep = "")
    },
    content = function(file) {
      
      write.csv(fitted.results.RD()$estimates.tab.results, file, row.names = FALSE)
    }
  )
  

# the end of server function  
}




# Run the app -+-+-+-
shinyApp(ui = ui, server = server)



