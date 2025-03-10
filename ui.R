cat(file = stderr(), "ui.R started", "\n")

source('Shiny_Libraries.R')


sidebar <- dashboardSidebar(disable = TRUE, width = 165,
                            useShinyjs(),
                            sidebarMenu(
                              menuItem("Load", tabName = "load", selected = TRUE)
                            )
)

body <- dashboardBody(
  useShinyjs(),
  tabItems(
    
    # Design 
    tabItem(tabName = "load",
            fluidRow(
              column(width = 2,
                     fluidRow(
                       box(title = "Load Files", status = "primary", solidHeader = TRUE, collapsible = FALSE, align = "left", width = 12, height = 700,
                           
                           tags$h4("1. Load Protocol..."),
                           fluidRow(align = "center", 
                                    shinyFilesButton('sfb_protocol_file', label = 'Load Protocol File', title = 'Please select protocol/sample file', multiple = FALSE,
                                      style = "color: #fff; background-color: #337ab7; border-color: #2e6da4; display:center")),
                           span(textOutput("protocol_file_name"), style = "color:blue; font-size:9px"), 
                           br(),
                           tags$h4("2. Load data..."),
                           fluidRow(align = "center", 
                                    shinyFilesButton('sfb_psm_file', label = 'Load PSM File', title = 'Please select tsv psm file', multiple = FALSE,
                                      style = "color: #fff; background-color: #337ab7; border-color: #2e6da4; display:center")),
                           span(textOutput("psm_file_name"), style = "color:blue; font-size:9px"), 
                           br(),
                           tags$h4("3. Prepare data..."),
                           fluidRow(align = "center", 
                                    actionButton("prepare_data", label = "Prepare Data", width = 150,
                                                 style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                           br(),
                           tags$h4("4. Create Excel..."),
                           fluidRow(align = "center", 
                                    textInput("excel_filename", label = "File Name", value = str_c("project_qualitative_", format(Sys.Date(), "%m%d%y"), ".xlsx"), width = 250)
                           ),
                           fluidRow(align = "center", 
                                    actionButton("create_excel", label = "Create Excel", width = 150,
                                        style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
                           )
                       )
                     )
                  ),
  
              column(width = 10,  
                     fluidRow(
                       tabBox(id="process_data", width = 12, height = 750,
                        
                        tabPanel("Peptide Data",
                                 fluidRow(

                                   column(width = 2,
                                          textInput("peptide_data_accession", label = "Accession", value = "", width = 300)
                                   ),
                                   column(width = 2,
                                          textInput("peptide_data_description", label = "Description", value = "", width = 300)
                                   ),
                                   column(width = 2,
                                          textInput("peptide_data_name", label = "Name", value = "", width = 300)
                                   ),
                                   column(width = 2,
                                          textInput("peptide_data_gene", label = "Genes", value = "", width = 300)
                                   ),
                                   column(width = 2,
                                          textInput("peptide_data_sequence", label = "Sequence", value = "", width = 300)
                                   ),
                                   column(width = 2,
                                          actionButton("peptide_data_table_apply", label = "Filter Table", width = 150,
                                                       style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                   )
                                 ),
                          fluidRow(                   
                            column(width =12, offset =0,
                              hr(),      
                              tags$head(tags$style("#peptide_table{color: blue; font-size: 12px;}")),
                              DT::dataTableOutput("peptide_table", width ='100%')
                           )
                         )
                        ),                              
                              
                        tabPanel("Protein data",
                              fluidRow(
 
                                   column(width = 2,
                                          textInput("protein_data_accession", label = "Accession", value = "", width = 300)
                                   ),
                                   column(width = 2,
                                          textInput("protein_data_description", label = "Description", value = "", width = 300)
                                   ),
                                   column(width = 2,
                                          textInput("protein_data_name", label = "Name", value = "", width = 300)
                                   ),
                                   column(width = 2,
                                          textInput("protein_data_gene", label = "Genes", value = "", width = 300)
                                   ),
                                   column(width = 2,
                                          actionButton("protein_data_table_apply", label = "Filter Table", width = 150,
                                                       style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                   )
                                 ),
                            fluidRow(         
                              column(width =12, offset =0,
                                hr(),
                                tags$head(tags$style("#protein_table{color: blue; font-size: 12px;}")),
                                DT::dataTableOutput("protein_table", width ='100%')
                              )
                            )
                          ),
                        
                        tabPanel("Plot: Total Intensity",
                                 column(width =12, offset =0,
                                        div(
                                          style = "position:relative",
                                          plotOutput("intensity_plot", width = "60vw", height = "80vh")
                                        )
                                 )
                        ),
                        
                        tabPanel("Plot: ADH",
                                 column(width =12, offset =0,
                                        div(
                                          style = "position:relative",
                                          plotOutput("adh_plot", width = "60vw", height = "80vh")
                                        )
                                 )
                        ),

                        tabPanel("Plot: Casein",
                                 column(width =12, offset =0,
                                        div(
                                          style = "position:relative",
                                          plotOutput("casein_plot", width = "60vw", height = "80vh")
                                        )
                                 )
                        ),
                        
                        tabPanel("Plot: Custom",
                           fluidRow(
                             column(width = 2,
                                    textInput("protein_accession", label = "Accession", value = "", width = 300)
                                    ),
                             column(width = 2,
                                    textInput("protein_description", label = "Description", value = "", width = 300)
                                    ),
                             column(width = 2,
                                    textInput("protein_name", label = "Name", value = "", width = 300)
                             ),
                             column(width = 2,
                                    textInput("protein_gene", label = "Genes", value = "", width = 300)
                             ),
                             column(width = 2,
                                    textInput("peptide_sequence", label = "Sequence", value = "", width = 300)
                                    ),
                             column(width = 2,
                                    actionButton("protein_plot_apply", label = "Show Plot", width = 150,
                                                 style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                    )
                                ),
                            fluidRow(
                                 column(width =12, offset =0,
                                        div(
                                          style = "position:relative",
                                          plotOutput("protein_plot", width = "40vw", height = "60vh")
                                        )
                                 ))
                        ),
                        
                        
                        tabPanel("Meta Data",
                                 fluidRow(         
                                   column(width =12, offset =0,
                                          hr(),
                                          tags$head(tags$style("#meta_table{color: blue; font-size: 12px;}")),
                                          DT::dataTableOutput("meta_table", width ='100%')
                                   )
                                 )
                        )
                        
                        
                        
                        
                        
                        
                        

              )
          )
      )
    )
    )
    
    #---------------------------------------------------------------------
    
    
  )
)


dashboardPage(
  dashboardHeader(title = "Duke Proteomics Qualitative Data Return", titleWidth = 325),
  sidebar,
  body
)
#end of ui.R