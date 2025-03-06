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
                                    shinyFilesButton('sfb_protocol_file', label = 'Load Protocol File', title = 'Please select excel design file', multiple = FALSE,
                                      style = "color: #fff; background-color: #337ab7; border-color: #2e6da4; display:center")),
                           span(textOutput("protocol_file_name"), style = "color:blue; font-size:9px"), 
                           br(),
                           tags$h4("2. Load Peptide data..."),
                           fluidRow(align = "center", 
                                    shinyFilesButton('sfb_peptide_file', label = 'Load Peptide File', title = 'Please select tsv peptide file', multiple = FALSE,
                                      style = "color: #fff; background-color: #337ab7; border-color: #2e6da4; display:center")),
                           span(textOutput("peptide_file_name"), style = "color:blue; font-size:9px"), 
                           br(),
                           tags$h4("3. Load Protein data..."),
                           fluidRow(align = "center", 
                                    shinyFilesButton('sfb_protein_file', label = 'Select Protein File', title = 'Please select tsv protein file', multiple = FALSE,
                                      style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                           span(textOutput("protein_file_name"), style = "color:blue; font-size:9px"), 
                           br(),
                           tags$h4("4. Prepare data..."),
                           fluidRow(align = "center", 
                                    actionButton("prepare_data", label = "Prepare Data", width = 150,
                                                 style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                           br(),
                           tags$h4("5. Create Excel..."),
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
                                          textInput("peptide_data_gene", label = "Gene", value = "", width = 300)
                                   ),
                                   column(width = 2,
                                          textInput("peptide_data_accession", label = "Protein Accession", value = "", width = 300)
                                   ),
                                   column(width = 2,
                                          textInput("peptide_data_description", label = "Protein Description", value = "", width = 300)
                                   ),
                                   column(width = 2,
                                          textInput("peptide_data_sequence", label = "Modified Sequence", value = "", width = 300)
                                   ),
                                   column(width = 2,
                                          actionButton("peptide_data_table_apply", label = "Filter Table", width = 300,
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
                                          textInput("protein_data_gene", label = "Gene", value = "", width = 300)
                                   ),
                                   column(width = 2,
                                          textInput("protein_data_accession", label = "Protein Accession", value = "", width = 300)
                                   ),
                                   column(width = 2,
                                          textInput("protein_data_description", label = "Protein Description", value = "", width = 300)
                                   ),
                                   column(width = 2,
                                          textInput("protein_data_sequence", label = "Modified Sequence", value = "", width = 300)
                                   ),
                                   column(width = 2,
                                          actionButton("protein_data_table_apply", label = "Filter Table", width = 300,
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

                        tabPanel("Plot: Protein",
                           fluidRow(
                             column(width = 2,
                                    textInput("protein_gene", label = "Gene", value = "", width = 300)
                             ),
                             column(width = 2,
                                    textInput("protein_accession", label = "Protein Accession", value = "", width = 300)
                                    ),
                             column(width = 2,
                                    textInput("protein_description", label = "Protein Description", value = "", width = 300)
                                    ),
                             column(width = 2,
                                    textInput("peptide_sequence", label = "Modified Sequence", value = "", width = 300)
                                    ),
                             column(width = 2,
                                    actionButton("protein_plot_apply", label = "Show Plot", width = 300,
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
                        )

                       )
            )
      )
    )
    ),
    
    #Parameters
    tabItem(tabName = "samples",
            fluidRow(
              column(width = 2,
                     fluidRow(
                       box(id = "sample_box", title = "Process Biocrates Data...", status = "primary", solidHeader = TRUE, collapsible = FALSE, align = "left", width = 12, height = 750,
                           selectInput(inputId = "material_select", label = h4("Material for Data Output", style="color:blue"),  choices = c("1", "2", "3"), 
                                       selected = "1"),
                           hr(),
                           tags$h4("Normalize Data", style="color:blue"),
                           selectInput(inputId = "norm_select", label = h4("Select Normalization Type"),  choices = c("None", "SPQC", "NIST", "Golden West"), 
                                       selected = "None"),
                           hr(),
                           tags$h4("Filter Data", style="color:blue"),
                           checkboxInput("spqc_filter", label = "SPQC %CV Filter", value = FALSE),
                           numericInput("spqc_filter_value", label = "Max %CV", value = 30, min = 0, max = 100),
                           checkboxInput("missing_filter", label = "Max% <LOD values", value = FALSE),
                           numericInput("missing_filter_value", label = "Max % <LOD", value = 50, min = 0, max = 100),
                           br(),
                           hr(),
                           actionButton("process_material", label = "Process Data", width = 150,
                                        style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
                           
                       )
                       
                     )),
              
              column(width = 10,  
                     fluidRow(
                       tabBox(id="explore_data", width = 12, height = 750,
                              
                              tabPanel("Data",
                                       column(width =12, offset =0,
                                              hr(),
                                              tags$head(tags$style("#material_table{color: blue; font-size: 12px;}")),
                                              DT::dataTableOutput("material_table", width ='100%')
                                       )
                              ),
                              
                              tabPanel("Normalized Data",
                                       column(width =12, offset =0,
                                              hr(),
                                              tags$head(tags$style("#norm_material_table{color: blue; font-size: 12px;}")),
                                              DT::dataTableOutput("norm_material_table", width ='100%')
                                       )
                              ),
                              
                              tabPanel("Filtered Data",
                                       column(width =12, offset =0,
                                              hr(),
                                              tags$head(tags$style("#filter_material_table{color: blue; font-size: 12px;}")),
                                              DT::dataTableOutput("filter_material_table", width ='100%')
                                       )
                              ),
                              
                              tabPanel("SPQC Data",
                                       column(width =12, offset =0,
                                              hr(),
                                              tags$head(tags$style("#spqc_material_table{color: blue; font-size: 12px;}")),
                                              DT::dataTableOutput("spqc_material_table", width ='100%')
                                       )
                              ),
                              
                              tabPanel("SPQC Barplot",
                                       column(width =12, offset =0,
                                              hr(),
                                              imageOutput("spqc_bar")
                                       )
                              ),
                              
                              tabPanel("SPQC Boxplot",
                                       column(width =12, offset =0,
                                              hr(),
                                              imageOutput("spqc_box")
                                       )
                              ),
                              
                              tabPanel("Norm Factor",
                                       column(width =12, offset =0,
                                              hr(),
                                              imageOutput("spqc_line")
                                       )
                              ),
                              
                              
                              tabPanel("PCA",
                                       column(width =3, offset =0,
                                              radioButtons("data_type", label = h4("Data Type", style="color:blue"),
                                                           choices = list("Unfiltered" = 1, "Filtered" = 2), 
                                                           selected = 1),
                                              br(),
                                              radioButtons("norm_option", label = h4("Show Normalized?", style="color:blue"),
                                                           choices = list("Raw" = 1, "Normalized" = 2), 
                                                           selected = 1),
                                              br(),
                                              tags$h4("Misc. Options", style="color:blue"),
                                              checkboxInput("pca_by_plate", label = "Separate by Plate?", value = FALSE),
                                              checkboxInput("pca_spqc_only", label = "Show only SPQC?", value = FALSE),
                                              checkboxInput("remove_gw_nist", label = "Remove NIST/Golden West?", value = FALSE),
                                              br(),
                                              actionButton("create_pca", label = "Create PCA", width = 150,
                                                           style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                              hr(),
                                              br(),
                                              br(),
    
                                              dropdownButton(
                                                selectInput("stats_pca2d_x", label = "pca xaxis", choices = list("PC1", "PC2", "PC3", "PC4", "PC5"), 
                                                            selected = "PC1"),
                                                selectInput("stats_pca2d_y", label = "pca yaxis", choices = list("PC1", "PC2", "PC3", "PC4", "PC5"), 
                                                            selected = "PC2"),
                                                textInput("stats_pca2d_title", label="plot title", value = "pca2d", width = 200),
                                                sliderInput("stats_pca2d_label_size", label = h5("Label Size"), min = 1, 
                                                            max = 50, value = 11),
                                                sliderInput("stats_pca2d_title_size", label = h5("Title Size"), min = 10, 
                                                            max = 50, value = 20),
                                                sliderInput("stats_pca2d_dot_size", label = h5("Point Size"), min = 1, 
                                                            max = 10, value = 2),
                                                circle = TRUE, status = "danger", icon = icon("cogs"), width = "300px", size = "sm",
                                                tooltip = tooltipOptions(title = "Click to see inputs !")
                                              )),
                                           column(width =9, offset =0,
                                                div(
                                                  style = "position:relative",
                                                  plotOutput("stats_pca2d", width = 800, height = 600,
                                                             hover = hoverOpts("plot_pca2d_hover", delay = 100, delayType = "debounce")),
                                                  uiOutput("hover_pca2d_info")
                                                ),
                                                downloadButton('download_stats_pca2d')
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