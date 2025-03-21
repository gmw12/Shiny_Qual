options(shiny.maxRequestSize = 4000*1024^2)
cat(file = stderr(), "server.R started", "\n")

#app_version <- '2025.01.08'
source("Shiny_Startup.R")

#set user if site_user does not exist
if (!exists("site_user")){
  set_user()
}

shinyServer(function(session, input, output) {
  cat(file = stderr(), "\n\n", "Shiny Server started ...1", "\n")
  showModal(modalDialog("Loading app...", footer = NULL))
  
  source("Shiny_Source.R")
  
  #set file choosers
  set_file_choosers(session, input, output, volumes)

  #------------------------------------------------------------------------------------------------------  
  #Load design file
  observeEvent(input$sfb_protocol_file, {
    cat(file = stderr(), "\n\n", "sfb_design_file button clicked...", "\n")
    
    if (is.list(input$sfb_protocol_file)) {
      
      protocol_sfb <- parseFilePaths(volumes, input$sfb_protocol_file)
      protocol_path <- str_extract(protocol_sfb$datapath, "^/.*/")
      protocol_filename <- protocol_sfb$datapath
      
      protocol_path <<- protocol_path
      protocol_filename <<- protocol_filename
      
      volumes <<- c(dd = protocol_path, volumes)
      
      set_file_choosers_data(session, input, output, volumes)
      
      output$protocol_file_name <- renderText({ stringr::str_c(basename(protocol_filename)) })
    }
    cat(file = stderr(), "\n\n", "sfb_design_file button clicked...end", "\n\n\n")
  })
  
  #------------------------------------------------------------------------------------------------------  
  #Load data file
  observeEvent(input$sfb_precursor_file, {
    
    cat(file = stderr(), "sfb_precursor_file button clicked...", "\n")
    
    if (is.list(input$sfb_precursor_file)) {
      showModal(modalDialog("Loading precursor data...", footer = NULL))
      
      precursor_sfb <- parseFilePaths(volumes, input$sfb_precursor_file)
      precursor_path <- str_extract(precursor_sfb$datapath, "^/.*/")
      precursor_filename <- precursor_sfb$datapath
      
      precursor_path <<- precursor_path
      precursor_filename <<- precursor_filename
      
      df_precursor <- data.table::fread(file = precursor_filename, header = TRUE, stringsAsFactors = FALSE, sep = "\t")
      
      output$precursor_file_name <- renderText({ stringr::str_c(basename(precursor_filename)) })
      
      precursor_prepare(df_precursor)
      
      create_data_table(session, input, output, "peptide")
      
      if (ptm==FALSE) {create_data_table(session, input, output, "protein")}
      
      intensity_plot(session, input, output)
      
      qc_plot(session, input, output, "adh")
      
      qc_plot(session, input, output, "casein")
      
      create_meta_table(session, input, output)
        
      removeModal()
    }
    
    cat(file = stderr(), "sfb_precursor_file button clicked...end", "\n\n\n")
  }) 


  #------------------------------------------------------------------------------------------------------  
  observeEvent(input$prepare_data, {
    showModal(modalDialog("Preparing Data...", footer = NULL))
    cat(file = stderr(), "prepare data clicked...", "\n")
    
    create_data_table(session, input, output, "peptide")

    if (ptm == FALSE) {create_data_table(session, input, output, "protein")}
    
    intensity_plot(session, input, output)
    
    qc_plot(session, input, output, "adh")
    
    qc_plot(session, input, output, "casein")
    
    create_meta_table(session, input, output)
    
    removeModal()
    cat(file = stderr(), "prepare data clicked...end", "\n\n\n")
  })
  
  #------------------------------------------------------------------------------------------------------  
  observeEvent(input$peptide_data_table_apply, {
    cat(file = stderr(), "peptide data table apply clicked...", "\n")
    
    create_data_table(session, input, output, "peptide")
    
    cat(file = stderr(), "peptide data table apply clicked...end", "\n")
  })
  
  #------------------------------------------------------------------------------------------------------  
  observeEvent(input$protein_data_table_apply, {
    cat(file = stderr(), "protein data table apply clicked...", "\n")
    
    create_data_table(session, input, output, "protein")
    
    cat(file = stderr(), "protein data table apply clicked...end", "\n")
  })
  
  #------------------------------------------------------------------------------------------------------  
  observeEvent(input$protein_plot_apply, {
    cat(file = stderr(), "protein_plot clicked...", "\n")
    
    protein_plot(session, input, output)
    
    cat(file = stderr(), "protein_plot clicked...end", "\n")
  })
    
  #------------------------------------------------------------------------------------------------------  
  observeEvent(input$create_excel, {
    showModal(modalDialog("Saving excel...", footer = NULL))
    
    cat(file = stderr(), "saving excel clicked...", "\n")
    
    bodyStyle <- createStyle(halign = "center", textDecoration = "bold", wrapText = TRUE)
    dataStyle <- createStyle(halign = "center")
    
    wb <- openxlsx::loadWorkbook(protocol_filename)
    
    filename <- str_c(protocol_path, input$excel_filename)
    
    nextsheet <- 2
    
    # addWorksheet(wb, "Table2 Raw precursor")
    # writeData(wb, sheet = nextsheet, df_precursor)
    # addStyle(wb, sheet = nextsheet, rows = 1, cols = 1:ncol(df_precursor), style = bodyStyle)
    # setColWidths(wb, sheet = nextsheet, cols = 1:5, widths = 20)
    
    #nextsheet <- nextsheet + 1
    cat(file = stderr(), "saving excel...peptide", "\n")
    addWorksheet(wb, "Table2 Peptides")
    writeData(wb, sheet = nextsheet, df_peptide)
    addStyle(wb, sheet = nextsheet, rows = 1, cols = 1:ncol(df_peptide), style = bodyStyle)
    if (ptm) {center_cols <- c(1,3,4,6,7,8,9,10,11)} else {center_cols <- c(1,3,4,6,7)}
    for (center_col in center_cols){
      addStyle(wb, sheet = nextsheet, rows = 2:nrow(df_peptide), cols=center_col, style = dataStyle)
    }
    setColWidths(wb, sheet = nextsheet, cols = c(1,3), widths = 20)
    setColWidths(wb, sheet = nextsheet, cols = c(4), widths = 15)
    setColWidths(wb, sheet = nextsheet, cols = c(2,5), widths = 30)
    
    #if df_protein exists add worksheet
    if(exists("df_protein")) {
      cat(file = stderr(), "saving excel...protein", "\n")
      nextsheet <- nextsheet + 1
      addWorksheet(wb, "Table3 Proteins")
      writeData(wb, sheet = nextsheet, df_protein)
      addStyle(wb, sheet = nextsheet, rows = 1, cols = 1:ncol(df_protein), style = bodyStyle)
      center_cols <- c(1,3,4,5)
      for (center_col in center_cols){
        addStyle(wb, sheet = nextsheet, rows = 2:nrow(df_peptide), cols=center_col, style = dataStyle)
      }
      setColWidths(wb, sheet = nextsheet, cols = c(1,3,5), widths = 20)
      setColWidths(wb, sheet = nextsheet, cols = c(4), widths = 15)
      setColWidths(wb, sheet = nextsheet, cols = c(2), widths = 30)
    }
    
    if(exists("df_peptide_ptm")) {
      cat(file = stderr(), "saving excel...peptide_ptm", "\n")
      nextsheet <- nextsheet + 1
      addWorksheet(wb, "Table4 Phos Peptides")
      writeData(wb, sheet = nextsheet, df_peptide_ptm)
      addStyle(wb, sheet = nextsheet, rows = 1, cols = 1:ncol(df_peptide_ptm), style = bodyStyle)
      setColWidths(wb, sheet = nextsheet, cols = c(1,3,4), widths = 20)
      setColWidths(wb, sheet = nextsheet, cols = c(2,5), widths = 30)
      if (ptm) {center_cols <- c(1,3,4,6,7,8,9,10,11)} else {center_cols <- c(1,3,4,6,7)}
      for (center_col in center_cols){
        addStyle(wb, sheet = nextsheet, rows = 2:nrow(df_peptide), cols=center_col, style = dataStyle)
      }
    }
    
    renameWorksheet(wb, 1, "Table1 SampleSheet")
    saveWorkbook(wb, filename, overwrite = TRUE)

    cat(file = stderr(), "saving excel clicked... end", "\n")
    
    removeModal()
  }) 
  


  
  removeModal()     
})
