cat(file = stderr(), "Shiny_Data.R", "\n")


#---------------------------------------------------------------------
precursor_to_peptide <- function(){
  cat(file = stderr(), "Function precursor_to_peptide...", "\n")
  
  #remove duplicate rows from df_peptide
  df_peptide <<- df_peptide[!duplicated(df_peptide),]
  
  cat(file = stderr(), "Function precursor_to_peptide...end", "\n")
}



#---------------------------------------------------------------------
adh_plot <- function(session, input, output){
  cat(file = stderr(), "Function adh_plot...", "\n")
  
  df_adh <- df_peptide[grep("ADH1_YEAST|ADH2_YEAST", df_peptide$PG.ProteinNames),]
  
  #using dplyr filter only columns that contain "PEP.Quantity" in the name
  df_adh <- df_adh |> dplyr::select(contains("PEP.Quantity"))
  adh_colnames <- colnames(df_adh)
  
  i=1
  for (name in adh_colnames){
    positions <- str_locate_all(name, "_")
    new_name <- substr(name, 1, (unlist(positions)[2]-1))
    adh_colnames[i] <- new_name
    i <- i +1
  }
  
  df <- data.frame(adh_colnames, colSums(df_adh, na.rm = TRUE))
  colnames(df) <- c("Sample", "Sum")
  
  adh_cv <- 100 * round(sd(df$Sum)/mean(df$Sum), digits = 3)
  adh_title <- str_c("ADH, CV = ", adh_cv, "%")
  
  #using ggplot greate barplot of adh_sum, then show the plot
  create_adh_plot <- reactive({
    ggplot2::ggplot(data = df, aes(x = Sample, y = Sum, fill = Sample)) +
      ggplot2::geom_bar(stat = "identity")+ 
      ggplot2::labs(title = adh_title, x = "Sample", y = "Total Intensity") +
      ggplot2::theme(legend.position="none") +
      ggplot2::theme(plot.title = element_text(hjust = 0.5, size = 16), 
                     axis.title = element_text(size = 12, color = "black"),
                     axis.text.x = element_text(size = 12, angle = 45, color = "black", vjust = 0.5),
                     axis.text.y = element_text(size = 12,  color = "black")
      ) 
  })
  
  output$adh_plot <- renderPlot({
    req(create_adh_plot())
    create_adh_plot()
  })
  
  
  cat(file = stderr(), "Function adh_plot...end", "\n\n")
}

#---------------------------------------------------------------------
intensity_plot <- function(session, input, output){
  cat(file = stderr(), "Function intensity_plot...", "\n")
  
  df <- df_peptide
  
  #using dplyr filter only columns that contain "PEP.Quantity" in the name
  df <- df |> dplyr::select(contains("PEP.Quantity"))
  df_colnames <- colnames(df)

  i=1
  for (name in df_colnames){
    positions <- str_locate_all(name, "_")
    new_name <- substr(name, 1, (unlist(positions)[2]-1))
    df_colnames[i] <- new_name
    i <- i +1
  }
  
  df_plot <- data.frame(df_colnames, colSums(df, na.rm = TRUE))
  colnames(df_plot) <- c("Sample", "Sum")
  
  no_adh <- df_plot[!grepl("ADH", df_plot$Sample),]
  
  
  plot_cv <- 100 * round(sd(no_adh$Sum)/mean(no_adh$Sum), digits = 3)
  plot_title <- str_c("Total Intensity, CV (no ADH) = ", plot_cv, "%")
  
  #using ggplot greate barplot of adh_sum, then show the plot
  create_intensity_plot <- reactive({
    ggplot2::ggplot(data = df_plot, aes(x = Sample, y = Sum, fill = Sample)) +
      ggplot2::geom_bar(stat = "identity")+ 
      ggplot2::labs(title = plot_title, x = "Sample", y = "Total Intensity") +
      ggplot2::theme(legend.position="none") +
      ggplot2::theme(plot.title = element_text(hjust = 0.5, size = 16), 
            axis.title = element_text(size = 12, color = "black"),
            axis.text.x = element_text(size = 12, angle = 45, color = "black", vjust = 0.5),
            axis.text.y = element_text(size = 12,  color = "black")
      ) 
  })
  
  output$intensity_plot <- renderPlot({
    req(create_intensity_plot())
    create_intensity_plot()
  })
  
  
  cat(file = stderr(), "Function intensity_plot...end", "\n\n")
}


#---------------------------------------------------------------------
protein_plot <- function(session, input, output){
  cat(file = stderr(), "Function protein_plot...", "\n")
  
  df <- df_peptide
  
  if (input$protein_accession != ""){
    df <- df[grep(input$protein_accession, df$PG.ProteinNames),]
  }
  
  if (input$protein_description != ""){
    df <- df[grep(input$protein_description, df$PG.ProteinDescriptions),]
  }
  
  if (input$peptide_sequence != ""){
    df <- df[grep(input$peptide_sequence, df$EG.ModifiedSequence),]
  }
  
  #using dplyr filter only columns that contain "PEP.Quantity" in the name
  df <- df |> dplyr::select(contains("PEP.Quantity"))
  df_colnames <- colnames(df)
  
  i=1
  for (name in df_colnames){
    positions <- str_locate_all(name, "_")
    new_name <- substr(name, 1, (unlist(positions)[2]-1))
    df_colnames[i] <- new_name
    i <- i +1
  }
  
  df_plot <- data.frame(df_colnames, colSums(df, na.rm = TRUE))
  colnames(df_plot) <- c("Sample", "Sum")
  
  no_adh <- df_plot[!grepl("ADH", df_plot$Sample),]
  
  
  plot_cv <- 100 * round(sd(no_adh$Sum)/mean(no_adh$Sum), digits = 3)
  plot_title <- str_c("Total Intensity, CV (no ADH) = ", plot_cv, "%")
  
  #using ggplot greate barplot of adh_sum, then show the plot
  create_intensity_plot <- reactive({
    ggplot2::ggplot(data = df_plot, aes(x = Sample, y = Sum, fill = Sample)) +
      ggplot2::geom_bar(stat = "identity")+ 
      ggplot2::labs(title = plot_title, x = "Sample", y = "Total Intensity") +
      ggplot2::theme(legend.position="none") +
      ggplot2::theme(plot.title = element_text(hjust = 0.5, size = 16), 
                     axis.title = element_text(size = 12, color = "black"),
                     axis.text.x = element_text(size = 12, angle = 45, color = "black", vjust = 0.5),
                     axis.text.y = element_text(size = 12,  color = "black")
      ) 
  })
  
  output$protein_plot <- renderPlot({
    req(create_intensity_plot())
    create_intensity_plot()
  })
  
  
  cat(file = stderr(), "Function protein_plot...end", "\n\n")
}