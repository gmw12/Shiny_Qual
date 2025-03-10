cat(file = stderr(), "Shiny_Data.R", "\n")


#---------------------------------------------------------------------
precursor_prepare <- function(df_psm){
  cat(file = stderr(), "Function precursor_prepare...", "\n")
  
  if (any(grepl("EG.PTMProbabilities", names(df_psm)))) {
    ptm <- TRUE
  }else {
    ptm <- FALSE
  }
  
  ptm <<- ptm
  
  df_meta <<- data.frame("psm" = nrow(df_psm))
  
  #select columns and clean
  if(ptm){
    df_psm <- precursor_to_precursor_ptm_bg(df_psm)
    df_psm <<- df_psm
    df_peptide <- rollup_sum(df_psm, "peptide")
    df_peptide <<- df_peptide
    df_peptide_ptm <- df_peptide[grep("Phospho", df_peptide$Sequence),]
    df_peptide_ptm <- df_peptide_ptm[df_peptide_ptm$Local2 =="Y",]
    df_peptide_ptm <<- df_peptide_ptm
  }else{
    df_psm <- precursor_to_precursor_bg(df_psm)
    df_psm <<- df_psm
    df_peptide <- rollup_sum(df_psm, "peptide")
    df_peptide <<- df_peptide
    df_protein <- rollup_sum(df_peptide, "protein")
    df_protein <<- df_protein
  }
  
  df_meta$peptide <<- nrow(df_peptide)
  if(exists("df_protein")) {df_meta$protein <<- nrow(df_protein)}
  if(exists("df_peptide_ptm")) {df_meta$peptide_ptm <<- nrow(df_peptide_ptm)}
  df_meta$sample_number <<- sample_number
  
  
  cat(file = stderr(), "Function precursor_prepare...end", "\n")
}

#----------------------------------------------------------------------------------------
precursor_to_precursor_bg <- function(df_psm){
  cat(file = stderr(), "Function precursor_to_precursor_bg", "\n")
  
  df_colnames <- c("Accession", "Description", "Name", "Genes", "Organisms", "Sequence", "PrecursorId", "PeptidePosition")  
  n_col <- length(df_colnames)
  
  df_info <- df_psm |> dplyr::select(contains('ProteinAccessions'), contains('ProteinDescriptions'), contains('ProteinNames'), contains('Genes'), contains('Organisms'),
                            contains('ModifiedSequence'), contains('PrecursorId'), contains('PeptidePosition'))
  df_data <- df_psm |> dplyr::select(contains("EG.TotalQuantity"))
  
  data_colnames <- colnames(df_data)
  i=1
  for (name in data_colnames){
    first_space <- unlist(str_locate(name, " "))[1]
    name <- substr(name, first_space+1, nchar(name))
    positions <- unlist(str_locate_all(name, "_"))[2]
    new_name <- substr(name, 1, positions-1)
    data_colnames[i] <- str_c(new_name, "_TotalQuantity")
    i <- i +1
  }
  
  colnames(df_data) <- data_colnames
  
  df_data[df_data ==  "Filtered"] <- 0
  df_data[is.na(df_data)] <- 0  
  df_data[df_data == "NaN"] <- 0
  df_data <- as.data.frame(lapply(df_data, as.numeric))
  df_data <- round(df_data, digits = 0)

  sample_number <- ncol(df_data) 
  
  colnames(df_info) <- df_colnames  

  df_info$Description <- stringr::str_c(df_info$Description, ", org=", df_info$Organisms) 
  df_info$Organisms <- NULL
  
  df_psm <- cbind(df_info, df_data)
  sample_number <<- sample_number
  
  cat(file = stderr(), "precursor_to_precursor_bg... complete", "\n\n")
  return(df_psm)
}


#--------------------------------------------------------------------------------
rollup_sum <- function(df, rollup_type){
  cat(file = stderr(), "function rollup_sum...", "\n")
  
  if (rollup_type == "peptide" & ptm == FALSE){
    cat(file = stderr(), "function rollup_type = peptide", "\n")
    df_peptide <- df |> dplyr::select("Accession", "Description", "Name", "Genes", "Sequence", "PeptidePosition", contains("TotalQuantity"))
    df_peptide <- tibble::add_column(df_peptide, "Precursors"=1, .after="PeptidePosition")
    df_peptide <- df_peptide |> dplyr::group_by(Accession, Description, Name, Genes, Sequence, PeptidePosition) |> dplyr::summarise_all(list(sum))
    df_peptide <- data.frame(dplyr::ungroup(df_peptide))
    
    df_peptide$sum <- rowSums(df_peptide[,(ncol(df_peptide)-sample_number+1):ncol(df_peptide)], na.rm = TRUE)
    df_peptide <- df_peptide[order(df_peptide$sum, decreasing = TRUE),]
    df_peptide <- df_peptide |> dplyr::select(-sum)
    
    cat(file = stderr(), "function rollup_sum...end", "\n\n")
    return(df_peptide)
    
  }else if (rollup_type == "peptide" & ptm == TRUE){
    cat(file = stderr(), "function rollup_type = peptide_ptm", "\n")
    df_peptide <- df |> dplyr::select("Accession", "Description", "Name", "Genes", "Sequence", "PeptidePosition", "PTM_Loc", "Protein_PTM_Loc",  contains("TotalQuantity"))
    df_peptide <- tibble::add_column(df_peptide, "Precursors"=1, .after="PeptidePosition")
    df_peptide <- df_peptide |> dplyr::group_by(Accession, Description, Name, Genes, Sequence, PeptidePosition, PTM_Loc, Protein_PTM_Loc) |> dplyr::summarise_all(list(sum))
    df_peptide <- data.frame(dplyr::ungroup(df_peptide))
    df_local <- rollup_local(df)
    #sort df_peptide by sequence
    df_peptide <- df_peptide[order(df_peptide$Sequence),]
    df_local <- df_local[order(df_local$Sequence),]
    #drop sequence column from df_local
    df_local <- df_local |> dplyr::select(-Sequence)
    df_peptide_info <- df_peptide |> dplyr::select("Accession", "Description", "Name", "Genes", "Sequence", "PeptidePosition", "Precursors", "PTM_Loc", "Protein_PTM_Loc")
    df_peptide_data <- df_peptide |> dplyr::select(contains("TotalQuantity"))
    df_peptide <- cbind(df_peptide_info, df_local, df_peptide_data)
    
    df_peptide$sum <- rowSums(df_peptide[,(ncol(df_peptide)-sample_number+1):ncol(df_peptide)], na.rm = TRUE)
    df_peptide <- df_peptide[order(df_peptide$sum, decreasing = TRUE),]
    df_peptide <- df_peptide |> dplyr::select(-sum)
    df_peptide <- df_peptide[order(df_peptide$Local2, decreasing = TRUE),]
    
    
    cat(file = stderr(), "function rollup_sum...end", "\n\n")
    return(df_peptide)
    
  }else if (rollup_type == "protein"){
    cat(file = stderr(), "function rollup_type = protein", "\n")
    df_protein <- df |> dplyr::select("Accession", "Description", "Name", "Genes", contains("TotalQuantity"))
    df_protein <- tibble::add_column(df_protein, "Peptides"=1, .after="Genes")
    df_protein <- df_protein |> dplyr::group_by(Accession, Description, Name, Genes) |> dplyr::summarise_all(list(sum))
    df_protein <- data.frame(dplyr::ungroup(df_protein))
    
    df_protein$sum <- rowSums(df_protein[,(ncol(df_protein)-sample_number+1):ncol(df_protein)], na.rm = TRUE)
    df_protein <- df_protein[order(df_protein$sum, decreasing = TRUE),]
    df_protein <- df_protein |> dplyr::select(-sum)
    
    cat(file = stderr(), "function rollup_sum...end", "\n\n")
    return(df_protein)
  }
}


#------------------------------------------------------------
rollup_local <- function(localized_data) {
  cat(file = stderr(), "Function rollup_local...", "\n")

  str_to_numlist <- function(str_in) {
    num_out <- strsplit(str_in, ",") |> unlist() |> as.numeric()
    return(num_out)
  }
  
  require(foreach)
  require(doParallel)
  cores <- detectCores()
  cl <- makeCluster(cores - 2)
  registerDoParallel(cl)
  
  local_unique <- data.frame(unique(localized_data$Sequence))
  local_unique$Local <- ""
  local_unique$Local2 <- ""
  colnames(local_unique) <- c("Sequence", "Local", "Local2")
  
  #for (i in (1:nrow(local_unique))) {
  parallel_result <- foreach(i = 1:nrow(local_unique), .combine = rbind) %dopar% { 
    if(grepl("Phospho", local_unique$Sequence[i])) {
      
      test_df <- localized_data[localized_data$Sequence == local_unique$Sequence[i],]
      
      if(nrow(test_df) > 1) {
        first_value <- TRUE
        for (r in (1:nrow(test_df))) {
          if (first_value) { 
            temp1 <- str_to_numlist(test_df$Local[r])
            if (!is.na(temp1[[1]])) {
              first_value <- FALSE
            }
          }else {
            temp2 <- str_to_numlist(test_df$Local[r])
            if (!is.na(temp2[[1]])) {
              temp1 <- pmax(temp1, temp2)
            }
          }
        }
      }else {
        temp1 <- str_to_numlist(test_df$Local[1])
      }
      
      if (max(temp1) >= 0.75) {
        if (min(temp1) >= 0.75) {
          local2 <- "Y"
        } else {
          local2 <- "P"
        }
      }else {
        local2 <- "N"
      }
      
      #local_unique$Local[i] <- list(temp1) 
      #local_unique$Local2[i] <- local2
      
    }else {
      temp1 <- ""
      local2 <- ""
    }
    
    #local_unique$Local[i] <- list(temp1) 
    #local_unique$Local2[i] <- local2
    list(temp1, local2)
  }
  
  stopCluster(cl) 
  
  parallel_result <- data.frame(parallel_result)
  row.names(parallel_result) <- NULL
  parallel_result <- cbind(local_unique$Sequence, parallel_result)
  colnames(parallel_result) <- c("Sequence", "Local", "Local2")
  
  numlist_to_string <- function(x) {
    return(toString(paste(unlist(x$Local) |> as.character() |> paste(collapse = ","))))
  }
  
  numlist_to_string2 <- function(x) {
    return(toString(paste(unlist(x$Local2) |> as.character() |> paste(collapse = ","))))
  }
  
  parallel_result$Local <- apply(parallel_result, 1, numlist_to_string)
  parallel_result$Local2 <- apply(parallel_result, 1, numlist_to_string2)
  
  cat(file = stderr(), "Function rollup_local...end", "\n")
  return(parallel_result)
}

#------------------------------------------------------------


#---------------------------------------------------------------------
qc_plot <- function(session, input, output, type){
  cat(file = stderr(), "Function qc_plot...", "\n")
  
  if (type == "adh") { df <- df_peptide[grep("P00330", df_peptide$Accession),] }
  
  if (type == "casein") { df <- df_peptide[grep("P02662|P02663", df_peptide$Accession),] }
  
  #using dplyr filter only columns that contain "PEP.Quantity" in the name
  df <- df |> dplyr::select(contains("TotalQuantity"))
  qc_colnames <- colnames(df)
  
  i=1
  for (name in qc_colnames){
    positions <- str_locate_all(name, "_")
    new_name <- substr(name, 1, (unlist(positions)[2]-1))
    qc_colnames[i] <- new_name
    i <- i +1
  }
  
  df_plot <- data.frame(qc_colnames, colSums(df, na.rm = TRUE))
  colnames(df_plot) <- c("Sample", "Sum")
  
  df_plot <- df_plot[!grepl("ADH", df_plot$Sample),] 
  
  qc_cv <- 100 * round(sd(df_plot$Sum)/mean(df_plot$Sum), digits = 3)
  df_meta$adh_cv <<- round(qc_cv, digits = 3)
  
  if (type == "adh") {qc_title <- str_c("ADH, CV = ", qc_cv, "%") }
  
  if (type == "casein") {qc_title <- str_c("Casein, CV = ", qc_cv, "%") }
  
  #using ggplot greate barplot of adh_sum, then show the plot
  create_qc_plot <- reactive({
    ggplot2::ggplot(data = df_plot, aes(x = Sample, y = Sum, fill = Sample)) +
      ggplot2::geom_bar(stat = "identity")+ 
      ggplot2::labs(title = qc_title, x = "Sample", y = "Total Intensity") +
      ggplot2::theme(legend.position="none") +
      ggplot2::theme(plot.title = element_text(hjust = 0.5, size = 16), 
                     axis.title = element_text(size = 12, color = "black"),
                     axis.text.x = element_text(size = 12, angle = 45, color = "black", vjust = 0.5),
                     axis.text.y = element_text(size = 12,  color = "black")
      ) 
  })
  
  if (type == "adh") {
    output$adh_plot <- renderPlot({
      req(create_qc_plot())
      create_qc_plot()}
    )}

  if (type == "casein") {
    output$casein_plot <- renderPlot({
      req(create_qc_plot())
      create_qc_plot()}
    )}
  
  cat(file = stderr(), "Function qc_plot...end", "\n\n")
}

#---------------------------------------------------------------------
intensity_plot <- function(session, input, output){
  cat(file = stderr(), "Function intensity_plot...", "\n")
  
  df <- df_peptide
  
  #using dplyr filter only columns that contain "PEP.Quantity" in the name
  df <- df |> dplyr::select(contains("TotalQuantity"))
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
  
  df_plot <- df_plot[!grepl("ADH", df_plot$Sample),]
  
  plot_cv <- 100 * round(sd(df_plot$Sum)/mean(df_plot$Sum), digits = 3)
  df_meta$total_intensity_cv <<- round(plot_cv, digits = 3)
  plot_title <- str_c("Total Intensity, CV = ", plot_cv, "%")
  
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
  
  if (input$protein_gene != ""){
    df <- df[grep(input$protein_gene, df$Genes),]
  }
  
  if (input$protein_accession != ""){
    df <- df[grep(input$protein_accession, df$Accession),]
  }
  
  if (input$protein_name != ""){
    df <- df[grep(input$protein_accession, df$Name),]
  }
  
  if (input$protein_description != ""){
    df <- df[grep(input$protein_description, df$Description),]
  }
  
  if (input$peptide_sequence != ""){
    df <- df[grep(input$peptide_sequence, df$Sequence),]
  }
  
  #using dplyr filter only columns that contain "PEP.Quantity" in the name
  df <- df |> dplyr::select(contains("TotalQuantity"))
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
  
  df_plot<- df_plot[!grepl("ADH", df_plot$Sample),]
  
  plot_cv <- 100 * round(sd(df_plot$Sum)/mean(df_plot$Sum), digits = 3)
  plot_title <- str_c("Total Intensity, CV  = ", plot_cv, "%")
  
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

#---------------------------------------------------------------------
filter_protein_table <- function(session, input, output){
  cat(file = stderr(), "Function filter protein table...", "\n")
  
  df <- df_protein
  
  if (input$protein_data_accession != ""){
    df <- df[grep(input$protein_data_accession, df$Accession),]
  }
  
  if (input$protein_data_gene != ""){
    df <- df[grep(input$protein_data_gene, df$Genes),]
  }
  
  if (input$protein_data_name != ""){
    df <- df[grep(input$protein_data_name, df$Name),]
  }
  
  if (input$protein_data_description != ""){
    df <- df[grep(input$protein_data_description, df$PG.Description),]
  }

  cat(file = stderr(), "Function filter protein table...", "\n\n\n")
  return(df)
}

#---------------------------------------------------------------------
filter_peptide_table <- function(session, input, output){
  cat(file = stderr(), "Function filter peptide table...", "\n")
  
  df <- df_peptide
  
  if (input$peptide_data_accession != ""){
    df <- df[grep(input$peptide_data_accession, df$Accession),]
  }
  
  if (input$peptide_data_gene != ""){
    df <- df[grep(input$peptide_data_gene, df$Genes),]
  }
  
  if (input$peptide_data_name != ""){
    df <- df[grep(input$peptide_data_name, df$Name),]
  }
  
  if (input$peptide_data_description != ""){
    df <- df[grep(input$peptide_data_description, df$Description),]
  }
  
  if (input$peptide_data_sequence != ""){
    df <- df[grep(input$peptide_data_sequence, df$Sequence),]
  }
  
  cat(file = stderr(), "Function filter peptide table...", "\n\n\n")
  return(df)
}


#---------------------------------------------------------------------
#----------------------------------------------------------------------------------------
precursor_to_precursor_ptm_bg <- function(df){
  cat(file = stderr(), "Function precursor_to_precursor_ptm_bg", "\n")
  
  df_phos_prob <- df |> dplyr::select(contains('PTMProbabilities')) 
  df_phos_prob <- df_phos_prob |> dplyr::select(contains('Phospho')) 
  
  df_colnames <- c("Accession", "Description", "Name", "Genes", "Organisms", "Sequence", "PrecursorId", "PeptidePosition", "ProteinPTMLocations")  
  n_col <- length(df_colnames)
  
  df_info <- df |> dplyr::select(contains('ProteinAccessions'), contains('ProteinDescriptions'), contains('ProteinNames'), contains('Genes'), contains('Organisms'),
                            contains('ModifiedSequence'), contains('PrecursorId'), contains('PeptidePosition'),contains('ProteinPTMLocations'))
  
  df_data <- df |> dplyr::select(contains("TotalQuantity"))
  
  data_colnames <- colnames(df_data)
  i=1
  for (name in data_colnames){
    first_space <- unlist(str_locate(name, " "))[1]
    name <- substr(name, first_space+1, nchar(name))
    positions <- unlist(str_locate_all(name, "_"))[2]
    new_name <- substr(name, 1, positions-1)
    data_colnames[i] <- str_c(new_name, "_TotalQuantity")
    i <- i +1
  }
  
  colnames(df_data) <- data_colnames
  
  df_data[df_data ==  "Filtered"] <- 0
  df_data[is.na(df_data)] <- 0  
  df_data[df_data == "NaN"] <- 0
  df_data <- as.data.frame(lapply(df_data, as.numeric))
  df_data <- round(df_data, digits = 0)
  
  sample_number <- ncol(df_data) 
  sample_number <<- sample_number
  
  colnames(df_info) <- df_colnames  
  
  df <- cbind(df_info, df_data)
  
  df$Description <- stringr::str_c(df$Description, ", org=", df$Organisms) 
  df$Organisms <- NULL
  
  phos_which <- which(grepl("Phospho", df$Sequence))
  df_phos <- df[phos_which,]
  df_phos_prob <- df_phos_prob[phos_which,]
  df_other <- df[-phos_which,]
  
  local_df <- data.frame(localize_summary(df_phos, df_phos_prob))
  df_phos <- tibble::add_column(df_phos, "Protein_PTM_Loc" = local_df$Protein_PTM_Loc, .after="PrecursorId")
  df_phos <- tibble::add_column(df_phos, "PTM_Loc" = local_df$PTM_Loc, .after="PrecursorId")
  df_phos <- tibble::add_column(df_phos, "Local2" = local_df$Local2, .after="PrecursorId")
  df_phos <- tibble::add_column(df_phos, "Local" = local_df$Local, .after="PrecursorId")
  
  df_other <- tibble::add_column(df_other, "Protein_PTM_Loc"= "" , .after="PrecursorId")
  df_other <- tibble::add_column(df_other, "PTM_Loc" = "", .after="PrecursorId")
  df_other <- tibble::add_column(df_other, "Local2"= "" , .after="PrecursorId")
  df_other <- tibble::add_column(df_other, "Local" = "", .after="PrecursorId")
  
  df <- rbind(df_phos, df_other)

  cat(file = stderr(), "precursor_to_precursor_ptm_bg complete", "\n\n")
  return(df)
}

#----------------------------------------------------------------------------------------
localize_summary <- function(df_phos, df_phos_prob){
  cat(file = stderr(), "Function localize_summary...", "\n")
  
  require(foreach)
  require(doParallel)
  cores <- detectCores()
  cl <- makeCluster(cores - 2)
  registerDoParallel(cl)
  
  #Step 1 consolicate localization into one list of max local for each position
  #create df of just probabilities
  df_phos_prob[df_phos_prob=="Filtered"] <- ""
  
  df_local <- data.frame(cbind(df_phos$Sequence, df_phos$PeptidePosition, df_phos$ProteinPTMLocations))
  colnames(df_local) <- c("ModSequence", "PeptidePosition", "ProteinPTMLocations")
  
  df_local$Stripped <- gsub("\\[.*?\\]", "", df_local$ModSequence)
  df_local$Stripped <- gsub("_", "", df_local$Stripped)
  
  #Step 2 reduce modified sequence to STY with phos residue marked with *
  df_local$phos_seq <- gsub("\\[Phospho \\(STY\\)\\]", "*", df_local$ModSequence)
  df_local$phos_seq <- gsub("_", "", df_local$phos_seq)
  df_local$phos_seq <- gsub("\\[.*?\\]", "", df_local$phos_seq)
  df_local$phos_seq <- gsub("[^STY*]", "", df_local$phos_seq)
  
  
  #new step
  df_local$ModSequence2 <- df_local$ModSequence
  df_local$ModSequence2 <- gsub("S\\[Phospho \\(STY\\)\\]", "s", df_local$ModSequence2)
  df_local$ModSequence2 <- gsub("T\\[Phospho \\(STY\\)\\]", "t", df_local$ModSequence2)
  df_local$ModSequence2 <- gsub("Y\\[Phospho \\(STY\\)\\]", "y", df_local$ModSequence2)
  df_local$ModSequence2 <- gsub("\\[.*?\\]", "", df_local$ModSequence2)
  df_local$ModSequence2 <- gsub("_", "", df_local$ModSequence2)
  df_local$ModSequence2 <- gsub("\\[.*?\\]", "", df_local$ModSequence2)
  df_local$ModSequence2 <- gsub("_", "", df_local$ModSequence2)
  
  
  #new step
  df_local$PTM_Loc <- ""
  
  for (r in (1:nrow(df_local))) {
    find_s <- unlist(stringr::str_locate_all(df_local$ModSequence2[r], "s"))
    find_t <- unlist(stringr::str_locate_all(df_local$ModSequence2[r], "t"))
    find_y <- unlist(stringr::str_locate_all(df_local$ModSequence2[r], "y"))
    
    
    if (length(find_s) > 0) {
      find_s <- unlist(stringr::str_split(paste("S", find_s, collapse = " ", sep = ""), pattern=" "))
      find_s <- find_s[1:(length(find_s)/2)]
    }else{
      find_s <- ""
    }
    
    
    if (length(find_t) > 0) {
      find_t <- unlist(stringr::str_split(paste("T", find_t, collapse = " ", sep = ""), pattern=" "))
      find_t <- find_t[1:(length(find_t)/2)]
    }else{
      find_t <- ""
    }
    
    if (length(find_y) > 0) {
      find_y <- unlist(stringr::str_split(paste("Y", find_y, collapse = " ", sep = ""), pattern=" "))
      find_y <- find_y[1:(length(find_y)/2)]
    }else{
      find_y <- ""
    }
    
    final_all <- c(find_s, find_t, find_y)
    final_all <- final_all[final_all != ""]
    final_all <- paste(final_all, collapse = ",", sep = ",")
    df_local$PTM_Loc[r] <- final_all  
  }
  
  
  #new step
  df_local$Protein_PTM_Loc <- gsub("([CM][0-9]+)", "", df_local$ProteinPTMLocations) 
  df_local$Protein_PTM_Loc <- gsub("\\(,", "\\(",  df_local$Protein_PTM_Loc) 
  df_local$Protein_PTM_Loc <- gsub("\\),", "\\)",  df_local$Protein_PTM_Loc) 
  df_local$Protein_PTM_Loc <- gsub(",\\(", "\\(",  df_local$Protein_PTM_Loc) 
  df_local$Protein_PTM_Loc <- gsub(",\\)", "\\)",  df_local$Protein_PTM_Loc) 
  df_local$Protein_PTM_Loc <- gsub("\\(", "",  df_local$Protein_PTM_Loc)  
  df_local$Protein_PTM_Loc <- gsub("\\)", "",  df_local$Protein_PTM_Loc)  
  df_local$Protein_PTM_Loc <- gsub(",,,,", ",",  df_local$Protein_PTM_Loc)  
  df_local$Protein_PTM_Loc <- gsub(",,,", ",",  df_local$Protein_PTM_Loc) 
  df_local$Protein_PTM_Loc <- gsub(",,", ",",  df_local$Protein_PTM_Loc)  
  
  # determines residue location for phos on sequence reduced to STY
  parallel_result1 <- foreach(r = 1:nrow(df_local), .combine = c) %dopar% {
    phos_count <- stringr::str_count(df_local$phos_seq[r], "\\*")
    temp_list <- c()
    if (phos_count >= 1) {
      phos_loc <- stringr::str_locate_all(df_local$phos_seq[r], "\\*")
      for(c in (1:phos_count)){
        temp_list <- c(temp_list, (phos_loc[[1]][[c]] - c))
      }
    }
    list(temp_list)
  }
  
  df_local$phos_res <- parallel_result1
  
  
  #consolidates probabilities for each sample and takes the highest prob for each residue
  parallel_result2 <- foreach(r = 1:nrow(df_phos_prob), .combine = c) %dopar% {
    first_value <- FALSE
    for (c in (1:ncol(df_phos_prob))) {
      if (!first_value) { 
        temp1 <- unlist(stringr::str_split(df_phos_prob[[r,c]], ";")) |> as.numeric() 
        if (!is.na(temp1[[1]])) {
          first_value <- TRUE
        }
      }else {
        temp2 <- unlist(stringr::str_split(df_phos_prob[[r,c]], ";")) |> as.numeric()
        if (!is.na(temp2[[1]])) {
          temp1 <- pmax(temp1, temp2)
        }
      }
    }
    list(temp1)
  }
  
  df_local$pr <- parallel_result2
  
  #mark as localized or not
  parallel_result3 <- foreach(r = 1:nrow(df_local), .combine = rbind) %dopar% {
    prob <- unlist(df_local$pr[r])
    residue <- unlist(df_local$phos_res[r])
    local <- c()
    for (c in length(residue)) {
      local <- c(local, prob[residue]) 
    }
    if (max(local) >= 0.75) {
      if (min(local) >= 0.75) {
        local2 <- "Y"
      } else {
        local2 <- "P"
      }
    }else {
      local2 <- "N"
    }
    list(local, local2)
  }
  
  parallel_result3 <- data.frame(parallel_result3)
  colnames(parallel_result3) <- c("Local", "Local2")
  row.names(parallel_result3) <- NULL
  
  numlist_to_string <- function(x) {
    return(toString(paste(unlist(x$Local) |> as.character() |> paste(collapse = ","))))
  }
  
  numlist_to_string2 <- function(x) {
    return(toString(paste(unlist(x$Local2) |> as.character() |> paste(collapse = ","))))
  }
  
  parallel_result3$Local <- apply(parallel_result3, 1, numlist_to_string)
  parallel_result3$Local2 <- apply(parallel_result3, 1, numlist_to_string2)
  
  parallel_result3$Protein_PTM_Loc <- df_local$Protein_PTM_Loc
  parallel_result3$PTM_Loc <-  df_local$PTM_Loc
  
  stopCluster(cl) 
  cat(file = stderr(), "Function localize_summary...end", "\n")
  return(parallel_result3) 
}

#----------------------------------------------------------------------------------------
meta_data<- function(){
  cat(file = stderr(), "Function meta_data...", "\n")
  
  #create empty dataframe called df_meta
  df_meta <- data.frame("psm" = nrow(df_psm))
  df_meta$peptide <- nrow(df_peptide)
  if(exists("df_protein")) {df_meta$protein <- nrow(df_protein)}
  if(exists("df_peptide_ptm")) {df_meta$peptide_ptm <- nrow(df_peptide_ptm)}
  df_meta$sample_number <- sample_number

  
  cat(file = stderr(), "Function meta_data...end", "\n")
}