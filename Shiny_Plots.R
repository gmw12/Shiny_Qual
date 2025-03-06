cat(file = stderr(), "Shiny_PCA.R", "\n")


#------------------------------------------------------------------------------------------------------
create_qc_plots <- function(sesion, input, output, params){
  cat(file = stderr(), "Function create_qc_plots", "\n")
  showModal(modalDialog("Creating Plots...", footer = NULL))
  
  bg_qc_bar <- callr::r_bg(func = qc_grouped_plot_bg, args = list("QC", params), stderr = str_c(params$error_path,  "//error_qcbarplot.txt"), supervise = TRUE)
  bg_qc_box <- callr::r_bg(func = box_plot_bg, args = list("QC", params), stderr = str_c(params$error_path, "//error_qcboxplot.txt"), supervise = TRUE)
  
  bg_qc_box$wait()
  bg_qc_bar$wait()

  print_stderr("error_qcbarplot.txt")
  print_stderr("error_qcboxplot.txt")

  wait_cycle <- 0
  while (!file.exists(str_c(params$plot_path,"QC_boxplot.png"))) {
    if (wait_cycle < 10) {
      Sys.sleep(0.5)
      wait_cycle <- wait_cycle + 1
    }
  }
  
  ui_render_qc_plots(session, input, output)
  
  cat(file = stderr(), "create_qc_plots...end", "\n")
  removeModal()
}

#------------------------------------------------------------------------------------------------------
create_spqc_plots <- function(sesion, input, output, params){
  cat(file = stderr(), "Function create_spqc_plots", "\n")
  showModal(modalDialog("Creating Plots...", footer = NULL))
  
  bg_spqc_bar <- callr::r_bg(func = qc_grouped_plot_bg, args = list("SPQC", params), stderr = str_c(params$error_path,  "//error_spqcbarplot.txt"), supervise = TRUE)
  bg_spqc_box <- callr::r_bg(func = box_plot_bg, args = list("SPQC", params), stderr = str_c(params$error_path, "//error_spqcboxplot.txt"), supervise = TRUE)
  bg_norm_line <- callr::r_bg(func = norm_line_bg, args = list(params), stderr = str_c(params$error_path, "//error_normlineplot.txt"), supervise = TRUE)
 
  bg_spqc_box$wait()
  bg_spqc_bar$wait()
  #bg_norm_line$wait()
  
  print_stderr("error_spqcbarplot.txt")
  print_stderr("error_spqcboxplot.txt")
  print_stderr("error_normlineplot.txt")
  
  wait_cycle <- 0
  
  while (!file.exists(stringr::str_c(params$plot_path, "SPQC_", params$material_select, "_barplot.png"))) {
    if (wait_cycle < 10) {
      Sys.sleep(0.5)
      wait_cycle <- wait_cycle + 1
    }
  }
  
  ui_render_spqc_plots(session, input, output, params)
  
  cat(file = stderr(), "create_spqc_plots...end", "\n")
  removeModal()
}

#------------------
qc_grouped_plot_bg <- function(plot_title, params) {
  cat(file = stderr(), stringr::str_c("function qc_grouped_plot_bg...."), "\n")
  source('Shiny_File.R')
    
  
  if (plot_title == "QC"){
    df <- read_table_try("QC_Report", params)
    df_qc1 <- df |> dplyr::select(contains("QC1.Level"))
    row_remove <- which(df_qc1 == 0.0010, arr.ind = TRUE)
    if(length(row_remove) > 0) {df <- df[-row_remove,]}
    test <- df |> dplyr::select(contains("Accuracy")) 
    lower_limit <- 100 - params$qc_acc
    upper_limit <- 100 + params$qc_acc
    test[test < lower_limit | test > upper_limit] <- 0
    test[test > 0] <- 1
    lower_limit_text <- stringr::str_c("Accuracy ", lower_limit, "-", upper_limit)
    upper_limit_text <- stringr::str_c("Accuracy Outside Range")
    file_name <- stringr::str_c(params$plot_path, plot_title, "_barplot.png")
  }
  
  if (plot_title == "SPQC"){
    df <- read_table_try(stringr::str_c("SPQC_Report_", params$material_select), params)
    test <- df |> dplyr::select(contains("SPQC"))
    test <- test |> dplyr::select(contains("CV"))
    #replace na with 0
    test[is.na(test)] <- 0
    test[test > params$qc_acc] <- 0
    test[test > 0] <- 1
    lower_limit_text <- stringr::str_c("CV < ", params$qc_acc)
    upper_limit_text <- stringr::str_c("CV > ", params$qc_acc)
    file_name <- stringr::str_c(params$plot_path, plot_title, "_", params$material_select, "_barplot.png")
  }
  
  
  
  good <- colSums(test)
  bad <- nrow(test) - good
  
  data_plot = data.frame(matrix(vector(), ncol(test)*2, 3,
                         dimnames=list(c(), c("Sample", "QC", "Count"))),
                  stringsAsFactors=F)
  
  colnames(test) <- gsub("Accuracy", "", colnames(test))
  data_plot$Sample <- c(colnames(test), colnames(test))
  data_plot$Count <- c(good, bad)
  data_plot$QC <- c(rep(lower_limit_text, ncol(test)), rep(upper_limit_text, ncol(test)))
  
  plot_title <- stringr::str_c(plot_title, " Barplot")
  
  # Grouped
  ggplot2::ggplot(data_plot, ggplot2::aes(fill = QC, y = Count, x = Sample)) + 
    ggplot2::geom_bar(position = "dodge", stat = "identity") +
    ggplot2::ggtitle(plot_title) + 
    ggplot2::xlab(NULL) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), 
                   axis.text.x = ggplot2::element_text(size = 5, angle = 45, hjust = 1, color = "black"),
                   axis.text.y = ggplot2::element_text(size = 5,  color = "black"))
  ggplot2::ggsave(file_name, width = 8, height = 6)
  
  cat(file = stderr(), stringr::str_c("function qc_grouped_plot_bg....end"), "\n")
}


#Box plot-------------------------------------------------
box_plot_bg <- function(plot_title, params) {
  cat(file = stderr(), "Function box_plot_bg", "\n")
  
  source("Shiny_File.R")
  
  if (plot_title == "QC"){ 
    df <- read_table_try("QC_Report", params)
    df_qc1 <- df |> dplyr::select(contains("QC1.Level"))
    row_remove <- which(df_qc1 == 0.0010, arr.ind = TRUE)
    if(length(row_remove) > 0) {df <- df[-row_remove,]}
    test <- df |> dplyr::select(contains("Accuracy")) 
    df_box <- df |> dplyr::select(contains("Accuracy"))
    #remove "Accuracy" from column names
    colnames(df_box) <- gsub("Accuracy", "", colnames(df_box))
    df_box_wide <- tidyr::pivot_longer(df_box, cols = colnames(df_box), names_to = "Sample", 
                                values_to = "Stat")
    x_name <- "Accuracy"
    file_name <- stringr::str_c(params$plot_path, plot_title, "_boxplot.png")
    }
  
  
  if (plot_title == "SPQC"){ 
    df <- read_table_try(stringr::str_c("SPQC_Report_",params$material_select), params)
    df_box <- df |> dplyr::select(contains("SPQC"))
    df_box <- df_box |> dplyr::select(contains("CV"))
    #colnames(df_box) <- gsub("X", "", colnames(df_box))
    df_box_wide <- tidyr::pivot_longer(df_box, cols = colnames(df_box), names_to = "Sample", 
                                values_to = "Stat")
    df_box_wide$Sample <- gsub("X.CV.", "", df_box_wide$Sample)
    df_box_wide$Sample <- gsub("SPQC.uM.", "", df_box_wide$Sample)
    x_name <- "CV"
    file_name <- stringr::str_c(params$plot_path, plot_title,"_", params$material_select, "_boxplot.png")
  }
  
  df_box <- df_box |>  dplyr::mutate(across(!where(is.numeric), as.numeric))
  

  plottitle <- stringr::str_c(plot_title, " ", params$material_select, " Boxplot")
  
  #create color_list length of ncol df_acc
  color_list <- c("red", "blue", "darkgreen", "purple", "orange", "black", "brown", "cyan", "magenta", "yellow", "pink", "grey", "lightblue", "green", "darkred", "darkblue", "darkgreen", "purple", "darkorange", "hotpink", "maroon", "darkcyan", "darkmagenta", "yellow4", "maroon4", "darkgrey", "skyblue", "darkgreen", "gold", "khaki")
  
  ggplot2::ggplot(df_box_wide, ggplot2::aes(x=as.factor(Sample), y=Stat, fill = Sample)) + 
    ggplot2::geom_boxplot(alpha=0.2) +
    ggplot2::scale_color_manual(values = rev(unique(color_list))) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = "none", 
          axis.title.y = ggplot2::element_blank(),
          plot.title = ggplot2::element_text(hjust = 0.5) ) +
    ggplot2::labs(title = plottitle, x = x_name) +
    ggplot2::scale_x_discrete(limits = rev) +
    ggplot2::coord_flip() 
  ggplot2::ggsave(file_name, width = 8, height = 6)
  
  
  
  cat(file = stderr(), "Function box_plot_bg...end", "\n")
}

