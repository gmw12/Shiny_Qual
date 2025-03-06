cat(file = stderr(), "Shiny_Misc_Functions.R", "\n")

#---------------------------------------------------------------------
str_to_num <- function(df, str_list){
  
  col_select <- strsplit(unlist(str_list), ",")
  col_select <- as.numeric(unlist(col_select))
  
  return(df[,col_select])
}


#---------------------------------------------------------------------
str_to_numlist <- function(str_in) {
  
  num_out <- strsplit(str_in, ",") |> unlist() |> as.numeric()
  
  return(num_out)
}

#---------------------------------------------------------------------
str_to_numlist_max <- function(str_in) {
  if (str_in == "") {
    num_out <- ""
  }else {
    num_out <- max(strsplit(str_in, ",") |> unlist() |> as.numeric())
  }
  return(num_out)
}

#---------------------------------------------------------------------
round_columns <- function(df, search_text, round_digits) {
  if (is.numeric(search_text[1])) {
    select_cols = search_text
  }else {
    select_cols <- which(stringr::str_detect(colnames(df), search_text))
  }
  for (col in select_cols){
    df[,col] <- df |> dplyr::select(dplyr::all_of(col)) |> round(digits = round_digits)
  }
  return(df)
}

#---------------------------------------------------------------------
simple_plate_name <- function(df) {
  cat(file = stderr(), "Function - simple_plate_name...", "\n")
  # df <- df_data_raw
  
  plates <- unique(df$Plate.bar.code)
  plates <- paste(plates, collapse = " | ")
  plates <- strsplit(plates, " | ")
  plates <- unlist(plates)
  plates <- plates[plates != ""]
  plates <- plates[plates != "|"]
  plates <- unique(plates)
  
  common_left_str <- ""
  common <- TRUE
  i <- 1
  
  while (common) {
    test_str <- substr(plates[1], 1, i)
    cat(file = stderr(), stringr::str_c("test_str=", test_str), "\n")
    for (j in 1:length(plates)) {
      if (substr(plates[j], 1, i) != test_str) {
        cat(file = stderr(), stringr::str_c("i=",i," j=",j," test_str=", substr(plates[j], 1, i)), "\n")
        common <- FALSE
        break
      }
    }
    if (common) {common_left_str <- test_str}
    i <- i + 1
  }
  
  
  common_right_str <- ""
  common <- TRUE
  i <- 0
  
  while (common) {
    test_str <- substr(plates[1], nchar(plates[1])-i, nchar(plates[1]))
    cat(file = stderr(), stringr::str_c("test_str=", test_str), "\n")
    for (j in 1:length(plates)) {
      if (substr(plates[j], nchar(plates[1])-i, nchar(plates[1])) != test_str) {
        cat(file = stderr(), stringr::str_c("i=",i," j=",j," test_str=", substr(plates[1], nchar(plates[j])-i, nchar(plates[1]))), "\n")
        common <- FALSE
        break
      }
    }
    if (common) {common_right_str <- test_str}
    i <- i + 1
  }
  
  
  new_plate <- gsub(common_left_str, "", plates)
  new_plate <- gsub(common_right_str, "", new_plate)
  
  for (i in 1:length(plates)) {
    df <- df |>
      dplyr::mutate(dplyr::across(
        .cols = dplyr::everything(),
        ~stringr::str_replace( ., plates[[i]], new_plate[[i]] )
      ) )
  }
  
  cat(file = stderr(), "Function - simple_plate_name...end", "\n")
  return(df)
}

#----------------------------------------------------------
Simple_fread <- function(file) {
  data.table::fread(file = file, header = TRUE, stringsAsFactors = FALSE, sep = "\t")
} 

#----------------------------------------------
Simple_Excel <- function(df, sheetname, filename) {
  cat(file=stderr(), stringr::str_c("Simple_Excel -> ", filename), "\n")
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, sheetname)
  openxlsx::writeData(wb, sheet=1, df)  
  openxlsx::saveWorkbook(wb, filename, overwrite = TRUE)
}


#----------------------------------------------------------------------------------------
create_dir <- function(name){
  cat(file = stderr(), "Function create_dir...", "\n")
  if (fs::is_dir(name)) {
    #added file delete, dir delete not working on customer shiny server
    cat(file = stderr(), "dir exists, deleting...", "\n")
    do.call(file.remove, list(list.files(name, full.names = TRUE)))
    fs::dir_delete(name)
    cat(file = stderr(), "dir deleted, re-creating...", "\n")
    fs::dir_create(name, mode="u=rwx,go=rwx", recurse=TRUE)
  }else{
    cat(file = stderr(), stringr::str_c("dir does NOT exist, creating...", name), "\n")
    fs::dir_create(name, mode="u=rwx,go=rwx", recurse=TRUE)
  }
  
  if (fs::is_dir(name)) {
    name <- stringr::str_replace_all(name, "/", "//")
    name <- stringr::str_c(name, "//")
    cat(file = stderr(), stringr::str_c(name, " confirmed created...", "\n"))
  }else{
    cat(file = stderr(), stringr::str_c(name, " NOT created...", "\n"))
  }  
  
  cat(file = stderr(), "Function create_dir...end", "\n\n")
  return(name)
}
#----------------------------------------------------------------------------------------
create_dir_only <- function(name){
  cat(file = stderr(), "Function create_dir_only...", "\n")
  if (fs::is_dir(name)) {
    #added file delete, dir delete not working on customer shiny server
    cat(file = stderr(), "dir exists...", "\n")
  }else{
    dir_create(name)
    name <- str_replace_all(name, "/", "//")
    name <- str_c(name, "//")
    cat(file = stderr(), str_c(name, " created...", "\n"))
  }
  
  cat(file = stderr(), "Function create_dir_only...end", "\n\n")
  return(name)
}


#----------------------------------------------------------------------------------------
loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}


#----------------------------------------------------------------------------------------
get_max_rowid <- function(table_name, params) {
  cat(file = stderr(), "Function - get_max_rowid...", "\n")
  
  conn <- dbConnect(RSQLite::SQLite(), params$database_path) 
  query <- stringr::str_c("SELECT max(RowId) FROM ", table_name) 
  max_row <- dbGetQuery(conn, query)
  RSQLite::dbDisconnect(conn)
  
  cat(file = stderr(), "Function - get_max_rowid...end", "\n")
  return(max_row)
}
