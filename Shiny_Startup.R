cat(file = stderr(), "Shiny_Startup.R", "\n")

#--------------------------------------------------------------------------------


set_user <- function() {
  cat(file = stderr(), "Function - set_user", "\n")
  
  #set user to unkown to force app to find correct usr
  site_user <<- "unknown"
  volumes <<- "unknown"

  while (site_user == "unknown") {
    if (Sys.info()["nodename"] == "titanshinyu20") {
      #for titan_black VM
      volumes <<- c(dd = '/home/dpmsr/shared/h_drive', dd2 = '/home/dpmsr/shared/other_black', RawData = '/home/dpmsr/shared/RawData', wd = '.', Home = fs::path_home(), getVolumes()())
      site_user <<- "dpmsr"
    }else if (Sys.info()["nodename"] == "bob") {
      volumes <<- c(dd = '/home/dpmsr/mnt/h_black2', h1 = '/home/dpmsr/mnt/h_black1', h2 = '/home/dpmsr/mnt/h_black2', dc = 'home/dpmsr/mnt/RawData', wd = '.', Home = fs::path_home(), getVolumes()())
      site_user <<- "dpmsr"
    }else if (Sys.info()["nodename"] == "shiny-titan") {
      volumes <<- c(h2 = '/mnt/h_black2', h1 = '/mnt/h_black1', dc = '/mnt/RawData', wd = '.', Home = fs::path_home(), getVolumes()())
      site_user <<- "dpmsr"
    }else if (Sys.info()["nodename"] == "Gregorys-MBP.wired.duke.local" |Sys.info()["nodename"] == "gregorys-mbp.lan" | Sys.info()["nodename"] == "mac.lan" | Sys.info()["nodename"] == "Gregorys-MacBook-Pro.local" ) {
      volumes <<- c(dd = '/Users/gregwaitt/R/ShinyR_Biocrates', dd2 = '/Users/gregwaitt/R', dc = '/mnt/RawData', wd = '.', Home = fs::path_home(), getVolumes()())
      site_user <<- "dpmsr"
    }else{
      #for public website
      volumes <<- c(dd = '/data', wd = '.', Home = fs::path_home(), getVolumes()())
      site_user <<- "not_dpmsr"
      database_dir <<- "/data/database"
    }
  }
  
  #testing shiny
  #site_user <<- "not_dpmsr"
  
  cat(file = stderr(), str_c("site_user set to -->  ", site_user), "\n")
  cat(file = stderr(), str_c("volumes --> ", volumes), "\n")
  
  cat(file = stderr(), "Function - set_user...end", "\n\n")
  return()
}


#---------------------------------------------------------------------------------------------------------


set_file_choosers <- function(session, input, output, volumes) {
  cat(file = stderr(), "Function - set_file_choosers...", "\n")
  cat(file = stderr(), stringr::str_c("Volumes ---> ", volumes), "\n")
  
  shinyFileChoose(input, 'sfb_protocol_file', session = session, roots = volumes, filetypes = c('', 'xlsx'))
  shinyFileChoose(input, 'sfb_precursor_file', session = session, roots = volumes, filetypes = c('', 'tsv', 'txt'))
  
  cat(file = stderr(), "Function - set_file_choosers...end", "\n\n")
}

#---------------------------------------------------------------------------------------------------------


set_file_choosers_data <- function(session, input, output, volumes) {
  cat(file = stderr(), "Function - set_file_choosers_data...", "\n")
  cat(file = stderr(), stringr::str_c("Volumes ---> ", volumes), "\n")
  
  shinyFileChoose(input, 'sfb_precursor_file', session = session, roots = volumes, filetypes = c('', 'tsv', 'txt'))
  
  cat(file = stderr(), "Function - set_file_choosers_data...end", "\n")
}



#-------------------------------------------------------------------
named_list <- function(input_string) {
  cat(file = stderr(), "Function named_list...", "\n")
  
  input_string <- params$norm_type
  named_list <- strsplit(input_string, ", ")
  list_names <- named_list
  named_list <- as.list(named_list)
  test <- c(named_list)
  test <- unlist(test)
  
  names(named_list) <- c(test)
  cat(file = stderr(), "Function named_list...end", "\n")
  return(named_list)
}


