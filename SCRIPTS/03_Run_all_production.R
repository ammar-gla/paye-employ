#_______________________________________________________________________________
###  RUN ALL SCRIPTS ----
#_______________________________________________________________________________
  
  # ACTION: set whether to re-download all datasets, even if already exists
  redownload_all <- FALSE

  # HERE package needed for dynamic pathfinding
  library("here") 

#...............................................................................
#### Run LMU scripts ----
#...............................................................................

  
  # Create paths as strings
  source(here("SCRIPTS","SUBSCRIPTS","GLAE_paths.r"))
  
  # Data packages
  source(paste0(SUBSCRIPTS,"GLAE_packages_load",".r"))
  
  # Inputs such as borough codes in Nomis
  source(paste0(SUBSCRIPTS,"GLAE_data_presets",".r"))
  
  # Run the subscripts necessary for markdown
  source(paste0(SUBSCRIPTS,"GLAE_functions_load",".r"))
  
  
  # Misc datasets - LFS, CC and geographical
  source(paste0(SCRIPTS,"01. Analyse data.r"), encoding="utf-8") # encoding necessary to read symbols
  
  # Produce LMU markdown
  rmarkdown::render(paste0(SCRIPTS,"02. PAYE nationalities.Rmd"),
                    output_file = paste0(HTML_OUT,"PAYE employments ", format(Sys.Date(),"%B %Y"), 
                                        ".html"))
