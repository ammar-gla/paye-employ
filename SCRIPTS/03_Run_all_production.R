#_______________________________________________________________________________
###  RUN ALL SCRIPTS ----
#_______________________________________________________________________________
  
  # HERE package needed for dynamic pathfinding
  library("here") 
  
  ### Paths
  INPUT <- paste0(here("INPUT"),"/")
  INTERMEDIATE <- paste0(here("INTERMEDIATE"),"/")
  OUTPUT <- paste0(here("OUTPUT"),"/")
  HTML_OUT <- paste0(OUTPUT,"HTML/")
  SCRIPTS <- paste0(here("SCRIPTS"),"/")

#...............................................................................
#### Run LMU scripts ----
#...............................................................................

  # Misc datasets - LFS, CC and geographical
  source(paste0(SCRIPTS,"01. Analyse data.r"), encoding="utf-8") # encoding necessary to read symbols
  
  # Produce LMU markdown
  rmarkdown::render(paste0(SCRIPTS,"02. PAYE nationalities.Rmd"),
                    output_file = paste0(HTML_OUT,"PAYE employments ", format(Sys.Date(),"%B %Y"), 
                                        ".html"))
