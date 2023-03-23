#_______________________________________________________________________________
# 00. LOAD LIBRARIES AND SET UP PATHS ----
#_______________________________________________________________________________
  

  library("here")
  library("tidyverse")
  library("ggplot2")
  library("ggthemes")
  library("nomisr")
  library("devtools")
  library("remotes")
  library("scales")
  library("gglaplot")
  library("data.table")
  library("janitor")
  library("lubridate")
  library("readr")
  library("ggrepel")
  library("plotly")
  library("magrittr")
  library("zoo")
  library("openxlsx")

  #.............................................................................
  

  
  #.............................................................................
  
  ### Functions
  ### Define function to always present percentages with one decimal and figures without decimals
  perc_form = function(x, d=1) sprintf(paste0("%1.",d,"f"), x) 
  
  value_form = function(x,s=2,d= -1) format(signif(round(as.numeric(x), d),s), big.mark=",")

  ### Function to remove negative sign from character strings (needed as formatted numbers are not numerical)
  abs2 <- function(x) {
    y = gsub("-","",x)
    return(y)
  }
  
  ### Helper function to ensure legend labels are placed correctly
  reverse_legend_labels <- function(plotly_plot) {
    n_labels <- length(plotly_plot$x$data)
    plotly_plot$x$data[1:n_labels] <- plotly_plot$x$data[n_labels:1]
    plotly_plot
  }
  
  ### Helper function to re-introduce simplified modebar allowing chart download
  plotly_modebar <- function(plotly_plot) {
    plotly_plot <- plotly_plot %>% plotly::config(displayModeBar = TRUE) %>% # Allows menu bar such that image can be downloaded
      plotly::config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d","zoom2d","pan2d","autoScale2d","hoverClosestCartesian","hoverCompareCartesian","select2d","lasso2d")) %>% 
      plotly::config(displaylogo = FALSE)
    plotly_plot
  }
  
  #_____________________________________________________________________________
  # 01. IMPORT  DATASETS and clean ----
  #_____________________________________________________________________________
  
  # The data has columns grouped by sector (and overall) with four columns in each group:
  # total count, UK count, EU count and non-EU count
  # We reshape long by segment to easily calculate shares within each
  
  # The 2023 data has numbers in sheet names
  data_file_name <- "paye_employments_dec2022.xlsx"
  
  #.............................................................................
  ## 01.1. Load data and reshape ----
  #.............................................................................
  
  # Loop through each sheet
  region_sheets_names <- c("UK","UK_EU_nationals","England","North_East","North_West","Yorkshire_and_the_Humber","East_Midlands","West_Midlands","East","London","South_East","South_West","Scotland","Wales","Northern_Ireland")
  region_sheets <- setNames(as.character(c(1:length(region_sheets_names))),
                                  nm=region_sheets_names)
  
  
  for (region in names(region_sheets)) {
    dat_name <- tolower(region)
    
    load_dta <- readxl::read_excel(path = here("INPUT",data_file_name),
                                   sheet = region_sheets[[region]], skip=3) %>%
      clean_names()     %>%
      filter(date != "End of worksheet") %>% 
      rename_with(~str_replace(., "information_and_communication","in_information_and_communication")) %>% 
      rename_with(~str_replace(., "industry_administrative_and_support_services","in_administrative_and_support_services")) %>% 
      rename_with(~str_c(., "_in_overall"),ends_with("_counts")) %>% # Ensuring there is also a suffix for overall
      rename_with(~str_replace(.,"total_",""),!contains("total_employment")) %>% 
      mutate(across(where(is.character) & !date, 
                    ~ case_when(. == "[d]" ~ as.character(NA), # Where suppressed due to disclosure, make into NA
                                TRUE ~ .))) %>% 
      mutate(date = gsub("\\[(\\w)\\]","\\1",date)) %>% 
      mutate(across(where(is.character) & !date, as.numeric)) #ensuring all columns are numeric before reshaping long
    
    load_dta_long<- load_dta %>% 
      pivot_longer(cols = -date, names_to = c(".value","section"),names_sep = "_in_")
    
    assign(paste0("paye_",dat_name,"_data_long"),load_dta_long)
    remove(load_dta_long)
    remove(load_dta)
  }
  
  remove(region)
  
  regions <- tolower(region_sheets_names[!region_sheets_names %in% c("UK","UK_EU_nationals")])

  #.............................................................................
  ## 01.2. Calculate shares ----
  #.............................................................................
  
  data_list <- list() # For combining datasets
  i=1
  
  for (dat in c(regions,"uk")) {
    
    temp_dta <- get(paste0("paye_",dat,"_data_long")) %>% 
      mutate(geography = dat)
    
    
    
    for (pop in c("uk","eu","non_eu")) {
      temp_dta <- temp_dta %>% 
        mutate(!!sym(paste0(pop,"_nationals_employment_share")) := 100 * !!sym(paste0(pop,"_nationals_employment_counts"))/total_employment_counts)
    }
    
    data_list[[i]] <- temp_dta # Include dataset in list
    i = i+1 #Next iteration
    
    assign(paste0("paye_",dat,"_data_long"),temp_dta)
    remove(temp_dta)
    
  }
  
  #.............................................................................
  ## 01.3. Combine into one dataset ----
  #.............................................................................
  
  paye_master_long <- bind_rows(data_list) %>% 
    mutate(geography_name = case_when(geography == "uk" ~ "UK" ,   # Create clean name for charts
                                      TRUE ~ str_to_title(gsub("_"," ",geography))),
           date_day = lubridate::my(date),
           geo_rank = case_when(geography=="london" ~ 1,
                                geography=="uk" ~ 2,
                                TRUE ~ 3),
           section_name =  case_when(
             section == "public_administration_and_defence_social_security" ~ "Public admin & defence",
             section == "health_and_social_work" ~ "Health",
             section == "finance_and_insurance" ~ "Finance & insurance",
             section == "professional_scientific_and_technical" ~ "Professional services",
             section == "wholesale_and_retail_repair_of_motor_vehicles" ~ "Retail",
             section == "other_service_activities" ~ "Other services",
             section == "transportation_and_storage" ~ "Transport & storage",
             section == "arts_entertainment_and_recreation" ~ "Arts & recreation",
             section == "information_and_communication" ~ "Information & communication",
             section == "administrative_and_support_services" ~ "Administration",
             section == "accommodation_and_food_service_activities" ~ "Hospitality",
             section == "water_supply_sewerage_and_waste" ~ "Water",
             TRUE ~ str_to_title(gsub("_"," ",section)))) %>% 
             arrange(geo_rank, geography,section,date_day) %>% 
    relocate(date,date_day,geography,geography_name,section,section_name)
  
  #.............................................................................
  # Expand the dataset by calculating changes since pandemic began (taken as Feb 2020 in PAYE)
  #.............................................................................
  
  # Specify vector which defines the order wanted for nationality
  nat_names <- c("Total","UK","EU","Non-EU")
  
  # Note: data will be made longer by the nationality type and count/share measure type
  paye_master_long_detail <- paye_master_long %>% 
    rename(overall_nationals_employment_counts=total_employment_counts) %>% 
    pivot_longer(cols=contains("nationals_employment"), names_to = c("nationality","measure_name"), names_pattern="(.*)_nationals_employment_(.*)",values_to = "measure_value") %>% 
    group_by(geography,section,nationality,measure_name) %>% 
    mutate(d_change_feb20 =  case_when(date_day<="2020-02-01" ~ NA_real_,
                                       TRUE ~ measure_value-measure_value[date_day=="2020-02-01"]),
           p_change_feb20 =  case_when(date_day<="2020-02-01" | measure_name=="share" ~ NA_real_,
                                       TRUE ~ 100* (measure_value-measure_value[date_day=="2020-02-01"])/(measure_value[date_day=="2020-02-01"])),
           index_feb20 = case_when(date_day<"2020-02-01" | measure_name=="share" ~ NA_real_,
                                   TRUE ~ 100* (measure_value)/(measure_value[date_day=="2020-02-01"])),
           nationality_name = case_when(nationality == "overall" ~ "Total",
                                        nationality == "non_eu" ~ "Non-EU",
                                        TRUE ~ toupper(nationality))) %>% 
    ungroup() %>% 
    arrange(geography,section,factor(nationality,levels=nat_names),date_day,measure_name) %>% 
    relocate(geography,geography_name,section,section_name,nationality,nationality_name,date,date_day,measure_name,measure_value)
  
  # Export to Excel
  write.xlsx(paye_master_long_detail,paste0(OUTPUT,"/DATA/","long_paye_data.xlsx"))
  
  
  #_____________________________________________________________________________
  # 02. Charting ----
  #_____________________________________________________________________________ 
  
  # Store London charts in a list for easy use in markdown
  london_charts <- list()
  chart_n = 1
  
  # Max date
  last_date_month <- format(max(paye_master_long$date_day),"%B %Y")
  
  #.............................................................................
  ## 02.1. One section for all regions, London and UK highlighted ----
  #.............................................................................

  # Define some helper vectors
  sections_list <- unique(paye_master_long$section_name)
  num_sections <- length(sections_list)
  num_regions <- length(regions)+1 # account for UK
  regions_sort <- c("London","UK",setdiff(sort(unique(paye_master_long$geography_name)),c("London","UK"))) # This puts London and UK first to use for named palette
  
  # Loop through possible sections
  for (dat_section in sections_list) {
    
    # For the palette, highlight 2 (London and UK) and set rest for contextual
    context_num <- num_regions -2
    pal <- gla_pal(gla_theme = "default", palette_type = "highlight", n = c(2,context_num ))
    pal_named <- setNames(object=pal,nm = regions_sort)
    
    # Likewise for size of lines
    scale_size <- setNames(object = c(2 * mm_to_pt,2 * mm_to_pt, rep(1* mm_to_pt,num_regions-2)),nm = regions_sort)
    
    theme_set(theme_gla(gla_theme = "default"))
    
    # Loop through each nationality type
    for (share_pop in c("uk","eu","non_eu")) {
      
      # Clean name for labelling in chart
      pop_name <- toupper(gsub("_","-",share_pop))
      share_var <- paste0(share_pop,"_nationals_employment_share")
      count_var <- paste0(share_pop,"_nationals_employment_counts")
      chart_name <- paste0(share_pop,"_",dat_section)
      
      
      # Produce chart
      regions_trend <- paye_master_long %>% 
      filter(section_name == dat_section) %>% 
      ggplot(mapping = aes(x = date_day, y = get(share_var), 
                           group = geography_name, 
                           text = paste(
                             geography_name, "\n",
                             date, "\n",
                             pop_name," share: ", perc_form(get(share_var),d=1), "%", "\n",
                             sep = ""))) +
        ggla_line(aes(colour = geography_name,size = geography_name)) +
        scale_size_manual(values = scale_size,
                          breaks = c("London","UK")) +
        scale_colour_manual(values = pal_named,
                            breaks = c("London","UK")) +
        ggla_labelline(x1 = ymd("2020-03-01")) + # mark lockdowns start
        ggla_axisat0() +
        geom_hline(aes(yintercept=0), colour="gray45") +
        coord_cartesian(clip = 'off') +
        scale_y_continuous(expand = c(0, 0), labels = dollar_format(prefix = "", 
                                                                    largest_with_cents = 1,
                                                                    suffix = "%")) +
        scale_x_date() +
        theme(plot.margin = unit(c(1,1,1,1), "cm"))+
        labs(title = paste0("Share of ",pop_name ," nationals across regions"),
             subtitle = paste0("Employments in industry: ", dat_section),
             caption = "PAYE RTI data") +
        theme(plot.caption = element_text(color = rgb(166,166,166,maxColorValue = 255)))
      
      ggsave(here::here("OUTPUT","PLOTS","by_section",gsub(" ","_",paste0(chart_name,".png"))), device = "png", width = 8, height = 8, units = "in")
      
      # Save useful charts in the list
      if (dat_section %in% c("Hospitality","Overall")) {
        
        plotly <- ggplotly(regions_trend,tooltip = "text") %>% 
          ggla_plotly_settings()    %>% 
          layout(title = list(text = paste0("<b>","Share of ",pop_name ," nationals across regions","</b>",
                                            "<br>",
                                            "<sup>",
                                            paste0("Employments in industry: ", dat_section),
                                            "</sup>",
                                            "<br>"),
                              font = list(size = 22),
                              y = .95, xref = "plot"),
                 xaxis = list(tickfont = list(size = 15)),
                 yaxis = list(tickfont = list(size = 15)),
                 legend = list(font = list(size = 15),title=list(text="")),
                 hovermode = "closest") %>% 
          plotly_modebar
        
        london_charts[[chart_n]] <- plotly
        names(london_charts)[chart_n] <- chart_name
        
        chart_n <- chart_n + 1 
      }
      
      
    }
    
  #.............................................................................
  # Horizontal bars with share of nationalities in each region
  #.............................................................................

    # Sort the regions with UK and London first, then in order of least UK
    regions_nats <- paye_master_long %>% 
      filter(section_name == dat_section & date_day==max(date_day) & geography_name!="England") %>% 
      arrange((uk_nationals_employment_share)) %>% # Arrange those with less UK nationals first
      pull(geography_name)
    
    regions_nats_sort <- c("UK","London",setdiff(regions_nats,c("UK","London"))) # Ensure UK/London is first
    
    # For the palette. To ensure colours are consistent throughout analysis, do some manual adjustments
    nats_used <- c("EU","Non-EU","UK")
    
    # Let UK be own less flashy colour
    pal <- c(gla_pal(gla_theme = "default", palette_type = "quantitative",main_colours = "ldndkpink", n = 2))
    pal_named <- c("UK"="#cccccc","EU"=pal[1],"Non-EU"=pal[2])
    
    theme_set(theme_gla(gla_theme = "default"))
    
    # Misc settings
    last_date <- paye_master_long_detail %>% filter(date_day == max(date_day)) %>% filter(row_number()==1) %>%  pull(date)
    chart_name <- paste0("emp_shares_",dat_section)
    
    # Produce chart
    regioncomp_shares <- paye_master_long_detail %>% 
      filter(section_name == dat_section & measure_name=="share" & nationality_name %in% nats_used & date_day == max(date_day)) %>% 
      ggplot(mapping = aes(x = factor(geography_name,levels = rev(regions_nats_sort)), y = measure_value, color = factor(nationality_name,levels = nats_used), fill=factor(nationality_name,levels = nats_used),
                           text = paste(
                             geography_name, "\n",
                             "Nationality: ", nationality_name, "\n",
                             "Share: ", perc_form(measure_value,d=1), "%", "\n",
                             sep = ""))) +
      geom_bar(stat = "identity", position = "stack", width=0.5)+ # bars
      coord_flip()  + # flip to horizontal
      scale_color_manual(values = pal_named, aesthetics = "colour")+
      scale_fill_manual(values = pal_named, aesthetics = "fill")+
      scale_y_continuous(expand = c(0, 0), labels = dollar_format(prefix = "", 
                                                                  largest_with_cents = 1,
                                                                  suffix = "%")) +
      scale_x_discrete(limits = rev(c("UK","London","",setdiff(regions_nats_sort,c("UK","London"))))) + #to create blank column
      theme(plot.margin = unit(c(1,1,1,1), "cm"))+
      theme(panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x = element_line( size=.5 ),
            axis.text.y = ggplot2::element_text(
              hjust = 0, vjust = 0.5,
              margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0,
                                       unit = "pt")),
            axis.ticks.length.y = ggplot2::unit(x = 0, units = "pt")) + 
      labs(title = paste0("Share of nationals in employments by region"),
           subtitle = paste0(dat_section," payrolled employments, ",last_date),
           caption = "PAYE RTI data") +
      theme(plot.caption = element_text(color = rgb(166,166,166,maxColorValue = 255)))
    
    ggsave(here::here("OUTPUT","PLOTS","by_section",gsub(" ","_",paste0(chart_name,".png"))), device = "png", width = 8, height = 8, units = "in")
    
    if (dat_section %in% c("Hospitality","Overall","Construction")) {
      plotly <- ggplotly(regioncomp_shares,tooltip = "text") %>% 
        ggla_plotly_settings()    %>% 
        layout(title = list(text = paste0("<b>","Share of nationals in employments by region","</b>",
                                          "<br>",
                                          "<sup>",
                                          paste0(dat_section," payrolled employments, ",last_date),
                                          "</sup>",
                                          "<br>"),
                            font = list(size = 22),
                            y = .95, xref = "plot"),
               xaxis = list(tickfont = list(size = 15)),
               yaxis = list(tickfont = list(size = 15)),
               legend = list(font = list(size = 15),title=list(text="")),
               hovermode = "closest") %>% 
        reverse_legend_labels %>% 
        plotly_modebar
      
      london_charts[[chart_n]] <- plotly
      names(london_charts)[chart_n] <- chart_name
      
      chart_n <- chart_n + 1 
    }
    
    
  }
  
  remove(dat_section)
  remove(share_pop)

  #.............................................................................
  ## 02.2. Compare sections within regions ----
  #.............................................................................

  #.............................................................................
  # Horizontal bars with share of nationalities in each section
  #.............................................................................
  
  main_regions <- c("London","UK","England")
  
  # Loop through possible regions
  for (dat_region in main_regions) {
    
    # Only plot the top N sections (+ Overall) in each region, defined by number of total employees in last date
    n_top <- 10 + 1
    top_sections <- paye_master_long %>% 
      filter(geography_name == dat_region & date_day==max(date_day)) %>% 
      arrange(desc(total_employment_counts)) %>% 
        slice_head(n=n_top) %>% # Overall is guaranteed to be top no matter what
      arrange((uk_nationals_employment_share)) %>% # Arrange those with less UK nationals first
      pull(section_name)
    
    top_sections_sort <- c("Overall",setdiff(top_sections,c("Overall"))) # Ensure overall is first
    
    # For the palette
    nats_used <- c("Non-EU","EU","UK")
    
    pal <- c(gla_pal(gla_theme = "default", palette_type = "quantitative",main_colours = "ldndkpink", n = 2))
    pal_named <- c("UK"="#cccccc","EU"=pal[1],"Non-EU"=pal[2])
    
    theme_set(theme_gla(gla_theme = "default"))
    
    # Misc settinghs
    last_date <- paye_master_long_detail %>% filter(date_day == max(date_day)) %>% filter(row_number()==1) %>%  pull(date)
    chart_name <- paste0("emp_shares_",dat_region)
    
    # Produce chart
    region_topsec <- paye_master_long_detail %>% 
      filter(geography_name == dat_region & section_name %in% top_sections & measure_name=="share" & nationality_name %in% nats_used & date_day == max(date_day)) %>% 
      ggplot(mapping = aes(x = factor(section_name,levels = rev(top_sections_sort)), y = measure_value, color = factor(nationality_name,levels = nats_used), fill=factor(nationality_name,levels = nats_used),
                           text = paste(
                             section_name, "\n",
                             "Share: ", perc_form(measure_value,d=1), "%", "\n",
                             sep = ""))) +
      geom_bar(stat = "identity", position = "stack", width=0.5)+ # bars
      coord_flip()  + # flip to horizontal
      scale_color_manual(values = pal_named, aesthetics = "colour")+
      scale_fill_manual(values = pal_named, aesthetics = "fill")+
      geom_hline(aes(yintercept=0), colour="gray45") +
      scale_y_continuous(expand = c(0, 0), labels = dollar_format(prefix = "", 
                                                                  largest_with_cents = 1,
                                                                  suffix = "%")) +
      scale_x_discrete(limits = rev(c("Overall","",setdiff(top_sections_sort,c("Overall"))))) +
      theme(plot.margin = unit(c(1,1,1,1), "cm"))+
      theme(panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x = element_line( size=.5 ),
            axis.text.y = ggplot2::element_text(
              hjust = 0, vjust = 0.5,
              margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0,
                                       unit = "pt")),
            axis.ticks.length.y = ggplot2::unit(x = 0, units = "pt")) + 
      labs(title = paste0("Share of nationals within ", dat_region),
           subtitle = paste0("Payrolled employments in top ", n_top-1, " industries, ",last_date),
           caption = "PAYE RTI data") +
      theme(plot.caption = element_text(color = rgb(166,166,166,maxColorValue = 255)))
    
    ggsave(here::here("OUTPUT","PLOTS","within_region",gsub(" ","_",paste0(chart_name,".png"))), device = "png", width = 8, height = 8, units = "in")
    
    if (dat_region=="London") {
      plotly <- ggplotly(region_topsec,tooltip = "text") %>% 
        ggla_plotly_settings()    %>% 
        layout(title = list(text = paste0("<b>","Share of ",pop_name ," nationals within ", dat_region,"</b>",
                                          "<br>",
                                          "<sup>",
                                          paste0("Payrolled employments in top ", n_top-1, " industries"),
                                          "</sup>",
                                          "<br>"),
                            font = list(size = 22),
                            y = .95, xref = "plot"),
               xaxis = list(tickfont = list(size = 15)),
               yaxis = list(tickfont = list(size = 15)),
               legend = list(font = list(size = 15),title=list(text="")),
               hovermode = "closest") %>% 
        plotly_modebar
      
      london_charts[[chart_n]] <- plotly
      names(london_charts)[chart_n] <- chart_name
      
      chart_n <- chart_n + 1 
    }
    
    #.............................................................................
    # Facetted charts with all three nationalities
    #.............................................................................
    
    # Reduce number of top sectors
    n_top <- 5 + 1
    # top_sections <- paye_master_long %>% 
    #   filter(geography_name == dat_region & date_day==max(date_day)) %>% 
    #   arrange(desc(total_employment_counts)) %>% 
    #   slice_head(n=n_top) %>% # Overall is guaranteed to be top no matter what
    #   pull(section_name)
    
    # Manually set the sectors!!
    sections_manual <- c("Overall","Construction","Health","Hospitality","Professional services","Retail")
    
    top_sections <- paye_master_long %>% 
      filter(geography_name == dat_region & date_day==max(date_day) & section_name %in% sections_manual) %>% 
      arrange(sections_manual) %>% 
      pull(section_name)
    
    top_sections_sort <- c("Overall",setdiff(top_sections,c("Overall"))) # Ensure overall is first
    
    # For the palette
    nat_names_sort <- c("Total","EU","Non-EU","UK")
    pal <- c(  #Four categories: overall, UK, EU, non-EU, but let the latter be shades of a colour
      gla_pal(gla_theme = "default", palette_type = "categorical", n = 1), #
      gla_pal(gla_theme = "default", palette_type = "quantitative", main_colours = "ldndkpink", n = 2))

    pal_named <- c("Total"=pal[1],"UK"="#cccccc","EU"=pal[2],"Non-EU"=pal[3])
    
    scale_size <- setNames(object = rep(1* mm_to_pt,4),nm = nat_names_sort)
    
    chart_name <- paste0("fac_",dat_region)
    
    # Produce chart
    region_facet <- paye_master_long_detail %>% 
      filter(geography_name == dat_region & section_name %in% top_sections & measure_name == "counts" & date_day>="2020-01-01" ) %>% 
      ggplot(mapping = aes(text = paste(
                             nationality_name, "\n",
                             format(date_day,"%b-%y"), "\n",
                             "Index: ", value_form(index_feb20,s=4,d=1),"\n",
                             sep = ""))) +
      ggla_line(aes(x = date_day,
                    y = index_feb20, 
                    group = nationality_name,
                    colour = nationality_name,
                    size = nationality_name)) +
      facet_wrap( ~ factor(section_name, level=top_sections_sort), ncol=3, scales="free") +
      scale_colour_manual(values = pal_named) +
      scale_size_manual(values = scale_size) +
      geom_vline(aes(xintercept = as.numeric(ymd("2020-03-01"))),
                 linetype = "dotted",
                 size = 1 * mm_to_pt,
                 colour = rgb(166,166,166,maxColorValue = 255)) + # mark lockdowns start
      coord_cartesian(clip = 'off') +
      scale_y_continuous(limits = c(60, 140),labels = dollar_format(prefix = "", 
                                                                  largest_with_cents = 1,
                                                                  suffix = "")) +
      scale_x_date(date_labels = "%b'%y",date_breaks = "9 months" ) +
      theme(plot.margin = unit(c(1,1,1,1), "cm"),
            panel.spacing = unit(1,"lines")) %>% 
      labs(title = paste0("Employment by nationality within ", dat_region),
           subtitle = paste0("Payrolled employments in selected industries, indexed to Feb 2020"),
           caption = "PAYE RTI data") +
      theme(plot.caption = element_text(color = rgb(166,166,166,maxColorValue = 255)))
    
    ggsave(here::here("OUTPUT","PLOTS","within_region",gsub(" ","_",paste0(chart_name,".png"))), device = "png", width = 8, height = 8, units = "in")
    
    if (dat_region %in% c("London","UK")) {
      
      ## There are some bugs with using ggplotly and ggla with facets. The below corrects the domain ranges.
      # Custom function to correct the facet chart placements
      correct_facets <- function(chart,max_fac_num=6) {
        
        chart_temp <- chart
        for (fac_num in 1:max_fac_num) {
          
          # Assign axes names
          if(fac_num==1) {
            xaxis_name <- "xaxis"
            yaxis_name <- "yaxis"
          }          else {
            xaxis_name <- paste0("xaxis",fac_num)
            yaxis_name <- paste0("yaxis",fac_num)
          }
          
          # Manually position axes and chart titles
          if (fac_num %in% c(1,4)) {
            xdom <- c(0,0.28)
          }          else if (fac_num %in% c(2,5)) {
            xdom <- c(0.35,0.63)
          }          else if (fac_num %in% c(3,6)) {
            xdom <- c(0.72,1)
          }
          if (fac_num %in% c(1,2,3)) {
            y_annot <- 0.95
            ydom <- c(0.60,0.95)
          }          else if (fac_num %in% c(4,5,6)) {
            y_annot <- 0.45
            ydom <- c(0.10,0.45)
          }
          
          # Replace attributes
          chart_temp[['x']][['layout']][['annotations']][[fac_num]][['font']][['family']] <-"Arial"
          chart_temp[["x"]][["layout"]][["annotations"]][[fac_num]][["y"]] <- y_annot
          
          chart_temp[["x"]][["layout"]][[xaxis_name]][["domain"]] <- xdom
          chart_temp[["x"]][["layout"]][[xaxis_name]][["tickfont"]][["family"]]<- "Arial"
          chart_temp[["x"]][["layout"]][[xaxis_name]][["tickfont"]][["color"]] <- "#666666"
          chart_temp[["x"]][["layout"]][[xaxis_name]][["tickfont"]][["size"]] <- 14
          
          chart_temp[["x"]][["layout"]][[yaxis_name]][["domain"]] <- ydom
          chart_temp[["x"]][["layout"]][[yaxis_name]][["tickfont"]][["family"]]<- "Arial"
          chart_temp[["x"]][["layout"]][[yaxis_name]][["tickfont"]][["color"]] <- "#666666"
          chart_temp[["x"]][["layout"]][[yaxis_name]][["tickfont"]][["size"]] <- 14
          
        }
        
        # Reorder elements for Total and UK to swtich legend position
        chart_temp$x$data[13:24] <- chart_temp$x$data[24:13] # UK is 19-24, Total is 13-18.
        chart <<- chart_temp
      }

      # The actual plot
      plotly  <- ggplotly(region_facet,tooltip = "text")    %>% 
        ggla_plotly_settings() %>% 
        layout(title = list(text = paste0("<b>","Employment by nationality within ", dat_region,"</b>",
                                          "<br>",
                                          "<sup>",
                                          paste0("Payrolled employments in selected industries, indexed to Feb 2020"),
                                          "</sup>",
                                          "<br>"),
                            font = list(size = 22),
                            y = .95, xref = "plot"),
               xaxis = list(tickfont = list(size = 14)),
               yaxis = list(tickfont = list(size = 14)),
               legend = list(font = list(size = 15),title=list(text="")),
               hovermode = "closest") %>% 
        plotly_modebar() %>%  
        correct_facets(max_fac_num = 6) %>% 
        reverse_legend_labels() 
      
      london_charts[[chart_n]] <- plotly
      names(london_charts)[chart_n] <- chart_name
      
      chart_n <- chart_n + 1 
      }
  }
  
  #.............................................................................
  ## 02.3. Sector deep dives ----
  #.............................................................................
  
  
  # Plot percentage change by sector within regions
  for (dat_region in main_regions) {
    
    
    # Only plot the top N sections (+ Overall) in each region, defined by number of total employees in last date
    n_top <- 10 + 1
    top_sections <- paye_master_long_detail %>% 
      filter(geography_name == dat_region & date_day==max(date_day) & measure_name=="counts" & nationality=="overall") %>% 
      arrange(desc(measure_value)) %>% 
      slice_head(n=n_top) %>% # Overall is guaranteed to be top no matter what
      arrange(desc(p_change_feb20)) %>% # Sort sectors with largest perc. change first
      pull(section_name)
    
    top_sections_sort <- c("Overall",setdiff(top_sections,c("Overall"))) # Ensure overall is first
    
    chart_name <- paste0("p_change_",dat_region)
    
    # For the palette
    nat_names_sort <- c("Total","UK","Non-EU","EU")
    pal <- c(  #Four categories: overall, UK, EU, non-EU, but let the latter be shades of a colour
      gla_pal(gla_theme = "default", palette_type = "categorical", n = 1), #
      gla_pal(gla_theme = "default", palette_type = "quantitative", main_colours = "ldndkpink", n = 2))
    
    pal_named <- c("Total"=pal[1],"UK"="#cccccc","EU"=pal[2],"Non-EU"=pal[3])
    
    
    section_change_bar <- paye_master_long_detail %>%
      filter( geography_name == dat_region & date_day == max(date_day) & measure_name == "counts" & section_name %in% top_sections)  %>% 
      ggplot(mapping = aes(x =  factor(section_name,levels=rev(top_sections_sort)), 
                           y = p_change_feb20, 
                           colour = factor(nationality_name,levels=rev(nat_names_sort)), #since horizontal bar reverses orders, we need to reverse too
                           fill = factor(nationality_name,levels=rev(nat_names_sort)),
                           text = paste(section_name, "\n",
                                        "Nationality: ",nationality_name, "\n",
                                        "Change: ", perc_form(p_change_feb20),"%", "\n",
                                        sep = "")))   +
      geom_bar(stat = "identity", position = position_dodge(), width=0.5)+ # bars
      coord_flip()  + # flip to horizontal
      geom_vline(xintercept=c(n_top - 0.5),colour="gray45", 
                 linetype = "dotted")+ #adds line below total
      geom_hline(aes(yintercept=0), colour="gray45") +
      scale_color_manual(values = rev(pal_named), aesthetics = "colour")+ #fills bars
      scale_fill_manual(values = rev(pal_named), aesthetics = "fill")+ #outlines bars
      theme_set(theme_gla(gla_theme = "default", y_label_length=100)) + #GLA theme and removes lines below y-axis labels
      scale_y_continuous(labels = dollar_format(prefix = "", 
                                                                     suffix = "%", 
                                                                     largest_with_cents = 1)) +
      theme(plot.margin = unit(c(1,1,1,1), "cm"))+
      scale_x_discrete() +
      theme(panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x = element_line( size=.5 ),
            axis.text.y = ggplot2::element_text(
              hjust = 0, vjust = 0.5,
              margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0,
                                       unit = "pt")),
            axis.ticks.length.y = ggplot2::unit(x = 0, units = "pt")) + 
       guides(colour=guide_legend(reverse=TRUE),
              fill=guide_legend(reverse=TRUE)) +
      labs(title = paste0("Payrolled employments percentage change by industry in ",dat_region),
           subtitle = paste0("Change by nationality Feb 2020-",last_date_month),
           caption = "\nSource: HMRC PAYE RTI.")+
      theme(plot.caption = element_text(color = rgb(166,166,166,maxColorValue = 255)))
    section_change_bar
    
    ggsave(here::here("OUTPUT","PLOTS","across_sections",gsub(" ","_",paste0(chart_name,".png"))), device = "png", width = 8, height = 8, units = "in")
    
    if (dat_region=="London") {
      plotly <- ggplotly(section_change_bar,tooltip = "text") %>% 
        ggla_plotly_settings()    %>% 
        layout(title = list(text = paste0("<b>","Payrolled employments percentage change by industry in ",dat_region,"</b>",
                                          "<br>",
                                          "<sup>",
                                          paste0("Change by nationality Feb 2020-",last_date_month," top ", n_top-1, " industries"),
                                          "</sup>",
                                          "<br>"),
                            font = list(size = 22),
                            y = .95, xref = "plot"),
               xaxis = list(tickfont = list(size = 15),showgrid=TRUE),
               yaxis = list(tickfont = list(size = 15),showgrid=FALSE),
               legend = list(font = list(size = 15),title=list(text=""))) %>% 
        reverse_legend_labels() %>% 
        plotly_modebar
      
      london_charts[[chart_n]] <- plotly
      names(london_charts)[chart_n] <- chart_name
      
      chart_n <- chart_n + 1 
    }
    
  }
  
  #.............................................................................
  ## 02.4. Ad hoc charts ----
  #.............................................................................
  
  #.............................................................................
  # Create trend chart for London nationalities overall
  #.............................................................................
  
  nats_used <- c("EU","Non-EU")
  
  pal <- gla_pal(gla_theme = "default", palette_type = "categorical", n = 2)
  pal_named <- setNames(object=pal,nm = nats_used)
  
  # Define function
  lond_trend <- function(var, sufx="",subtitle,yscale, ch_n=chart_n, l_ch=london_charts) {
    
    if (var =="share") {
      sufx <-  "%"
      desc <- "Share: "
      subtitle <- "Nationality as share of total payrolled employments"
      yscale <- c(15,24)
      f_x <- function(x) {
        perc_form(x)
      }  
    }
    if (var =="counts") {
      sufx <-  ""
      desc <- "Count: "
      subtitle <- "Count of total payrolled employments"
      yscale <- c(600000,1.1e6)
      f_x <- function(x) {
        value_form(x,s=3)
      }
    }
    
    chart_name <- paste0("london_trend_",var)
    
    trend_chart <- paye_master_long_detail %>% 
      filter(geography_name == "London" & section_name == "Overall" & measure_name == var & nationality_name %in% nats_used) %>% 
      ggplot(mapping = aes(x = date_day, y = measure_value, 
                           group = nationality_name,
                           text = paste("Nationality: ",nationality_name, "\n",
                                        date,"\n",
                                        desc,f_x(measure_value),sufx, "\n",
                                        sep = ""))) +
      ggla_line(aes(colour = nationality_name))+
      scale_colour_manual(values = pal_named)+
      geom_vline(aes(xintercept = as.numeric(ymd("2020-03-01"))),
                 linetype = "dotted",
                 size = 1 * mm_to_pt,
                 colour = rgb(166,166,166,maxColorValue = 255)) + # mark lockdowns start
      coord_cartesian(clip = 'off') +
      scale_y_continuous(limit=yscale,labels = dollar_format(prefix = "", 
                                                             largest_with_cents = 1,
                                                             suffix = sufx)) +
      scale_x_date(date_labels = "%b %y",date_breaks = "1 year") +
      theme(plot.margin = unit(c(1,1,1,1), "cm"))+
      labs(title = paste0("Employment by nationality within London"),
           subtitle = subtitle,
           caption = "PAYE RTI data, non-seasonally adjusted.") +
      theme(plot.caption = element_text(color = rgb(166,166,166,maxColorValue = 255)))
    
    ggsave(here::here("OUTPUT","PLOTS","ad_hoc",gsub(" ","_",paste0(chart_name,".png"))), device = "png", width = 8, height = 8, units = "in")
    
    # Plotly
    plotly <- ggplotly(trend_chart,tooltip = "text") %>% 
      ggla_plotly_settings()    %>% 
      layout(title = list(text = paste0("<b>","Employment by nationality within London","</b>",
                                        "<br>",
                                        "<sup>",
                                        paste0(subtitle),
                                        "</sup>",
                                        "<br>"),
                          font = list(size = 22),
                          y = .95, xref = "plot"),
             xaxis = list(tickfont = list(size = 15)),
             yaxis = list(tickfont = list(size = 15)),
             legend = list(font = list(size = 15),title=list(text="")),
             hovermode = "x") %>% 
      plotly_modebar
    
    london_charts[[chart_n]] <<- plotly
    names(london_charts)[chart_n] <<- chart_name
    
   # assign(london_charts,l_ch, envir = .GlobalEnv)
    
    chart_n <<- chart_n + 1 
    #assign(chart_n,ch_n, envir = .GlobalEnv)
  }
  
  # Counts chart
  lond_trend(var="counts")


  # Shares chart
  lond_trend(var="share")


  