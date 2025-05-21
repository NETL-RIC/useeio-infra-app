# ABOUT ----------------------------------------------------------------------
#
# Server.R
#
# Authors: F. Bartram, M. Henriksen, D. Carlson, S. Matthews
#
# Last updated: 2023-08-18
#
# Purpose: This project migrates the NETL Infrastructure version of the 
# U.S. Environmentally-Extended Input-Output (USSEIO) model from Excel to a 
# web-based interface.
#
# Setup: -----
# 1. Install necessary package
#   - shiny, v.1.7.4 (https://rstudio.github.io/shiny/)
# 2. Load libraries
#   - library(shiny)
#   - library(DT)
#   - library(ggplot2)
#   - library(plotly)
#   - library(reshape2)
#   - library(data.table)
#   - library(tm)
#   - library(SnowballC)
#   - library(scales)
#   - library(plyr)
#   - library(dplyr)
#   - library(shinyBS)
#   - library(gt)
#   - library(tidyverse)
#   - library(bigD)
#   - library(juicyjuice)
#   - library(reactR)
#   - library(openxlsx)
#   - library(shinycssloaders)
#   - library(shinyhttr)
#   - library(shinyWidgets)
#   - library(glue)

library(shiny)
library(DT)
library(ggplot2)
library(plotly)
library(reshape2)
library(data.table)
library(tm)
library(SnowballC)
library(scales)
library(plyr)
library(glue)
library(tidyverse)
library(dplyr)
library(shinyBS)
library(gt)
library(tidyverse)
library(bigD)
library(juicyjuice)
library(reactR)
library(openxlsx)
library(shinycssloaders)
library(shinyhttr)
library(shinyWidgets)

# 3. Run shiny app
#
# References: ----
# - https://www.mdpi.com/2076-3417/12/9/4469
# - https://github.com/USEPA/useeior
# - https://community.rstudio.com/t/update-dt-table-dropdowns-with-reactive-data-in-shiny/96100
# - https://mastering-shiny.org/index.html
# - https://groups.google.com/g/shiny-discuss/c/sCj7wGyw7Os
# - https://www.r-bloggers.com/2023/04/heatmap-formatting-of-a-table-with-dt/
#
#
# Load model created using buildModel('USEEIOv2.0.1-411') after loading the useeior package
 model <- readRDS("model.rds")
# FUNCTIONS ------------------------------------------------------------------
# - convert_naics_to_useeio


# Converts from the NAICS Codes to the related USEEIO title(s) based on crosswalk 
# which is part of USEEIOr model
convert_naics_to_useeio <- function(input_naics_code) {
  search_crosswalk <- function(input_naics_code) {
    naics_codes_crosswalk <- model[["crosswalk"]][["NAICS"]]
    useeio_codes_crosswalk <- model[["crosswalk"]][["USEEIO"]]
    match <- naics_codes_crosswalk ==  input_naics_code
    useeio_code <- useeio_codes_crosswalk[match & is.na(match) != TRUE]
    return(useeio_code)
  }
  useeio_code_output <- character(0)
  while ((nchar(input_naics_code) >= 2) & (length(useeio_code_output) == 0)) {
    useeio_code_output <- search_crosswalk(sub("0+$", "", input_naics_code))
    input_naics_code <-  substr(input_naics_code, 1, nchar(input_naics_code) - 1)
  }
  useeio_names <- model[["Commodities"]][["Name"]]
  useeio_codes_model <- model[["Commodities"]][["Code"]]
  useeio_name_output <- useeio_names[useeio_codes_model == useeio_code_output]
  return(useeio_name_output)
}

# Converts from the USEEIO title to related 6-digit NAICS Code(s) based on crosswalk which is part of USEEIOr model
convert_useeio_to_naics <- function(input_useeio_code) {
  naics_codes_crosswalk <- model[["crosswalk"]][["NAICS"]]
  useeio_codes_crosswalk <- model[["crosswalk"]][["USEEIO"]]
  match = useeio_codes_crosswalk == input_useeio_code
  naics_code <- (naics_codes_crosswalk[match & is.na(match) != TRUE])
  naics_code = paste(naics_code[nchar(naics_code) == 6])
  return(naics_code)
}

# - docsearch
# Returns a ranked list of USEEIO industry matches for a query

# Read dictionary of abbreviations
dict_sheet <- read.csv('Dict_Sheet.csv')
abbreviations_dict <- dict_sheet$Full.Name
names(abbreviations_dict) <- dict_sheet$Abbreviation

# examples_grouped_by_POS_and_source_no_descriptions3.csv contains examples grouped by industry
# name and separated by part of speech and source (NAICS website vs. literature).

# Creates doc list for each category
df_grouped <- read.csv("examples_grouped_by_POS_and_source_no_descriptions3.csv")[,-1]
doc.list <- split(df_grouped[, "nouns"], seq(nrow(df_grouped)))
doc.list2 <- split(df_grouped[, "lit_nouns"], seq(nrow(df_grouped)))
doc.list3 <- split(df_grouped[, "others"], seq(nrow(df_grouped)))
doc.list4 <- split(df_grouped[, "lit_others"], seq(nrow(df_grouped)))
N.docs <- length(doc.list)
names(doc.list) <- df_grouped$y
names(doc.list2) <- df_grouped$y
names(doc.list3) <- df_grouped$y
names(doc.list4) <- df_grouped$y


docsearch <- function(query) {
  defaultW <- getOption("warn")
  options(warn = -1)
  
  # Splits query by slash or dash and converts to all lower case
  query = gsub("/|-",' ', query)
  query = tolower(query)
  
  # Checks each words to see if it is in the dictionary, in which case full names
  # are added
  
  query_split <- strsplit(query, " ")[[1]]
  
  for (i in 1:length(query_split)) {
    index = which(tolower(names(abbreviations_dict)) %in% query_split[i])  
    name <- abbreviations_dict[index]
    if (all(!is.na(name))) {
      query <- paste(query, paste(name, collapse = ' '))
    }
  }
  
  # function search performs the search for a given doclist and query and calculates scores
  search <- function(query, doc.list) {
    my.docs <- VectorSource(c(doc.list, query))
    my.docs$Names <- c(names(doc.list), "query")
    my.corpus <- Corpus(my.docs)
    
    my.corpus <- tm_map(my.corpus, removePunctuation)
    my.corpus <- tm_map(my.corpus, stemDocument)
    my.corpus <- tm_map(my.corpus, removeNumbers)
    my.corpus <- tm_map(my.corpus, removeWords, c(stopwords("english")))
    my.corpus <- tm_map(my.corpus, tolower)
    my.corpus <- tm_map(my.corpus, stripWhitespace)
    
    
    term.doc.matrix.stm <- TermDocumentMatrix(my.corpus)
    term.doc.matrix <- as.matrix(term.doc.matrix.stm)
    
    
    # Remove rows for words only appearing in query to prevent NAN scores
    term.doc.matrix <- term.doc.matrix[rowSums(term.doc.matrix[,-ncol(term.doc.matrix)]) != 0, ]
    
    get.tf.idf.weights <- function(tf.vec, df) {
      # Computes tfidf weights from a term frequency vector and a document
      # frequency scalar
      weight = rep(0, length(tf.vec))
      weight[tf.vec > 0] = (1 + log2(tf.vec[tf.vec > 0])) * log2(N.docs/df)
      weight
    }
    
    get.weights.per.term.vec <- function(tfidf.row) {
      term.df <- sum(tfidf.row[1:N.docs] > 0)
      tf.idf.vec <- get.tf.idf.weights(tfidf.row, term.df)
      return(tf.idf.vec)
    }
    
    tfidf.matrix <- t(apply(term.doc.matrix, c(1), FUN = get.weights.per.term.vec))
    colnames(tfidf.matrix) <- colnames(term.doc.matrix)
    
    
    tfidf.matrix <- scale(tfidf.matrix, center = FALSE, scale = sqrt(colSums(tfidf.matrix^2)))
    
    query.vector <- tfidf.matrix[, (N.docs + 1)]
    tfidf.matrix <- tfidf.matrix[, 1:N.docs]
    
    doc.scores <- t(query.vector) %*% tfidf.matrix
    
    results.df <- data.frame(doc = names(doc.list), 
                             score = t(doc.scores), text = unlist(doc.list))
    results.df <- results.df[order(results.df$score, decreasing = TRUE), ]
    
    return( results.df)
    options(warn = defaultW)
  }
  
  
  # Performs search for each category and obtain scores
  df_test = search(query, doc.list)
  colnames(df_test)[2] <- "score_nouns"
  df_test2 = search(query, doc.list2)
  colnames(df_test2)[2] <- "score_lit_nouns"
  df_test3 = search(query, doc.list3)
  colnames(df_test3)[2] <- "score_others"
  df_test4 = search(query, doc.list4)
  colnames(df_test4)[2] <- "score_lit_others"
  
  # Merge all dataframes to one
  df_merged_nouns <- merge(df_test, df_test2, by = "doc", all.x = TRUE, all.y = TRUE)
  df_merged_others <- merge(df_test3, df_test4, by = "doc", all.x = TRUE, all.y = TRUE)
  df_merged <- merge(df_merged_nouns, df_merged_others, by = "doc", all.x = TRUE, all.y = TRUE)
  
  # Replace nas with zero (for industries which did not appear in the literature examples)
  df_merged[is.na(df_merged)] = 0
  
  # Apply weighting system (see documentation for explanation)
  df_merged$score_weighted = 6*df_merged$score_nouns + 8*df_merged$score_lit_nouns + df_merged$score_others + 6*df_merged$score_lit_others
  
  # Order industries by decreasing score
  df_merged <- df_merged[order(df_merged$score_weighted, decreasing = TRUE), ]
  
  choices = df_merged[, "doc"]
  # Remove empty strings, NAs, and duplicates
  choices = choices[nzchar(choices)]
  choices = na.omit(choices)
  choices = choices[!duplicated(choices)]
  return(choices)
}


drop_small_numbers <- function(input_vector) {
  zero_cutoff <- 0.001
  input_vector[input_vector <= zero_cutoff] = 0
  return(input_vector)
}

# Given a NAICS code as input, generates the link to the corresponding NAICS webpage
generate_link = function(naics_code) {
  url = paste0("https://www.census.gov/naics/?input=",naics_code,"&year=2012&details=",naics_code)
  url = htmltools::a(href = url, naics_code)
  naics_link = gt::html(as.character(url))
  return(naics_link)
}


# Replaces numbers less than 0.001 with 0 to improve formatting
drop_small_numbers <- function(input_vector) {
  zero_cutoff <- 0.001
  input_vector[input_vector <= zero_cutoff] = 0
  return(input_vector)
}

# Given a NAICS code as input, generates the link to the corresponding NAICS webpage
generate_link = function(naics_code) {
  url = paste0("https://www.census.gov/naics/?input=",naics_code,"&year=2012&details=", 
               naics_code)
  url = htmltools::a(href = url, naics_code)
  naics_link = gt::html(as.character(url))
  return(naics_link)
}

# Adds space after first colon in string
add_space <- function(text) {
  text_split = regmatches(text, regexpr(":", text), invert = TRUE)
  text = paste0(text_split[[1]][1],": ",text_split[[1]][2:length(text_split[[1]])])
  return(text)
}

#' Shiny App server for USEEIOr.
#' Calculates and displays economic, greenhouse gas, and other environmental impacts based on a loaded CSV file with equipment names
#' in one column and costs in another or manually entered industries and costs

server <- function(input, output, session) {
  
  # Show progress bar to indicate that app is loading
  progress <- Progress$new(session, min = 1, max = 2)
  progress$set(message = "Loading tabs... ", value = 1)
  on.exit(progress$close())

  # Access data ---------------------------------------------------------------
  
  # Access required matrices from model----------------------------
  N <- model[["N"]]
  L <- model[["L"]]
  C <- model[["C"]]
  M <- model[["M"]]
  A <- model[["A"]]
  Rho <- model[["Rho"]]
  
  # Industry names and codes  -------------------------------------------------
  
  commodities <- model[["Commodities"]]
  
  # Add spaces after colon
  for (i in 1:length(commodities$Description)) {
    commodities$Description[i] = add_space(commodities$Description[i])
  }
 
  industry_codes <-  commodities[["Code"]]
  industry_names <-  commodities[["Name"]]
  
  # List of chemical species of greenhouse gases used in the model -------------
  species_of_ghgs <- 
    model[["SatelliteTables"]][["totals_by_sector"]][["GHG"]][["Flowable"]][1:15]
  
  # Obtain information on non-GHG environmental impacts of interest ------------
  indicators_specific <- c("Acidification Potential", "Eutrophication Potential",
                           "Ozone Depletion", "Smog Formation Potential")
  c_indicators_specific <- C[rownames(C) %in% indicators_specific, ]
  C <- c_indicators_specific[, colSums(  c_indicators_specific) != 0]
  
  
  # Initialize demand vector to zero for all industries ------------------------
  demand <- numeric(length(industry_names))
  
  # Get indicator units (for non-greenhouse gas impacts) ----------------------
  indicator_names <- model[["Indicators"]][["meta"]][["Name"]]
  indicator_units <- model[["Indicators"]][["meta"]][["Unit"]]
  indicator_df <- cbind(indicator_names,indicator_units)
  indicator_df <- indicator_df[order(indicator_df[, 1]), ]
  units <- indicator_df[, 2]
  
  
  # Import GWP info from CSV file ----------------------------------------------
  gwp_df <- read.csv("IPCC GWP Factors.csv",
                     stringsAsFactors = FALSE, 
                     fileEncoding = "latin1"
  )
  gwps_of_interest <- gwp_df[gwp_df$Common.Name %in% c(species_of_ghgs,
                                                       "PFC-116; Hexafluoroethane",
                                                       "PFC-14; Carbon tetrafluoride",
                                                       "PFC-218; Perfluoropropane", 
                                                       "Methane (fossil)",
                                                       "PFC-318; Perfluorocyclobutane"), 
                                                        c(2, 4, 5)]
  
  # Change names of chemical species in dataframe gwps_of_interest to align with those in useeior model
  old_names <- c("Methane (fossil)", "PFC-116; Hexafluoroethane", 
                 "PFC-14; Carbon tetrafluoride", "PFC-218; Perfluoropropane", 
                 "PFC-318; Perfluorocyclobutane")
  new_names <- c("Methane","Hexafluoroethane", "Carbon tetrafluoride", 
                "Perfluoropropane","Perfluorocyclobutane")
  gwps_of_interest[(gwps_of_interest$Common.Name %in% old_names), 1] <- new_names
  
  
  # Captions and normalization factors for Other Environmental Impacts tab
  impacts_caption <- "This table shows the total impact for each of the categories 
                      listed, across all industries and relevant chemical species,
                      given in the indicated units. "
  
  acid_ptnl_caption <- "Acidification evaluates the increase in hydrogen ion (H+) 
                        resulting from emissions of a chemical species, which may 
                        be caused by the direct addition of acids or the addition 
                        of other species which increase the acidity through chemical 
                        or biological action. Species which cause acidification 
                        can lead to acid rain, and harm ecosystems and infrastructure. 
                        Primary contributors to acid rain include sulfur dioxide 
                        and nitrogen oxides released by fossil fuel combustion. 
                        Emissions are given in kg air emissions for each chemical species. "
  
  eutro_ptnl_caption <- "Eutrophication refers to the addition of excess nutrients, 
                        such as nitrogen and phosphorus, which may cause harm to 
                        aquatic ecosystems.  Emissions are given in kg water or 
                        air emissions for each chemical species. "
  
  o3_ptnl_caption <- "Ozone depletion evaluates substances which decrease the 
                      stratospheric ozone level. These substances are known as 
                      chlorofluorocarbons (CFCs). The ozone layer protects against 
                      radiation, which is harmful to human health, plants, and 
                      marine life. Emissions are given in kg air emissions for
                      each chemical species."
  
  
  smog_ptnl_caption <- "Smog Formation Potential evaluates creation of ground 
                        level ozone through chemical reactions between nitrogen 
                        oxides (NOx) and volatile organic compounds (VOCs) in 
                        sunlight. Ground level ozone can have human health effects, 
                        leading to respiratory issues, and can harm ecosystems 
                        and crops. Emissions are given in kg air emissions for 
                        each chemical species."
  
  normalization_factors <- c(91, 22, .16, 1400)
  names(normalization_factors) <- c("Acidification Potential",
                                    "Eutrophication Potential", 
                                    "Ozone Depletion", 
                                    "Smog Formation Potential")
  impact_categories_with_units <- c("Acidification Potential (kg SO2 eq)",
                                    "Eutrophication Potential (kg N eq)",
                                    "Ozone Depletion (kg CFC-11 eq)",
                                    "Smog Formation Potential (kg O3 eq)")
  
  # Set up reactive values -----------------------------------------------------
  # r$entry is a reactive value used to keep track of the method of data input
  # 1 indicates file upload and 0 indicates Manual Entry
  r <- reactiveValues(entry = 1)
  observeEvent(input$Upload, {
    r$entry <- 1
  })
  observeEvent(input$Manual, {
    r$entry <- 0
  })
  
  # v$num_industries is a reactive value used to keep track of whether the top 5 
  # or 10 industries should be displayed on graphs
  v <- reactiveValues(num_industries = 5)
  observeEvent(input$top5, {
    v$num_industries <- 5
  })
  observeEvent(input$top10, {
    v$num_industries <- 10
  })
  
  v_econ <- reactiveValues(num_industries_econ = 5)
  observeEvent(input$top5_econ, {
    v_econ$num_industries_econ <- 5
  })
  observeEvent(input$top10_econ, {
    v_econ$num_industries_econ <- 10
  })
  
  num_other_industries <- reactive({
    num_other_industries  <- 411 - v$num_industries
    
  })
  
  N_econ <- reactive({
    N_econ <- 411 - v_econ$num_industries_econ
  })
  
  # Controls hiding and showing of tabs ----------------------------------------
  # Hide GHG and Other Environmental Impacts tabs initially
  hideTab(inputId = "tabs", target = "File Upload")
  hideTab(inputId = "tabs", target = "Manual Entry")
  hideTab(inputId = "tabs", target = "GHG Impacts")
  hideTab(inputId = "tabs", target = "Other Environmental Impacts")
  hideTab(inputId = "tabs", target = "Economic Impacts")
  
  # When an entry method is selected, show the relevant tab and hide calculations tabs
  observeEvent(input$Upload, {
    showTab(inputId = "tabs", target = "File Upload", select = TRUE)
    hideTab(inputId = "tabs", target = "Manual Entry")
    hideTab(inputId = "tabs", target = "GHG Impacts")
    hideTab(inputId = "tabs", target = "Other Environmental Impacts")
    hideTab(inputId = "tabs", target = "Economic Impacts")
  })
  
  observeEvent(input$Manual, {
    showTab(inputId = "tabs", target = "Manual Entry", select = TRUE)
    hideTab(inputId = "tabs", target = "File Upload")
    hideTab(inputId = "tabs", target = "GHG Impacts")
    hideTab(inputId = "tabs", target = "Other Environmental Impacts")
    hideTab(inputId = "tabs", target = "Economic Impacts")
  })
  
  # Show tabs when a file is uploaded or when an enter button is clicked (if non-zero economic demand has been entered)
  observeEvent(input$file1, {
      showTab(inputId = "tabs", target = "GHG Impacts")
      showTab(inputId = "tabs", target = "Other Environmental Impacts")
      showTab(inputId = "tabs", target = "Economic Impacts")
  })
  
  observeEvent(input$enter, {
    if (sum(y()) != 0) {
      showTab(inputId = "tabs", target = "GHG Impacts")
      showTab(inputId = "tabs", target = "Other Environmental Impacts")
      showTab(inputId = "tabs", target = "Economic Impacts")
    } else {
      showNotification("Please enter at least one non-zero economic input.")
    }
  })
  
  observeEvent(input$enter_upload, {
    if (sum(y()) != 0) {
      showTab(inputId = "tabs", target = "GHG Impacts")
      showTab(inputId = "tabs", target = "Other Environmental Impacts")
      showTab(inputId = "tabs", target = "Economic Impacts")
    } else {
      showNotification("Please enter at least one non-zero economic input.")
    }
  })
  

  # Define color scheme --------------------------------------------------------
  graph_colors <- reactive({
    color_start <- 12 - v$num_industries
    graph_colors <- c(hue_pal()(1), hue_pal()(11)[color_start:11])
    return(graph_colors)
  }) 
  
  graph_colors_econ <- reactive({
    color_start <- 12 - v_econ$num_industries_econ
    graph_colors_econ <- c(hue_pal()(1), hue_pal()(11)[color_start:11])
    return(graph_colors_econ)
  }) 

  # Create table from uploaded file
  counter <- reactiveVal(0)
  get_sel_id <- reactive({
    isolate(counter(counter() + 1))
    paste0("sel", counter())
  })
  
  data <- reactive({
    req(input$file1)
    df <- read.csv(input$file1$datapath,
                   header = TRUE, sep = input$sep
    )
    
    for (i in 1:nrow(df)) {
      equipment_name <- df[i, 1]
      best_industries <- docsearch(equipment_name)
      ranked_industries = best_industries
      ranked_industries <- c(best_industries, industry_names)
      ranked_industries <- ranked_industries[!duplicated(ranked_industries)]
      df$industry[i] <- as.character(selectizeInput(paste0(get_sel_id(), i),
                                                    "",
                                                    choices = ranked_industries,
                                                    selected = ranked_industries[1],
                                                    width = "300px"
      ))
      
      
    }
    df
  })
  
  output$upload_table <- DT::renderDataTable(data(), 
                                             colnames = c("Equipment Name","Cost",
                                                          "Please select an industry"), 
                                             rownames = FALSE,
                                             escape = FALSE, 
                                             selection = "none", 
                                             server = FALSE,
                                             options = list(dom = "t", 
                                                            paging = FALSE, 
                                                            ordering = FALSE),
     callback = JS("table.rows().every(function(i, tab, row) {
        var $this = $(this.node());
        $this.attr('id', this.data()[0]);
        $this.addClass('shiny-input-container');
      });
      Shiny.unbindAll(table.table().node());
      Shiny.bindAll(table.table().node());"),
  )
  
  
  # Table of industry descriptions and links
  descriptions <- reactive({
    descriptions <- sapply(1:nrow(data()), function(i)
      commodities[commodities[,2] == input[[paste0(get_sel_id(), i)]],5])
    descriptions <- data.frame(descriptions)
  })
  
  naics_codes <- reactive({
    naics_codes <- list()
    for (i in 1:nrow(data())) {
      code = convert_useeio_to_naics(commodities[commodities[, 2] == 
                                                   input[[paste0(get_sel_id(), i)]], 1])
      naics_codes[[i]] <- code
    }
    return(naics_codes)
  })
  
  
  output$descriptions_table <-  gt::render_gt({
    links = list()
    for (i in 1:length(naics_codes())) {
      ith_input_naics_codes =  naics_codes()[[i]]
      ith_input_links = gt::html('')
      for (k in 1:length(ith_input_naics_codes)) {
        ith_input_links[[k]] = generate_link(ith_input_naics_codes[k])
      }
      links[[i]] =  ith_input_links
    }
    
    df <- tibble(
      Description = descriptions()$descriptions,
      "NAICS code(s)" =  links)
    df %>% gt()
    
    df
    
  })

  
  # For Manual Entry option ----------------------------------------------------
  
  num_inputs <- reactive({
    if ((input$num <= 0) || is.na(input$num)) {
      num = 1
    } else {
      num <- as.integer(input$num)
    }
    return(num)
  })

  
  output$manual_inputs <- renderUI({
    ui_parts <- c()
    for(i in 1:num_inputs()) {
      ui_parts[[i]] <- fluidRow(
        column(3,   selectizeInput(paste0("inds", i),
                                   label = paste(label = "Please select an industry:", i),
                                   choices = industry_names)
        ),
        column(3, 
               numericInput(paste0("amount", i),
                            label = paste(label = "Please enter a cost (in", 
                                          as.character(input$Year), 
                                          "U.S. Dollars):", i), value = 0)
        ),
      )
    }
    ui_parts
  })

  
  # Build demand vector --------------------------------------------------------
  y <- reactive({
    if (r$entry == 1) {
      # File upload
      industries <- sapply(1:nrow(data()), function(i) 
        input[[paste0(get_sel_id(), i)]])
      
      descriptions <- sapply(1:nrow(data()), function(i) 
        commodities[commodities[,2] == industries[i],5])
      
      for (i in 1:nrow(data())) {
        costs <- numeric(nrow(data()))
        costs[i] <- data()[i,2]
        if (is.na(costs[i])) {
          costs[i] = 0
        }
        
        demand[which(industry_names == industries[i])] <- costs[i] + 
          demand[which(industry_names == industries[i])]
      }
    }
    else {
      
      # Manual Entry
      for (i in seq_len(num_inputs())) {
        cost <- input[[paste0("amount", i)]]
        industry <- input[[paste0("inds", i)]]
        index <- which(industry_names == industry)
        demand[index] <- demand[index] + cost
      }
    }
    
    # Inflate or deflate demand based on chosen input year
    price_year_ratios <- Rho[, input$Year]
    demand <- demand * price_year_ratios
    return(demand)
  })
  
  # Economic Calculations ------------------------------------------------------
  
  # economic_result stores total economic impact on each sector 
  economic_result <- reactive({
    economic_result <- L %*% (y())
    return(economic_result)
  })
  
  direct_economic_result <- reactive({
    direct_economic_result <- A %*% (y())
    return(direct_economic_result)
  })
  
  economics_df <- reactive({
    # Dataframe economics_df contains industry code and name, and economic impact for each industry
    economics_df <- data.frame(industry_codes, industry_names, economic_result(), 
                               direct_economic_result())
    economics_df <- economics_df[order(rowSums(economics_df[3]), decreasing = TRUE), ]
    return(economics_df)
  })
  
  economics_top_industries_df <- reactive({
    # Returns dataframe with information about 5 or 10 (depending on user selection) 
    # industries which contribute most to emissions.
    economics_top_industries_df <- economics_df()[1:v_econ$num_industries_econ, ]
    start <- v_econ$num_industries_econ + 1
    others <- colSums(economics_df()[start:nrow(economics_df()), 3:4])
    economics_top_industries_df[nrow(economics_top_industries_df) + 1, ] <- 
      c(paste("All Other", N_econ(),"Industries"), 
        paste("All Other", N_econ(),"Industries"), as.numeric(others))
    economics_top_industries_df$economic_result <- as.numeric(economics_top_industries_df$economic_result)
    economics_top_industries_df$direct_economic_result <- as.numeric(economics_top_industries_df$direct_economic_result)
    # Lock in factor level order to ensure that the graphs will have the same order of industries as the dataframe
    economics_top_industries_df$industry_names <- factor(
      economics_top_industries_df$industry_names, 
      levels = economics_top_industries_df$industry_names)
    return(economics_top_industries_df)
  })
  
  # Table for total impacts
  output$economics_table <- renderDataTable({
    totals_line <- c( 'Total', 'Total', colSums(economics_df()[,3:4]))
    economics_table <- rbind(totals_line, economics_df())
    for (col in 3:4) {
      economics_table[, col] <- as.numeric(as.character(economics_table[, col]))
      economics_table[, col] <- drop_small_numbers(economics_table[, col])
    }
    economics_table[, 3:4] <- signif(economics_table[, 3:4], 3)
    economics_table[, 3:4] <- sapply(economics_table[, 3:4], 
                                     function(x) paste0("$",x))
    
    for (col in 3:4) {
      economics_table[, col] = prettyNum( economics_table[, col], big.mark = ",", scientific = FALSE)
    }
    DT::datatable(economics_table,
                  colnames = c("Industry Code","Industry Name",
                               "Total Economic Impact (2012 U.S. Dollars)",
                               "Direct Economic Impact (2012 U.S. Dollars)"),
                  rownames = FALSE, 
                  
    )
  })
  
  output$plot_economic <- renderPlotly({
    economics_top_industries_df <- economics_top_industries_df()
    economics_top_industries_df[, 3:4] <- sapply(economics_top_industries_df[, 3:4], 
                                     function(x) paste0("$",x))
    ggplot(data = economics_top_industries_df,
           aes(x = industry_names, y = economic_result)) +
      geom_bar(stat = "identity", fill = graph_colors_econ()) +
      coord_flip() +
      ggtitle("Total Economic Impact on Top Industries") +
      xlab("Industry") +
      ylab("Total Economic Impact (2012 U.S. Dollars)") +
      theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1))
  })
  
  output$plot_economic_direct <- renderPlotly({
    # Returns plot of economic impact for industries which contribute most to emissions
    ggplot(data = economics_top_industries_df(),
           aes(x = industry_names, y = direct_economic_result)) +
      geom_bar(stat = "identity", fill = graph_colors_econ()) +
      coord_flip() +
      ggtitle("Direct Economic Impact on Top Industries") +
      xlab("Industry") +
      ylab("Direct Economic Impact (2012 U.S. Dollars)") +
      theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1))
  })
  
  
  # Download button
  output$downloadEconomics <- downloadHandler(
    filename = function() {
      paste0("Economic_Impacts", ".csv")
    },
    content = function(file) {
      # Write the dataset to the `file` that will be downloaded
      write.csv(economics_df(), file)
    }
  )
  
  
  # Output speciated_ghgs stores the GHG emissions broken down by industry and chemical species
  speciated_ghgs <- reactive({
    req(sum(economic_result()) != 0)
    speciated_ghgs <- data.frame(industry_names)
    # Calculate emissions of each GHG in species for all industries (kg)
    for (i in 1:length(species_of_ghgs)) {
      flow <- paste(species_of_ghgs[i], "/emission/air/kg", sep = "")
      species_vector <- M[flow, ]
      species_matrix <- matrix(diag(species_vector), ncol = 411)
      emissions_by_industry <- species_matrix %*% (economic_result())
      speciated_ghgs[species_of_ghgs[i]] <- 
        as.numeric(as.character(emissions_by_industry))
    }
    
    colnames(speciated_ghgs)[1] <- "Industry"
    
    # Return with more significant/ well-known species first
    ghg_order <-  c("Industry", "Carbon dioxide","Methane", "Nitrous oxide", 
                    species_of_ghgs[7:12], species_of_ghgs[1:4], species_of_ghgs[14:15])
    speciated_ghgs <- setcolorder(speciated_ghgs, ghg_order)
    
    # Total amounts for each chemical species
    totals_line <- c('Totals', colSums(speciated_ghgs[,2:ncol(speciated_ghgs)]))
    speciated_ghgs <- rbind(totals_line, speciated_ghgs)
    for (col in 2:ncol(speciated_ghgs)) {
      speciated_ghgs[, col] <- as.numeric(as.character(speciated_ghgs[, col]))
    }
    return(speciated_ghgs)
  })
  
  
  #   Speciated Emissions Calculations, 100 yr ----------------------------------
  # Calculate emissions of each GHG in species for all industries (kg co2 eq. - 100 yr GWP)
  # unsorted (for use to calculate totals)
  
  speciated_ghgs_100yr_unsorted <- reactive({  
    speciated_ghgs_100yr_unsorted <- speciated_ghgs()[-1,]
    for (i in 1:length(species_of_ghgs)) {
      gwp <- gwps_of_interest[gwps_of_interest$Common.Name == species_of_ghgs[i], 2]
      speciated_ghgs_100yr_unsorted[species_of_ghgs[i]] <- 
        speciated_ghgs_100yr_unsorted[species_of_ghgs[i]] * gwp
    }
    return(speciated_ghgs_100yr_unsorted)
  })
  
  # Sorted
  speciated_ghgs_100yr <- reactive({
    ghg_for_totals <-  speciated_ghgs_100yr_unsorted()[, -1]
    rownames(ghg_for_totals) <-  speciated_ghgs_100yr_unsorted()[, 1]
    ghg_totals <- as.vector(rowSums(ghg_for_totals))
    speciated_ghgs_100yr <-  speciated_ghgs_100yr_unsorted()[order(ghg_totals,
                                                                   decreasing = TRUE), ]
    return(speciated_ghgs_100yr)
  })
  
  speciated_ghgs_for_table <- reactive({  
    ghg_for_totals <-  speciated_ghgs()[, -1]
    rownames(ghg_for_totals) <-  speciated_ghgs()[, 1]
    ghg_totals <- as.vector(rowSums(ghg_for_totals))
    speciated_ghgs_for_table <-  speciated_ghgs()[order(ghg_totals, 
                                                                   decreasing = TRUE), ]
    return(speciated_ghgs_for_table)
  })
  
  # Table of emissions broken down by chemical species and industry
  # Applies separate color scale for each species, with the highest for that species being red 
  # and the lowest being green. Colors only show for the first 10 rows (after the totals row)
  
  output$ghg_table  <- renderDataTable({
    speciated_ghgs_100yr_display <- speciated_ghgs_for_table()
    speciated_ghgs_100yr_display[, species_of_ghgs] <- 
      signif(speciated_ghgs_100yr_display[, species_of_ghgs], 3)
    speciated_ghgs_100yr_calculations <- speciated_ghgs_100yr_display
    speciated_ghgs_100yr_calculations[, species_of_ghgs] <- 
      format(speciated_ghgs_100yr_display[, species_of_ghgs], 
             scientific = FALSE
      )
    colfunc <- colorRamp(c("green", "red"))
    log_first_rows <- log(speciated_ghgs_100yr_display[2:11, species_of_ghgs])
    colors <- lapply(log_first_rows, function(x) {
      lowest  <- min(x)
      highest <- max(x)
      # function to map from (lowest, highest) to (0, 1), mapping 0 to 0.5
      interpfun <- splinefun(
        c(lowest, highest),
        c(0, 1)
      )
      y <- interpfun(x)
      cols <- colfunc(y)
      rgb(cols[, 1L], cols[, 2L], cols[, 3L], maxColorValue = 255)
    })
    cap1 <- HTML(paste0("This table shows the speciated emissions, in kg, for each industry. On the top are the species of greenhouse gases. On the left are the industries. They are automatically ordered with industries which contribute most to total GWP impacts (kg CO", tags$sub("2"), "e, AR6, 100-yr) on top. Please change the page or extend the number of rows to see more industries. The first 10 lines in the table are colored using a logarithmic scale such that for each column, which represents a certain species, industries which contribute most to that species are colored in red and those which contribute least are colored in green."))
    
    dtable <- datatable(speciated_ghgs_100yr_display,rownames = FALSE, caption = cap1) %>%
      formatCurrency(species_of_ghgs, currency = "", interval = 3, mark = ",")
    for (species in species_of_ghgs) {
      dtable <- dtable %>% 
        formatStyle(species,
                    backgroundColor = 
                      styleEqual(speciated_ghgs_100yr_calculations[[species]][2:11], 
                                 colors[[species]]
                      )
        )
    }
    dtable
  })
  
  # Stacked graph for 100 year GWP
  output$stacked_plot_100yr <- renderPlotly({
    top_speciated_ghgs_100yr <- 
      speciated_ghgs_100yr()[speciated_ghgs_100yr()$Industry %in% 
                               top_industries_df()$industry_names, ]
    # Add "other" category for industry
    bottom_row <- 
      as.numeric(colSums(speciated_ghgs_100yr()[, 2:ncol(speciated_ghgs_100yr())]) -
                   colSums(top_speciated_ghgs_100yr[, 2:ncol(top_speciated_ghgs_100yr)]))
    top_speciated_ghgs_100yr[nrow(top_speciated_ghgs_100yr) + 1, 
                             2:ncol(top_speciated_ghgs_100yr)] <- bottom_row
    top_speciated_ghgs_100yr[nrow(top_speciated_ghgs_100yr), 1] <- paste("All Other", num_other_industries(),"Industries")
    
    # Add "other" category for species
    Other <- 
      rowSums(top_speciated_ghgs_100yr[,v$num_industries:ncol(top_speciated_ghgs_100yr)])
    top_speciated_ghgs_100yr <- cbind(top_speciated_ghgs_100yr, Other)
    
    # Converting data from wide to long format
    species_for_graph <- c("Industry","Carbon dioxide", "Methane","Nitrous oxide", 
                           "Other")
    data_long <- top_speciated_ghgs_100yr[, species_for_graph]      
    data_long <- reshape2::melt(data_long, id.vars = "Industry")
    colnames(data_long)[1:3] <- c("Industry", "Species", "Emissions")
    
    # Reorder to show most important industries on bottom
    data_long$Industry <- factor(data_long$Industry, levels = top_industries_df()$industry_names)
    
    ggplot(data_long,                 
           aes(x = Species, y = Emissions, fill = Industry)) +
      ggtitle("100 yr GWP by Industry and Chemical Species") +
      xlab("Chemical Species") +
      ylab(HTML(paste0("Global Warming Potential (kg CO", tags$sub("2"), "e) 
              [AR6, 100-yr time horizon]"))) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = graph_colors()) +
      theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1))
  })
  
  ghg_df <- reactive({
    # Dataframe ghg_df contains industry code and name, and economic impact and emissions for each industry
    ghg_df <- data.frame(industry_codes, industry_names, economic_result(), 
                         ghgs_total_100yr())
    return(ghg_df)
  })
  
  ghg_data_sorted <- reactive({
    # Sorts ghg_df by the ghg column
    ghg_data_sorted <- ghg_df()[order(ghg_df()$ghgs_total_100yr.., 
                                      decreasing = TRUE), ]
    return(ghg_data_sorted)
  })
  
  
  speciated_ghgs_100yr_for_download <- reactive({
    speciated_ghgs_100yr_for_download <- cbind(ghg_data_sorted()$industry_codes, 
                                               speciated_ghgs_100yr())
    colnames(speciated_ghgs_100yr_for_download)[1] <- "Code"
    return(speciated_ghgs_100yr_for_download)
  })
  
  speciated_ghgs_for_download <- reactive({
    speciated_ghgs_for_download <- cbind(c('Total', ghg_data_sorted()$industry_codes), 
                                         speciated_ghgs_for_table())
    colnames( speciated_ghgs_for_download)[1] <- "Code"
    return( speciated_ghgs_for_download)
  })
  
  
  # Download csv with emissions broken down by chemical species and industry
  output$download_ghgs <- downloadHandler(
    filename = function() {
      paste0("speciated_ghgs",".csv")
    },
    content = function(file) {
      write.csv(speciated_ghgs_for_download(), file)
    }
  )
  
  # Emissions calculations (totals) --------------------------------------------
  ghgs_total_100yr <- reactive({
    ghg_for_totals <- speciated_ghgs_100yr_unsorted()[, -1]
    rownames(ghg_for_totals) <-  speciated_ghgs_100yr_unsorted()[, 1]
    ghgs_total_100yr <- as.vector(rowSums(ghg_for_totals))
    return(ghgs_total_100yr)
  })
  
  top_industries_df <- reactive({
    # Returns dataframe with information about 5 or 10 (depending on user selection) 
    # industries which contribute most to emissions.
    top_industries_df <- ghg_data_sorted()[1:v$num_industries, ]
    start <- v$num_industries + 1
    others <- colSums(ghg_data_sorted()[start:nrow(ghg_data_sorted()), 3:4])
    top_industries_df[nrow(top_industries_df) + 1, ] <- 
      c(paste("All Other", num_other_industries(),"Industries"), paste("All Other", num_other_industries(),"Industries"), as.numeric(others))
    top_industries_df$economic_result <- as.numeric(top_industries_df$economic_result)
    top_industries_df$ghgs_total_100yr <- as.numeric(top_industries_df$ghgs_total_100yr)
    # Lock in factor level order to ensure that the graphs will have the same order of industries as the dataframe
    top_industries_df$industry_names <- factor(
      top_industries_df$industry_names, 
      levels = top_industries_df$industry_names)
    return(top_industries_df)
  })
  
  output$plot_gwp_totals_100_yr <- renderPlotly({
    # Returns plot of ghg emissions for industries which contribute most to emissions
    ggplot(data = top_industries_df(), 
           aes(x = industry_names, y = ghgs_total_100yr)) +
      geom_bar(stat = "identity", fill = graph_colors()) +
      coord_flip() +
      ggtitle("GWP of Top Industries") +
      xlab("Industry") +
      ylab(HTML(paste0("Global Warming Potential (kg CO", tags$sub("2"), "e) 
            [AR6, 100-yr time horizon]"))) +
      theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1))
  })
  
  ghg_df_20yr <- reactive({
    # Dataframe ghg_df contains industry code and name, and economic impact and emissions for each sector
    ghg_df_20yr <- data.frame(industry_codes, industry_names, economic_result(), 
                              ghgs_total_20yr())
    return(ghg_df_20yr)
  })
  
  ghg_data_sorted_20yr <- reactive({
    # Sorts ghg_df by the ghg column (sorting by 100 year GWP values to maintain consistency)
    ghg_data_sorted_20yr <- ghg_df_20yr()[order(ghg_df()$ghgs_total_100yr.., decreasing = TRUE), ]
    return(ghg_data_sorted_20yr)
  })
  
  # Table of results
  output$results_table  <- renderDataTable({
    ghg_data_sorted <- cbind(
      ghg_data_sorted(), 
      ghg_data_sorted_20yr()$ghgs_total_20yr..)
    totals_line <- c( 'Total', 'Total', colSums(ghg_data_sorted[,3:5]))
    ghg_data_sorted <- rbind(totals_line, ghg_data_sorted)
    for (col in 3:5) {
      ghg_data_sorted[,col] = as.numeric(as.character(ghg_data_sorted[,col]))
      ghg_data_sorted[,col] = drop_small_numbers(ghg_data_sorted[,col])
    }
    ghg_data_sorted[, 3:5] = signif(ghg_data_sorted[, 3:5], 3)
    
    ghg_data_sorted[, 3] <- sapply(ghg_data_sorted[, 3], function(x) paste0("$",x))
    
    for (col in 3:5) {
      ghg_data_sorted[, col] = prettyNum(ghg_data_sorted[, col], big.mark = ",", scientific = FALSE)
      # ghg_data_sorted[,col] = as.numeric(as.character(ghg_data_sorted[,col]))
    }
    
    DT::datatable(
      ghg_data_sorted, 
      rownames = FALSE, 
      colnames = c("Industry Code","Industry Name", 
                   "Total Economic Impact (U.S. Dollars) 2012", 
                   "GWP impacts (kg CO2e, AR6, 100-yr)",
                   "GWP impacts (kg CO2e, AR6, 20-yr)")
    )
  })
  
  
  # Table for download
  download_table <- reactive({
    ghg_data_sorted <- cbind(
      ghg_data_sorted(),
      ghg_data_sorted_20yr()$ghgs_total_20yr..)
    download_table <- format(ghg_data_sorted, digits = 3)
    colnames(download_table) <- c("Industry Code", 
                                  "Industry Name","Economic Impact (U.S. Dollars) 2012", 
                                  HTML(paste0("Global Warming Potential (kg CO", tags$sub("2"), "e) 
                                      [AR6, 100-yr time horizon]")),
                                  HTML(paste0("Global Warming Potential (kg CO", tags$sub("2"), "e) 
                            [AR6, 20-yr time horizon]")))
    return(download_table)
  })
  
  # Download button
  output$downloadResults <- downloadHandler(
    filename = function() {
      paste0("Economic_Impacts_and_GHG_Totals", ".csv")
    },
    content = function(file) {
      # Write the dataset to the `file` that will be downloaded
      write.csv(download_table(), file)
    }
  )
  
  
  # Calculate emissions of each ghg in species for all industries (kg CO", tags$sub("2"), " eq. - 100 yr GWP)
  # unsorted (for use to calculate totals)
  speciated_ghgs_20yr_unsorted <- reactive({  
    speciated_ghgs_20yr_unsorted <- speciated_ghgs()[-1,]
    for (i in 1:length(species_of_ghgs)) {
      gwp <- gwps_of_interest[gwps_of_interest$Common.Name == species_of_ghgs[i], 3]
      speciated_ghgs_20yr_unsorted[species_of_ghgs[i]] <- 
        speciated_ghgs_20yr_unsorted[species_of_ghgs[i]] * gwp
    }
    return(speciated_ghgs_20yr_unsorted)
  })
  
  # sorted
  speciated_ghgs_20yr <- reactive({  
    ghg_for_totals <-  speciated_ghgs_20yr_unsorted()[, -1]
    rownames(ghg_for_totals) <- speciated_ghgs_20yr_unsorted()[, 1]
    ghg_totals <- as.vector(rowSums(ghg_for_totals))
    speciated_ghgs_20yr <-  speciated_ghgs_20yr_unsorted()[order(
      ghg_totals, 
      decreasing = TRUE),]
    return(speciated_ghgs_20yr)
  })
  
  # Emissions calculations (totals, 20 yr GWP) --------------------------------------------
  ghgs_total_20yr <- reactive({
    ghg_for_totals <-  speciated_ghgs_20yr_unsorted()[, -1]
    rownames(ghg_for_totals) <-  speciated_ghgs_20yr_unsorted()[, 1]
    ghgs_total_20yr <- as.vector(rowSums(ghg_for_totals))
    return(ghgs_total_20yr)
  })
  
  
  
  
  top_industries_df_20yr <- reactive({
    # Returns dataframe with information about 5 or 10 (depending on user selection)
    # industries which contribute most to emissions.
    top_industries_df_20yr <- ghg_data_sorted_20yr()[1:v$num_industries, ]
    start <- v$num_industries + 1
    others <- colSums(ghg_data_sorted_20yr()[start:nrow(ghg_data_sorted_20yr()), 3:4])
    top_industries_df_20yr[nrow(top_industries_df_20yr) + 1, ] <- 
      c(paste("All Other", num_other_industries(),"Industries"), 
        paste("All Other", num_other_industries(),"Industries"), 
        as.numeric(others))
    top_industries_df_20yr$economic_result <- as.numeric(top_industries_df_20yr$economic_result)
    top_industries_df_20yr$ghgs_total_20yr <- as.numeric(top_industries_df_20yr$ghgs_total_20yr)
    # Lock in factor level order to ensure that the graphs will have the same order of industries as the dataframe
    top_industries_df_20yr$industry_names <- factor(
      top_industries_df_20yr$industry_names, 
      levels = top_industries_df_20yr$industry_names)
    return(top_industries_df_20yr)
  })
  
  #
  output$plot_gwp_totals_20_yr <- renderPlotly({
    # Returns plot of GWP for industries which contribute most to emissions
    ggplot(data = top_industries_df_20yr(), 
           aes(x = industry_names, y = ghgs_total_20yr)) +
      geom_bar(stat = "identity", fill = graph_colors()) +
      coord_flip() +
      ggtitle("GWP of Top Industries (using 20 year GWP)") +
      xlab("Industry") +
      ylab(HTML(paste0("Global Warming Potential (kg CO", tags$sub("2"), "e) 
          [AR6, 20-yr time horizon]"))) +
      theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1))
  })
  
  
  # Stacked graph for 20 year GWP
  output$stacked_plot_20yr <- renderPlotly({
    top_speciated_ghgs_20yr <- 
      speciated_ghgs_20yr()[speciated_ghgs_20yr()$Industry %in% 
                              top_industries_df()$industry_names, ]
    # Add "other" category for industry
    bottom_row <- 
      as.numeric(colSums(speciated_ghgs_20yr()[, 2:ncol(speciated_ghgs_20yr())]) -
                   colSums(top_speciated_ghgs_20yr[, 2:ncol(top_speciated_ghgs_20yr)]))
    top_speciated_ghgs_20yr[nrow(top_speciated_ghgs_20yr) + 1, 
                            2:ncol(top_speciated_ghgs_20yr)] <- bottom_row
    top_speciated_ghgs_20yr[nrow(top_speciated_ghgs_20yr), 1] <- paste("All Other", num_other_industries(),"Industries")
    
    # Add "other" category for species
    Other <- 
      rowSums(top_speciated_ghgs_20yr[,v$num_industries:ncol(top_speciated_ghgs_20yr)])
    top_speciated_ghgs_20yr <- cbind(top_speciated_ghgs_20yr, Other)
    
    
    # Converting data from wide to long format
    species_for_graph <- c("Industry","Carbon dioxide", "Methane","Nitrous oxide", 
                           "Other")
    data_long <- top_speciated_ghgs_20yr[, species_for_graph]      
    data_long <- reshape2::melt(data_long, id.vars = "Industry")
    colnames(data_long)[1:3] <- c("Industry", "Species", "Emissions")
    
    # Reorder to show most important industries on bottom
    data_long$Industry <- factor(data_long$Industry, levels = top_industries_df()$industry_names)
    
    ggplot(data_long,                 
           aes(x = Species, y = Emissions, fill = Industry)) +
      ggtitle("20 yr GWP by Industry and Chemical Species") +
      xlab("Chemical Species") +
      ylab(HTML(paste0("Global Warming Potential (kg CO", tags$sub("2"), "e) 
        [AR6, 20-yr time horizon]"))) +
      geom_bar(position = 'stack', stat = 'identity') +
      scale_fill_manual(values = graph_colors()) +
      theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1))
  })
  
  
  
  speciated_ghgs_20yr_for_download <- reactive({
    speciated_ghgs_20yr_for_download <- cbind(ghg_data_sorted()$industry_codes, 
                                              speciated_ghgs_20yr())
    colnames(speciated_ghgs_20yr_for_download)[1] <- "Code"
    return(speciated_ghgs_20yr_for_download)
  })
  
  
  # Download csv with emissions broken down by chemical species and industry for 20 year GWP
  output$download_gwp_20yr <- downloadHandler(
    filename = function() {
      paste0("speciated_ghgs_20yr", ".csv")
    },
    content = function(file) {
      # Write the dataset to the `file` that will be downloaded
      write.csv(speciated_ghgs_20yr_for_download(), file)
    }
  )
  
  # Non-GHG Impacts ----------------------------------------------------------
  
  # Total impact Calculations
  impacts <- reactive({
    impacts <- N  %*% (economic_result())
    impacts = cbind(impacts, units)
    impacts = impacts[indicators_specific,]
    colnames(impacts) = c("Amount","Unit")
    return(impacts)
  })
  
  
  
  # Table for total impacts
  output$impacts_table <- renderDataTable({
    impacts = impacts()
    impacts[, 1] =  signif((as.numeric(impacts[, 1])), 3)
    DT::datatable(impacts,caption = impacts_caption,
                  colnames = c("Impact Category","Amount","Unit"), 
    )
  })
  
  normalized_impacts <- reactive({
    normalized_impacts <- as.numeric(impacts()[,1])/normalization_factors
    normalized_impacts <- data.frame(normalized_impacts, normalization_factors)
    normalized_impacts$Indicator <- impact_categories_with_units
    # colnames(normalized_impacts) <- c("Indicator","Normalized_Impact")
    return(normalized_impacts)
  })
    
  # output$normalized_impacts_table <- renderDataTable({
  #   DT::datatable(normalized_impacts())
  # })
  
  output$plot_normalized_impacts <- renderPlotly({
    ggplot(data = normalized_impacts(),
           aes(x = Indicator, y = normalized_impacts)) +
      geom_bar(stat = "identity", fill = c('red','green','orange','brown')) +
      ggtitle("Normalized Environmental Impacts") +
      xlab("Impact Category") +
      ylab("Normalized Impact") +
      theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1))
  })

  
  # Download button for total impacts
  output$downloadImpacts <- downloadHandler(
    filename = function() {
      # Use the selected dataset as the suggested file name
      paste0("Impacts", ".csv")
    },
    content = function(file) {
      # Write the dataset to the `file` that will be downloaded
      write.csv(impacts(), file)
    }
  )
  
  # Impacts by industry and chemical species (used for below calculations)
  impacts_by_type <- reactive({
    M <- M[colSums(c_indicators_specific) != 0, ]
    impacts_by_type = data.frame(industry_names)
    for (i in 1:nrow(M)) {
      speciesvec <- M[i, ]
      speciesM <- matrix(diag(speciesvec), ncol = 411)
      emissions_by_industry <- speciesM %*% (economic_result())
      impacts_by_type[,rownames(M)[i]] <- 
        as.numeric(as.character(emissions_by_industry))
    }
    
    # Sorting by 100 year GWP values to maintain consistency
    impacts_by_type <- impacts_by_type[order(ghg_df()$ghgs_total_100yr..,
                                             decreasing = TRUE), ]
    rownames(impacts_by_type) = impacts_by_type$industry_names
    impacts_by_type = impacts_by_type[, -1]
    return(impacts_by_type)
  })
  
  
  
  # Impacts by industry and chemical species (used for below calculations)
  full_inventory <- reactive({
    impacts_by_type = data.frame(industry_names)
    for (i in 1:nrow(M)) {
      speciesvec <- M[i, ]
      speciesM <- matrix(diag(speciesvec), ncol = 411)
      emissions_by_industry <- speciesM %*% (economic_result())
      impacts_by_type[,rownames(M)[i]] <- 
        as.numeric(as.character(emissions_by_industry))
    }
    
    # Sorting by 100 year GWP values to maintain consistency
    impacts_by_type <- impacts_by_type[order(ghg_df()$ghgs_total_100yr..,
                                             decreasing = TRUE), ]
    rownames(impacts_by_type) <- impacts_by_type$industry_names
    impacts_by_type <- impacts_by_type[, -1]
    full_inventory <- data.frame(colSums(impacts_by_type))
    colnames(full_inventory) <- NULL
    return(full_inventory)
  })
  
  
  # Download full inventory
  output$full_inventory_download <- downloadHandler(
    filename = function() {
      # Use the selected dataset as the suggested file name
      paste0("Inventory", ".csv")
    },
    content = function(file) {
      # Write the dataset to the `file` that will be downloaded
      write.csv(full_inventory(), file)
    }
  )
  
  # Acidification potential
  acid_ptnl <- reactive({
    
    acid_ptnl_row <- C[1, ]
    acid_ptnl <-  acid_ptnl_row * t(impacts_by_type())
    
    # Just nonzero rows
    acid_ptnl <- t(acid_ptnl[rowSums(acid_ptnl) != 0, ])
    acid_ptnl_t <- t(acid_ptnl)
    acid_ptnl_t <- acid_ptnl_t[order(as.vector(colSums(acid_ptnl)),
                                    decreasing = TRUE), ]
    acid_ptnl = t(acid_ptnl_t)
    acid_ptnl = cbind(ghg_data_sorted()$industry_codes, acid_ptnl)
    colnames(acid_ptnl)[1] = "Code"
    for (i in 2:length(colnames(acid_ptnl))) {
      colnames(acid_ptnl)[i] = strsplit(colnames(acid_ptnl), split = "/")[[i]][1]
    }
    return(acid_ptnl)
  })
  
  output$acid_table  <- renderDataTable({
    acid_ptnl <- acid_ptnl()
    acid_ptnl[, -1] <- signif((as.numeric(acid_ptnl[, -1])), 3)
    DT::datatable(acid_ptnl[, -1], caption = acid_ptnl_caption ) %>%
      formatCurrency(colnames(acid_ptnl[, -1]), currency = "", interval = 3, mark = ",")
  })
  
  output$downloadAcid <- downloadHandler(
    filename = function() {
      paste0("Acidification_Potential", ".csv")
    },
    content = function(file) {
      # Write the dataset to the `file` that will be downloaded
      write.csv(acid_ptnl(), file)
    }
  )
  
  # Eutrophication potential
  eutro_ptnl <- reactive({
    eutro_ptnl_row <- C[2, ]
    eutro_ptnl <-  eutro_ptnl_row * t(impacts_by_type())
    # Just nonzero rows
    eutro_ptnl <- t(eutro_ptnl[rowSums(eutro_ptnl) != 0,])
    eutro_ptnl_t <- t(eutro_ptnl)
    eutro_ptnl_t <- eutro_ptnl_t[order(as.vector(colSums(eutro_ptnl)), decreasing = TRUE),]
    eutro_ptnl <- t(eutro_ptnl_t)
    eutro_ptnl <- cbind(ghg_data_sorted()$industry_codes, eutro_ptnl)
    colnames(eutro_ptnl)[1] = "Code"
    for (i in 1:length(colnames(eutro_ptnl))) {
      species = strsplit(colnames(eutro_ptnl), split = "/")[[i]][1]
      medium = strsplit(colnames(eutro_ptnl), split = "/")[[i]][3]
      colnames(eutro_ptnl)[i] = paste0(species, " (",medium, ")")
    }
    return(eutro_ptnl)
  })
  
  output$eutro_ptnl_table  <- renderDataTable({
    eutro_ptnl <- eutro_ptnl()
    eutro_ptnl[, -1] <- signif((as.numeric(eutro_ptnl[, -1])), 3)
    DT::datatable(eutro_ptnl[, -1], caption = eutro_ptnl_caption) %>%
      formatCurrency(colnames(eutro_ptnl[, -1]), currency = "", interval = 3, mark = ",")
  })
  
  output$downloadEutro <- downloadHandler(
    filename = function() {
      # Use the selected dataset as the suggested file name
      paste0("Eutrophication_Potential", ".csv")
    },
    content = function(file) {
      # Write the dataset to the `file` that will be downloaded
      write.csv(eutro_ptnl(), file)
    }
  )
  
  # Ozone Depletion
  o3_ptnl <- reactive({
    o3_ptnl_row <- C[3,]
    o3_ptnl <-  o3_ptnl_row * t(impacts_by_type())
    
    # Just nonzero rows
    o3_ptnl <- t(o3_ptnl[rowSums(o3_ptnl ) != 0,])
    
    o3_ptnl_t <- t(o3_ptnl)
    o3_ptnl_t <- o3_ptnl_t[order(as.vector(colSums(o3_ptnl )),
                                decreasing = TRUE),]
    o3_ptnl <- t(o3_ptnl_t)
    o3_ptnl <- cbind(ghg_data_sorted()$industry_codes, o3_ptnl)
    colnames(o3_ptnl )[1] <- "Code"
    for (i in 2:length(colnames(o3_ptnl))) {
      colnames(o3_ptnl)[i] = strsplit(colnames(o3_ptnl), split = "/")[[i]][1]
    }
    
    return(o3_ptnl)
  })
  
  output$o3_ptnl_table  <- renderDataTable({
    o3_ptnl <- o3_ptnl()
    o3_ptnl[, -1] <- signif((as.numeric(o3_ptnl[, -1])), 3)
    DT::datatable(o3_ptnl[, -1], caption = o3_ptnl_caption )  %>%
      formatCurrency(colnames(o3_ptnl[, -1]), currency = "", interval = 3, mark = ",")
  })
  
  output$download_o3_ptnl <- downloadHandler(
    filename = function() {
      # Use the selected dataset as the suggested file name
      paste0("Ozone_Depletion_Potential", ".csv")
    },
    content = function(file) {
      # Write the dataset to the `file` that will be downloaded
      write.csv(o3_ptnl(), file)
    }
  )
  
  
  # Smog Formation Potential
  smog_ptnl <- reactive({
    smog_ptnl_row <- C[4,]
    smog_ptnl  <-  smog_ptnl_row * t(impacts_by_type())
    # Just nonzero rows
    smog_ptnl  <- t(smog_ptnl[rowSums(smog_ptnl) != 0,])
    smog_ptnl_t <- t(smog_ptnl)
    smog_ptnl_t <- smog_ptnl_t[order(as.vector(colSums(smog_ptnl)),
                                     decreasing = TRUE),]
    smog_ptnl <- t(smog_ptnl_t)
    smog_ptnl <- cbind(ghg_data_sorted()$industry_codes, smog_ptnl)
    colnames(smog_ptnl)[1] <- "Code"
    for (i in 2:length(colnames(smog_ptnl))) {
      colnames(smog_ptnl)[i] = strsplit(colnames(smog_ptnl), split = "/")[[i]][1]
    }
    return(smog_ptnl)
  })
  
  output$Smog_table  <- renderDataTable({
    smog_ptnl <- smog_ptnl()
    smog_ptnl[, -1] <- signif((as.numeric(smog_ptnl[, -1])), 3)
    DT::datatable(smog_ptnl[, -1], caption = smog_ptnl_caption) %>%
      formatCurrency(colnames(smog_ptnl[, -1]), currency = "", interval = 3, mark = ",")
  })
  
  output$downloadSmog <- downloadHandler(
    filename = function() {
      # Use the selected dataset as the suggested file name
      paste0("Smog_Formation_Potential", ".csv")
    },
    content = function(file) {
      # Write the dataset to the `file` that will be downloaded
      write.csv(smog_ptnl(), file)
    }
  )
  
  # Allows the user to download all data at once as a zipped file ---------------
  to_download <- reactive({
    full_inventory = data.frame(rownames(full_inventory()), full_inventory())
    colnames(full_inventory) <- c("Flow","Amount")
    Acidification_Potential = data.frame(rownames(acid_ptnl()), acid_ptnl())
    colnames(Acidification_Potential) = c("Industry",colnames(acid_ptnl()))
    Eutrophication_Potential = data.frame(rownames(eutro_ptnl()), eutro_ptnl())
    colnames(Eutrophication_Potential) = c("Industry",colnames(eutro_ptnl()))
    Ozone_Depletion_Potential = data.frame(rownames(o3_ptnl()), o3_ptnl())
    colnames(Ozone_Depletion_Potential) = c("Industry",colnames(o3_ptnl()))
    Smog_Formation_Potential = data.frame(rownames(smog_ptnl()), smog_ptnl())
    colnames(Smog_Formation_Potential) = c("Industry",colnames(smog_ptnl()))
    speciated_ghgs = data.frame(speciated_ghgs_for_download())
    colnames(speciated_ghgs) = colnames((speciated_ghgs_for_download()))
    speciated_gwp_100yr = data.frame(speciated_ghgs_100yr_for_download())
    colnames(speciated_gwp_100yr) = colnames((speciated_ghgs_100yr_for_download()))
    speciated_gwp_20yr = data.frame(speciated_ghgs_20yr_for_download())
    colnames(speciated_gwp_20yr) = colnames((speciated_ghgs_20yr_for_download()))
    Economic_Impacts = data.frame(economics_df())
    colnames(Economic_Impacts) = c("Industry Code","Industry Name",
                                   "Total Economic Impact (U.S. Dollars) 2012",
                                   "Direct Economic Impact (U.S. Dollars) 2012")
    Economic_Impacts_and_GHG_Totals = data.frame(download_table())
    colnames(Economic_Impacts_and_GHG_Totals) = c("Industry Code","Industry Name", 
                                                  "Total Economic Impact (U.S. Dollars) 2012", 
                                                  "GWP impacts (kg CO2e, AR6, 100-yr)",
                                                  "GWP impacts (kg CO2e, AR6, 20-yr)")
    to_download = reactiveValues(
      Economic_Impacts_and_GHG_Totals = Economic_Impacts_and_GHG_Totals,
      speciated_ghgs = speciated_ghgs,
      speciated_gwp_100yr = speciated_gwp_100yr,
      speciated_gwp_20yr = speciated_gwp_20yr,
      Impacts = data.frame(impacts()),
      Acidification_Potential = Acidification_Potential,
      Eutrophication_Potential  = Eutrophication_Potential,
      Ozone_Depletion_Potential = Ozone_Depletion_Potential,
      Smog_Formation_Potential = Smog_Formation_Potential,
      Economic_Impacts = Economic_Impacts,
      full_inventory = full_inventory
      )
    
    return(to_download)
  })
  
  output$download_all <- downloadHandler(
    filename = function() {
      paste("all_data_", Sys.Date(), ".zip", sep = "")
    },
    content = function(file) {
      temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
      dir.create(temp_directory)
      
      reactiveValuesToList(to_download()) %>%
        imap(function(x,y) {
          if (!is.null(x)) {
            file_name <- glue("{y}.csv")
            readr::write_csv(x, file.path(temp_directory, file_name))
          }
        })
      zip::zip(
        zipfile = file,
        files = dir(temp_directory),
        root = temp_directory
      )
    },
    contentType = "application/zip"
    
  )
  
  
  
  # Generate file for upload to openLCA
  output$DownloadOpenLCA <- downloadHandler(
    filename = paste0("USEEIO OpenLCA Inventory", ".xlsx"),
    content = function(file) {
      template = "EEIO_Template_in_r_final.xlsx"
      withProgress(message = 'Downloading OpenLCA File', value = 0, {
      inputs <- read.xlsx(template, 4)
        outputs <- read.xlsx(template, 5)
        gen_info <- read.xlsx(template, 1, rowNames = FALSE)
        inventory_crosswalk = read.xlsx("Inventory_CW_r.xlsx", 1)
        subject = input$subject
        gen_info$X2[2] = paste('LCI of', subject)
        gen_info$X2[4] = paste0('This is an LCI of ', subject, '. It was created using the Environmentally-Extended Input Output (USEEIO) method developed by the EPA to conduct screening level LCI analysis.')
        timelt <- as.POSIXlt(Sys.time())
        gen_info$X2[6] = paste(toString(Sys.time()), timelt$zone)
        gen_info$X2[9] = paste(toString(Sys.time()), timelt$zone)

        Sys.time()[[1]][2:6]
        for (i in 1:nrow(full_inventory())) {
          name = rownames(full_inventory())[i]
          flow = inventory_crosswalk[inventory_crosswalk$Output_name == name,"Flow"]
          category = inventory_crosswalk[inventory_crosswalk$Output_name == name,"Category"]
          inputs$Amount[inputs$Flow == flow & inputs$Category == category] = full_inventory()[,1][i]
          outputs$Amount[outputs$Flow == flow & outputs$Category == category] = full_inventory()[,1][i]
        }
        wb <- loadWorkbook(template)
        writeData(wb, 'General information', gen_info$X2[2:9], startCol = "B", startRow = 3)
        writeData(wb, 'Administrative information', paste(toString(Sys.time()), timelt$zone), startCol = "B", startRow = 9)
        writeData(wb, 'Inputs', inputs$Amount, startCol = "D", startRow = 2)
        writeData(wb, 'Outputs', outputs$Amount[2:length(outputs$Amount)], startCol = "D", startRow = 3)
        writeData(wb, 'Outputs', paste(subject, 'Infrastructure'), startCol = "D", startRow = 2)
        writeData(wb, 'Flows', read.xlsx(template, 8, colNames = FALSE)[-1,], startCol = "A", startRow = 2, colNames = FALSE, keepNA = FALSE)
        writeData(wb, 'Flows', paste(subject, 'Infrastructure'), startCol = "D", startRow = 2)
        writeData(wb, 'Actors', read.xlsx(template, 15, colNames = FALSE)[-1,], startCol = "A", startRow = 2, colNames = FALSE, keepNA = FALSE)
        saveWorkbook(wb, file, overwrite = TRUE)
      })
    }
  )
  
  
}
















