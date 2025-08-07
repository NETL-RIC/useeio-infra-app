library(here)
library(plotly)
library(shinycssloaders) # provides withSpinner
# library(shinyhttr)
library(shinyWidgets)

# Links
useeio_url <- a(
  "(USEEIO)",
  href = "https://www.epa.gov/land-research/us-environmentally-extended-input-output-useeio-models"
)
naics_url <- a(
  "(NAICS)",
  href = "https://www.census.gov/naics/?input=&year=2012"
)

# Paths
input_folder <- here("src", "inputs")

# Instructions

# Home
p0 <- "The United States Environmental Protection Agency (EPA) and 
      National Energy Technology Lanboratory (NETL) GitHub project 
      code is provided on an 'as is' basis and the user assumes 
      responsibility for its use. EPA has relinquished control of the 
      information and no longer has responsibility to protect the integrity, 
      confidentiality, or availability of the information. 
      Any reference to specific commercial products, processes, or services 
      by service mark, trademark, manufacturer, or otherwise, does 
      not constitute or imply their endorsement, recommendation or favoring 
      by EPA. The EPA seal and logo shall not be used in any manner to 
      imply endorsement of any commercial product or activity by EPA or 
      the United States Government."

p1 <- tagList(
  "This web application estimates the economic, greenhouse gas, and
   other environmental impacts across the supply chain of production.
   It implements the U.S. EPA's Environmentally Extended Input Output",
  useeio_url,
  "method."
)

p2 <- "Environmentally Extended Input-Output models map economic activity into
      various industries and track the interdependence of these industries.
      They then use the environmental impacts per dollar spent in each industry
      to estimate the overall environmental impacts. This app accepts inputs of
      increments of demand in economic industries, and quickly estimates and
      graphically displays impact results.

This implementation requires you to first choose one of two options for
generating results by clicking a button below:"

p3 <- "Manual Entry - which allows individually entered inputs of demand for
       known industries."

p4 <- "File Upload - which requires a comma, semicolon, or tab separated text
       file that contains text descriptors of demand in one column and costs in
       another. A template for this file will be provided on the File Upload
       tab."

p5 <- "Manual Entry can be useful in examining the impacts of a number of
       already-known industries, while File Upload is best if you have many
       entries, or do not know what industry to map inputs to."

p6 <- "Please select the year for your economic inputs. The amount entered will
       be converted from U.S. dollars for the selected year to 2012 U.S.
       dollars. The results are always given in 2012 U.S. dollars. "

# File Upload tab
p_upload2 <- "After your file has been uploaded, an algorithm will attempt to
              automatically map the input of text-based demands with existing
              economic industries. It may take some time for the mapping results
              to display while the algorithm is running. For each entry in the
              input file, a predicted industry is  auto-selected based on a
              matching search of the text, followed by other possible industry
              matches. The industry options are ordered based on the likelihood
              of each containing the activities associated with the text name."

p_upload3 <- "You may keep or change the industry used for each input. If all
              industry names for a particular row appear in alphabetical order
              (Abrasive products, Adhesives, Air and gas compressors etc.), then
              no matches were found. You can try spelling out acronyms, as
              although the algorithm is designed to recognize many common
              acronyms from hydrogen production literature, this recognition
              is not comprehensive."

p_upload4 <- tagList(
  "Another table will appear below the table with the uploaded
   information. It will contain a brief description of the
   currently selected industries, as well as links to the website
   for corresponding North American Industry Classification System",
  naics_url,
  "industries. This is meant to guide you in selecting
   the correct industries by providing more information."
)

p_upload5 <- "Once a file has been uploaded, the Economic Impacts, GHG Impacts,
              and Other Environmental Impacts tabs with the data should appear
              automatically. If you wish to show these tabs and use data for a
              table which is currently uploaded (such as after going to Manual
              Entry and returning to File Upload), click on the Enter Current
              Upload button to do so. "

# Manual Entry tab
p_man1 <- "There are several required inputs on this tab."
p_man2 <- "Use the entry on top to control the number of individual economic
          industry demand inputs."
p_man3 <- "For each desired industry, click on the industry dropdown to browse
           the available industries. Enter the corresponding amount (in U.S.
           dollars) in the corresponding cost entry box."
p_man4 <- "After entering the desired inputs, click on the Economic Impacts,
          GHG Impacts, and Other Environmental Impacts tabs. "

# Economic Impacts tab
p_econ1 <- "This tab shows the economic results of the inputs to demand
            industries using either the manual or file upload method. "
p_econ2  <- "The following table shows the economic impact on each industry
            resulting from the input demand, including both the direct and total
            impacts, in 2012 U.S. dollars. The industry rows are by default
            sorted with the highest total economic impacts on top.  "
p_econ3  <- "You may customize the displayed table by:"
p_econ4 <- "In addition to all of these features, you can also click on the
            Download button to export the economic data for offline use. "
p_econ5 <- "You can switch between displaying the top 5 and top 10 industries
            (in terms of total economic impact), using the Top 5 Industries and
            Top 10 Industries buttons.  The following two plots show the total
            and direct economic impacts for each industry of the 5 (or 10)
            industries with the most total economic impact, as well as that of
            all industries not in these top industries (All Other N
            Industries).  "
p_econ6 <- "Both plots are organized with the industries with the most total
            economic impact on the bottom and those with the least on the top,
            except for All Other N Industries, which will always appear on top
            for consistency. The two plots have the industries arranged in the
            same order to allow for easy comparison. Place the pointer over a
            bar in the plots to reveal the specific numbers. "

# GHG Impacts Tab
p_ghg1 <- "This tab shows the greenhouse gas (GHG) emissions and global warming
          potential (GWP) results of the inputs to demand industries using
          either the manual or file upload method. The download everything
          button will download a .zip file with all of the .csv file downloads
          from the Economics Impacts, GHG Impacts, and Other Environmental
          Impacts tabs. "

p_ghg2 <- HTML(paste0("This table shows the GWP impacts (kg CO",
                      tags$sub("2"),
                      "e, AR6) of each industry resulting from the input demand,
                      using first a 100- and then a 20-year timeframe, as well
                      as total economic impacts in 2012 U.S. dollars. The rows
                      are automatically ordered with industries which contribute
                      most to GWP impacts (kg CO",
                      tags$sub("2"),
                      "e, AR6, 100-yr) on top."))

p_ghg3 <- HTML(paste0("These plots show the GWP impacts (kg CO",
                      tags$sub("2"),
                      "e, AR6) for each industry of the 5 (or 10) industries
                      with the most total GWP impacts (kg CO",
                      tags$sub("2"),
                      "e, AR6, 100-yr)), as well as that of all industries not
                      in these top industries (All Other N Industries), using
                      first the 100- and then the 20-year timeframe. You can
                      switch between displaying the top 5 and top 10 industries
                      using the Top 5 Industries and Top 10 Industries buttons.
                      The plots are organized with the industries which
                      contribute most to GWP impacts (kg CO",
                      tags$sub("2"),
                      "e, AR6, 100-yr) on the bottom and those with the least
                      on the top, except for All Other N Industries, which will
                      always appear on top for consistency. The two plots
                      have the industries arranged in the same order to allow
                      for easy comparison. Place the pointer over a bar in the
                      plots to reveal the specific number."))

p_other <- "This tab shows the acidification, eutrophication, ozone depletion,
            and smog formation potentials of the inputs to demand industries
            using either the manual or file upload method.   "

# Other Environmental Impacts tab
normalization_caption <- "This graph shows the normalized amount for each of the
                          four impact categories shown above. Results are
                          normalized by dividing by the normalization factors,
                          in impact per person year, calculated using the
                          inventories from the US (2008). "

# Load model
model_file <- file.path(input_folder, "model.rds")
model <- readRDS(model_file)
Rho <- model[["Rho"]]

# Options for Spinner for File Upload tab
options(spinner.color = "#0275D8",
        spinner.color.background = "#ffffff",
        spinner.size = 2)

ui <- fluidPage(
  # App title ----
  div(
    style = "display: flex; align-items: center; gap: 10px; margin-bottom: 20px;",
    img(src = "images/useeio_logo.png", height = "150px"),  
    h1("USEEIO Web Application")
  ),
  tabsetPanel(
    mainPanel(
      tabsetPanel(id = "tabs",
        tabPanel("Home",
          h1("Disclaimer"),
          h4(p0),
          h1("Introduction"),
          h4(p1),
          h4(p2),
          h4(p3),
          h4(p4),
          h4(p5),
          h1("Please select an entry method"),
          actionButton("Manual", "Manual Entry"),
          actionButton("Upload", "File Upload"),
          h4(p6),
          selectInput(
            "Year",
            label = "Please select a year",
            choices = colnames(Rho),
            selected = "2012"
          )
        ),
        tabPanel("File Upload",
          sidebarPanel(
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),
            fileInput(
              "file1",
              "Please upload a file with equipment in the first column and
               costs in the second. Click these buttons to indicate the type
               of file.",
              multiple = FALSE,
              accept = c("text/csv", "text/comma-separated-values,text/plain",
                         ".csv")
            ),
            # Horizontal line ----
            tags$hr(),
            # Horizontal line ----
            tags$hr(),

            # NEW
            downloadButton(
              "download_example",
              "Download Example"
            ),
          ),

          tableOutput("contents"),
          span("You can upload a file of one of the chosen types indicated on
               the sidebar of the File Upload tab. Make sure to select the
               appropriate separator"),
          strong("before"),
          span("uploading the file. You can use the attached file Download
                Example Template (CSV) File as an example and update it with
                your desired inputs. Please ensure that the entries in the cost
                column are formatted as numbers and not dollars. Blank entries
                in the cost column will be treated as zeros."),
          strong("Please note"),
          span("that all File Uploads must include headers for the tool to work
               properly. A use case for such a file upload is a bill of
               materials for a facility with equipment names (e.g., pumps) and
               their associated costs."),
          br(),
          br(),
          p(p_upload2),
          p(p_upload3),
          p(p_upload4),
          p(p_upload5),

          actionButton("enter_upload", "Enter Current Upload"),

          # Display data tables
          DT::dataTableOutput('upload_table'),

          # spinner (to indicate table is loading)
          withSpinner(gt::gt_output('descriptions_table'), type = 2)
        ),

        tabPanel("Manual Entry",
          h4(p_man1),
          h4(p_man2),
          h4(p_man3),
          h4(p_man4),
          tabsetPanel(
            type = "tabs",
            numericInput("num", "Enter number of industries:", value = 1),
          ),
          uiOutput("manual_inputs"),
          actionButton("enter", "Show Results")
        ),
        tabPanel("Economic Impacts",
          p(p_econ1),
          p(p_econ2),
          p(p_econ3),

          # Bullet points
          tags$ul(
            tags$li("Resorting by other columns - Click on the headers to
                     reorder the table based on that column"),
            tags$li("Change the number of rows displayed in the table – Change
                     the selection for “Show N entries”"),
            tags$li("Scroll through additional rows – Click buttons under the
                     table"),
            tags$li("Search all fields in the table by using the search bar"),
          ),

          p(p_econ4),

          DT::dataTableOutput('economics_table'),
          p(p_econ5),
          p(p_econ6),
          actionButton("top5_econ", "Top 5 Industries"),
          actionButton("top10_econ", "Top 10 Industries"),
          plotlyOutput("plot_economic"),
          plotlyOutput("plot_economic_direct"),
          downloadButton("downloadEconomics", "Download"),
        ),
        tabPanel("GHG Impacts",
          p(p_ghg1),
          downloadButton("download_all", "Download Everything"),
          p(p_ghg2),
          tags$ul(
            tags$li("Resorting by other columns - Click on the headers to
                     reorder the table based on that column"),
            tags$li("Change the number of rows displayed in the table – Change
                     the selection for “Show N entries”"),
            tags$li("Scroll through additional rows – Click buttons under
                     the table"),
            tags$li("Search all fields in the table by using the search bar"),
          ),
          DT::dataTableOutput('results_table'),
          actionButton("top5", "Top 5 Industries"),
          actionButton("top10", "Top 10 Industries"),
          p(p_ghg3),
          plotlyOutput("plot_gwp_totals_100_yr"),
          plotlyOutput("plot_gwp_totals_20_yr"),
          downloadButton("downloadResults", "Download"),
          h4('Speciated GHG Emissions by Industry'),
          DT::dataTableOutput('ghg_table'),
          downloadButton("download_ghgs", "Download GHG Emissions Data"),
          plotlyOutput("stacked_plot_100yr"),
          # BUG: missing download function for this!
          downloadButton("download_gwp_100yr",
                         HTML(paste0("Download GWP impacts (kg CO",
                                     tags$sub("2"), "e, AR6, 100-yr)"))),
          plotlyOutput("stacked_plot_20yr"),
          downloadButton("download_gwp_20yr",
                         HTML(paste0("Download GWP impacts (kg CO",
                                     tags$sub("2"), "e, AR6, 20-yr)"))),

        ),
        tabPanel(
          "Other Environmental Impacts",
          # Display data table
          h4('Total environmental impacts:'),
          p(p_other),
          downloadButton("full_inventory_download", "Download Full Inventory"),

          textInput("subject",
                    "Please enter a subject (for use on OpenLCA Spreadsheet):",
                    value = "", width = NULL, placeholder = NULL),
          downloadButton("DownloadOpenLCA",
                         "Download Inventory in OpenLCA Format"),
          # BUT: not rendering
          DT::dataTableOutput('impacts_table'),
          # BUG: undefined method
          # DT::dataTableOutput("full_inventory_table"),
          h5(normalization_caption),
          plotlyOutput("plot_normalized_impacts"),
          downloadButton("downloadImpacts", "Download"),
          h4('Acidification Potential (kg)'),
          DT::dataTableOutput('acid_table'),
          downloadButton("downloadAcid", "Download"),
          h4('Eutrophication Potential (kg)'),
          DT::dataTableOutput('eutro_ptnl_table'),
          downloadButton("downloadEutro", "Download"),
          h4('Ozone Depletion (kg)'),
          DT::dataTableOutput('o3_ptnl_table'),
          downloadButton("download_o3_ptnl", "Download"),
          h4('Smog Formation Potential (kg)'),
          DT::dataTableOutput('Smog_table'),
          downloadButton("downloadSmog", "Download"),
          DT::dataTableOutput('Impacts_by_ind'),
        )
      )
    )
  )
)
