# NETL's USEEIO Infrastructure Model Web Application
This web application implements the U.S. Environmentally-Extended Input-Output (USEEIO) model in order to conduct a screening-level life cycle analysis (LCA).
The USEEIO model is an open-source model developed and maintained by the Environmental Protection Agency (EPA), along with colleagues and contractors and is available on GitHub ([.html](https://github.com/USEPA/USEEIO)).
The USEEIO model incorporates environmental data into pre-existing Economic Input-Output Models, which look at the interdependence of different economic industries.
This web app supports infrastructure applications, and the industry-mapping algorithm is tailored towards the use case of energy infrastructure; however, it can be used to implement USEEIO in any area for which this model is applicable.

## Disclaimer
The United States Environmental Protection Agency (EPA) and National Energy Technology Lanboratory (NETL) GitHub project code is provided on an "as is" basis and the user assumes responsibility for its use. EPA has relinquished control of the information and no longer has responsibility to protect the integrity , confidentiality, or availability of the information. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by EPA. The EPA seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity by EPA or the United States Government.

## Download Supporting Data
The supporting data to run this web application is available on the National Energy Technology Laboratory's Energy Data eXchange (EDX).
To download the resources over the API, first, request an API key from the following URL:

- https://edx.netl.doe.gov/user/register

Then run the script, download_edx.R, found in this repository, which can be completed from the command line or terminal using the following command:

```bash
Rscript download_edx.R
```

This will prompt you to enter your API key, then download the seven supporting data files from EDX to the 'src/inputs' folder.

## Install and Run
This web application was designed to run within a Docker container.
The Dockerfile extends the rocker/shiny stack published by the Rocker Project (https://rocker-project.org/), installs the R library dependencies (as shown in the following table), and opens port 3838 on localhost, which is the server's port (as defined in run.R).

With Docker Desktop installed (https://www.docker.com/products/docker-desktop/), run the following command from the root folder of this repository to create the docker image:

```bash
docker build -t my-shiny-app .
```

To create the container, run the image, remembering to assign a host port (e.g., 3838) to bind to the container's internal port (i.e., 3838).
While running, access the web app by navigating to the URL (e.g., http://localhost:3838), which will connect to the web app within the container.

The original image may be archived for collaboration into a tarball using the following command:

```bash
docker save -o my-image.tar my-shiny-app
```

The image may be loaded into another Docker Desktop installation using the following command:

```bash
docker load -i my-image.tar
```

Follow the same instructions above to run the container.

| Package          | Description                                                                                                                                                                                                                                                                                                                                                              | URL                                      |
| :--------------- | :----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | :--------------------------------------- |
| `shiny`          | A web application framework for R that allows you to build interactive web applications directly from R.                                                                                                                                                                                                                                                                  | [URL](https://www.rstudio.com/shiny/) |
| `DT`             | An R interface to the DataTables JavaScript library, making it easy to display data frames and matrices as interactive HTML tables.                                                                                                                                                                                                                                      | [URL](https://rstudio.github.io/DT/) |
| `SnowballC`      | Implements the Snowball stemming algorithm, which is used to reduce words to their base or root form.                                                                                                                                                                                                                                                                     | [URL](https://cran.r-project.org/web/packages/SnowballC/) |
| `data.table`     | An extension of R's `data.frame` that provides high-performance enhancements for data manipulation, including fast aggregation, subsetting, and reshaping of large datasets.                                                                                                                                                                                             | [URL](https://r-datatable.com/) |
| `dplyr`          | A grammar of data manipulation, providing a consistent set of verbs (functions) for common data wrangling tasks like filtering rows, selecting columns, arranging data, and summarizing.                                                                                                                                                                                   | [URL](https://dplyr.tidyverse.org/) |
| `ggplot2`        | An elegant and versatile system for creating statistical graphics based on "The Grammar of Graphics." It allows for the creation of a wide variety of static plots.                                                                                                                                                                                                     | [URL](https://ggplot2.tidyverse.org/) |
| `glue`           | Provides a simple way to create dynamic strings (string interpolation) by embedding R expressions directly within curly braces inside a string.                                                                                                                                                                                                                         | [URL](https://glue.tidyverse.org/) |
| `gt`             | A package for easily creating information-rich, publication-quality tables from tabular data. It provides a highly customizable and flexible way to format tables.                                                                                                                                                                                                        | [URL](https://gt.rstudio.com/) |
| `here`           | Helps with file path management by constructing paths in a robust and OS-independent way, relative to the project's root directory. This avoids common issues with hardcoding paths.                                                                                                                                                                                    | [URL](https://here.r-lib.org/) |
| `openxlsx`       | Facilitates reading, writing, and editing of `.xlsx` files without the need for Java. It supports styling, formulas, and other Excel features.                                                                                                                                                                                                                          | [URL](https://cran.r-project.org/web/packages/openxlsx/) |
| `plotly`         | An R wrapper for Plotly.js, allowing you to create interactive, web-based plots (e.g., scatter plots, line plots, 3D plots) that can be embedded in Shiny apps or standalone HTML files.                                                                                                                                                                                  | [URL](https://plotly.com/r/) |
| `purrr`          | A complete and consistent functional programming toolkit for R. It provides tools for working with functions and vectors, making code more readable and robust.                                                                                                                                                                                                          | [URL](https://purrr.tidyverse.org/) |
| `reactR`         | Provides an R interface to React, a JavaScript library for building user interfaces. It enables the use of React components within R and Shiny applications.                                                                                                                                                                                                             | [URL](https://cran.r-project.org/web/packages/reactR/) |
| `readr`          | A fast and friendly way to read rectangular data (like CSV, TSV, and fixed-width files) into R. It's part of the Tidyverse and is optimized for performance and consistency.                                                                                                                                                                                              | [URL](https://readr.tidyverse.org/) |
| `reshape2`       | A package for flexibly reshaping data between wide and long formats. It provides the `melt` and `dcast`/`acast` functions, which are fundamental for data transformation.                                                                                                                                                                                                | [URL](https://cran.r-project.org/web/packages/reshape2/) |
| `scales`         | Provides a set of tools for creating nicely formatted breaks and labels for axes and legends in `ggplot2` and other plots. It helps control the appearance of numerical and categorical scales.                                                                                                                                                                              | [URL](https://scales.r-lib.org/) |
| `tm`             | A comprehensive text mining framework for R, providing functionalities for text pre-processing (e.g., corpus creation, cleaning, stemming, tokenization) and analysis.                                                                                                                                                                                                     | [URL](https://cran.r-project.org/web/packages/tm/) |
| `zip`            | Allows for the creation and extraction of zip archives within R. It's useful for bundling files or extracting data from compressed sources.                                                                                                                                                                                                                              | [URL](https://cran.r-project.org/web/packages/zip/) |
| `shinycssloaders`| Adds loading animations to Shiny outputs, providing visual feedback to users while calculations or data loading are in progress. This improves the user experience by indicating that the app is working.                                                                                                                                                                   | [URL](https://cran.r-project.org/web/packages/shinycssloaders/) |
| `shinyWidgets`   | A collection of custom input widgets and other tools designed to enhance Shiny applications, offering more diverse and visually appealing UI elements than the default Shiny widgets.                                                                                                                                                                                   | [URL](https://cran.r-project.org/web/packages/shinyWidgets/) |


## Overview
In order to implement the USEEIO model, this application uses the *useeior* package \[1\] in the "R" Software platform.
This package is used to generate the USEEIOv2.0.1-411 model, which is based on commodities, has 411 sectors, uses 2012 input-output data, and is based on 2012 US dollars.
This model is stored as an RDS file along with the app for easy access since it takes more time to regenerate it using the package.
Most of the calculations are done using native R objects such as matrices and data tables, which are stored as part of this model.
See \[1\] for more information on these calculations.
The spreadsheet, IPCC GWP Factors.csv, contains the updated IPCC AR6 values for the global warming potential (GWP) of various species of greenhouse gases, using both the 100- and 20-year timeframes, from \[2\] and is used instead of the GWP information in the model, since the *useeior* model uses IPCC AR4 values.
The application also provides information on the mass flows of emissions of each greenhouse gas species, which can allow for other GWP values to be used.

R Shiny was used to create an interactive web application, which allows users to input economic data and view results \[3\].
On the home tab, the user can select a year for economic inputs.
All economic inputs will then be converted from that year into 2012 U.S. dollars, which are the basis for the model.
There are two different ways for the user to input information on industries and corresponding costs into the app.
The manual entry method allows individually entered inputs of demand for known industries and can be useful in examining the impacts of a relatively small number of already-known industries.
It uses code from \[4\].
The file upload tab allows the user to upload a file with equipment names in one column and costs in another, and is best if the user has many entries, or does not know which industries to map equipment to. 
This feature is specifically geared towards users looking to perform CAPEX type assessments for infrastructure projects.

For the 'File Upload' tab, code from \[3\] and \[5\] is used to allow for the user to upload a file and specify the delimiter and then select an industry for each row from a corresponding dropdown.
The industries will appear in the dropdown in their order of likelihood of containing the equipment in the same row according to the industry-mapping algorithm (exampled in more detail in the relevant section).
The industries which have no match to the given piece of equipment (a score of zero) will appear in alphabetical order after those with non-zero scores.
Thus, all industries will appear in the dropdown.

A graph on the 'Other Environmental Impacts' tab shows the normalized environmental impacts for the four impact categories included on this tab, calculated by dividing by a normalization factor from \[6\] given in units of the impact category per person year.
This allows all categories to be displayed on the same y-axis by putting them in the same units.
A brief description of each impact category was written using information from \[7\] in order to familiarize users with the impact categories.

Code based on \[8\] was used to add heat map coloring to the table on the 'GHG Impacts' tab.
Code based on \[9\] was used to create a button on the GHG Impacts tab, which allows the user to download all of the CSV files throughout the tool as a single zip file.
Code was also used for creating the progress bar and to help create the openLCA file.
\[10\] provided general guidance on the use of R Shiny for creating the application.

## The Industry Mapping Algorithm
An industry-mapping algorithm was created to map a text input, such as the name of a piece of equipment, to a ranked list of the most likely industries for it to belong to.
The intention is that, for a significant portion of searches, a best-match industry will appear within the first few items in the dropdown, therefore making the task of selecting an industry relatively easy.

A number of different methods and algorithms were tested.
The use of machine learning was considered, but, ultimately, this was decided not to be the most practical method given the time frame of the project and relatively limited number of examples to use as training data.

For a preliminary version of the algorithm, the comma-separated values (CSV) file titled, example_mappings.csv, which contains one column with the names of example equipment items (x) and another with the industries which they belong to (y), was used to provide examples of equipment and appropriate industry mapping.
This file was created by using the spreadsheet, 2012_NAICS_Index_File, downloaded from the NAICS website \[11\] and saved as a CSV file.
The items in the column titled INDEX ITEM DESCRIPTION were used as example inputs (x).
The NAICS12 column contains the 2012 NAICS codes and were mapped to their corresponding USEEIO names using the *convert_naics_to_useeio* function, which uses the crosswalk provided as part of the USEEIOr model.
For cases for which a particular NAICS code mapped to more than one USEEIO industry, the pieces of equipment for these NAICS industries were placed with all of these USEEIO industries.

Additionally, equipment names and corresponding industries from a spreadsheet with previous manual mappings were added, as well as a further list of examples which were found in \[12\], \[13\], \[14\], and \[15\].
Sources were selected to be representative of types of equipment that users might have for the energy infrastructure use case.
These original examples were specifically focused on the hydrogen production use case.
These examples were manually mapped to NAICS industries based on descriptions and then mapped to the USEEIO names using the *convert_naics_to_useeio* function.
For examples for which more than one industry seemed potentially appropriate depending on specifications which would not necessarily be provided in an equipment list, the examples were mapped to all appropriate industries.
Several industries were removed because they are unlikely to be relevant for the energy infrastructure use case.
These examples will still appear in the dropdown but will not be within those that are selected by the search algorithm and appear first in the dropdown.

The examples from the literature were repeated 10 times, while the examples from the 2012_NAICS_Index_File were repeated only once in order to place more weight on the examples that are more likely to be relevant to use cases pertaining to energy infrastructure.
The industry descriptions from the NAICS website were also included.
Since the 2012 descriptions were not available in CSV format, the 2017 descriptions were used.
The spreadsheet titled, 2017_NAICS_Descriptions (1).csv, contains these descriptions.
Only the descriptions associated with 6-digit codes were used.
The examples and descriptions were grouped by industry name to create the file, examples_and_descriptions.csv, which is used by a preliminary version of the search algorithm (*original_docsearch*).

The *docsearch* function, which implements the industry-mapping algorithm, is based on code supplied by \[16\], which scores and ranks the relevance of documents to a particular query.
It takes the data frame of descriptions and examples grouped by USEEIO code and uses the industries as the document names and the combined examples and descriptions as the document contents.
Some modifications were made to this code, including eliminating words that appear in the query but not any of the documents to prevent Not a Number (NAN) scores caused by division by zero, which would prevent the documents from being able to be sorted based on score.
In a preliminary version of the algorithm, each word in the query was repeated the same number of times as its position (repeating the first word once, the second word twice etc.).
This put more weight on the end of phrases, since this is where the actual type of equipment is most often located (e.g., "Boiler Feed Water Pump" should be categorized as a pump and not a boiler or feed water).

For a later version of the algorithm, the CSV files titled, NAICS_examples.csv and lit_examples.csv, were used.
These are set up in a manner similar to the file, example_mappings.csv; however, they are divided into separate sheets based on their source (NAICS website or literature).
Here, the literature examples are not repeated.
Instead, a weighting system is applied so that more weight is placed on the literature examples.
Also, the text in each example is divided based on part of speech (nouns vs. verbs, adjectives, and adverbs).
This accomplishes a similar objective as the original repeating of later words in the query but is more sophisticated and less sensitive to differences in the ordering of words (acid gas removal, low temperature vs. low temperature acid gas removal).
Additionally, both of these weighting systems are more able to be adjusted and optimized for accuracy.
The contents of the two spreadsheets are divided by part of speech and combined to a single data frame in the code titled part of speech processing.

An additional 243 examples were taken from \[17\], \[18\], \[19\], \[20\], \[21\], \[22\], and \[23\] in order to be more representative of energy infrastructure in general instead of being focused on hydrogen production.
Since many of the types of equipment from the original literature sources were similar to these examples, the algorithm performed fairly well.
These examples were used to test different algorithms by comparing the manual mapping to the list generated by the algorithms.
For cases for which an item is mapped to two industries, one of the associated rankings will appear too high.
However, this is only the case for a small portion of examples, and is the same for all algorithms, so it should not impact the comparison.
The testing examples for which the manually mapped industry did not appear as the first item according to the algorithm were then added to the file, lit_examples.csv, in order to capture types of equipment that may not have been represented in the original list.
After adding approximately half of the examples, broken up between sources to be representative of how the algorithm would perform on new examples, the algorithm was tested again with the remaining examples.

The manual mappings, taken as ground truth, appeared as the top choice for 32% of examples, and in the first three, five, and ten choices for 61%, 71% and 78% of examples respectively for the original algorithm (without division by part of speech and source).
The algorithm with the components divided and weighted by part of speech and source was found to perform the best.
For this algorithm, different weights were tried, and weights of 6 for literature nouns, 3 for literature non-nouns, 2 for the NAICS nouns, and 1 for NAICS non-nouns were chosen based on the performance both with and without the additional examples.
This led the manual mapping to appear within the first one, three, five, and ten choices for 52%, 65%, 74% and 81% of searches, respectively, prior to the addition of some of the examples, and with corresponding percentages of 53%, 71%, 79% and 87% after this addition.

A dictionary was added to address common acronyms and abbreviations that may be used in equipment names.
The terms are supplied by the acronym lists from \[12\] and \[15\], as well as acronyms and abbreviations used in the literature examples.
The code checks each word in the query and, if an acronym in the dictionary appears, it will add the full name(s) to the query to allow it to be included in the search.
In cases where an acronym can stand for more than one name, all associated names are added.

## Built-in Functions
The `convert_useeio_to_naics` uses the crosswalk to convert from the USEEIO to the corresponding NAICS code(s) in much the same way as the `convert_useeio_to_naics` function converts in the opposite direction.
The function, `generate_link`, takes a NAICS code and returns the corresponding link to the NAICS website.
There are a few other functions that improve the formatting.

The `drop_small_numbers` function displays numbers less than 0.001 as 0, since, given the uncertainty inherent in the model, numbers this small are essentially zero and showing small numbers with many decimal places impacts the formatting of the tables.
The full decimal place values can be seen by downloading the CSV files.
The function, `add_space`, adds a space after the first colon in a string using code based on \[24\] and is used to improve the formatting of the text descriptions for the USEEIO industries.

## Warnings and Limitations
While the user can re-sort columns of data tables on the app interface, the columns may sort incorrectly.
This is because of the formatting applied (such as adding dollar signs and controlling the number of digits).
It was decided that it was more important to maintain this formatting than to ensure that the columns sort correctly.

If the user wants to upload a file a second time, they will have to reload the application.

Currently, the industry selections used in the calculations will not update even after a new file has been uploaded.

If the user switches to manual entry after uploading a file and then back to file upload, the impacts tabs will initially be hidden even when the uploaded file is still present.
It can be shown using the 'Enter Current Upload' button.

Some of the waste flows from the inventory are not included in the file for upload to openLCA because the corresponding elementary flow information was not found.

When the app is first opened, some tabs that are intended to be hidden may appear while the app is loading.

The industry-mapping algorithm takes time to run.
The openLCA file takes a fairly long time to download due to the size of the document.
In these cases, a progress bar or loading symbol will appear to inform the user that the app is in progress.
The openLCA file may also take some time to upload to openLCA.

## Bibliography

  \[1\]    M. Li, W. Ingwersen, B. Young, J. Vendries and C. Birney, \"useeior: An Open-Source R Package for Building and Using US Environmentally-Extended Input--Output Models,\" *Applied Sciences,* vol. 12, no. 9, p. 4469, 2022.

  \[2\]    IPCC, \"Summary for Policymakers. In: Climate Change 2023: Synthesis Report. Contribution of Working Groups I, II and III to the Sixth Assessment Report of the Intergovernmental Panel on Climate Change,\" IPCC, Geneva, Switzerland, 2023.

  \[3\]    \"Shiny,\" Posit, \[Online\]. Available: https://shiny.posit.co/r/gallery/widgets/file-upload/. \[Accessed 12 July 2023\].

  \[4\]    Ash, \"How to change location of numeric input boxes in R Shiny,\" Stack Overflow, 19 January 2021. \[Online\]. Available: https://stackoverflow.com/questions/65797188/how-to-change-location-of-numeric-input-boxes-in-r-shiny. \[Accessed 30 July 2023\].

  \[5\]    g\. rbcavanaugh, \"Update DT table dropdowns with reactive data in shiny,\" Posit, 14 February 2021. \[Online\]. Available: https://community.rstudio.com/t/update-dt-table-dropdowns-with-reactive-data-in-shiny/96100. \[Accessed 3 July 2023\].

  \[6\]    M. Ryberg, M. D. M. Vieira, M. Zgola, J. Bare and R. K. Rosenbaum, \"Updated US and Canadian normalization factors for TRACI 2.1,\" *Clean Technologies and Environmental Policy,* vol. 16, p. 329--339, 2013.

  \[7\]    J. Bare, Tool for the Reduction and Assessment of Chemical and Other Environmental Impacts (TRACI) TRACI version 2.1 User's Guide, Washington, DC: U.S. EPA Office of Research and Development, 2012.

  \[8\]    S. Laurent, \"Heatmap formatting of a table with 'DT',\" R bloggers , 7 April 2023. \[Online\]. Available: https://www.r-bloggers.com/2023/04/heatmap-formatting-of-a-table-with-dt/. \[Accessed 4 July 2023\].

  \[9\]    A. Baxter, \"\[SOLVED\]-HOW TO DOWNLOAD MULTIPLE FILES FROM R/SHINY APP?-R,\" 16 December 2021. \[Online\]. Available: https://www.appsloveworld.com/r/100/83/how-to-download-multiple-files-from-r-shiny-app. \[Accessed 4 July 2023\].

  \[10\]   H. Wickham, Mastering Shiny, O'Reilly Media, 2020.

  \[11\]   U.S. Census Bureau, \"North American Industry Classification System,\" 9 July 2023. \[Online\]. Available: https://www.census.gov/naics/?year=2007&input=333132&details=333132. \[Accessed 9 July 2023\].

  \[12\]   e\. a. E. Lewis, \"Comparison of Commercial State-of-the-art, Fossil-based Hydrogen Production Technologies,\" National Energy Technology Laboratory, Pittsburgh, 2022.

  \[13\]   M. H. Barecka, J. W. Ager and A. A. Lapkin, \"Techno-economic assessment of emerging CO2 electrolysis technologies,\" *STAR Protoc,* vol. 2, no. 4, 2021.

  \[14\]   A. Mayyas, M. Ruth, B. Pivovar and G. Bender, \"Manufacturing Cost Analysis for Proton Exchange Membrane Water Electrolyzers,\" National Renewable Energy Laboratory, Golden, 2018.

  \[15\]   A. A. T. E. P. Spath, \"Biomass to Hydrogen Production Detailed Design May 2005 and Economics Utilizing the Battelle Columbus Laboratory Indirectly Heated Gasifier,\" National Renewable Energy Laboratory, Golden, 2005.

  \[16\]   B. Ogorek, \"Build a search engine in 20 minutes or less,\" R-bloggers, 27 March 2013. \[Online\]. Available: https://www.r-bloggers.com/2013/03/build-a-search-engine-in-20-minutes-or-less/. \[Accessed 9 July 2023\].

  \[17\]   S. Hughes, A. Zoelle, M. Woods, S. Henry, S. Homsy, S. K. N. Pidaparti, H. Hoffman, K. Forrest, A. Sheriff, T. Fout, W. Summers and S. Herron, \"Cost of Capturing CO2 from Industrial Sources,\" National Energy Technology Laboratory, Pittsburgh, 2022.

  \[18\]   T. Schmitt, S. Leptinsky, M. Turner, A. Zoelle, C. W. White, S. Hughes, S. Homsy, M. Woods, H. Hoffman, T. Shultz and R. James, \"COST AND PERFORMANCE BASELINE FOR FOSSIL ENERGY PLANTS VOLUME 1: BITUMINOUS COAL AND NATURAL GAS TO ELECTRICITY,\" National Energy Technology Laboratory, Pittsburgh, 2022.

  \[19\]   D. Morgan, T. Grant and D. Remson, *FECM/NETL CO2 Transport Cost Model (2022),* Pittsburgh, PA: National Energy Technology Laboratory, 2022.

  \[20\]   National Energy Technology Laboratory, *FE/NETL CO2 Saline Storage Cost Model,* Pittsburgh, PA: U.S. Department of Energy, 2017.

  \[21\]   N. Madlool, R. Saidur, M. Hossain and N. Rahim, \"A critical review on energy use and savings in the cement industries,\" *Renewable and Sustainable Energy Reviews,* vol. 15, no. 4, pp. 2042-2060, May 2011.

  \[22\]   Agico Cement, \"Cement Equipment,\" Agico Cement, 2022. \[Online\]. Available: https://www.cement-plants.com/cement-equipment/.

  \[23\]   M. Turner, T. Schmitt, M. Oakes, J. Konrade, M. Bleckinger and M. Sturdivan, \"Cost and Performance Baseline for Fossil Energy Plants, Volume 3: Low Rank Coal and Natural Gas to Electricity,\" National Energy Technology Laboratory, Pittsburgh, 2023.

  \[24\]   R. Scriven, \"strsplit on first instance \[duplicate\],\" Stack Overflow, 8 October 2014. \[Online\]. Available: https://stackoverflow.com/questions/26246095/strsplit-on-first-instance. \[Accessed 30 July 2023\].

  \[25\]   R. Scriven. \[Online\].

  \[26\]   Ben, \"Shiny + downloadHandler + Openxlsx does not generate a xlsx file,\" Stack Overflow, 12 November 2018. \[Online\]. Available: https://stackoverflow.com/questions/50948024/shiny-downloadhandler-openxlsx-does-not-generate-a-xlsx-file. \[Accessed 17 August 2023\].

  \[27\]   pata-eth, \"Progress bar and downloadHandler,\" GitHub, Inc, 6 April 2017. \[Online\]. Available: https://github.com/rstudio/shiny/issues/1660. \[Accessed 17 August 2023\].
