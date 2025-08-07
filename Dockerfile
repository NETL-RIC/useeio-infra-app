# Use the official R Shiny image as the base
FROM rocker/shiny:latest

# Install system dependencies (if needed)
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# Install R packages required by your app
RUN R -e "install.packages(c('shiny', 'DT', 'SnowballC', 'data.table', 'dplyr', 'ggplot2', 'glue', 'gt', 'here', 'openxlsx', 'plotly', 'purrr', 'reactR', 'readr', 'reshape2', 'scales', 'tm', 'zip', 'shinycssloaders', 'shinyWidgets'), repos='https://cloud.r-project.org')"

# Copy your app and run.R to the image
COPY . /srv/shiny-server/

# Ensure www folder is accessible (Shiny looks for static content here)
# The www folder should be in the same directory as the Shiny app
WORKDIR /srv/shiny-server/

# Expose the default Shiny port
EXPOSE 3838

# Run your R script
CMD ["Rscript", "/srv/shiny-server/run.R"]