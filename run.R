library(shiny)
library(here)

ui_path <- here("src", "ui.R")
server_path <- here("src", "server.R")

source(ui_path)
source(server_path)

# Set the resource path for the www directory explicitly
# This tells Shiny where to find static files referenced in img() tags
addResourcePath(prefix = "images", directoryPath = here("www"))

shinyApp(ui, server, options = list(port = 3838, host = "0.0.0.0"))
