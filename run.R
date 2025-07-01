library(shiny)
library(here)

ui_path <- here("src", "ui.R")
server_path <- here("src", "server.R")

source(ui_path)
source(server_path)

shinyApp(ui, server, options = list(port = 3838, host = "0.0.0.0"))
