library(shiny)
library(here)

ui_path <- here("src", "ui.R")
server_path <- here("src", "server.R")

source(ui_path)
source(server_path)

shinyApp(ui, server)
