library(shiny)

port<-7347
Sys.setenv(PORT = port)

shiny::runApp(
  appDir = getwd(),
  host = '127.0.0.1',
  port = as.numeric(port)
)