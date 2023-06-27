# library(shiny)
# 
# port <- Sys.getenv('PORT', "8080")  # Setează portul ca fiind 8080 dacă variabila de mediu PORT nu este disponibilă
# 
# # Definește o adresă IP pentru host în funcție de variabila de mediu din Heroku
# host <- if (Sys.getenv('PORT') == '') '0.0.0.0' else '127.0.0.1'
# 
# shiny::runApp(
#   appDir = getwd(),
#   host = host,
#   port = as.numeric(port)
# )

# run.R
library(shiny)
port <- Sys.getenv('PORT')
shiny::runApp(
  appDir = getwd(),
  host = '0.0.0.0',
  port = as.numeric(port)
)