# Spinner 
options(spinner.color = "#0275D8", spinner.color.background="#ffffff", spinner.size =2)

# Suppress Warnings
options(warn = - 1)   

# Issue Connection Stop --- This should be moved to the bottom of global.R
shiny::onStop(function(){DBI::dbDisconnect(con)})
