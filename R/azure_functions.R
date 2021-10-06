# Assign Database Connection
synapse_server <- "swhscphipprduks01.sql.azuresynapse.net"
synapse_database <- "exploratorydb"
connection_driver <- "{ODBC Driver 17 for SQL Server}"

con <- DBI::dbConnect(odbc::odbc(),
                 driver = connection_driver,
                 database = synapse_database,
                 Authentication="ActiveDirectoryMSI",
                 server = synapse_server)

# Table Function
getTable <- function(table) {
  query <- paste("SELECT * FROM", table)
  data <- DBI::dbGetQuery(con, query)
  message(paste0("Data retrieved from ", table))
  return(data)
}

# Standard SQL Language
# getTableFiltered <- function(table) {
#   query <- paste("SELECT * FROM", table, "WHERE ('CreatedOn' >= '20210910')")
#   data <- dbGetQuery(con, query)
#   message(paste0("Data retrieved from ", table))
#   return(data)
# }

# Adjusted Table Function Using dplyr
getTableFiltered <- function(table, date) {
  query <- dplyr::tbl(con, table) %>%
    dplyr::filter(CreatedOn >= date)
  
  dplyr::show_query(query)
  data <- as.data.frame(query)
  message(paste0("Data retrieved from ", table, ". Filtered from ", date))
  return(data)
}

# Testing function

#q1 <- tbl(con, "Locations") %>%
#  filter(CreatedOn >= "20210830",
#         TypeOfPlace == "School or college") %>% 
#  left_join(tbl(con, "CollectContactsCalls"), by = c("CollectCallId" = "Id")) %>%
#  left_join(tbl(con, "Cases"), by = "CaseNumber") %>% 
#  filter(!is.na(CaseNumber)) 

#show_query(q1)

#q1.data <- as.data.frame(q1)


