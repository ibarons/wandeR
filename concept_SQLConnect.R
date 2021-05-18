
conn <- DBI::dbConnect(odbc::odbc(),
  Driver = "SQL Server",
  server = "iomsdb01.database.windows.net",
  Database = "IOMS-db01",
  UID = "iomsdbadmin",
  PWD = 
  port = '1433'
)


