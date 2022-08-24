REEM.package.load <- function(PKG){
  for (p in PKG) {
    if (!require(p,character.only = TRUE)){
      install.packages(p); 
      require(p,character.only = TRUE)
    }
  }
}


REEM.oracle.connect <- function(){
  # Define RODBC connection to ORACLE
  get.connected <- function(schema = sql_schema, username, password) { #BF changed from AFSC to AFSCP1 - that's how it's set up on my computer
    (echo <- FALSE)
    if (!hasArg(username)) {username <- getPass(msg = "Enter your ORACLE Username: ")}
    if (!hasArg(password)) {password <- getPass(msg = "Enter your ORACLE Password: ")
    }
    channel <- RODBC::odbcConnect(paste(schema), paste(username), paste(password), believeNRows = FALSE)
  }
  
  # Execute the connection
  suppressWarnings(channel <- get.connected())
  while (channel == -1) {
    cat("Unable to connect. Username or password may be incorrect. Please re-enter.\n\n")
    suppressWarnings(channel <- get.connected())
  }
  if (class(channel) == "RODBC") {
    cat("\nSuccessfully connected to Oracle!\n\n")
  }
  
  return(channel)
}



