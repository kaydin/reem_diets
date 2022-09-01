#-----------------------------------
#Load and name-clean files
REEM.loadclean.RACE <- function(pathname="data/local_racebase"){

  a <- list.files(pathname, pattern = "\\.csv")
  
  for (i in 1:length(a)) {
    b <- read.csv(file = paste("data/local_racebase", a[i], sep="/"))
    b <- janitor::clean_names(b)
    if (names(b)[1] %in% "x1") {
      b$x1 <- NULL
    }
    # KYA Note - assign is a global assigment (to environment)
    cat("loading and cleaning",a[i],"\n"); flush.console()
    assign(x = gsub(pattern = "\\.csv", replacement = "", x = a[i]), value = b, envir = .GlobalEnv)
  }
  
}

#-----------------------------------
# Loads packages required for scripts
REEM.package.load <- function(PKG){
  for (p in PKG) {
    if (!require(p,character.only = TRUE)){
      install.packages(p); 
      require(p,character.only = TRUE)
    }
  }
}

#-----------------------------------
# Connects to Oracle database
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



