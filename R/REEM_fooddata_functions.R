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


