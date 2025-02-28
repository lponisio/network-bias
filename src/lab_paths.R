#add in your system name and copy to your base directory :)

sysname <- Sys.info()["nodename"]
## LP, RH, RM
if(sysname == "UO-2007086" | sysname  == "Roses-MacBook-Air.local" |
   sysname == "DESKTOP-T144ERR") {
  local.path <- "~/Dropbox (University of Oregon)/"
  ## ASF
} else if(sysname == "DESKTOP-BK24TM3" | sysname == "OMENL") {
  local.path <- "C:/Users/ale_s/Dropbox (University of Oregon)/"
  ## Laura
} else if(sysname == "XXDESKTOP-U23E6UE") { 
  local.path <- "~/laura/Dropbox (University of Oregon)/'"
  ## Lili
} else if(sysname == "CNS-G-PATA69481"){
  local.path <-  " C:/Users/lb37426/Dropbox (University of Oregon)/"
}
