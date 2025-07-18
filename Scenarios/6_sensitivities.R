library(foreach)

# Specify pattern ####
pattern <- "_a_m3$|01_m3$|08a_m3$|19a_m3$|28a_m3_f2_0310$|31a_m3_f2_0310$"

## Get folder names ####
full_names <- list.files(
  path = here::here("Scenarios"),
  pattern = pattern,
  full.names = TRUE
)

full_names

# Choose what to run ####
run_s1 <- FALSE
run_s2 <- FALSE
run_s3 <- TRUE
run_s4 <- TRUE

setup_path <- function(dir) {
  if (dir.exists(dir)) {
    unlink(dir, recursive = TRUE)
  }
  dir.create(dir)
}

setup_ss3 <- function(dir) {
  file.copy(from = here::here(scenario, files_ss_new),
            to = here::here(dir, files_ss_new))
  file.rename(here::here(dir, files_ss_new),
              here::here(dir, files_ss))
}

# List files to copy and rename
files_ss_new <- c(
  "control.ss_new",
  "data_echo.ss_new",
  "forecast.ss_new",
  "starter.ss_new",
  "ss3.exe"
)

files_ss <- c(
  "controlfile.ctl",
  "datafile.dat",
  "forecast.ss",
  "starter.ss",
  "ss3.exe"
)

foreach::foreach(i = seq_along(full_names)) %do% {
  
  # Choose scenario  ####
  scenario <- full_names[i]
  
  #_s1 higher CV on growth young ####
  if (run_s1 == TRUE) {
    
    dir_s1 <- paste0(scenario, "_s1")
    setup_path(dir_s1)
    setup_ss3(dir_s1)
    
    start <- r4ss:::SS_readstarter(here::here(dir_s1, "starter.ss"),
                                   verbose = FALSE)
    dat <- r4ss:::SS_readdat(file = here::here(dir_s1, start$datfile),
                             version = 3.3, verbose = FALSE)
    ctl <- r4ss::SS_readctl(file = here::here(dir_s1, start$ctlfile),
                            datlist = dat, verbose = FALSE)
    ctl$MG_parms[5,3]
    ctl$MG_parms[5,3] = 0.25
    ctl$MG_parms[5,3]
    ctl$MG_parms[17,3]
    ctl$MG_parms[17,3] = 0.25
    ctl$MG_parms[17,3]
    
    r4ss::SS_writectl(
      ctllist = ctl,
      outfile = here::here(dir_s1, start$ctlfile),
      overwrite = TRUE
    )
    
  }
    
  #_s2 higher age and lower m ####
  if (run_s2 == TRUE) {
    
    dir_s2 <- paste0(scenario, "_s2")
    setup_path(dir_s2)
    setup_ss3(dir_s2)
    
    start <- r4ss:::SS_readstarter(here::here(dir_s2, "starter.ss"),
                                   verbose = FALSE)
    dat <- r4ss:::SS_readdat(file = here::here(dir_s2, start$datfile),
                             version = 3.3, verbose = FALSE)
    ctl <- r4ss::SS_readctl(file = here::here(dir_s2, start$ctlfile),
                            datlist = dat, verbose = FALSE)
    
    dat$Nages
    dat$Nages = 28
    dat$Nages
    
    dat$N_agebins
    dat$N_agebins = dat$Nages
    dat$N_agebins
    
    dat$agebin_vector
    dat$agebin_vector = c(0:(dat$Nages-1))
    dat$agebin_vector
    
    dat$ageerror
    dim(dat$ageerror)
    dat$ageerror = cbind(
      dat$ageerror, dat$ageerror[,1:(dat$Nages-dim(dat$ageerror)[2]+1)] 
    )
    dat$ageerror
    dim(dat$ageerror)
    
    ctl$MG_parms[1,3] 
    ctl$MG_parms[1,3] = 0.193
    ctl$MG_parms[1,3] 
    ctl$MG_parms[13,3] 
    ctl$MG_parms[13,3] = 0.193
    ctl$MG_parms[13,3]
    
    ctl$SR_parms[1,1]
    ctl$SR_parms[1,1] = 3
    ctl$SR_parms[1,1]
    
    r4ss::SS_writedat(
      datlist = dat,
      outfile = here::here(dir_s2, start$datfile),
      overwrite = TRUE,
      verbose = FALSE
    )
    
    r4ss::SS_writectl(
      ctllist = ctl,
      outfile = here::here(dir_s2, start$ctlfile),
      overwrite = TRUE
    )  
    
  }
  
  #_s3 set up for catch stdev 2  ####
  if (run_s3 == TRUE) {
    dir_s3 <- paste0(scenario, "_s3")
    setup_path(dir_s3)
    setup_ss3(dir_s3)
    
    start <- r4ss:::SS_readstarter(here::here(dir_s3, "starter.ss"),
                                   verbose = FALSE)
    dat <- r4ss:::SS_readdat(file = here::here(dir_s3, start$datfile),
                             version = 3.3, verbose = FALSE)
    ctl <- r4ss::SS_readctl(file = here::here(dir_s3, start$ctlfile),
                            datlist = dat, verbose = FALSE)
    
    dat$catch[1,5]
    dat$catch[1,5] = 2
    dat$catch[1,5]
    
    r4ss::SS_writedat(
      datlist = dat,
      outfile = here::here(dir_s3, start$datfile),
      overwrite = TRUE,
      verbose = FALSE
    )
  }

  #_s4 higher catch uncertainty, higher age and lower m ####
  if (run_s4 == TRUE) {
    
    dir_s4 <- paste0(scenario, "_s4")
    setup_path(dir_s4)
    setup_ss3(dir_s4)
    
    start <- r4ss:::SS_readstarter(here::here(dir_s4, "starter.ss"),
                                   verbose = FALSE)
    dat <- r4ss:::SS_readdat(file = here::here(dir_s4, start$datfile),
                             version = 3.3, verbose = FALSE)
    ctl <- r4ss::SS_readctl(file = here::here(dir_s4, start$ctlfile),
                            datlist = dat, verbose = FALSE)
    
    dat$Nages
    dat$Nages = 28
    dat$Nages
    
    dat$N_agebins
    dat$N_agebins = dat$Nages
    dat$N_agebins
    
    dat$catch[1,5]
    dat$catch[1,5] = 2
    dat$catch[1,5]
    
    dat$agebin_vector
    dat$agebin_vector = c(0:(dat$Nages-1))
    dat$agebin_vector
    
    dat$ageerror
    dim(dat$ageerror)
    dat$ageerror = cbind(
      dat$ageerror, dat$ageerror[,1:(dat$Nages-dim(dat$ageerror)[2]+1)] 
    )
    dat$ageerror
    dim(dat$ageerror)
    
    ctl$MG_parms[1,3] 
    ctl$MG_parms[1,3] = 0.193
    ctl$MG_parms[1,3] 
    ctl$MG_parms[13,3] 
    ctl$MG_parms[13,3] = 0.193
    ctl$MG_parms[13,3]
    
    ctl$SR_parms[1,1]
    ctl$SR_parms[1,1] = 3
    ctl$SR_parms[1,1]
    
    r4ss::SS_writedat(
      datlist = dat,
      outfile = here::here(dir_s4, start$datfile),
      overwrite = TRUE,
      verbose = FALSE
    )
    
    r4ss::SS_writectl(
      ctllist = ctl,
      outfile = here::here(dir_s4, start$ctlfile),
      overwrite = TRUE
    )  
    
  }
  
}
