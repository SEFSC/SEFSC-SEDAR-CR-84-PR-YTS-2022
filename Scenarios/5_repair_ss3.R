library(foreach)

# Specify pattern
pattern <- "84_pr_f3_"

# Get folder names ####
full_names <- list.files(
  path = here::here("Scenarios"),
  pattern = pattern,
  full.names = TRUE
)

full_names

foreach::foreach(i = seq_along(full_names)) %do% {

  start <- r4ss:::SS_readstarter(here::here(full_names[i], "starter.ss"),
                                 verbose = FALSE)
  dat <- r4ss:::SS_readdat(file = here::here(full_names[i], start$datfile),
                           version = 3.3, verbose = FALSE)
  ctl <- r4ss::SS_readctl(file = here::here(full_names[i], start$ctlfile),
                          datlist = dat, verbose = FALSE)
  fcast <- r4ss::SS_readforecast(file = here::here(full_names[i], "forecast.ss"),
                            verbose = FALSE)
  
  
  
  ctl$F_Method
  ctl$F_Method = 2
  
  ctl$maxF
  ctl$maxF = 4
  ctl$maxF
  
  ctl$F_setup
  ctl$F_setup[1] = 0.05
  ctl$F_setup[2] = 1.00
  ctl$F_setup[3] = 0.00
  ctl$F_setup
  
  # dat$maximum_size = 56
  
  # Min Size_DblN_ascend_se_Commercial(1)	
  # ctl$size_selex_parms[3, 1]
  # ctl$size_selex_parms[3, 1] = -10
  # ctl$size_selex_parms[3, 1]
  
  # Min Size_DblN_ascend_se_Commercial(1)	
  # ctl$size_selex_parms[3, 1]
  # ctl$size_selex_parms[3, 1] = -10
  # ctl$size_selex_parms[3, 1]
  
  # #  Size_DblN_peak_NCRMP(2)
  # ctl$size_selex_parms[1, 4] = 29 
  # ctl$size_selex_parms[7, 4] = 7
  # ctl$size_selex_parms[7, 1] = 6
  # ctl$size_selex_parms[7, 2] = 48
  # ctl$size_selex_parms[, 3] = ctl$size_selex_parms[, 4] 
  # 
  # # SPR Target
  # fcast$SPRtarget
  # fcast$SPRtarget = 0.4
  # fcast$SPRtarget
  # 
  # fcast$Btarget
  # fcast$Btarget = 0.4
  # fcast$Btarget
  # 
  # # Higher catch uncertainty
  # dat$catch[1, 5]
  # dat$catch[1, 5] = 0.3
  # dat$catch[1, 5]
  # 
  # dat$catch[1, 4]
  # dat$catch[1, 4] = 168.3114
  # dat$catch[1, 4]

  # Update age
  # dat$Nages
  # dat$Nages = 26
  # dat$Nages
  # 
  # dat$N_agebins
  # dat$N_agebins = dat$Nages
  # dat$N_agebins
  # 
  # dat$agebin_vector
  # dat$agebin_vector = c(0:(dat$Nages-1))
  # dat$agebin_vector
  # 
  # dat$ageerror
  # # dat$ageerror = cbind(dat$ageerror, dat$ageerror[dat$Nages])
  # dat$ageerror = dat$ageerror[, 1:(dat$Nages+1)]
  # dat$ageerror
  
  # Update M
  # ctl$MG_parms
  # ctl$MG_parms[1,3]
  #  ctl$MG_parms[1,3] = 0.208
  # ctl$MG_parms[13,3]
  # ctl$MG_parms[13,3] = 0.208
  # 
  # ctl$MG_parms[7,3]
  # ctl$MG_parms[7,3] = 2.93e-05
  # ctl$MG_parms[19,3]
  # ctl$MG_parms[19,3] = 2.93e-05
  # 
  # ctl$MG_parms[8,3]
  # ctl$MG_parms[8,3] = 2.8642
  # ctl$MG_parms[20,3]
  # ctl$MG_parms[20,3] = 2.8642
  
  # ctl$size_selex_parms[1, 1]
  # ctl$size_selex_parms[1, 1] = 15
  # ctl$size_selex_parms[1, 2]
  # ctl$size_selex_parms[1, 2] = 35
  # ctl$size_selex_parms[1, 3]
  # ctl$size_selex_parms[1, 3] = 25
  
  # ctl$size_selex_parms[3, 1]
  # ctl$size_selex_parms[3, 1] = -2
  # ctl$size_selex_parms[3, 2]
  # ctl$size_selex_parms[3, 2] = 6
  # ctl$size_selex_parms[3, 3]
  # ctl$size_selex_parms[3, 3] = 2
  
  # ctl$SR_parms[1, 1]
  # ctl$SR_parms[1, 1] = 4
  # ctl$SR_parms[1, 2] 
  # ctl$SR_parms[1, 2] = 7
  # ctl$SR_parms[1, 3]  
  # ctl$SR_parms[1, 3] = 5
  
  # ctl$init_F[1, 1]
  # ctl$init_F[1, 1] = 0
  # ctl$init_F[1, 2]
  # ctl$init_F[1, 2] = 4
  # ctl$init_F[1, 3]
  # ctl$init_F[1, 3] = 1.2
  
  # ctl$do_recdev = 1
  # 
  # r4ss::SS_writedat(
  #   datlist = dat,
  #   outfile = here::here(full_names[i], start$datfile),
  #   overwrite = TRUE,
  #   verbose = FALSE
  # )

  r4ss::SS_writectl(
    ctllist = ctl,
    outfile = here::here(full_names[i], start$ctlfile),
    overwrite = TRUE
  )

  # r4ss::SS_writeforecast(
  #   fcast,
  #   dir = here::here(full_names[i]),
  #   overwrite = TRUE,
  #   verbose = FALSE
  # )
  
  return(NULL)
}
