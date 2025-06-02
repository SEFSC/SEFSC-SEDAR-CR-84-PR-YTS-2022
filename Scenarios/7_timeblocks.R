library(foreach)
minbthresh <- -1
plots <- c(1:26)

setup_path <- function(dir) {
  if (dir.exists(dir)) {
    unlink(dir, recursive = TRUE)
  }
  dir.create(dir)
}

files_ss <- c(
  "controlfile.ctl",
  "datafile.dat",
  "forecast.ss",
  "starter.ss",
  "ss3.exe"
)

setup_ss3 <- function(source, dir) {
  file.copy(from = here::here(source, files_ss),
            to = here::here(dir, files_ss))
}

# Function to run ss3 and r4ss::SS_plots
runplot <- function(full_names, plots, minbthresh) {
  
  library(foreach)
  
  # Run models and plots
  i <- NULL
  foreach(i = seq_along(full_names)) %do% {
    model_run <- full_names[i]
    shell(paste("cd /d ", model_run, " && ss3 ", sep = ""))
    myreplist <- r4ss::SS_output(dir = model_run)
    if (dir.exists(paste0(model_run, "/plots"))) {
      unlink(paste0(model_run, "/plots"), recursive = TRUE)
    }
    r4ss::SS_plots(
      replist = myreplist, 
      plot = plots, 
      minbthresh = minbthresh, 
      pwidth = 4, 
      pheight = 3, 
      pheight_tall = 4,
      maxrows = 3,
      maxcols = 3
    )
  }
}


# One time block ####
f1_block_year = c(1987:2020)

f1_pattern <- "84_pr_f3_3cm_0648_0056_v24_f1_m1_86"
f1_cutout <- "84_pr_f3_3cm_0648_0056_v24_f1_m1_"

f1 = here::here("Scenarios", "v24_f1_m1")
f1_source = here::here(f1, f1_pattern)

foreach::foreach(i = seq_along(f1_block_year)) %do% {
  dir_f1 <- here::here(
    f1, paste0(cutout, stringr::str_sub(f1_block_year[i], - 2, - 1))
  )
  setup_path(dir_f1)
  setup_ss3(f1_source, dir_f1)  

  start <- r4ss:::SS_readstarter(here::here(dir_f1, "starter.ss"),
                                 verbose = FALSE)
  dat <- r4ss:::SS_readdat(file = here::here(dir_f1, start$datfile),
                           version = 3.3, verbose = FALSE)
  ctl <- r4ss::SS_readctl(file = here::here(dir_f1, start$ctlfile),
                          datlist = dat, verbose = FALSE)
  
  ctl$Block_Design
  ctl$Block_Design[[1]][2] = f1_block_year[i]
  ctl$Block_Design
  
  r4ss::SS_writectl(
    ctllist = ctl,
    outfile = here::here(dir_f1, start$ctlfile),
    overwrite = TRUE
  )
  
}

f1_full_names <- list.files(
  path = f1,
  full.names = TRUE
)

f1_full_names

# Create function (line 15) then run
runplot(f1_full_names, plots = plots, minbthresh = minbthresh)

# Read report files
f1_short_names <- list.files(here::here("Scenarios", "v24_f1_m1")) |>
  stringr::str_remove(f1_cutout)

f1_full_data <- f1_full_names |>
  purrr::map(r4ss::SS_output) |>
  rlang::set_names(f1_short_names)

f1_likelihood <- purrr::map(f1_full_data, "likelihoods_used") |>
  purrr::map(~ tibble::rownames_to_column(.x, "Label")) |>
  purrr::imap_dfr(~ dplyr::mutate(.x, scenario = .y))

f1_likelihood |> dplyr::filter(Label == "TOTAL") |>
  dplyr::filter(values == min(values))

f1_likelihood |> dplyr::filter(Label == "TOTAL") |>
  dplyr::arrange(values)


# Two time blocks ####
f2_block_year = c(1987:1997, 2001:2020)

f2_pattern <- "84_pr_f3_3cm_0648_0056_v24_f2_m1_9986"
f2_cutout <- "84_pr_f3_3cm_0648_0056_v24_f2_m1_99"

f2 = here::here("Scenarios", "v24_f2_m1")
f2_source = here::here(f2, f2_pattern)

foreach::foreach(i = seq_along(f2_block_year)) %do% {
  dir_f2 <- here::here(
    f2, paste0(f2_cutout, stringr::str_sub(f2_block_year[i], - 2, - 1))
  )
  setup_path(dir_f2)
  setup_ss3(f2_source, dir_f2)  
  
  start <- r4ss:::SS_readstarter(here::here(dir_f2, "starter.ss"),
                                 verbose = FALSE)
  dat <- r4ss:::SS_readdat(file = here::here(dir_f2, start$datfile),
                           version = 3.3, verbose = FALSE)
  ctl <- r4ss::SS_readctl(file = here::here(dir_f2, start$ctlfile),
                          datlist = dat, verbose = FALSE)
  
  ctl$Block_Design
  
  if(f2_block_year[i] < 1999) {
  ctl$Block_Design
  ctl$Block_Design[[1]][2] = f2_block_year[i]
  ctl$Block_Design[[1]][3] = f2_block_year[i] + 1
  ctl$Block_Design
  }
  else {
    ctl$Block_Design
    ctl$Block_Design[[1]][2] = 1999
    ctl$Block_Design[[1]][3] = 2000
    ctl$Block_Design[[1]][4] = f2_block_year[i]
    ctl$Block_Design
  }
  
  r4ss::SS_writectl(
    ctllist = ctl,
    outfile = here::here(dir_f2, start$ctlfile),
    overwrite = TRUE
  )
  
}

full_names <- list.files(
  path = f2,
  full.names = TRUE
)

full_names

# Create function (line 15) then run
runplot(plots = plots, minbthresh = minbthresh)

# Read report files
short_names <- list.files(here::here("Scenarios", "v24_f2_m1")) |>
  stringr::str_remove(f2_cutout)

full_data <- full_names |>
  purrr::map(r4ss::SS_output) |>
  rlang::set_names(short_names)

likelihood <- purrr::map(full_data, "likelihoods_used") |>
  purrr::map(~ tibble::rownames_to_column(.x, "Label")) |>
  purrr::imap_dfr(~ dplyr::mutate(.x, scenario = .y))

likelihood |> dplyr::filter(Label == "TOTAL") |>
  dplyr::filter(values == min(values))

likelihood |> dplyr::filter(Label == "TOTAL") |>
  dplyr::arrange(values)

# Three time blocks ####
f3_block_year = c(1987:1997, 2001:2006, 2010:2020)

f3_pattern <- "84_pr_f3_3cm_0648_0056_v24_f3_m1_990886"
f3_cutout <- "84_pr_f3_3cm_0648_0056_v24_f3_m1_9908"

f3 = here::here("Scenarios", "v24_f3_m1")
f3_source = here::here(f3, f3_pattern)

foreach::foreach(i = seq_along(f3_block_year)) %do% {
  dir_f3 <- here::here(
    f3, paste0(f3_cutout, stringr::str_sub(f3_block_year[i], - 2, - 1))
  )
  setup_path(dir_f3)
  setup_ss3(f3_source, dir_f3)  
  
  start <- r4ss:::SS_readstarter(here::here(dir_f3, "starter.ss"),
                                 verbose = FALSE)
  dat <- r4ss:::SS_readdat(file = here::here(dir_f3, start$datfile),
                           version = 3.3, verbose = FALSE)
  ctl <- r4ss::SS_readctl(file = here::here(dir_f3, start$ctlfile),
                          datlist = dat, verbose = FALSE)
  
  ctl$Block_Design
  
  if (f3_block_year[i] < 1999) {
    ctl$Block_Design
    ctl$Block_Design[[1]][2] = f3_block_year[i]
    ctl$Block_Design[[1]][3] = f3_block_year[i] + 1
    ctl$Block_Design
  } else if (f3_block_year[i] > 1999 & f3_block_year[i] < 2008) {
    ctl$Block_Design
    ctl$Block_Design[[1]][2] = 1999
    ctl$Block_Design[[1]][3] = 2000
    ctl$Block_Design[[1]][4] = f3_block_year[i]
    ctl$Block_Design[[1]][5] = f3_block_year[i] + 1
    ctl$Block_Design
  } else {
    ctl$Block_Design
    ctl$Block_Design[[1]][2] = 1999
    ctl$Block_Design[[1]][3] = 2000
    ctl$Block_Design[[1]][4] = 2008
    ctl$Block_Design[[1]][5] = 2009
    ctl$Block_Design[[1]][6] = f3_block_year[i]
    ctl$Block_Design
  }
  
  r4ss::SS_writectl(
    ctllist = ctl,
    outfile = here::here(dir_f3, start$ctlfile),
    overwrite = TRUE
  )
  
}

full_names <- list.files(
  path = f3,
  full.names = TRUE
)

full_names

# Create function (line 15) then run
runplot(plots = plots, minbthresh = minbthresh)

# Read report files
short_names <- list.files(here::here("Scenarios", "v24_f3_m1")) |>
  stringr::str_remove(f3_cutout)

full_data <- full_names |>
  purrr::map(r4ss::SS_output) |>
  rlang::set_names(short_names)

likelihood <- purrr::map(full_data, "likelihoods_used") |>
  purrr::map(~ tibble::rownames_to_column(.x, "Label")) |>
  purrr::imap_dfr(~ dplyr::mutate(.x, scenario = .y))

likelihood |> dplyr::filter(Label == "TOTAL") |>
  dplyr::filter(values == min(values))