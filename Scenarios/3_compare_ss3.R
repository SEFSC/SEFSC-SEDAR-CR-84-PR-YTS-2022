# Specify pattern of runs (ex: "84_stx", "m2$")
pattern <- "19a_m3$|31a_m3_f2_0310$|19_m3$|31_m3_f2_0310$"
folder_name <- "m3_test"

# Specify cutout string to match short names in scenarios.csv
cutout <- "84_pr_f3_3cm_0648_0056_"

# Specify plot_notes comparison labels (e.g., "sensitivity" or "scenario")
note_label <- "report"

# Specify sizeselex plot settings
minbthresh <- -1
btarg <- -1
sprtarg <- -1

# Specify output folder name ####
output_dir <- here::here("scenarios", paste0("plots_", folder_name))
output_dir_plain <- paste0(output_dir, "_plain")

# Read names 
full_names <- list.files(
  here::here("Scenarios"),
  pattern = pattern,
  full.names = TRUE
) |>
  stringr::str_subset(pattern = "plots", negate = TRUE)

full_names

# Format short names
short_names <- list.files(here::here("scenarios"), pattern = pattern) |>
  stringr::str_remove(cutout) |>
  stringr::str_subset(pattern = "plots", negate = TRUE)

short_names

# Read notes
notes <- read.csv(here::here("scenarios", "scenarios.csv"))

plot_notes <- notes |>
  dplyr::filter(scenario %in% short_names)

# Read report files
full_data <- r4ss::SSgetoutput(dirvec = full_names) |>
  rlang::set_names(short_names) |>
  r4ss::SSsummarize()

# Prep location for plots
if (dir.exists(output_dir)) unlink(output_dir, recursive = TRUE)
dir.create(output_dir)

if (dir.exists(output_dir_plain)) unlink(output_dir_plain, recursive = TRUE)
dir.create(output_dir_plain)

# Plot comparisons
r4ss::SSplotComparisons(
  full_data,
  png = TRUE,
  plotdir = output_dir,
  legendlabels = short_names,
  sprtarg = sprtarg,
  btarg = btarg,
  minbthresh = minbthresh,
  pwidth = 4, pheight = 3
)

r4ss::SSplotComparisons(
  full_data,
  png = TRUE,
  plotdir = output_dir_plain,
  legend = FALSE,
  sprtarg = sprtarg,
  btarg = btarg,
  minbthresh = minbthresh,
  pwidth = 4, pheight = 3
)

# Read sizeselex
short_sizeselex <- read.csv(
  here::here("Scenarios", "scenarios_size_selex.csv")
) |>
  dplyr::filter(scenario %in% short_names) |>
  dplyr::mutate(block = as.factor(block))

# Plot selectivity
plot_sizeselex <- function(
  fleets = unique(short_sizeselex$Fleet),
  scenarios = unique(short_sizeselex$scenario),
  factors = c("Lsel"),
  sizesel_file = "",
  aes_color = "scenario",
  aes_lty = "block"
) {

  short_sizeselex |>
    dplyr::filter(
      Fleet %in% fleets,
      scenario %in% scenarios,
      Factor %in% factors,
      !is.na(selex)
    ) |>
    dplyr::mutate(
      size = as.numeric(size),
      selex = as.numeric(selex),
      Fleet = dplyr::recode(Fleet, "1" = "Commercial",
                            "2" = "NCRMP")
    ) |>
    ggplot2::ggplot(ggplot2::aes(
      x = size,
      y = selex,
      color = .data[[aes_color]],
      lty = .data[[aes_lty]]
    )) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~ Fleet) +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "Length (cm)",
                  y = "Selectivity",
                  lty = "Block",
                  color = "Model Scenario")
                  

  ggplot2::ggsave(
    here::here(output_dir, paste0("sizeselex", sizesel_file, ".png")),
    width = 6.5,
    height = 5,
    bg = "white"
  )
}

plot_sizeselex()

plot_sizeselex(
  fleets = c("1"),
  sizesel_file = "_fleet1"
)
