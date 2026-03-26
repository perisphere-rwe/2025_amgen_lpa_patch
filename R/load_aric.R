load_aric <- function(){


  fpath_christy <- file.path('..',
                             '..',
                             '..',
                             '..',
                             '..',
                             'proj',
                             'epi',
                             'CVDGeneNas',
                             'bcjaeger',
                             'amgen-lpa',
                             'christy')

  # fpath_events <- file.path(fpath_christy, 'events')
  #
  #
  # events <- haven::read_sas(file.path(fpath_events, 'aricevents.sas7bdat')) %>%
  #   transmute(id,
  #             status_ascvd = ascvdEventPrimary,
  #             time_ascvd = ascvdTimePrimary/365.25)

  events <- haven::read_sas('data/aricprimaryeventspatch23.sas7bdat') %>%
    transmute(id,
              prevhf = prevHF,
              status_ascvd = ascvdNoProcEventPrimary,
              time_ascvd = ascvdNoProcTimePrimary,
              status_chd = miChdEventPrimary,
              time_chd = miChdTimePrimary,
              status_cvd = totalCvdEventPrimary,
              time_cvd = totalCvdTimePrimary)

  input <- haven::read_sas(file.path(fpath_christy, 'aric.sas7bdat')) %>%
    left_join(events, by = 'id') %>%
    # restricts to aric visit 4
    drop_na(age)

  # to get same lpa values below detection limit
  set.seed(730)

  output <- input %>%
    mutate(study = 'aric') %>%
    select(-inJhs, -id) %>%
    mutate(
      lpa_nmol = if_else(lpa_nmol < 1.12,
                         true = runif(n(), 0, 1.12),
                         false = lpa_nmol)
    )

  setDT(output)


  output[currentEtoh == "T", currentEtoh := NA_character_]
  output[, currentEtoh := as.numeric(currentEtoh)]

  output[prevdiabetes == "T", prevdiabetes := NA_character_]
  output[, prevdiabetes := as.numeric(prevdiabetes)]

  output[cholMeds == "T", cholMeds := NA_character_]
  output[, cholMeds := as.numeric(cholMeds)]

  output[currentSmoke == "T", currentSmoke := NA_character_]
  output[currentSmoke == "", currentSmoke := NA_character_]
  output[, currentSmoke := as.numeric(currentSmoke)]

  # summarytools::view(summarytools::dfSummary(output))

  output %>%
    data_clean() %>%
    data_derive() %>%
    data_select()

}


# old code to do aric conversion
# Y= A + B*X + C*X2

# Where: Y = Lp(a) conc. in nmol/L
# X = Lp(a) conc. in mg/dL
# A = -0.01995
# B = 2.285
# C = 0.001371
# lpa = (-0.01995) + (2.285) * lpa_mgdl + (0.001371) * (lpa_mgdl)^2,
