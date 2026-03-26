
load_mesa <- function(){

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

  input <- haven::read_sas(file.path(fpath_christy, 'mesa.sas7bdat')) %>%
    mutate(prevhf = factor('no', levels = c("no", "yes")), .after = prevchd)

  events <- haven::read_sas('data/mesaeventspatch.sas7bdat') %>%
    transmute(id,
              status_chd = miChdEventPrimary,
              time_chd = miChdTimePrimary,
              status_ascvd = ascvdNoProcEventPrimary ,
              time_ascvd = ascvdNoProcTimePrimary,
              status_cvd = totalCvdEventPrimary,
              time_cvd = totalCvdTimePrimary)

  # events <- haven::read_sas(file.path(fpath_events, 'mesaevents.sas7bdat')) %>%
  #   transmute(id,
  #             # status_chd = miChdEventPrimary,
  #             # time_chd = miChdTimePrimary/365.25,
  #             status_ascvd = ascvdEventPrimary,
  #             time_ascvd = ascvdTimePrimary/365.25)


  output <- input %>%
    mutate(
      study = 'mesa',
      lpa_nmol = pmax(lpa_nmol, 0.01),
      sex = factor(sex, levels = c("F", "M")),
      lpa_nmol = if_else(lpa_nmol < 1.12,
                         true = runif(n(), 0, 1.12),
                         false = lpa_nmol)
    ) %>%
    left_join(events, by = 'id') %>%
    select(-id)

  # summarytools::view(summarytools::dfSummary(output))

  output %>%
    data_clean() %>%
    data_derive() %>%
    data_select()

}

