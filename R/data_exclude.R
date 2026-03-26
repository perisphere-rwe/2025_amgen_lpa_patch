

data_exclude <- function(data,
                         age_range,
                         exclusion_fname){


  e0 <- data %>%
    drop_na(lpa_nmol, lpa_mgdl)

  e1 <- e0 %>%
    filter(age >= age_range[1], age < age_range[2])

  e2 <- e1 %>%
    filter(hx_stroke == 'no', hx_chd == 'no')

  e3 <- e2 %>%
    drop_na(age, sex, smoke_current, bmi, sbp, med_htn,
            diabetes, chol_total, chol_hdl, med_statin, egfr_2021)

  e4 <- e3 %>%
    filter(chol_total >= 130, chol_total <= 320,
           chol_hdl >= 20, chol_hdl <= 100,
           sbp >= 90, sbp <= 200,
           bmi >= 18.5, bmi <= 40,
           egfr_2021 >= 15, egfr_2021 <= 140)

  e5 <- e4 %>%
    drop_na(time_ascvd, status_ascvd,
            time_cvd, status_cvd)

  exclusions <-
    list("Measured Lp(a) at baseline exam" = e0,
         "Aged _lower_ to < _upper_ years at exam" = e1,
         "No history of CVD at exam" = e2,
         "Complete data for variables used by PREVENT equations" = e3,
         "All values in recommended range for PREVENT equations" = e4,
         "With follow-up for CVD events" = e5) %>%
    map_dfr(
      ~ summarize_each_group(.x, n = n(), groups = 'study'),
      .id = 'inclusion'
    ) %>%
    select(-.group_variable) %>%
    pivot_wider(names_from = .group_level, values_from = n) %>%
    mutate(
      inclusion = str_replace(inclusion, '_lower_', as.character(age_range[1])),
      inclusion = str_replace(inclusion, '_upper_', as.character(age_range[2]))
    )

  write_csv(exclusions, file = glue('data/exclusions_{exclusion_fname}.csv'))

  e5

}
