

data_derive <- function(data){


  data %>%
    mutate(

      # this column just makes it easier to do subgroup analysis
      # by splitting the data by whichever column is needed. In
      # the overall analysis, we split the data by the constant
      # overall column. In other subgroup analyses, e.g., sex,
      # we split the data by sex.
      overall = 'overall',

      lpa_nmol_log = log(lpa_nmol+1),

      lpa_nmol_catg = cut(lpa_nmol,
                          breaks = c(0, 100, 200, Inf),
                          right = FALSE,
                          labels = c('lt_100', 'gteq_100_lt_200', 'gteq_200')),

      lpa_mgdl_log = log(lpa_mgdl+1),

      lpa_mgdl_catg = cut(lpa_nmol,
                          breaks = c(0, 40, 80, Inf),
                          right = FALSE,
                          labels = c('lt_40', 'gteq_40_lt_80', 'gteq_80')),

      age_cat = cut(age,
                    breaks = c(0, 55, 70, Inf),
                    include.lowest = TRUE,
                    right = FALSE,
                    labels = c("lt_55",
                               "gteq_55_lt_70",
                               "gteq_70")),


      gfrCysCrea = CKDEpi2021.creat(creatinine = serumCreatinine,
                                    sex = as.numeric(sex == 'male'),
                                    age = age),

      egfr_lt60 = if_else(gfrCysCrea < 60, 'yes', 'no'),

      cvd_risk_10yr = predict_10yr_cvd_risk(age_years = age,
                                            sex = sex,
                                            smoke_current = currentSmoke,
                                            chol_total_mgdl = totalChol,
                                            chol_hdl_mgdl = HDL,
                                            bp_sys_mmhg = SBP,
                                            bp_meds = HtnMedCodes,
                                            statin_meds = statinMedCodes,
                                            diabetes = prevdiabetes,
                                            bmi = BMI,
                                            egfr_mlminm2 = gfrCysCrea,
                                            equation_version = "Khan_2023",
                                            prevent_type = "base",
                                            override_boundary_errors = TRUE),

      cvd_risk_30yr = predict_30yr_cvd_risk(age_years = age,
                                            sex = sex,
                                            smoke_current = currentSmoke,
                                            chol_total_mgdl = totalChol,
                                            chol_hdl_mgdl = HDL,
                                            bp_sys_mmhg = SBP,
                                            bp_meds = HtnMedCodes,
                                            statin_meds = statinMedCodes,
                                            diabetes = prevdiabetes,
                                            bmi = BMI,
                                            egfr_mlminm2 = gfrCysCrea,
                                            equation_version = "Khan_2023",
                                            prevent_type = "base",
                                            override_boundary_errors = TRUE),

      ascvd_risk_10yr = predict_10yr_ascvd_risk(age_years = age,
                                                sex = sex,
                                                smoke_current = currentSmoke,
                                                chol_total_mgdl = totalChol,
                                                chol_hdl_mgdl = HDL,
                                                bp_sys_mmhg = SBP,
                                                bp_meds = HtnMedCodes,
                                                statin_meds = statinMedCodes,
                                                diabetes = prevdiabetes,
                                                bmi = BMI,
                                                egfr_mlminm2 = gfrCysCrea,
                                                equation_version = "Khan_2023",
                                                prevent_type = "base",
                                                override_boundary_errors = TRUE),

      ascvd_risk_30yr = predict_30yr_ascvd_risk(age_years = age,
                                                sex = sex,
                                                smoke_current = currentSmoke,
                                                chol_total_mgdl = totalChol,
                                                chol_hdl_mgdl = HDL,
                                                bp_sys_mmhg = SBP,
                                                bp_meds = HtnMedCodes,
                                                statin_meds = statinMedCodes,
                                                diabetes = prevdiabetes,
                                                bmi = BMI,
                                                egfr_mlminm2 = gfrCysCrea,
                                                equation_version = "Khan_2023",
                                                prevent_type = "base",
                                                override_boundary_errors = TRUE),

      chd_risk_10yr = predict_10yr_chd_risk(age_years = age,
                                            sex = sex,
                                            smoke_current = currentSmoke,
                                            chol_total_mgdl = totalChol,
                                            chol_hdl_mgdl = HDL,
                                            bp_sys_mmhg = SBP,
                                            bp_meds = HtnMedCodes,
                                            statin_meds = statinMedCodes,
                                            diabetes = prevdiabetes,
                                            bmi = BMI,
                                            egfr_mlminm2 = gfrCysCrea,
                                            equation_version = "Khan_2023",
                                            prevent_type = "base",
                                            override_boundary_errors = TRUE),

      chd_risk_30yr = predict_30yr_chd_risk(age_years = age,
                                            sex = sex,
                                            smoke_current = currentSmoke,
                                            chol_total_mgdl = totalChol,
                                            chol_hdl_mgdl = HDL,
                                            bp_sys_mmhg = SBP,
                                            bp_meds = HtnMedCodes,
                                            statin_meds = statinMedCodes,
                                            diabetes = prevdiabetes,
                                            bmi = BMI,
                                            egfr_mlminm2 = gfrCysCrea,
                                            equation_version = "Khan_2023",
                                            prevent_type = "base",
                                            override_boundary_errors = TRUE)



    ) %>%
    mutate(
      race = case_when(
        black      == 'yes' ~ "black",
        white      == 'yes' ~ "white",
        hispanic   == 'yes' ~ "hispanic",
        southAsian == 'yes' ~ "south_asian",
        eastAsian  == 'yes' ~ "east_asian"
      ),
      .keep = 'unused'
    )

}
