
data_select <- function(data){

  data %>%
    select(
      overall,
      study,
      # outcomes
      lpa_nmol,
      lpa_mgdl,
      lpa_nmol_log,
      lpa_mgdl_log,
      lpa_nmol_catg,
      lpa_mgdl_catg,
      # outcomes
      status_ascvd,
      time_ascvd,
      status_cvd,
      time_cvd,
      status_chd,
      time_chd,
      cvd_risk_10yr,
      cvd_risk_30yr,
      chd_risk_10yr,
      chd_risk_30yr,
      ascvd_risk_10yr,
      ascvd_risk_30yr,
      # covariates
      age,
      sex,
      race,
      bmi = BMI,
      smoke_current = currentSmoke,
      etoh_current = currentEtoh,
      hx_stroke = prevstroke,
      hx_chd = prevchd,
      hx_hf = prevhf,
      waist_hip_ratio = waistHipRatio,
      diabetes = prevdiabetes,
      htn = prevHtn,
      chol_total = totalChol,
      chol_hdl = HDL,
      chol_ldl = LDL,
      med_chol = cholMeds,
      egfr_2021 = gfrCysCrea,
      egfr_lt60,
      sbp = SBP,
      dbp = DBP,
      uacr,
      med_statin = statinMedCodes,
      med_htn = HtnMedCodes,
      trigs,
    )

}
