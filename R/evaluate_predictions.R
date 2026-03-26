
evaluate_predictions <- function(data, risk_horizon){

  prediction_mats <- data %>%
    select(prevent, patch, enhance) %>%
    map(~matrix(.x, ncol = 1))

  risk_threshold = case_when(risk_horizon == 10 ~ 0.10,
                             risk_horizon > 10 ~ 0.20)

  sc <- Score(prediction_mats,
              formula = Surv(time, status) ~ 1,
              data = data,
              times = risk_horizon,
              summary = c('IPA'),
              plots = 'Calibration')

  # uses Harrell's C (as in protocol)
  auc <- list(
    prevent = concordance(object = Surv(time, status) ~ I(1-prevent), data = data),
    patch = concordance(object = Surv(time, status) ~ I(1-patch), data = data),
    enhance = concordance(object = Surv(time, status) ~ I(1-enhance), data = data)
  ) %>%
    map_dfr(~tibble(est = as.numeric(.x['concordance']),
                    se = sqrt(as.numeric(.x['var']))),
            .id = 'model') %>%
    mutate(lwr = est - 1.96 * se,
           upr = est + 1.96 * se) %>%
    select(-se) %>%
    mutate(stat = 'auc', .after = 'model')

  auc_contrasts <- bind_rows(

    patch = compareC(timeX = data$time,
                     statusX = data$status,
                     scoreZ = 1 - data$prevent,
                     scoreY = 1 - data$patch) %>%
      as.list() %>%
      as_tibble() %>%
      slice(1) %>%
      transmute(stat = 'auc_vs_prevent',
                est = est.diff_c,
                lwr = est.diff_c - 1.96 * sqrt(est.vardiff_c),
                upr = est.diff_c + 1.96 * sqrt(est.vardiff_c)),

    enhance = compareC(timeX = data$time,
                               statusX = data$status,
                               scoreZ = 1 - data$prevent,
                               scoreY = 1 - data$enhance) %>%
      as.list() %>%
      as_tibble() %>%
      slice(1) %>%
      transmute(stat = 'auc_vs_prevent',
                est = est.diff_c,
                lwr = est.diff_c - 1.96 * sqrt(est.vardiff_c),
                upr = est.diff_c + 1.96 * sqrt(est.vardiff_c)),

    .id = 'model'

  )

  # what should be used for C-stat (not in protocol)
  # auc <- sc$AUC$score %>%
  #   transmute(model, stat = 'auc',
  #             est = AUC,
  #             lwr = lower,
  #             upr = upper)
  #
  # auc_contrasts <- sc$AUC$contrasts %>%
  #   filter(reference == 'prevent') %>%
  #   transmute(model,
  #             stat = 'auc_vs_prevent',
  #             est = delta.AUC,
  #             lwr = lower,
  #             upr = upper)

  cal_data <- sc %>%
    plotCalibration(cens.method = 'local',
                    method = 'q',
                    q = 10,
                    times = risk_horizon,
                    plot = FALSE) %>%
    getElement('plotFrames') %>%
    rbindlist(idcol = 'model')

  cal_params <- cal_data %>%
    group_by(model) %>%
    group_modify(.f = ~ {

      # quantile regression
      mdl <- rq(Obs ~ Pred, data = .x)

      mdl_smry <- summary(mdl, se = 'boot')

      mdl_smry$coefficients %>%
        as_tibble() %>%
        transmute(
          stat = c("calib_int", "calib_slp"),
          est = Value,
          lwr = Value - 1.96 * `Std. Error`,
          upr = Value + 1.96 * `Std. Error`
        )
    })

  test_stats <- data %>%
    ungroup() %>%
    bootstraps(times = 25) %>%
    mutate(
      result = map(
        .x = splits,
        .f = ~ {

          test_consequences(formula = Surv(time, status) ~ prevent + patch + enhance,
                            data = training(.x),
                            statistics = c("pos_rate", "test_pos_rate",
                                           "sens", "spec", "ppv", "npv"),
                            thresholds = risk_threshold,
                            time = risk_horizon) %>%
            select(-label, -n, -threshold) %>%
            rename(model=variable)

        }
      )
    ) %>%
    unnest(result) %>%
    select(-splits, -id) %>%
    group_by(model) %>%
    summarize(
      across(everything(),
             .fns = list(
               ..est = ~median(.x, na.rm = TRUE),
               ..lwr = ~quantile(.x, probs = 0.025, na.rm = TRUE),
               ..upr = ~quantile(.x, probs = 0.975, na.rm = TRUE))
      )
    ) %>%
    pivot_longer(cols = -model) %>%
    separate(col = 'name', into = c("stat", "type"), sep = "_\\.\\.") %>%
    pivot_wider(names_from = type, values_from = value)

  nri_patch <- nricens(time = data$time,
                       event = data$status,
                       p.std = data$prevent,
                       p.new = data$patch,
                       t0 = risk_horizon,
                       cut = risk_threshold,
                       niter = 25) %>%
    tidy_nri(model_label = 'patch')

  nri_enhance <- nricens(time = data$time,
                         event = data$status,
                         p.std = data$prevent,
                         p.new = data$enhance,
                         t0 = risk_horizon,
                         cut = risk_threshold,
                         niter = 25) %>%
    tidy_nri(model_label = 'enhance')

  bind_rows(auc,
            auc_contrasts,
            cal_params,
            test_stats,
            nri_patch,
            nri_enhance) %>%
    as_tibble()


}

