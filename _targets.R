source("packages.R")
source("conflicts.R")

# Load your R files
tar_source()

# Allow crew package to use 3 parallel workers
tar_option_set(
  controller = crew_controller_local(workers = 10),
  deployment = 'main'
)

results_version_major <- 1
results_version_minor <- 1

if(!dir.exists(glue("doc/results-v{results_version_major}"))){
  dir.create(glue("doc/results-v{results_version_major}"))
}

analysis_guide <- expand_grid(
  .lpa_defn = c('lpa_nmol', 'lpa_mgdl'),
  .lpa_trafo = c('log_trafo', 'no_trafo', 'catg_trafo'),
  .risk_horizon = c(10, 27.5),
  .risk_type = c("ascvd", "cvd", "chd")
) %>%
  filter(.lpa_trafo %in% c('no_trafo', 'catg_trafo'),
         .lpa_defn == 'lpa_nmol',
         .risk_type %in% c("ascvd", "cvd"))

tar_plan(

  # data_aric ----
  data_aric = load_aric(),
  # data_mesa ----
  data_mesa = load_mesa(),
  # data_cardia ----
  data_cardia = read_rds('data/data_cardia.rds') %>%
    mutate(overall = 'overall',
           study = sample(x = c("cardia_train", "cardia_test"),
                          size = n(),
                          replace = TRUE)),


  subgroups = c("overall", "sex"),

  # data_excluded_30yr_risk ----
  tar_target(data_excluded_30yr_risk, command = {

    bind_rows(data_aric, data_mesa, data_cardia) %>%
      data_exclude(age_range = c(30, 60),
                   exclusion_fname = '30yr') %>%
      mutate(risk_horizon = max(analysis_guide$.risk_horizon),
             .after = study)

  }),

  # data_excluded_10yr_risk ----
  tar_target(data_excluded_10yr_risk, command = {

    bind_rows(data_aric, data_mesa, data_cardia) %>%
      data_exclude(age_range = c(30, 80),
                   exclusion_fname = '10yr') %>%
      mutate(risk_horizon = min(analysis_guide$.risk_horizon),
             .after = study)

  }),

  # data_train ----
  tar_target(data_train, command = {

    list(data_excluded_10yr_risk,
         data_excluded_30yr_risk) %>%
      map_dfr(filter, study %in% c('mesa', 'cardia_train'))

  }),

  # data_test ----
  tar_target(data_test, command = {

    list(data_excluded_30yr_risk,
         data_excluded_10yr_risk) %>%
      map_dfr(filter, study %in% c('aric', 'cardia_test'))

  }),


  # meta ----
  meta = data_train %>%
    mutate(study = factor(study, levels = c('aric',
                                            'mesa',
                                            'cardia_train',
                                            'cardia_test'))) %>%
    as_data_dictionary() %>%
    bind_dictionary(
      data_dictionary(
        numeric_variable(name = '.resid',
                         label = "Observed minus expected Lp(a)",
                         units = 'nmol/L',
                         divby_modeling = 25)
      ),
      drop_unmatched_variables = FALSE
    ) %>%
    data_document(),

  tar_target(name = fig_lpa_spline, command = {

    data <- bind_rows(train = data_train, test = data_test,
                      .id = 'data_type')

    result <- data %>%
      group_by(data_type, risk_horizon) %>%
      group_map(
        .f = ~ {

          mdl <- coxph(
            formula = Surv(time_cvd, status_cvd) ~ bs(lpa_nmol),
            x = TRUE,
            data = .x
          )

          grid <-
            tibble(lpa_nmol = seq(1, 300, length.out = 100))

          grid$pred <- as.numeric(
            predictRisk(mdl, newdata = grid, times = 10)
          )

          expand_grid(grid, .y)

        }
      )

    bind_rows(result)

  }),


  # analysis_lpa_nmol_log_trafo_30yr_chd

  # .risk_horizon = 27.5
  # .lpa_defn = 'lpa_nmol'
  # .risk_type = 'ascvd'
  # .lpa_trafo = 'catg_trafo'

  .analyses <- tar_map(

    values = analysis_guide,

    names = c(.lpa_defn, .lpa_trafo, .risk_horizon, .risk_type),

    # analysis ----
    tar_target(analysis, command = {

      set_default_dictionary(meta)

      # use linear regression to estimate the expected value of Lp(a) in molar
      # concentration based on the components of the PREVENT equations including
      # age, sex, smoking, body mass index, systolic blood pressure,
      # antihypertensive medication use, diabetes, total and HDL cholesterol,
      # statin use, and estimated glomerular filtration rate

      data_train_horizon <- data_train %>%
        filter(risk_horizon == .risk_horizon) %>%
        translate_data(apply_category_labels = FALSE,
                       apply_variable_labels = FALSE,
                       units = 'model') %>%
        mutate(across(ends_with("catg"), as.numeric))


      data_test_horizon <- data_test %>%
        filter(risk_horizon == .risk_horizon)  %>%
        translate_data(apply_category_labels = FALSE,
                       apply_variable_labels = FALSE,
                       units = 'model') %>%
        mutate(across(ends_with("catg"), as.numeric))


      if(.lpa_trafo == 'log_trafo'){
        lpa_var <- paste(.lpa_defn, "log", sep = '_')
      } else if (.lpa_trafo == 'catg_trafo') {
        lpa_var <- paste(.lpa_defn, 'catg', sep = '_')
      } else {
        lpa_var <- .lpa_defn
      }

      risk_var <- paste(.risk_type, "risk",
                        ifelse(.risk_horizon==10, '10yr', '30yr'),
                        sep = '_')

      # lp(a) patch

      control_vars <- c("age",
                        "sex",
                        "smoke_current",
                        "sbp",
                        "med_htn",
                        "diabetes",
                        "chol_total",
                        "chol_hdl",
                        "med_statin",
                        "egfr_lt60") %>%
        paste(collapse = ' + ')

      formula_patch_lm <- as.formula(
        glue("{lpa_var} ~ {control_vars}")
      )

      fit_lm <- lm(formula = formula_patch_lm, data = data_train_horizon)

      # I want lp(a) to be presented per divby unit increase in the
      # survival model output, but I want it on the original scale
      # in the linear model output.
      mult_by <- meta$variables[[lpa_var]]$divby_modeling %||% 1

      fit_lm_out <- tidy(fit_lm) %>%
        mutate(estimate = estimate * mult_by,
               std.error = std.error * mult_by,
               conf.low = estimate - 1.96 * std.error,
               conf.high = estimate + 1.96 * std.error) %>%
        append_term_key() %>%
        index_terms()

      # adds predicted values and residuals to data
      data_train_horizon <- fit_lm %>%
        augment(data = data_train_horizon)

      fit_lm_fig <- ggplot(data_train_horizon) +
        aes(sample = .resid) +
        stat_qq() +
        stat_qq_line()

      surv_outcome_vars <- c("time", "status") %>%
        paste(.risk_type, sep = '_') %>%
        set_names(c("time", "status"))

      surv_outcome <- surv_outcome_vars %>%
        paste(collapse = ', ') %>%
        paste("Surv(", ., ")", sep = '')

      formula_patch <- as.formula(
        glue("{surv_outcome} ~ .resid + {control_vars}")
      )

      fit_cox <- coxph(formula = formula_patch, data = data_train_horizon)

      fit_cox_out <- tidy(fit_cox,
                          conf.int = TRUE,
                          exponentiate = TRUE) %>%
        append_term_key() %>%
        index_terms()

      beta <- as.numeric(coef(fit_cox)['.resid'])

      data_predictions <- data_test_horizon %>%
        # adds column called .resid, which is observed - fitted lp(a)
        augment(newdata = ., x = fit_lm) %>%
        as.data.table() %>%
        setnames(old = risk_var,
                 new = 'prevent') %>%
        .[, let(patch = prevent * exp(beta * .resid),
                enhance = pmin(prevent * 1.11 ^ (lpa_nmol/50), 1))] %>%
        select(all_of(subgroups),
               all_of(surv_outcome_vars),
               prevent, patch, enhance) %>%
        as_tibble()

      tibble(.risk_horizon = .risk_horizon,
             .risk_type = .risk_type,
             .lpa_defn = .lpa_defn,
             .lpa_trafo = .lpa_trafo,
             fit_lm = list(fit_lm),
             fit_lm_fig = list(fit_lm_fig),
             fit_lm_tidy = list(fit_lm_out),
             fit_cox = list(fit_cox),
             fit_cox_tidy = list(fit_cox_out),
             predictions = list(data_predictions))


    }, deployment = 'main'),

    # evaluation ----
    tar_target(evaluation, pattern = map(subgroups), command = {

      analysis %>%
        select(-starts_with("fit")) %>%
        unnest(predictions) %>%
        select(starts_with("."),
               prevent, patch, enhance, time, status,
               all_of(c(group_level = subgroups))) %>%
        mutate(group_variable = subgroups, .before = group_level) %>%
        relocate(starts_with("group"), .before = 1) %>%
        nest(predictions = c(prevent, patch, enhance, time, status)) %>%
        mutate(evaluation = map2(.x = predictions,
                                 .y = .risk_horizon,
                                 .f = evaluate_predictions))

    }, deployment = 'main')


  ),

  tar_combine(analyses, .analyses[[1]]),
  tar_combine(evaluations, .analyses[[2]]),

  # document targets ----

  tar_render(
    doc_results,
    path = here::here("doc/results.Rmd"),
    output_file = paste0("results", "-v", results_version_major, "/",
                         "results-", basename(here()),
                         "-v", results_version_major,
                         "-",  results_version_minor,
                         ".docx")
  )

) %>%
  tar_hook_before(
    hook = {source("conflicts.R")},
    names = everything()
  )

