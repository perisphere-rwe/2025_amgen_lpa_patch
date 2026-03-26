
tidy_nri <- function(x, model_label){
  as_tibble(x$nri, rownames = 'stat') %>%
    filter(str_detect(stat, 'NRI')) %>%
    select(stat,
           est = Estimate,
           lwr = Lower,
           upr = Upper) %>%
    mutate(stat = recode(stat,
                         'NRI' = 'nri_overall',
                         "NRI+" = 'nri_plus',
                         "NRI-" = "nri_minus"),
           model = model_label, .before = 1)
}
