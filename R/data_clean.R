

find_01_vars <- function(data){

  data %>%
    purrr::map_lgl(~is.numeric(.x) && length(unique(na.omit(.x))) == 2) %>%
    which() %>%
    names() %>%
    setdiff("miChdProcEventPrimary")

}

find_NY_vars <- function(data){

  data %>%
    purrr::map_lgl(
      ~ all(unique(na.omit(.x)) %in% c("N", "Y")) &&
        length(unique(na.omit(.x))) == 2
    ) %>%
    which() %>%
    names()

}

blanks_to_na <- function(x){
  x[trimws(x) == ""] <- NA
  x
}

data_clean <- function(data){

  data %>%

    # set missing values of race categories to 0
    # (missing values don't really indicate missing for these)
    mutate(across(c(black, white, hispanic, southAsian, eastAsian),
                  ~if_else(is.na(.x), 0, .x))) %>%

    # some of these are all 0 and won't be edited by find_01_vars
    mutate(across(c(black, white, hispanic, southAsian, eastAsian,
                    prevchd, prevstroke),
                  ~ factor(.x,
                           levels = c(0, 1),
                           labels = c("no", "yes")))) %>%

    # catch blanks now to prep for next step
    mutate(across(c(where(is.character), where(is.factor)),
                  blanks_to_na)) %>%

    mutate(

      sex = factor(sex,
                   levels = c("M", "F"),
                   labels = c("male", "female")),

      postMeno = if_else(sex == "male", 0, postMeno),

      across(
        .cols = c(all_of(find_01_vars(.)), -starts_with("status")),
        ~ factor(.x,
                 levels = c(0, 1),
                 labels = c("no", "yes"))
      ),

      across(
        all_of(find_NY_vars(.)),
        ~ factor(.x,
                 levels = c("N", "Y"),
                 labels = c("no", "yes"))
      ),

      across(where(is.character), as.factor),

      # convert from days to years
      across(starts_with("time_"), ~.x / 365.25),

      SBP = as.numeric(SBP),
      DBP = as.numeric(DBP)
    )

}



