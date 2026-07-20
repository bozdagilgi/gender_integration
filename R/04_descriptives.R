###Descriptives



df <- combined_RA_adult_ind %>%
  mutate(
    gender = as_factor(HH_02_RA)   # keeps value labels (e.g., Male/Female)
  )


num_vars <- df %>% select(where(is.numeric)) %>% names()

num_desc_gender <- df %>%
  group_by(gender) %>%
  summarise(across(
    all_of(num_vars),
    list(
      n = ~sum(!is.na(.)),
      mean = ~mean(., na.rm = TRUE),
      sd = ~sd(., na.rm = TRUE),
      min = ~min(., na.rm = TRUE),
      max = ~max(., na.rm = TRUE)
    ),
    .names = "{.col}__{.fn}"
  ), .groups = "drop")

num_desc_gender


cat_vars <- df %>%
  select(where(~is.factor(.) || is.character(.) || inherits(., "haven_labelled"))) %>%
  names()

cat_desc_gender <- map_dfr(cat_vars, function(v){
  tmp <- df %>%
    mutate(cat = if (inherits(.data[[v]], "haven_labelled")) as_factor(.data[[v]]) else as.factor(.data[[v]])) %>%
    count(gender, cat, name = "n") %>%
    group_by(gender) %>%
    mutate(pct = round(100 * n / sum(n), 2)) %>%
    ungroup() %>%
    mutate(variable = v) %>%
    select(variable, gender, category = cat, n, pct)
  tmp
})


write.csv(num_desc_gender, "R/descriptive_numeric_by_gender.csv", row.names = FALSE)
write.csv(cat_desc_gender, "R/descriptive_categorical_by_gender.csv", row.names = FALSE)
cat_desc_gender