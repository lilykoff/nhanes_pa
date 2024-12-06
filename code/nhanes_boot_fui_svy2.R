nhanes_boot_fui_svy2 = function(seed, df, n_boot = 500) {
  set.seed(seed)
  # sample w replacement same dataframe
  # df = nhanes_df
  # x = df %>%
  #   # slice(1:200) %>%
  #   select(SEQN, age, gender, contains("masked"), contains("wt"), AC_tf) %>%
  #   tf_unnest(AC_tf) %>%
  #   rename(epoch = AC_tf_arg, AC = AC_tf_value) %>%
  #   nest(data = -epoch) %>%
  #   slice(1) %>% select(data) %>%
  #   unnest(cols = c(data))
  des = survey::svydesign(
    id = ~ masked_variance_pseudo_psu,
    strata = ~ masked_variance_pseudo_stratum,
    weights = ~ wtmec4yr_adj_norm,
    data = df,
    nest = TRUE
  )

  bootstrap_rep_design = as_bootstrap_design(des,
                                             # samp_method_by_stage = c("PPSWOR", "PPSWOR"),
                                             type = "Rao-Wu-Yue-Beaumont",
                                             replicates = n_boot)

  model = svyglm(AC ~ age + gender, family = gaussian(), design = bootstrap_rep_design,
                 return.replicates = TRUE)
  r = model$replicates
  colnames(r) = names(model$coefficients)
  res =
    r %>%
    as.data.frame() %>%
    mutate(boot = row_number()) %>%
    pivot_longer(cols = -boot) %>%
    rename(coef = value,
           term = name)
}
