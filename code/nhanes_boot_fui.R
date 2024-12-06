nhanes_boot_fui = function(seed, df) {
  # df = nhanes_df
  # seed = 1
  set.seed(seed)
  # sample w replacement same dataframe
  bs_df =
    df %>%
    sample_frac(size = 1, replace = TRUE) %>%
    select(SEQN, age, gender, AC_tf)

  X_des =
    model.matrix(
      SEQN ~ gender + age,
      data = bs_df
    )

  Hmat = solve(t(X_des) %*% X_des) %*% t(X_des)  # (X'X)^{-1}X'

  min_regressions =
    bs_df %>%
    tf_unnest(AC_tf) %>%
    rename(epoch = AC_tf_arg, AC = AC_tf_value) %>%
    select(epoch, AC) %>%
    nest(data = -epoch) %>%
    mutate(
      data = map(data, as.matrix),
      coef = map(.x = data, ~ Hmat %*% .x)) %>%
    select(epoch, coef) %>%
    unnest(coef)
  # perform the the regression at each epoch and get the coefficient estimates

  min_regressions$term = rep(c("(Intercept)", "genderFemale", "age"), 1440)

  smooth_coefs =
    min_regressions %>%
    select(epoch, term, coef) %>%
    tf_nest(coef, .id = term, .arg = epoch) %>%
    mutate(smooth_coef = tf_smooth(coef)) %>%
    select(term, smooth_coef)

}
