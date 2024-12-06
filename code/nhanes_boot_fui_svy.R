nhanes_boot_fui_svy = function(seed, df) {
  set.seed(seed)
  # sample w replacement same dataframe

  min_regressions =
    df %>%
    select(SEQN, age, gender, contains("masked"), contains("wt"), AC_tf) %>%
    tf_unnest(AC_tf) %>%
    rename(epoch = AC_tf_arg, AC = AC_tf_value) %>%
    nest(data = -epoch) %>%
    mutate(
      model = map(.x = data,
                  .f = function(x){
                    des = survey::svydesign(
                      id = ~ masked_variance_pseudo_psu,
                      strata = ~ masked_variance_pseudo_stratum,
                      weights = ~ wtmec4yr_adj_norm,
                      data = x,
                      nest = TRUE
                    )
                    bootstrap_rep_design = as_bootstrap_design(des,
                                                               type = "Rao-Wu-Yue-Beaumont",
                                                               replicates = 1)
                    model = svyglm(AC ~ age + gender, family = gaussian(), design = bootstrap_rep_design,
                                   return.replicates = TRUE)
                    r = model$replicates
                    r %>% t() %>% as.data.frame() %>% mutate(term = names(model$coefficients)) %>% rename(coef = V1)})
    ) %>%
    select(epoch, model) %>%
    unnest(model)

  # perform the the regression at each epoch and get the coefficient estimates

  smooth_coefs =
    min_regressions %>%
    select(epoch, term, coef) %>%
    tf_nest(coef, .id = term, .arg = epoch) %>%
    mutate(smooth_coef = tf_smooth(coef)) %>%
    select(term, smooth_coef)

}
