reweight_accel = function(data,
                          return_unadjusted_wts=FALSE,
                          age_bks = c(0, 18, 30, 40, 50, 60, 70, 80, 85),
                          right=FALSE,
                          demo){
  stopifnot(all(c("SEQN","data_release_cycle","full_sample_2_year_mec_exam_weight","full_sample_2_year_interview_weight") %in% colnames(data)))
  stopifnot(all(data$data_release_cycle %in% c(7,8)))
  if(any(duplicated(data$SEQN))) stop("Data must be in the form of one row per participant")

  ret <- data

  vars_wts <- c("full_sample_2_year_interview_weight_unadj", "full_sample_2_year_mec_exam_weight_unadj",
                "full_sample_2_year_interview_weight_unadj_norm","full_sample_2_year_mec_exam_weight_unadj_norm",
                "wtint4yr_unadj", "wtint4yr_unadj_norm",
                "wtmec4yr_unadj", "wtmec4yr_unadj_norm",
                "full_sample_2_year_interview_weight_adj", "full_sample_2_year_interview_weight_adj_norm",
                "wtint4yr_adj", "wtint4yr_adj_norm",
                "wtmec4yr_adj", "wtmec4yr_adj_norm")

  if(any(vars_wts %in% colnames(data))){
    warning(paste0("Variables:",  paste0(vars_wts[vars_wts %in% colnames(data)],collapse=", ") ," found in data. These have been overwritten."))
  }

  for(i in vars_wts) ret[[i]] <- NULL
  rm(list=c("vars_wts","i"))


  uwave     <- sort(unique(ret$data_release_cycle))
  n_age_bks <- length(age_bks)


  if(return_unadjusted_wts){
    ret$full_sample_2_year_interview_weight_unadj <- ret$full_sample_2_year_interview_weight
    ret$full_sample_2_year_mec_exam_weight_unadj <- ret$full_sample_2_year_mec_exam_weight

    ret$full_sample_2_year_interview_weight_unadj_norm <- ret$full_sample_2_year_interview_weight/mean(ret$full_sample_2_year_interview_weight)
    ret$full_sample_2_year_mec_exam_weight_unadj_norm <- ret$full_sample_2_year_mec_exam_weight/mean(ret$full_sample_2_year_mec_exam_weight)

    ## calculate raw/normalized unadjusted 4-year weights
    if(length(uwave) > 1){
      ret$wtint4yr_unadj      <- ret$full_sample_2_year_interview_weight/2
      ret$wtint4yr_unadj_norm <- ret$wtint4yr_unadj/mean(ret$wtint4yr_unadj)

      ret$wtmec4yr_unadj      <- ret$full_sample_2_year_mec_exam_weight/2
      ret$wtmec4yr_unadj_norm <- ret$wtmec4yr_unadj/mean(ret$wtmec4yr_unadj)
    }
  }



  # data(list=c("Covariate_C","Covariate_D"), envir=environment(), package="rnhanesdata")
  # demo <- rbind(Covariate_C, Covariate_D)

  ## create age categories, 85+ are coded as missing so impute a value >= 85

  demo =
    demo %>%
    mutate(age_mn = if_else(!is.na(age_in_months_at_screening_0_to_24_mos),
                        age_in_months_at_screening_0_to_24_mos / 12,
                        age_in_years_at_screening))

  demo$age_cat_mn <- demo$age_cat_ex <- cut(demo$age_mn, breaks=age_bks, right=right)


  demo$Race2 <- factor(demo$race_hispanic_origin, levels=c("Mexican American", "Other Hispanic","Non-Hispanic White","Non-Hispanic Black","Other Race - Including Multi-Rac"),
                       labels=c("Mexican American", "Other", "Other","Black","Other"))

  full_sample_2_year_mec_exam_weight_adj <- full_sample_2_year_interview_weight_adj <- rep(NA, nrow(ret))
  for(i in seq_along(uwave)){
    for(j in levels(demo$gender)){
      for(k in levels(demo$Race2)){
        for(l in levels(demo$age_cat_mn)){
          inx_int_full <- which(demo$gender == j & demo$Race2 == k & demo$age_cat_mn == l & demo$data_release_cycle==uwave[i])
          inx_mec_full <- which(demo$gender == j & demo$Race2 == k & demo$age_cat_ex == l & demo$data_release_cycle==uwave[i])
          seqn_int     <- demo$SEQN[which(demo$gender == j & demo$Race2 == k & demo$age_cat_mn == l & demo$data_release_cycle==uwave[i])]
          seqn_mec     <- demo$SEQN[which(demo$gender == j & demo$Race2 == k & demo$age_cat_ex == l & demo$data_release_cycle==uwave[i])]

          inx_ret_int <- which(ret$SEQN %in% seqn_int)
          inx_ret_mec <- which(ret$SEQN %in% seqn_mec)


          if(length(inx_ret_int) > 0){
            wt_int_full <- sum(demo$full_sample_2_year_interview_weight[inx_int_full])
            wt_int_ret  <- sum(ret$full_sample_2_year_interview_weight[inx_ret_int])

            full_sample_2_year_interview_weight_adj[inx_ret_int] <- ret$full_sample_2_year_interview_weight[inx_ret_int]*wt_int_full/wt_int_ret
          }

          if(length(inx_ret_mec) > 0){
            wt_mec_full <- sum(demo$full_sample_2_year_mec_exam_weight[inx_mec_full])
            wt_mec_ret  <- sum(ret$full_sample_2_year_mec_exam_weight[inx_ret_mec])

            full_sample_2_year_mec_exam_weight_adj[inx_ret_mec] <- ret$full_sample_2_year_mec_exam_weight[inx_ret_mec]*wt_mec_full/wt_mec_ret
          }
        }
      }
    }

  }



  ret$full_sample_2_year_interview_weight_adj      <- full_sample_2_year_interview_weight_adj
  ret$full_sample_2_year_interview_weight_adj_norm <- ret$full_sample_2_year_interview_weight_adj/mean(ret$full_sample_2_year_interview_weight_adj)

  ret$full_sample_2_year_mec_exam_weight_adj      <- full_sample_2_year_mec_exam_weight_adj
  ret$full_sample_2_year_mec_exam_weight_adj_norm <- ret$full_sample_2_year_mec_exam_weight_adj/mean(ret$full_sample_2_year_mec_exam_weight_adj)

  if(length(uwave) > 1){
    ret$wtint4yr_adj      <- ret$full_sample_2_year_interview_weight_adj/2
    ret$wtint4yr_adj_norm <- ret$wtint4yr_adj/mean(ret$wtint4yr_adj)

    ret$wtmec4yr_adj      <- ret$full_sample_2_year_mec_exam_weight_adj/2
    ret$wtmec4yr_adj_norm <- ret$wtmec4yr_adj/mean(ret$wtmec4yr_adj)
  }

  ret
}


