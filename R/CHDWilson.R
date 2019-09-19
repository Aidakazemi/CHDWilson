#' CHD prediction in middle-aged white population using TC and LDL-C categories
#'
#' @param age number
#' @param sex binary
#' @param TC  Total cholestrol categorical
#' @param HDL-C HDL cholesterol categorical
#' @param bloodP categorical
#' @param systolic without regard to the usr of antihypertensive medication
#' @param diastolic without regard to the usr of antihypertensive medication
#' @param diabetes binary
#' @param smoker binary during the past 12 month
#'
#' @return
#' @example
#' @source

CHD <- function (age, sex, TC, LDL, HDL, systolic, diastolic, diabetes, smoker) {


  #categories of blood pressure systolic

  if (systolic < 120) {
    #Normal blood pressure
    BP_s = 1
  } else if (systolic >= 120 & systolic < 130) {
    #normal blood pressure (including optimal)
    BP_s = 2
  } else if (systolic >= 130 & systolic < 140) {
    #High normal blood pressure
    BP_s = 3
  } else if (systolic >= 140 & systolic < 160) {
    #Hypertension stage I blood pressure
    BP_s = 4
  } else if (systolic >= 160) {
    #Hypertension stage II-IV
    BP_s = 5
  }

  #categories of blood pressure diastolic

  if (diastolic < 80) {
    #Normal blood pressure
    BP_d = 1
  } else if (diastolic >= 80 & diastolic < 85) {
    #normal blood pressure (including optimal)
    BP_d = 2
  } else if (diastolic >= 85 & diastolic < 90) {
    #High normal blood pressure
    BP_d = 3
  } else if (diastolic >= 90 & diastolic < 100) {
    #Hypertension stage I blood pressure
    BP_d = 4
  } else if (diastolic >= 100) {
    #Hypertension stage II-IV
    BP_d = 5
  }


  if (BP_s == BP_d) {
    BP = BP_s
  } else if (BP_s < BP_d) {
    BP = BP_d
  } else if (BP_s > BP_d) {
    BP = BP_s
  }




 #================================================================ Men =======================================================================
  if (sex == 1) {
 #TCl coefficient in range using TC categories
  if (TC < 160) {
    TC_tccoef = -0.65945
  } else if (TC >= 160 & TC <= 199) {
    TC_tccoef = 0
  } else if (TC >= 200 & TC <= 239) {
    TC_tccoef = 0.17692
  } else if (TC >= 240 & TC <= 279) {
    TC_tccoef = 0.50539
  } else if (TC >= 280) {
    TC_tccoef = 0.65713
  }

  #LDL coefficient in range using LDL categories
  if (LDL < 100) {
    LDL_ldlcoef = -0.69281
  } else if (LDL >= 100 & LDL <= 129) {
    LDL_ldlcoef = 0
  } else if (LDL >= 130 & LDL <= 159) {
    LDL_ldlcoef = 0.00389
  } else if (LDL >= 160 & LDL <= 189) {
    LDL_ldlcoef = 0.26755
  } else if (LDL >= 190) {
    LDL_ldlcoef = 0.56705
  }

  #HDL-C coefficient in range using TC categories
  if (HDL < 35) {
    HDL_tccoef = 0.49744
  } else if (HDL >= 35 & HDL <= 44) {
    HDL_tccoef =  0.24310
  } else if (HDL >= 45 & HDL <= 49) {
    HDL_tccoef = 0
  } else if (HDL >= 50 & HDL <= 59) {
    HDL_tccoef = -0.05107
  } else if (HDL >= 60) {
    HDL_tccoef = -0.48660
  }

    #HDL-C coefficient in range using LDL categories
    if (HDL < 35) {
      HDL_ldlcoef = 0.48598
    } else if (HDL >= 35 & HDL <= 44) {
      HDL_ldlcoef =  0.21643
    } else if (HDL >= 45 & HDL <= 49) {
      HDL_ldlcoef = 0
    } else if (HDL >= 50 & HDL <= 59) {
      HDL_ldlcoef = -0.04710
    } else if (HDL >= 60) {
      HDL_ldlcoef = -0.34190
    }



  #systolic coefficient in range using TC categories
   if (BP == 1) {
     BP_tccoef = -0.00226
   } else if (BP == 2) {
     v = 0
   } else if (BP == 3) {
     BP_tccoef = 0.28320
   } else if (BP == 4) {
     BP_tccoef = 0.52168
   } else if (BP == 5) {
     BP_tccoef = 0.61859
   }

    #systolic coefficient in range using LDL categories
    if (BP == 1) {
      BP_ldlcoef = -0.02642
    } else if (BP == 2) {
      BP_ldlcoef = 0
    } else if (BP == 3) {
      BP_ldlcoef = 0.30104
    } else if (BP == 4) {
      BP_ldlcoef = 0.55714
    } else if (BP == 5) {
      BP_ldlcoef = 0.65107
    }



  #============================================================= Women ====================================================================
  } else if (sex == 0) {
  #TCl coefficient in range using TC categories
  if (TC < 160) {
    TC_tccoef = -0.26138
  } else if (TC >= 160 & TC <= 199) {
    TC_tccoef = 0
  } else if (TC >= 200 & TC <= 239) {
    TC_tccoef = 0.20771
  } else if (TC >= 240 & TC <= 279) {
    TC_tccoef = 0.24385
  } else if (TC >= 280) {
    TC_tccoef = 0.53513
  }

    #LDL coefficient in range using LDL categories
    if (LDL < 100) {
      LDL_ldlcoef = -0.42616
    } else if (LDL >= 100 & LDL <= 129) {
      LDL_ldlcoef = 0
    } else if (LDL >= 130 & LDL <= 159) {
      LDL_ldlcoef = 0.01366
    } else if (LDL >= 160 & LDL <= 189) {
      LDL_ldlcoef = 0.26948
    } else if (LDL >= 190) {
      LDL_ldlcoef = 0.33251
    }

  #HDL-C coefficient in range using TC categories
  if (HDL < 35) {
    HDL_tccoef = 0.84312
  } else if (HDL >= 35 & HDL <= 44) {
    HDL_tccoef =  0.37796
  } else if (HDL >= 45 & HDL <= 49) {
    HDL_tccoef = 0.19785
  } else if (HDL >= 50 & HDL <= 59) {
    HDL_tccoef = 0
  } else if (HDL >= 60) {
    HDL_tccoef = -0.42951
  }


    #HDL-C coefficient in range using LDL categories
    if (HDL < 35) {
      HDL_ldlcoef = 0.88121
    } else if (HDL >= 35 & HDL <= 44) {
      HDL_ldlcoef =  0.36312
    } else if (HDL >= 45 & HDL <= 49) {
      HDL_ldlcoef = 0.19247
    } else if (HDL >= 50 & HDL <= 59) {
      HDL_ldlcoef = 0
    } else if (HDL >= 60) {
      HDL_ldlcoef = -0.35404
    }



    #systolic coefficient in range using TC categories
    if (BP == 1) {
      BP_tccoef = -0.53363
    } else if (BP == 2) {
      systolic_tccoef = 0
    } else if (BP == 3) {
      systolic_tccoef = 0.06773
    } else if (BP == 4) {
      systolic_tccoef = 0.26288
    } else if (BP == 5) {
      systolic_tccoef = 0.46573
    }

    #systolic coefficient in range using LDL categories
    if (BP == 1) {
      BP_ldlcoef = -0.51204
    } else if (BP == 2) {
      BP_ldlcoef = 0
    } else if (BP == 3) {
      BP_ldlcoef = -0.03484
    } else if (BP == 4) {
      BP_ldlcoef = 0.28533
    } else if (BP == 5) {
      BP_ldlcoef = 0.50403
    }


 }
    #================================================== Prediction using TC =========================================================
  L_tc_men <- 0.04826 * age + TC_tccoef + HDL_tccoef + BP_tccoef + 0.42839 * diabetes + 0.52337 * smoker

  #values of the means
  G_tc_men <- 3.0975

  A_tc_men <- L_tc_men - G_tc_men

  B_tc_men <- exp(A_tc_men)

  #Baseline survival function at 10 years for men:
  s_tc_men <- 0.90015

  P_tc_men <- round((1 - (s_tc_men) ^ B_tc_men) * 100, digits = 0)



  L_tc_women <- 0.33766 * age + (-0.00268) * age ^ 2 + TC_tccoef + HDL_tccoef + BP_tccoef + 0.59626 * diabetes + 0.29246 * smoker

  #values of the means
  G_tc_women <- 9.92545

  A_tc_women <- L_tc_women - G_tc_women

  B_tc_women <- exp(A_tc_women)

  #Baseline survival function at 10 years for women:
  s_tc_women <- 0.96246

  P_tc_women <- round((1 - (s_tc_women) ^ B_tc_women)* 100, digits = 0)

  #================================================== Prediction using LDL =========================================================
  L_ldl_men <- 0.04808 * age + LDL_ldlcoef + HDL_ldlcoef + BP_ldlcoef + 0.42146 * diabetes + 0.54377 * smoker

  #values of the means
  G_ldl_men <- 3.00069

  A_ldl_men <- L_ldl_men - G_ldl_men

  B_ldl_men <- exp(A_ldl_men)

  #Baseline survival function at 10 years for men:
  s_ldl_men <- 0.90017

  P_ldl_men <- round((1 - (s_ldl_men) ^ B_ldl_men) * 100, digits = 0)



  L_ldl_women <- 0.33994 * age + (-0.0027) * age ^ 2 + LDL_ldlcoef + HDL_ldlcoef + BP_ldlcoef + 0.61313 * diabetes + 0.29737 * smoker

  #values of the means
  G_ldl_women <- 9.914136

  A_ldl_women <- L_ldl_women - G_ldl_women

  B_ldl_women <- exp(A_ldl_women)

  #Baseline survival function at 10 years for women:
  s_ldl_women <- 0.96278

  P_ldl_women <- round((1 - (s_ldl_women) ^ B_ldl_women)* 100, digits = 0)




  results <- list()

  if (sex == 1) {
    results$ tc_CHDprediction <- P_tc_men
    results$ldl_CHDprediction <- P_ldl_men
    return (results)

  } else if (sex == 0) {
    results$ tc_CHDprediction <- P_tc_women
    results$ldl_CHDprediction <- P_ldl_women
    return (results)

  }

}

