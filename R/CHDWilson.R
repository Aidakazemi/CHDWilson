#' Predicting Coronary Heart Disease (CHD) over ten years using TChol and LDL categories
#'
#' @param age A number for age
#' @param gender A binary variables taking 1 for men and 0 for women
#' @param TChol  A number for Total cholestrol in mg/dL
#' @param HDL A number for HDL cholesterol  in mg/dL
#' @param LDL A number for LDL cholesterol  in mg/dL
#' @param SBP  A number for SBP Blood pressure in mm Hg without regard to the use of antihypertensive medication
#' @param DBP A number for DBP blood pressure in mm Hg without regard to the use of antihypertensive medication
#' @param diabetes A binary variable taking 1 if the participant was under treatment with insulin or oral hypoglycemic agents, if casual blood glucose determinations exceeded 150 mg/dL at two clinic visits in the original cohort, or if fasting blood glucose exceeded 140 mg/dL at the initial examination of the Offspring Study participants
#' @param smoker A binary variable taking 1 for person who smoked during the past 12 month and 0 therwise
#'
#' @examples
#'
#' predictchd (age = 55, gender = 1, TChol = 250, LDL = 120, HDL = 39,
#'             SBP = 146, DBP = 88, diabetes = 0 , smoker =1)
#' predictchd (age = 30, gender = 0, TChol = 170, LDL = 120, HDL = 39,
#'             SBP = 145, DBP = 88, diabetes = 0 , smoker =1)
#'
#' @source  \url{https://www.ahajournals.org/doi/full/10.1161/01.CIR.97.18.1837}
#' @export
predictchd <- function (age, gender, TChol, LDL, HDL, SBP, DBP, diabetes, smoker) {


  #categories of blood pressure SBP based on Joint National Committee (JNC-V) blood pressure

  if (SBP < 120) {
    #Normal blood pressure
    BP_s = 1
  } else if (SBP >= 120 & SBP < 130) {
    #normal blood pressure (including optimal)
    BP_s = 2
  } else if (SBP >= 130 & SBP < 140) {
    #High normal blood pressure
    BP_s = 3
  } else if (SBP >= 140 & SBP < 160) {
    #Hypertension stage I blood pressure
    BP_s = 4
  } else if (SBP >= 160) {
    #Hypertension stage II-IV
    BP_s = 5
  }

  #categories of blood pressure DBP based on Joint National Committee (JNC-V) blood pressure

  if (DBP < 80) {
    #Normal blood pressure
    BP_d = 1
  } else if (DBP >= 80 & DBP < 85) {
    #normal blood pressure (including optimal)
    BP_d = 2
  } else if (DBP >= 85 & DBP < 90) {
    #High normal blood pressure
    BP_d = 3
  } else if (DBP >= 90 & DBP < 100) {
    #Hypertension stage I blood pressure
    BP_d = 4
  } else if (DBP >= 100) {
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
  if (gender == 1) {

 #TCholl coefficient in range using TChol categories
  if (TChol < 160) {
    TChol_TCholcoef = -0.65945
  } else if (TChol >= 160 & TChol <= 199) {
    TChol_TCholcoef = 0
  } else if (TChol >= 200 & TChol <= 239) {
    TChol_TCholcoef = 0.17692
  } else if (TChol >= 240 & TChol <= 279) {
    TChol_TCholcoef = 0.50539
  } else if (TChol >= 280) {
    TChol_TCholcoef = 0.65713
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


  #HDL-C coefficient in range using TChol categories
  if (HDL < 35) {
    HDL_TCholcoef = 0.49744
  } else if (HDL >= 35 & HDL <= 44) {
    HDL_TCholcoef =  0.24310
  } else if (HDL >= 45 & HDL <= 49) {
    HDL_TCholcoef = 0
  } else if (HDL >= 50 & HDL <= 59) {
    HDL_TCholcoef = -0.05107
  } else if (HDL >= 60) {
    HDL_TCholcoef = -0.48660
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


  #SBP coefficient in range using TChol categories
   if (BP == 1) {
     BP_TCholcoef = -0.00226
   } else if (BP == 2) {
     BP_TCholcoef = 0
   } else if (BP == 3) {
     BP_TCholcoef = 0.28320
   } else if (BP == 4) {
     BP_TCholcoef = 0.52168
   } else if (BP == 5) {
     BP_TCholcoef = 0.61859
   }


    #SBP coefficient in range using LDL categories
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
  } else if (gender == 0) {
  #TCholl coefficient in range using TChol categories
  if (TChol < 160) {
    TChol_TCholcoef = -0.26138
  } else if (TChol >= 160 & TChol <= 199) {
    TChol_TCholcoef = 0
  } else if (TChol >= 200 & TChol <= 239) {
    TChol_TCholcoef = 0.20771
  } else if (TChol >= 240 & TChol <= 279) {
    TChol_TCholcoef = 0.24385
  } else if (TChol >= 280) {
    TChol_TCholcoef = 0.53513
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


  #HDL-C coefficient in range using TChol categories
  if (HDL < 35) {
    HDL_TCholcoef = 0.84312
  } else if (HDL >= 35 & HDL <= 44) {
    HDL_TCholcoef =  0.37796
  } else if (HDL >= 45 & HDL <= 49) {
    HDL_TCholcoef = 0.19785
  } else if (HDL >= 50 & HDL <= 59) {
    HDL_TCholcoef = 0
  } else if (HDL >= 60) {
    HDL_TCholcoef = -0.42951
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


    #SBP coefficient in range using TChol categories
    if (BP == 1) {
      BP_TCholcoef = -0.53363
    } else if (BP == 2) {
      BP_TCholcoef = 0
    } else if (BP == 3) {
      BP_TCholcoef = 0.06773
    } else if (BP == 4) {
      BP_TCholcoef = 0.26288
    } else if (BP == 5) {
      BP_TCholcoef = 0.46573
    }


    #SBP coefficient in range using LDL categories
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
    #================================================== Prediction using TChol =========================================================

  L_TChol_men <- 0.04826 * age + TChol_TCholcoef + HDL_TCholcoef + BP_TCholcoef + 0.42839 * diabetes + 0.52337 * smoker

  #values of the means
  G_TChol_men <- 3.0975

  A_TChol_men <- L_TChol_men - G_TChol_men

  B_TChol_men <- exp(A_TChol_men)

  #Baseline survival function at 10 years for men:
  s_TChol_men <- 0.90015

  P_TChol_men <- round((1 - (s_TChol_men) ^ B_TChol_men) * 100, digits = 0)



  L_TChol_women <- 0.33766 * age + (-0.00268) * age ^ 2 + TChol_TCholcoef + HDL_TCholcoef + BP_TCholcoef + 0.59626 * diabetes + 0.29246 * smoker

  #values of the means
  G_TChol_women <- 9.92545

  A_TChol_women <- L_TChol_women - G_TChol_women

  B_TChol_women <- exp(A_TChol_women)

  #Baseline survival function at 10 years for women:
  s_TChol_women <- 0.96246

  P_TChol_women <- round((1 - (s_TChol_women) ^ B_TChol_women)* 100, digits = 0)

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

  if (gender == 1) {
    results$CHDprediction_use_TChol <- P_TChol_men
    results$CHDprediction_use_ldl <- P_ldl_men
    return (results)

  } else if (gender == 0) {
    results$CHDprediction_use_TChol <- P_TChol_women
    results$CHDprediction_use_ldl <- P_ldl_women
    return (results)

  }

}

