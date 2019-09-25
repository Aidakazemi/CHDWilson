#' Predicting Coronary Heart Disease (CHD) over ten years using Tchol and LDLchol categories
#' @param age A number for age
#' @param gender A binary variables taking 1 for men and 0 for women
#' @param Tchol  A number for total serum cholesterol in mg/dL measured by the Abell-Kendall method
#' @param HDLchol A number for high-density lipoprotein (HDL) Cholesterol in mg/dL
#' @param LDLchol A number for LDLchol cholesterol  in mg/dL
#' @param SBP  A number for SBP blood pressure in mm Hg without regard to the use of antihypertensive medication
#' @param DBP A number for DBP blood pressure in mm Hg without regard to the use of antihypertensive medication
#' @param diabetes A binary variable taking 1 if the patient was under treatment with insulin or oral hypoglycemic agents, if casual blood glucose determinations exceeded 140 mg/dL
#' @param smoker A binary variable taking 1 for person who smoked during the past 12 month and 0 therwise
#'
#' @examples
#'
#' predicTcholHD (age = 55, gender = 1, Tchol = 250, LDLchol = 120, HDLchol = 39, SBP = 146, DBP = 88, diabetes = 0 , smoker =1)
#' predicTcholHD (age = 30, gender = 0, Tchol = 170, LDLchol = 120, HDLchol = 39, SBP = 145, DBP = 88, diabetes = 0 , smoker =1)
#'
#' @source  \url{https://www.ahajournals.org/doi/full/10.1161/01.CIR.97.18.1837}



predicTcholHD <- function (age, gender, Tchol, LDLchol, HDLchol, SBP, DBP, diabetes, smoker) {


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

 #Tcholl coefficient in range using Tchol categories
  if (Tchol < 160) {
    Tchol_Tcholcoef = -0.65945
  } else if (Tchol >= 160 & Tchol <= 199) {
    Tchol_Tcholcoef = 0
  } else if (Tchol >= 200 & Tchol <= 239) {
    Tchol_Tcholcoef = 0.17692
  } else if (Tchol >= 240 & Tchol <= 279) {
    Tchol_Tcholcoef = 0.50539
  } else if (Tchol >= 280) {
    Tchol_Tcholcoef = 0.65713
  }


  #LDLchol coefficient in range using LDLchol categories
  if (LDLchol < 100) {
    LDLchol_LDLcholcoef = -0.69281
  } else if (LDLchol >= 100 & LDLchol <= 129) {
    LDLchol_LDLcholcoef = 0
  } else if (LDLchol >= 130 & LDLchol <= 159) {
    LDLchol_LDLcholcoef = 0.00389
  } else if (LDLchol >= 160 & LDLchol <= 189) {
    LDLchol_LDLcholcoef = 0.26755
  } else if (LDLchol >= 190) {
    LDLchol_LDLcholcoef = 0.56705
  }


  #HDLchol-C coefficient in range using Tchol categories
  if (HDLchol < 35) {
    HDLchol_Tcholcoef = 0.49744
  } else if (HDLchol >= 35 & HDLchol <= 44) {
    HDLchol_Tcholcoef =  0.24310
  } else if (HDLchol >= 45 & HDLchol <= 49) {
    HDLchol_Tcholcoef = 0
  } else if (HDLchol >= 50 & HDLchol <= 59) {
    HDLchol_Tcholcoef = -0.05107
  } else if (HDLchol >= 60) {
    HDLchol_Tcholcoef = -0.48660
  }


    #HDLchol-C coefficient in range using LDLchol categories
    if (HDLchol < 35) {
      HDLchol_LDLcholcoef = 0.48598
    } else if (HDLchol >= 35 & HDLchol <= 44) {
      HDLchol_LDLcholcoef =  0.21643
    } else if (HDLchol >= 45 & HDLchol <= 49) {
      HDLchol_LDLcholcoef = 0
    } else if (HDLchol >= 50 & HDLchol <= 59) {
      HDLchol_LDLcholcoef = -0.04710
    } else if (HDLchol >= 60) {
      HDLchol_LDLcholcoef = -0.34190
    }


  #SBP coefficient in range using Tchol categories
   if (BP == 1) {
     BP_Tcholcoef = -0.00226
   } else if (BP == 2) {
     BP_Tcholcoef = 0
   } else if (BP == 3) {
     BP_Tcholcoef = 0.28320
   } else if (BP == 4) {
     BP_Tcholcoef = 0.52168
   } else if (BP == 5) {
     BP_Tcholcoef = 0.61859
   }


    #SBP coefficient in range using LDLchol categories
    if (BP == 1) {
      BP_LDLcholcoef = -0.02642
    } else if (BP == 2) {
      BP_LDLcholcoef = 0
    } else if (BP == 3) {
      BP_LDLcholcoef = 0.30104
    } else if (BP == 4) {
      BP_LDLcholcoef = 0.55714
    } else if (BP == 5) {
      BP_LDLcholcoef = 0.65107
    }


  #============================================================= Women ====================================================================
  } else if (gender == 0) {
  #Tcholl coefficient in range using Tchol categories
  if (Tchol < 160) {
    Tchol_Tcholcoef = -0.26138
  } else if (Tchol >= 160 & Tchol <= 199) {
    Tchol_Tcholcoef = 0
  } else if (Tchol >= 200 & Tchol <= 239) {
    Tchol_Tcholcoef = 0.20771
  } else if (Tchol >= 240 & Tchol <= 279) {
    Tchol_Tcholcoef = 0.24385
  } else if (Tchol >= 280) {
    Tchol_Tcholcoef = 0.53513
  }


    #LDLchol coefficient in range using LDLchol categories
    if (LDLchol < 100) {
      LDLchol_LDLcholcoef = -0.42616
    } else if (LDLchol >= 100 & LDLchol <= 129) {
      LDLchol_LDLcholcoef = 0
    } else if (LDLchol >= 130 & LDLchol <= 159) {
      LDLchol_LDLcholcoef = 0.01366
    } else if (LDLchol >= 160 & LDLchol <= 189) {
      LDLchol_LDLcholcoef = 0.26948
    } else if (LDLchol >= 190) {
      LDLchol_LDLcholcoef = 0.33251
    }


  #HDLchol-C coefficient in range using Tchol categories
  if (HDLchol < 35) {
    HDLchol_Tcholcoef = 0.84312
  } else if (HDLchol >= 35 & HDLchol <= 44) {
    HDLchol_Tcholcoef =  0.37796
  } else if (HDLchol >= 45 & HDLchol <= 49) {
    HDLchol_Tcholcoef = 0.19785
  } else if (HDLchol >= 50 & HDLchol <= 59) {
    HDLchol_Tcholcoef = 0
  } else if (HDLchol >= 60) {
    HDLchol_Tcholcoef = -0.42951
  }


    #HDLchol-C coefficient in range using LDLchol categories
    if (HDLchol < 35) {
      HDLchol_LDLcholcoef = 0.88121
    } else if (HDLchol >= 35 & HDLchol <= 44) {
      HDLchol_LDLcholcoef =  0.36312
    } else if (HDLchol >= 45 & HDLchol <= 49) {
      HDLchol_LDLcholcoef = 0.19247
    } else if (HDLchol >= 50 & HDLchol <= 59) {
      HDLchol_LDLcholcoef = 0
    } else if (HDLchol >= 60) {
      HDLchol_LDLcholcoef = -0.35404
    }


    #SBP coefficient in range using Tchol categories
    if (BP == 1) {
      BP_Tcholcoef = -0.53363
    } else if (BP == 2) {
      BP_Tcholcoef = 0
    } else if (BP == 3) {
      BP_Tcholcoef = 0.06773
    } else if (BP == 4) {
      BP_Tcholcoef = 0.26288
    } else if (BP == 5) {
      BP_Tcholcoef = 0.46573
    }


    #SBP coefficient in range using LDLchol categories
    if (BP == 1) {
      BP_LDLcholcoef = -0.51204
    } else if (BP == 2) {
      BP_LDLcholcoef = 0
    } else if (BP == 3) {
      BP_LDLcholcoef = -0.03484
    } else if (BP == 4) {
      BP_LDLcholcoef = 0.28533
    } else if (BP == 5) {
      BP_LDLcholcoef = 0.50403
    }


 }
    #================================================== Prediction using Tchol =========================================================

  L_Tchol_men <- 0.04826 * age + Tchol_Tcholcoef + HDLchol_Tcholcoef + BP_Tcholcoef + 0.42839 * diabetes + 0.52337 * smoker

  #values of the means
  G_Tchol_men <- 3.0975

  A_Tchol_men <- L_Tchol_men - G_Tchol_men

  B_Tchol_men <- exp(A_Tchol_men)

  #Baseline survival function at 10 years for men:
  s_Tchol_men <- 0.90015

  P_Tchol_men <- round((1 - (s_Tchol_men) ^ B_Tchol_men) * 100, digits = 0)



  L_Tchol_women <- 0.33766 * age + (-0.00268) * age ^ 2 + Tchol_Tcholcoef + HDLchol_Tcholcoef + BP_Tcholcoef + 0.59626 * diabetes + 0.29246 * smoker

  #values of the means
  G_Tchol_women <- 9.92545

  A_Tchol_women <- L_Tchol_women - G_Tchol_women

  B_Tchol_women <- exp(A_Tchol_women)

  #Baseline survival function at 10 years for women:
  s_Tchol_women <- 0.96246

  P_Tchol_women <- round((1 - (s_Tchol_women) ^ B_Tchol_women)* 100, digits = 0)

  #================================================== Prediction using LDLchol =========================================================

  L_LDLchol_men <- 0.04808 * age + LDLchol_LDLcholcoef + HDLchol_LDLcholcoef + BP_LDLcholcoef + 0.42146 * diabetes + 0.54377 * smoker

  #values of the means
  G_LDLchol_men <- 3.00069

  A_LDLchol_men <- L_LDLchol_men - G_LDLchol_men

  B_LDLchol_men <- exp(A_LDLchol_men)

  #Baseline survival function at 10 years for men:
  s_LDLchol_men <- 0.90017

  P_LDLchol_men <- round((1 - (s_LDLchol_men) ^ B_LDLchol_men) * 100, digits = 0)



  L_LDLchol_women <- 0.33994 * age + (-0.0027) * age ^ 2 + LDLchol_LDLcholcoef + HDLchol_LDLcholcoef + BP_LDLcholcoef + 0.61313 * diabetes + 0.29737 * smoker

  #values of the means
  G_LDLchol_women <- 9.914136

  A_LDLchol_women <- L_LDLchol_women - G_LDLchol_women

  B_LDLchol_women <- exp(A_LDLchol_women)

  #Baseline survival function at 10 years for women:
  s_LDLchol_women <- 0.96278

  P_LDLchol_women <- round((1 - (s_LDLchol_women) ^ B_LDLchol_women)* 100, digits = 0)





results <- list()

if (age > 29 & age < 75) {

if (gender == 1) {
  results$CHDprediction_use_Tchol <- P_Tchol_men
  results$CHDprediction_use_LDLchol <- P_LDLchol_men
  return (results)

    } else if (gender == 0) {
  results$CHDprediction_use_Tchol <- P_Tchol_women
  results$CHDprediction_use_LDLchol <- P_LDLchol_women
  return (results)

    }


} else {
    results$prediction <- 'Not_in_age_range'
    return (results)
  }
}



