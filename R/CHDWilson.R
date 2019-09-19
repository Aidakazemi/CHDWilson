#' CHD prediction using TC and LDL-C categories
#'
#' @param

CHDWilson <- function(age, TC ) {
}
  #Cholestrol coefficient in range
  if TC < 160 {
    TC_coef = -0.65945
  } els if TC >= 160 & TC <=199 {
    TC_coef = 0
  } else if TC >= 200 & TC <= 239 {
    TC_coef = 0.17692
  } else if TC >= 240 & TC <= 279 {
    TC_coef = 0.50539
  } else if TC >= 280 {
    TC_coef = 0.65713
  }


  #HDL-C coefficient in range
  if HDL-C < 35 {
    HDL_coef = 0.49744
  } els if HDL-C >= 35 & HDL-C <= 44 {
    HDL_coef =  0.24310
  } else if HDL-C >= 45 & HDL-C <= 49 {
    HDL_coef = 0
  } else if HDL-C >= 50 & HDL-C <= 59 {
    HDL_coef = -0.05107
  } else if HDL-C >= 60 {
    HDL_coef = -0.48660
  }


  # Blood pressure in range
  L_Chol_men = 0.04826 * age + TC_coef
}

}
}
