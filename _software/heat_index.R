heat_index <- function(T, RH, as_numeric = FALSE) {
  T <- T * 9/5 + 32
  HI <- -42.379 + 2.04901523*T + 10.14333127*RH - .22475541*T*RH -
    .00683783*T*T - .05481717*RH*RH + .00122874*T*T*RH +
    .00085282*T*RH*RH - .00000199*T*T*RH*RH
  HI <- (HI - 32)*5/9
  if (as_numeric) ifelse(HI <= 53, HI, NA)
  else {
    if (HI > 53) "Too hot! (Inputs are out of bounds.)"
    else paste(round(HI), "deg C")
  }

}
