#' Return info about fDOM correction equation
#'
#' Used to compactly return the info needed to initialize diffrent fDOM formulas selected by the user.
#'
#' @param method the user selected fDOM correction method
#'
#' @returns a list of length 4:
#' - equation: the formatted equation to display in UI
#' - source: the citation for the equation and/or defaults
#' - params: a list of the default equation values to display as numericInputs
#' - fun: a function that takes fdom_tc and turb and returns corrected fDOM
#' @noRd
get_equation <- function(method){
  if(method == "none"){
    info <- list(
      equation = "",
      source = "",
      params = list(),
      fun = function(fdom_tc, turb, parms){
        fdom_tc}
    )
  }

  if(method == "inverse_poly"){
    info <- list(
    equation = "\\(fDOM_{T,t} = (x + yt + zt^2)fDOM_T\\)",
    source = "Fleck et al. 2026",
    params = list(
      x = list(value = 0.96, step = 0.01),
      y = list(value = 0.0097, step = 0.0001),
      z = list(value = 9.6e-6, step = 0.0000001)),
    fun = function(fdom_tc, turb, parms){
      (parms$x + (parms$y * turb) + (parms$z * turb^2)) * fdom_tc}
    )}

  if(method == "1p_exponential"){
    info <- list(
      equation = "\\(fDOM_{T,t} = \\frac{fDOM_T}{e^{\\alpha t}}\\)",
      source = "Downing et al. 2012; Akie et al. 2024",
      params = list(
        a = list(value = -0.003, step = 0.001)),
      fun = function(fdom_tc, turb, parms){
        fdom_tc / exp(parms$a * turb)}
    )}

  if(method == "2p_exponential"){
    info <- list(
      equation = "\\(fDOM_{T,t} = \\frac{fDOM_T}{b e^{ct}}\\)",
      source = "Fleck et al. 2026",
      params = list(
        b = list(value = 0.995, step = 0.001),
        c = list(value = -0.0062, step = 0.0001)),
      fun = function(fdom_tc, turb, parms){
        fdom_tc / (parms$b * exp(parms$c*turb))}
    )}

  if(method == "5p_exponential"){
    info <- list(
      equation = "\\(fDOM_{T,t} = fDOM_T(a + be^{ct} + de^{et})\\)",
      source = "Fleck et al. 2026",
      params = list(
        a = list(value = 0.053, step = 0.001),
        b = list(value = 0.647, step = 0.001),
        c = list(value = -0.0047, step = 0.0001),
        d = list(value = 0.354, step = 0.001),
        e = list(value = -0.0224, step = 0.0001)),
      fun = function(fdom_tc, turb, parms){
        fdom_tc * (parms$a + (parms$b*exp(parms$c*turb)) + (parms$d*exp(parms$e*turb)))}
    )}

  return(info)
}
