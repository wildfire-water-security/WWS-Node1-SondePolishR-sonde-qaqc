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

  if(method == "temperature"){
    info <- list(
      equation = "\\(fDOM_{T} = \\frac{fDOM}{1 + \\rho (T - 25)}\\)",
      source = "Watras et al. 2011",
      params = list(
        rho = list(value = -0.011, step=0.001)),
      fun = function(fdom, temp, parms){
        fdom / (1 + parms$rho*(temp - 25))}
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

#' Determine which fDOM observations have already been corrected
#'
#' @param proj A `sondeproj` object.
#' @param type Which change are you looking for? Choices are "temp" or "turb".
#'
#' @noRd
is_corrected <- function(proj, type){
  stopifnot(type %in% c("temp", "turb"))

  flag <- ifelse(type == "temp", "CHG03", "CHG04")
  grepl(flag, proj$flags$flag_chg$fDOM_QSU)}


#' Apply fDOM temperature and turbidity corrections
#'
#' Takes functions and coefficient values and applies them to fDOM data within a sonde project to apply fDOM corrections.
#' Uses flags stored within the project to only correct data that hasn't previously been corrected. Additionally,
#' turbidity corrections will only be applied to temperature corrected data.
#'
#' @details
#' To apply a correction you must supply a list object with the following structure:
#' - params: a named list with parameter values where the names match the arguments within the function
#' - fun: a function to apply to the fDOM data where the first argument is fDOM, the second is either
#' temperature or turbidity, and the third is parameters within the function, pulled from `params`.
#'
#'
#' @param proj A `sondeproj` object.
#' @param temp Either `NULL` or a list object (see details) for temperature correction.
#' @param turb Either `NULL` or a list object (see details) for turbidity correction.
#'
#' @returns A `data.frame` object with fDOM updated with the corrections.
#' @export
#' @md
#' @examples
#' temp <- list(params = list(rho = -0.011),
#'              fun = function(fdom, temp, parms){fdom / (1 + parms$rho*(temp - 25))})
#' corr_data <- correct_fdom(example_sondeproj, temp=temp, turb=NULL)
#'
correct_fdom <- function(proj, temp=NULL, turb=NULL){
  stopifnot(inherits(proj, "sondeproj"), is.data.frame(proj$data),
            is.null(temp) || is.list(temp) & is.function(temp$fun),
            is.null(turb) || is.list(turb) & is.function(turb$fun))

  data <- proj$data

  #correct for temperature (only if not previously corrected)
  if(!is.null(temp)){
    data <- data %>%
      mutate(fDOM_QSU_corr = temp$fun(.data$fDOM_QSU, .data$Temp_C, temp$params),
             past_corr = is_corrected(proj, "temp"),
             fDOM_QSU = ifelse(!.data$past_corr, .data$fDOM_QSU_corr,.data$fDOM_QSU)) %>%
      select(-c("fDOM_QSU_corr", "past_corr"))
  }

  #correct for turbidity (only if temp corrected and not previously corrected)
  if(!is.null(turb)){
    data <- data %>%
      mutate(fDOM_QSU_corr = turb$fun(.data$fDOM_QSU, .data$Turbidity_FNU, turb$params),
             past_Tcorr = is_corrected(proj, "temp"),
             past_tcorr = is_corrected(proj, "turb"),
             fDOM_QSU = ifelse(.data$past_Tcorr & !.data$past_tcorr, .data$fDOM_QSU_corr, .data$fDOM_QSU)) %>%
      select(-c("fDOM_QSU_corr", "past_Tcorr", "past_tcorr"))
  }

  return(data)
}
