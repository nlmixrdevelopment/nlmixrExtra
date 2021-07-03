#' Generate linear pharmacokinetic model ordinary differential equation code
#' 
#' @param ... passed to other methods
#' @param compartments Character vector of compartment names
#' @return A character vector of code to insert
#' @examples 
#' # Manually specify the model to use
#' linear_model_ode_onecmt_ev(ka=ka_drug1, cl=cl_drug1, v=v_drug1)
#' # Automatically detect the model to use
#' linear_model_ode(ka=ka_drug1, cl=cl_drug1, v=v_drug1)
#' @export
linear_model_ode <- function(..., compartments=NULL) {
  if (...length() == 2) {
    ret <- linear_model_ode_onecmt_iv(..., compartments=compartments)
  } else if (...length() == 3) {
    ret <- linear_model_ode_onecmt_ev(..., compartments=compartments)

  } else if (...length() == 4) {
    ret <- linear_model_ode_twocmt_iv(..., compartments=compartments)
  } else if (...length() == 5) {
    ret <- linear_model_ode_twocmt_ev(..., compartments=compartments)

  } else if (...length() == 6) {
    ret <- linear_model_ode_threecmt_iv(..., compartments=compartments)
  } else if (...length() == 7) {
    ret <- linear_model_ode_threecmt_ev(..., compartments=compartments)

  } else {
    stop("invalid number of arguments: ", ...length())
  }
  ret
}

#' @describeIn linear_model_ode Three compartment extravascular model
#' @param ka Absorption rate as an unquoted string
#' @param cl Clearanc as an unquoted string
#' @param q Intercompartmental clearance between the first and second compartment as an unquoted string
#' @param q2 Intercompartmental clearance between the first and third compartment as an unquoted string
#' @param v Central volume of distribution as an unquoted string
#' @param vp First peripheral volume of distribution as an unquoted string
#' @param vp2 Second peripheral volume of distribution as an unquoted string
#' @export
linear_model_ode_threecmt_ev <- function(ka, cl, q, q2, v, vp, vp2, compartments=NULL) {
  default_compartments <- c("depot", "central", "periph1", "periph2")
  if (is.null(compartments)) {
    compartments <- default_compartments
  }
  stopifnot(length(compartments) == length(default_compartments))
  stopifnot(!missing(ka))
  stopifnot(!missing(cl))
  stopifnot(!missing(q))
  stopifnot(!missing(q2))
  stopifnot(!missing(v))
  stopifnot(!missing(vp))
  stopifnot(!missing(vp2))
  kel <- sprintf("%s/%s", deparse(substitute(cl)), deparse(substitute(v)))
  k12 <- sprintf("%s/%s", deparse(substitute(q)), deparse(substitute(v)))
  k13 <- sprintf("%s/%s", deparse(substitute(q2)), deparse(substitute(v)))
  k21 <- sprintf("%s/%s", deparse(substitute(q)), deparse(substitute(vp)))
  k31 <- sprintf("%s/%s", deparse(substitute(q2)), deparse(substitute(vp2)))
  c(
    sprintf(
      "d/dt(%s) = -%s*%s",
      compartments[1], deparse(substitute(ka)), compartments[1]
    ),
    sprintf(
      "d/dt(%s) = %s*%s - (%s + %s + %s)*%s + %s*%s + %s*%s",
      compartments[2],
      deparse(substitute(ka)), compartments[1],
      kel, k12, k13, compartments[2],
      k21, compartments[3],
      k31, compartments[4]
    ),
    sprintf(
      "d/dt(%s) = %s*%s - %s*%s",
      compartments[3],
      k12, compartments[2],
      k21, compartments[3]
    ),
    sprintf(
      "d/dt(%s) = %s*%s - %s*%s",
      compartments[4],
      k13, compartments[2],
      k31, compartments[4]
    )
  )
}

#' @describeIn linear_model_ode Three compartment intravascular model
#' @export
linear_model_ode_threecmt_iv <- function(cl, q, q2, v, vp, vp2, compartments=NULL) {
  default_compartments <- c("central", "periph1", "periph2")
  if (is.null(compartments)) {
    compartments <- default_compartments
  }
  stopifnot(length(compartments) == length(default_compartments))
  stopifnot(!missing(cl))
  stopifnot(!missing(q))
  stopifnot(!missing(q2))
  stopifnot(!missing(v))
  stopifnot(!missing(vp))
  stopifnot(!missing(vp2))
  kel <- sprintf("%s/%s", deparse(substitute(cl)), deparse(substitute(v)))
  k12 <- sprintf("%s/%s", deparse(substitute(q)), deparse(substitute(v)))
  k13 <- sprintf("%s/%s", deparse(substitute(q2)), deparse(substitute(v)))
  k21 <- sprintf("%s/%s", deparse(substitute(q)), deparse(substitute(vp)))
  k31 <- sprintf("%s/%s", deparse(substitute(q2)), deparse(substitute(vp2)))
  c(
    sprintf(
      "d/dt(%s) = - (%s + %s + %s)*%s + %s*%s + %s*%s",
      compartments[1],
      kel, k12, k13, compartments[1],
      k21, compartments[2],
      k31, compartments[3]
    ),
    sprintf(
      "d/dt(%s) = %s*%s - %s*%s",
      compartments[2],
      k12, compartments[1],
      k21, compartments[2]
    ),
    sprintf(
      "d/dt(%s) = %s*%s - %s*%s",
      compartments[3],
      k13, compartments[1],
      k31, compartments[3]
    )
  )
}

#' @describeIn linear_model_ode Two compartment extravascular model
#' @export
linear_model_ode_twocmt_ev <- function(ka, cl, q, v, vp, compartments=NULL) {
  default_compartments <- c("depot", "central", "periph1")
  if (is.null(compartments)) {
    compartments <- default_compartments
  }
  stopifnot(length(compartments) == length(default_compartments))
  stopifnot(!missing(ka))
  stopifnot(!missing(cl))
  stopifnot(!missing(q))
  stopifnot(!missing(v))
  stopifnot(!missing(vp))
  kel <- sprintf("%s/%s", deparse(substitute(cl)), deparse(substitute(v)))
  k12 <- sprintf("%s/%s", deparse(substitute(q)), deparse(substitute(v)))
  k21 <- sprintf("%s/%s", deparse(substitute(q)), deparse(substitute(vp)))
  c(
    sprintf(
      "d/dt(%s) = -%s*%s",
      compartments[1], deparse(substitute(ka)), compartments[1]
    ),
    sprintf(
      "d/dt(%s) = %s*%s - (%s + %s)*%s + %s*%s",
      compartments[2],
      deparse(substitute(ka)), compartments[1],
      kel, k12, compartments[2],
      k21, compartments[3]
    ),
    sprintf(
      "d/dt(%s) = %s*%s - %s*%s",
      compartments[3],
      k12, compartments[2],
      k21, compartments[3]
    )
  )
}

#' @describeIn linear_model_ode Two compartment extravascular model
#' @export
linear_model_ode_twocmt_iv <- function(cl, q, v, vp, compartments=NULL) {
  default_compartments <- c("central", "periph1")
  if (is.null(compartments)) {
    compartments <- default_compartments
  }
  stopifnot(length(compartments) == length(default_compartments))
  stopifnot(!missing(cl))
  stopifnot(!missing(q))
  stopifnot(!missing(v))
  stopifnot(!missing(vp))
  kel <- sprintf("%s/%s", deparse(substitute(cl)), deparse(substitute(v)))
  k12 <- sprintf("%s/%s", deparse(substitute(q)), deparse(substitute(v)))
  k21 <- sprintf("%s/%s", deparse(substitute(q)), deparse(substitute(vp)))
  c(
    sprintf(
      "d/dt(%s) = - (%s + %s)*%s + %s*%s",
      compartments[1],
      kel, k12, compartments[1],
      k21, compartments[2]
    ),
    sprintf(
      "d/dt(%s) = %s*%s - %s*%s",
      compartments[2],
      k12, compartments[1],
      k21, compartments[2]
    )
  )
}

#' @describeIn linear_model_ode One compartment extravascular model
#' @export
linear_model_ode_onecmt_ev <- function(ka, cl, v, compartments=NULL) {
  default_compartments <- c("depot", "central")
  if (is.null(compartments)) {
    compartments <- default_compartments
  }
  stopifnot(length(compartments) == length(default_compartments))
  stopifnot(!missing(ka))
  stopifnot(!missing(cl))
  stopifnot(!missing(v))
  kel <- sprintf("%s/%s", deparse(substitute(cl)), deparse(substitute(v)))
  c(
    sprintf(
      "d/dt(%s) = -%s*%s",
      compartments[1], deparse(substitute(ka)), compartments[1]
    ),
    sprintf(
      "d/dt(%s) = %s*%s - %s*%s",
      compartments[2],
      deparse(substitute(ka)), compartments[1],
      kel, compartments[2]
    )
  )
}

#' @describeIn linear_model_ode One compartment intravascular model
#' @export
linear_model_ode_onecmt_iv <- function(cl, v, compartments=NULL) {
  default_compartments <- c("central", "periph1")
  if (is.null(compartments)) {
    compartments <- default_compartments
  }
  stopifnot(length(compartments) == length(default_compartments))
  stopifnot(!missing(cl))
  stopifnot(!missing(v))
  kel <- sprintf("%s/%s", deparse(substitute(cl)), deparse(substitute(v)))
  sprintf(
    "d/dt(%s) = -%s*%s",
    compartments[1],
    kel, compartments[1]
  )
}
