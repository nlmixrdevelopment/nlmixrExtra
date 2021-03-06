% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nlmixr_tran.R
\name{nlmixr_trans}
\alias{nlmixr_trans}
\title{Extend and translate nlmixr models}
\usage{
nlmixr_trans(object, ...)
}
\arguments{
\item{object}{A function specifying the nlmixr model}

\item{...}{Currently ignored}
}
\value{
\code{object} modified by expanding functions
}
\description{
Extend and translate nlmixr models
}
\examples{
model <- function() {
  ini({
    lcl <- (0.01)
    lv <- log(0.5)
    eta_cl ~ 0.1
    add_err <- 0.1
  })
  model({
    linear_model_ode_onecmt_iv(cl=exp(lcl+eta_cl), v=exp(lv))
    cp <- central/exp(lv)
    cp ~ add(add_err)
  })
}
nlmixr_trans(model, linear_model_ode_onecmt_iv)
}
