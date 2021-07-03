test_that("nlmixr_trans expected errors", {
  expect_error(
    nlmixr_trans(model_ref1, "linear_model_ode_onecmt_iv"),
    regexp="all ... arguments must be function names"
  )
})

test_that("nlmixr_trans", {
  # Single row replacement
  model_ref1 <- function() {
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
  model_ref1_result <- function() {
    ini({
      lcl <- (0.01)
      lv <- log(0.5)
      eta_cl ~ 0.1
      add_err <- 0.1
    })
    model({
      d/dt(central) = -exp(lcl + eta_cl)/exp(lv) * central
      cp <- central/exp(lv)
      cp ~ add(add_err)
    })
  }
  expect_equal(
    nlmixr_trans(model_ref1, linear_model_ode_onecmt_iv),
    model_ref1_result
  )
  
  # Multiple row replacement
  model_ref2 <- function() {
    ini({
      lka <- log(1)
      lcl <- (0.01)
      lv <- log(0.5)
      eta_cl ~ 0.1
      add_err <- 0.1
    })
    model({
      linear_model_ode_onecmt_ev(ka=exp(lka), cl=exp(lcl+eta_cl), v=exp(lv))
      cp <- central/exp(lv)
      cp ~ add(add_err)
    })
  }
  model_ref2_result <- function() {
    ini({
      lka <- log(1)
      lcl <- (0.01)
      lv <- log(0.5)
      eta_cl ~ 0.1
      add_err <- 0.1
    })
    model({
      d/dt(depot) = -exp(lka) * depot
      d/dt(central) = exp(lka) * depot - exp(lcl + eta_cl)/exp(lv) * central
      cp <- central/exp(lv)
      cp ~ add(add_err)
    })
  }
  expect_equal(
    nlmixr_trans(model_ref2, linear_model_ode_onecmt_ev, linear_model_ode),
    model_ref2_result
  )

  # Multiple replacement functions
  model_ref3 <- function() {
    ini({
      lka <- log(1)
      lcl <- (0.01)
      lv <- log(0.5)
      eta_cl ~ 0.1
      add_err <- 0.1
    })
    model({
      linear_model_ode_onecmt_ev(ka=exp(lka), cl=exp(lcl+eta_cl), v=exp(lv))
      linear_model_ode_twocmt_iv(
        cl=exp(lcl_drug2 + eta_cl_drug2),
        q=exp(lq_drug2),
        v=exp(lv_drug2),
        vp=exp(lvp_drug2),
        compartments=c("centrald2", "periphd2")
      )
      cp <- central/exp(lv)
      cp ~ add(add_err)
    })
  }
  model_ref3_result <- function() {
    ini({
      lka <- log(1)
      lcl <- (0.01)
      lv <- log(0.5)
      eta_cl ~ 0.1
      add_err <- 0.1
    })
    model({
      d/dt(depot) = -exp(lka) * depot
      d/dt(central) = exp(lka) * depot - exp(lcl + eta_cl)/exp(lv) * central
      d/dt(centrald2) = -(exp(lcl_drug2 + eta_cl_drug2)/exp(lv_drug2) + exp(lq_drug2)/exp(lv_drug2)) * centrald2 + exp(lq_drug2)/exp(lvp_drug2) * periphd2
      d/dt(periphd2) = exp(lq_drug2)/exp(lv_drug2) * centrald2 - exp(lq_drug2)/exp(lvp_drug2) * periphd2
      cp <- central/exp(lv)
      cp ~ add(add_err)
    })
  }
  expect_equal(
    nlmixr_trans(model_ref3, linear_model_ode_onecmt_ev, linear_model_ode_twocmt_iv),
    model_ref3_result
  )
})
