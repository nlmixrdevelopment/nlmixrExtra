test_that("linear_model_ode, confirm autodetection", {
  expect_equal(
    linear_model_ode(ka=my_ka, cl=my_cl, q=my_q, q2=my_q2, v=my_v, vp=my_vp, vp2=my_vp2),
    linear_model_ode_threecmt_ev(ka=my_ka, cl=my_cl, q=my_q, q2=my_q2, v=my_v, vp=my_vp, vp2=my_vp2)
  )
  expect_equal(
    linear_model_ode(cl=my_cl, q=my_q, q2=my_q2, v=my_v, vp=my_vp, vp2=my_vp2),
    linear_model_ode_threecmt_iv(cl=my_cl, q=my_q, q2=my_q2, v=my_v, vp=my_vp, vp2=my_vp2)
  )
  expect_equal(
    linear_model_ode(ka=my_ka, cl=my_cl, q=my_q, v=my_v, vp=my_vp),
    linear_model_ode_twocmt_ev(ka=my_ka, cl=my_cl, q=my_q, v=my_v, vp=my_vp)
  )
  expect_equal(
    linear_model_ode(cl=my_cl, q=my_q, v=my_v, vp=my_vp),
    linear_model_ode_twocmt_iv(cl=my_cl, q=my_q, v=my_v, vp=my_vp)
  )
  expect_equal(
    linear_model_ode(ka=my_ka, cl=my_cl, v=my_v),
    linear_model_ode_onecmt_ev(ka=my_ka, cl=my_cl, v=my_v)
  )
  expect_equal(
    linear_model_ode(cl=my_cl, v=my_v),
    linear_model_ode_onecmt_iv(cl=my_cl, v=my_v)
  )
})

test_that("linear_model_ode, confirm argument errors", {
  expect_error(
    linear_model_ode(cl=my_cl),
    regexp="invalid number of arguments: 1"
  )
  expect_error(
    linear_model_ode(1, 2, 3, 4, 5, 6, 7, 8),
    regexp="invalid number of arguments: 8"
  )
})

test_that("linear_model_ode, confirm outputs of specific models", {
  # Confirm parameter naming works
  expect_equal(
    linear_model_ode_threecmt_ev(ka=my_ka, cl=my_cl, q=my_q, q2=my_q2, v=my_v, vp=my_vp, vp2=my_vp2),
    c(
      "d/dt(depot) = -my_ka*depot",
      "d/dt(central) = my_ka*depot - (my_cl/my_v + my_q/my_v + my_q2/my_v)*central + my_q/my_vp*periph1 + my_q2/my_vp2*periph2", 
      "d/dt(periph1) = my_q/my_v*central - my_q/my_vp*periph1",
      "d/dt(periph2) = my_q2/my_v*central - my_q2/my_vp2*periph2"
    )
  )
  # Confirm compartment naming works
  expect_equal(
    linear_model_ode_threecmt_ev(
      ka=my_ka, cl=my_cl, q=my_q, q2=my_q2,
      v=my_v, vp=my_vp, vp2=my_vp2,
      compartments=c("my_depot", "my_central", "my_periph1", "my_periph2")
    ),
    c(
      "d/dt(my_depot) = -my_ka*my_depot",
      "d/dt(my_central) = my_ka*my_depot - (my_cl/my_v + my_q/my_v + my_q2/my_v)*my_central + my_q/my_vp*my_periph1 + my_q2/my_vp2*my_periph2", 
      "d/dt(my_periph1) = my_q/my_v*my_central - my_q/my_vp*my_periph1",
      "d/dt(my_periph2) = my_q2/my_v*my_central - my_q2/my_vp2*my_periph2"
    )
  )
  expect_equal(
    linear_model_ode_threecmt_iv(cl=my_cl, q=my_q, q2=my_q2, v=my_v, vp=my_vp, vp2=my_vp2),
    c(
      "d/dt(central) = - (my_cl/my_v + my_q/my_v + my_q2/my_v)*central + my_q/my_vp*periph1 + my_q2/my_vp2*periph2", 
      "d/dt(periph1) = my_q/my_v*central - my_q/my_vp*periph1",
      "d/dt(periph2) = my_q2/my_v*central - my_q2/my_vp2*periph2"
    )
  )
  expect_equal(
    linear_model_ode_twocmt_ev(ka=my_ka, cl=my_cl, q=my_q, v=my_v, vp=my_vp),
    c(
      "d/dt(depot) = -my_ka*depot",
      "d/dt(central) = my_ka*depot - (my_cl/my_v + my_q/my_v)*central + my_q/my_vp*periph1",
      "d/dt(periph1) = my_q/my_v*central - my_q/my_vp*periph1"
    )
  )
  expect_equal(
    linear_model_ode_twocmt_iv(cl=my_cl, q=my_q, v=my_v, vp=my_vp),
    c(
      "d/dt(central) = - (my_cl/my_v + my_q/my_v)*central + my_q/my_vp*periph1",
      "d/dt(periph1) = my_q/my_v*central - my_q/my_vp*periph1"
    )
  )
  expect_equal(
    linear_model_ode_onecmt_ev(ka=my_ka, cl=my_cl, v=my_v),
    c(
      "d/dt(depot) = -my_ka*depot",
      "d/dt(central) = my_ka*depot - my_cl/my_v*central"
    )
  )
  expect_equal(
    linear_model_ode_onecmt_iv(cl=my_cl, v=my_v),
    "d/dt(central) = -my_cl/my_v*central"
  )
})
