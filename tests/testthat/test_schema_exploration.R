context("schema exploration")

test_that("setup", {
  skip_on_cran()
  skip_on_travis()
  expect_silent(setup_database())
})

test_that("show commands", { 
  skip_on_cran()
  skip_on_travis()
  
  ## show_users(CON)
  ## show_grants(CON, "nonuser")
  ## show_databases(CON)
  ## show_measurements(CON, db = DB)
  ## show_series(CON, DB)
  ## show_tag_keys(CON, DB)
  ## show_tag_values(CON, DB, key = "b")
  ## show_field_keys(CON, DB)
  ## show_retention_policies(CON, DB)
  ## show_diagnostics(CON)
  ## influx_query(CON,
  ##              query = "SHOW DIAGNOSTICS",
  ##              handler = identity, csv = T)
  ## show_stats(CON)
  
  expect_silent(show_diagnostics(CON))
  expect_is(show_users(CON), "influxdbr.response")
  expect_error(show_grants(CON, "not_a_user"))
  expect_is(show_databases(CON), class = "influxdbr.response")
  expect_is(show_measurements(CON, DB), "influxdbr.response")
  expect_is(show_series(CON, DB), "influxdbr.response")
  expect_is(show_tag_keys(CON, DB), class = "influxdbr.response")
  expect_is(show_tag_values(CON, DB, key = "b"), class = "influxdbr.response")
  expect_is(show_field_keys(CON, DB), class = "influxdbr.response")
  expect_is(show_retention_policies(CON, DB), class = "influxdbr.response")

})
