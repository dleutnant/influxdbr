context("influx_query")

test_that("setup", {
  skip_on_cran()
  skip_on_travis()
  expect_silent(setup_database())
})

test_that("single query works", { 
  skip_on_cran()
  skip_on_travis()

  expect_equal(As, influx_query(CON, DB, query = "select * from A")[names(As)])
  expect_equal(As, influx_query(CON, DB, query = "select * from A", chunked = T)[names(As)])
  expect_equal(As, influx_select(CON, DB, field_keys = "*", measurement = "A")[names(As)])
  expect_equal(As, influx_select(CON, DB, field_keys = "*", measurement = "A")[names(As)])
  expect_equal(As, influx_select(CON, DB, field_keys = c("a", "b", "c", "d"), measurement = "A")[names(As)])
  expect_equal(Bs, influx_query(CON, DB, query = "select * from B")[names(Bs)])
  expect_equal(Bs, influx_query(CON, DB, query = "select * from B", chunked = 1)[names(Bs)])
  expect_equal(Bs, influx_select(CON, DB, field_keys = "*", measurement = "B")[names(Bs)])
  expect_equal(Bs, influx_select(CON, DB, field_keys = c("A", "B", "c"), measurement = "B")[names(Bs)])

  expect_equal(A, influx_query(CON, DB, query = "select * from A group by *")[names(A)])
  expect_equal(A, influx_query(CON, DB, query = "select * from A group by *", chunked = T)[names(A)])
  expect_equal(A, influx_select(CON, DB, field_keys = "*", measurement = "A", group_by = "b")[names(A)])
  expect_equal(B, influx_query(CON, DB, query = "select * from B group by *", chunked = 1)[names(B)])
  expect_equal(B, influx_select(CON, DB, field_keys = "*", measurement = "B", group_by = "B")[names(B)])

  out <- influx_query(CON, DB, query = "select * from A, B")
  out2 <- influx_query(CON, DB, query = "select * from A, B", chunked = T)
  expect_equal(out, out2)

  expect_equal(As, out[out$measurement == "A", names(As)])
  Bq <- out[out$measurement == "B", names(Bs)]
  rownames(Bq) <- NULL
  expect_equal(Bs, Bq)

  out <- influx_query(CON, DB, query = "select * from A, B group by *")
  out2 <- influx_query(CON, DB, query = "select * from A, B group by *", chunked = 1)
  ## influx_query(CON, DB, query = "select * from A, B group by *", chunked = 1, csv = T)
  expect_equal(out, out2)
  
  expect_equal(A, droplevels(out[out$measurement == "A", names(A)]))
  Bq <- out[out$measurement == "B", names(Bs)]
  rownames(Bq) <- NULL
  expect_equal(B, droplevels(Bq))

  nms <- c("time", "a", "c", "b")
  Atmp <- droplevels(A[A$d == "e", nms])
  rownames(Atmp) <- NULL
  expect_equal(Atmp,
               influx_select(CON, DB,
                             field_keys = c("a", "c"),
                             group_by = "*", 
                             measurement = "A",
                             where = "d = 'e'")[nms])

})


test_that("multiple queries with no chunking work", { 
  skip_on_cran()
  skip_on_travis()

  expect_equal(influx_query(CON, DB, query = "select * from A; select * from B"),
               list(influx_query(CON, DB, query = "select * from A"),
                    influx_query(CON, DB, query = "select * from B")))

  expect_equal(influx_query(CON, DB, query = "select * from A group by *; select * from B group by *"),
               list(influx_query(CON, DB, query = "select * from A group by *"),
                    influx_query(CON, DB, query = "select * from B group by *")))
})

test_that("multiple queries with chunking work", { 
  skip_on_cran()
  skip_on_travis()

  expect_equal(influx_query(CON, DB, query = "select * from A; select * from B", chunked = T),
               list(influx_query(CON, DB, query = "select * from A", chunked = 1),
                    influx_query(CON, DB, query = "select * from B")))

  ## influx_query(CON, DB, query = "select * from A; select * from B", chunked = T, csv = T)

  expect_equal(influx_query(CON, DB, query = "select * from A group by *; select * from B group by *", chunked = 1),
               list(influx_query(CON, DB, query = "select * from A group by *", chunked = T),
                    influx_query(CON, DB, query = "select * from B group by *")))
})

test_that("empty results propagate as NULLs", { 
  skip_on_cran()
  skip_on_travis()
  
  expect_null(influx_query(CON, DB, query = "select * from BLABLA where X='Y'"))
  expect_null(influx_query(CON, DB, query = "select * from A where X='Y'"))
  expect_null(influx_query(CON, DB, query = "select * from A where a=10"))

  expect_equal(list(NULL, NULL),
               influx_query(CON, DB, query = "select * from BLABLA where X='Y'; select * from A where a=10"))

  res <- influx_query(CON, DB, query = "select * from A where a=1; select * from A where a=10")
  expect_equal(res[[1]][names(As)], As[As$a == 1, ])
  expect_null(res[[2]])
})
