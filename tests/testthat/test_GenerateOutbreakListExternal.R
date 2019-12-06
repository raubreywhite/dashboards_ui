context("GenerateOutbreakListExternal")

test_that("Basic example", {
  library(data.table)

  df <- GenFakeResultsFull(granularity = "weekly", syndrome = "gastro", xmunicipEnd = "municip0301")
  dk <- GenFakeResultsFull(granularity = "weekly", syndrome = "gastro", xmunicipEnd = "municip0301")

  df[, status := "Normal"]
  dk[, status := "Normal"]

  dk[.N, status := "Medium"]

  emails <- data.table(
    email = c("a@fhi.no", "b@fhi.no"),
    location = c("municip0301", "municip0301"),
    level = c("medium", "high")
  )
  alerts <- AlertsEmailConverter(emails)

  res <- GenerateOutbreakListExternal(
    df = df,
    dk = dk,
    saveFiles = NULL,
    alerts = alerts
  )

  testthat::expect_equal(nrow(res), 1)
})
