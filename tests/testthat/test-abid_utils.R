testthat::test_that("switch_panel returns a shiny tag", {
  ui <- switch_panel(
    label = "Test",
    switch_id = "enabled",
    shiny::selectInput(
      "x",
      NULL,
      c("A", "B")
    )
  )
  
  testthat::expect_s3_class(ui, "shiny.tag")
})

testthat::test_that("switch_panel contains switch id", {
  ui <- switch_panel(
    label = "Test",
    switch_id = "enabled",
    shiny::selectInput(
      "x",
      NULL,
      c("A", "B")
    )
  )
  
  html <- as.character(ui)
  
  testthat::expect_match(
    html,
    "enabled"
  )
})

testthat::test_that("switch_panel contains label", {
  ui <- switch_panel(
    label = "My Label",
    switch_id = "enabled"
  )
  
  html <- as.character(ui)
  
  testthat::expect_match(
    html,
    "My Label"
  )
})
