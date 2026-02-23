library(shinytest2)

# test_that("{shinytest2} recording: app-test", {
#   local_app_support(test_path("../../inst/app"))
#   app <- AppDriver$new(test_path("../../inst/app"), name = "app-test", height = 911,
#       width = 1619)
#   app$expect_values()
# })
#

test_that("{shinytest2} recording: app", {
  local_app_support(test_path("../../dev/app"))
  app <- AppDriver$new(test_path("../../dev/app"), name = "app", height = 911, width = 1619)
  app$set_inputs(name = "Katie")
  app$click("greet")
  app$expect_values()
})
