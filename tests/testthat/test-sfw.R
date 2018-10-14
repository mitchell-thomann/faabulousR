
test_that("gen_team_dat", {
  expect_equal(nrow(gen_team_dat(team_labels = c("team1", "team2", "team3"), weeks = 3)), 9)
})
