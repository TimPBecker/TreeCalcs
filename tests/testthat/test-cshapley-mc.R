library(TreeCalcs)

test_that("cShapleyMC computes accurate linear Shapley values", {
  tree <- tree_node("root_node", list(
    tree_node("group1", list(
      tree_leaf("a", 8),
      tree_leaf("b", 2)
    )),
    tree_node("group2", list(
      tree_leaf("c", 5),
      tree_leaf("d", 1)
    ))
  ))

  shap_mc <- cShapleyMC(tree, "sum", n_samples = 100, seed = 42)
  expect_equal(shap_mc[["a"]], 8)
  expect_equal(shap_mc[["b"]], 2)
  expect_equal(shap_mc[["c"]], 5)
  expect_equal(shap_mc[["d"]], 1)
  expect_equal(sum(shap_mc), 16)
})

test_that("cShapleyMC satisfies efficiency property with non-linear value function", {
  tree <- tree_node("root_node", list(
    tree_node("group1", list(
      tree_leaf("a", 8),
      tree_leaf("b", 2)
    )),
    tree_node("group2", list(
      tree_leaf("c", 5),
      tree_leaf("d", 1)
    ))
  ))

  sq_calc <- function(df) {
    if (nrow(df) == 0) 0 else sum(df$value)^2
  }

  # Efficiency must hold exactly regardless of sample size
  for (n in c(10, 50, 200)) {
    shap_mc <- cShapleyMC(tree, sq_calc, n_samples = n, seed = 123)
    expect_equal(sum(shap_mc), 256, tolerance = 1e-10)
  }
})

test_that("cShapleyMC converges to exact cshapley values as n_samples increases", {
  tree <- tree_node("root_node", list(
    tree_node("group1", list(
      tree_leaf("a", 8),
      tree_leaf("b", 2)
    )),
    tree_node("group2", list(
      tree_leaf("c", 5),
      tree_leaf("d", 1)
    ))
  ))

  sq_calc <- function(df) {
    if (nrow(df) == 0) 0 else sum(df$value)^2
  }

  exact <- cshapley(tree, sq_calc)
  # exact values: a=128, b=32, c=80, d=16
  mc_estimate <- cShapleyMC(tree, sq_calc, n_samples = 30000, seed = 42)

  expect_equal(mc_estimate[["a"]], exact[["a"]], tolerance = 1.5)
  expect_equal(mc_estimate[["b"]], exact[["b"]], tolerance = 1.5)
  expect_equal(mc_estimate[["c"]], exact[["c"]], tolerance = 1.5)
  expect_equal(mc_estimate[["d"]], exact[["d"]], tolerance = 1.5)
  expect_equal(sum(mc_estimate), sum(exact), tolerance = 1e-10)
})

test_that("cShapleyMC is deterministic and reproducible when seed is specified", {
  tree <- tree_node("root_node", list(
    tree_node("group1", list(
      tree_leaf("a", 8),
      tree_leaf("b", 2)
    )),
    tree_node("group2", list(
      tree_leaf("c", 5),
      tree_leaf("d", 1)
    ))
  ))

  sq_calc <- function(df) {
    if (nrow(df) == 0) 0 else sum(df$value)^2
  }

  # 1. Calling twice with the same explicit seed gives identical results
  res1 <- cShapleyMC(tree, sq_calc, n_samples = 300, seed = 777)
  res2 <- cShapleyMC(tree, sq_calc, n_samples = 300, seed = 777)
  expect_identical(res1, res2)

  # 2. Different seeds give different estimates (non-identical)
  res3 <- cShapleyMC(tree, sq_calc, n_samples = 300, seed = 888)
  expect_false(identical(res1, res3))

  # 3. Setting R's global set.seed reproducible when seed = NULL
  set.seed(456)
  res_glob1 <- cShapleyMC(tree, sq_calc, n_samples = 300, seed = NULL)
  set.seed(456)
  res_glob2 <- cShapleyMC(tree, sq_calc, n_samples = 300, seed = NULL)
  expect_identical(res_glob1, res_glob2)
})

test_that("cShapleyMC works with single-leaf trees", {
  leaf <- tree_leaf("solo", 42)
  shap <- cShapleyMC(leaf, "sum", n_samples = 50, seed = 1)
  expect_equal(shap[["solo"]], 42)

  sq_calc <- function(df) if (nrow(df) == 0) 0 else sum(df$value)^2
  shap_sq <- cShapleyMC(leaf, sq_calc, n_samples = 50, seed = 1)
  expect_equal(shap_sq[["solo"]], 42^2)
})

test_that("cShapleyMC works with bank/desk hierarchy and non-linear payoff", {
  bank_tree <- tree_node("Bank", list(
    tree_node("Desk_A", list(
      tree_leaf("Trade_1", 12)
    )),
    tree_node("Desk_B", list(
      tree_leaf("Trade_2", 24),
      tree_leaf("Trade_3", -24)
    ))
  ))

  f_max_pos <- function(df) {
    if (nrow(df) == 0) 0 else max(0, sum(df$value))
  }

  shap_mc <- cShapleyMC(bank_tree, f_max_pos, n_samples = 5000, seed = 999)
  expect_equal(sum(shap_mc), 12, tolerance = 1e-10)
  expect_named(shap_mc, c("Trade_1", "Trade_2", "Trade_3"), ignore.order = TRUE)
})

test_that("cShapleyMC works with data.tree objects", {
  skip_if_not_installed("data.tree")

  root_dt <- data.tree::Node$new("top")
  g1 <- root_dt$AddChild("group1")
  g1$AddChild("x", value = 10)
  g1$AddChild("y", value = 20)

  shap_dt <- cShapleyMC(root_dt, "sum", n_samples = 50, seed = 1)
  expect_equal(shap_dt[["x"]], 10)
  expect_equal(shap_dt[["y"]], 20)
  expect_equal(sum(shap_dt), 30)
})

test_that("cshapleyMC alias and parameter aliases work correctly", {
  tree <- tree_node("root", list(
    tree_node("sub", list(tree_leaf("x", 10), tree_leaf("y", 20)))
  ))

  # Alias cshapleyMC
  shap1 <- cshapleyMC(tree, "sum", n_samples = 50, seed = 42)
  expect_equal(shap1[["x"]], 10)

  # Parameter alias samples
  shap2 <- cShapleyMC(tree, "sum", samples = 50, seed = 42)
  expect_identical(shap1, shap2)

  # Parameter alias nSamples
  shap3 <- cShapleyMC(tree, "sum", nSamples = 50, seed = 42)
  expect_identical(shap1, shap3)
})

test_that("cShapleyMC performs input validation correctly", {
  tree <- tree_node("root", list(tree_leaf("x", 10)))

  expect_error(cShapleyMC(tree, "sum", n_samples = 0), "n_samples must be a positive integer")
  expect_error(cShapleyMC(tree, "sum", n_samples = -10), "n_samples must be a positive integer")
  expect_error(cShapleyMC(tree, "sum", n_samples = "ten"), "n_samples must be a positive integer")
  expect_error(cShapleyMC(tree, "sum", seed = "invalid"), "seed must be a single numeric value")
  expect_error(cShapleyMC(tree, "unknown_op"), "should be one of")
})
