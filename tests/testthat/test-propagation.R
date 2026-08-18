library(TreeCalcs)

test_that("Tree propagation with sum computes correct values across all levels", {
  tree <- tree_node("root_node", list(
    tree_node("internal1", list(
      tree_leaf("leaf_a", 10),
      tree_leaf("leaf_b", 20)
    )),
    tree_node("internal2", list(
      tree_leaf("leaf_c", 30),
      tree_leaf("leaf_d", 40)
    ))
  ))

  prop <- propagate_tree(tree, op = "sum", as_data_tree = FALSE)

  # leaf values unchanged
  expect_equal(prop$children[[1]]$children[[1]]$value, 10)
  expect_equal(prop$children[[1]]$children[[2]]$value, 20)
  expect_equal(prop$children[[2]]$children[[1]]$value, 30)
  expect_equal(prop$children[[2]]$children[[2]]$value, 40)

  # internal node values
  expect_equal(prop$children[[1]]$value, 30)  # 10 + 20
  expect_equal(prop$children[[2]]$value, 70)  # 30 + 40
  expect_equal(prop$value, 100)               # 30 + 70
})

test_that("Tree propagation with max, min, prod, and mean operators", {
  tree <- tree_node("root_node", list(
    tree_node("branch", list(
      tree_leaf("x", 2),
      tree_leaf("y", 8)
    ))
  ))

  prop_max <- propagate_tree(tree, op = "max", as_data_tree = FALSE)
  expect_equal(prop_max$children[[1]]$value, 8)
  expect_equal(prop_max$value, 8)

  prop_min <- propagate_tree(tree, op = "min", as_data_tree = FALSE)
  expect_equal(prop_min$children[[1]]$value, 2)
  expect_equal(prop_min$value, 2)

  prop_prod <- propagate_tree(tree, op = "prod", as_data_tree = FALSE)
  expect_equal(prop_prod$children[[1]]$value, 16)
  expect_equal(prop_prod$value, 16)

  prop_mean <- propagate_tree(tree, op = "mean", as_data_tree = FALSE)
  expect_equal(prop_mean$children[[1]]$value, 5)
  expect_equal(prop_mean$value, 5)
})

test_that("Tree propagation works on data.tree objects", {
  skip_if_not_installed("data.tree")

  root_dt <- data.tree::Node$new("top")
  sub1 <- root_dt$AddChild("sub1")
  sub1$AddChild("l1", value = 15)
  sub1$AddChild("l2", value = 25)
  sub2 <- root_dt$AddChild("sub2")
  sub2$AddChild("l3", value = 60)

  res_dt <- propagate_tree(root_dt, op = "sum", as_data_tree = TRUE)

  expect_true(inherits(res_dt, "Node"))
  expect_equal(res_dt$value, 100) # (15 + 25) + 60
  expect_equal(res_dt$children[[1]]$value, 40)
  expect_equal(res_dt$children[[2]]$value, 60)
})
