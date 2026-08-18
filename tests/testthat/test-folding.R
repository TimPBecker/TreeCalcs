library(TreeCalcs)

test_that("Tree folding correctly reduces tree to scalar", {
  tree <- tree_node("root_node", list(
    tree_node("group1", list(
      tree_leaf("a", 10),
      tree_leaf("b", 20)
    )),
    tree_node("group2", list(
      tree_leaf("c", 30),
      tree_leaf("d", 40)
    ))
  ))

  expect_equal(fold_tree(tree, "sum"), 100)
  expect_equal(fold_tree(tree, "count"), 4)
  expect_equal(fold_tree(tree, "max"), 40)
  expect_equal(fold_tree(tree, "min"), 10)
  expect_equal(fold_tree(tree, "prod"), 240000) # 10 * 20 * 30 * 40
})

test_that("Tree folding works on single-leaf tree", {
  tree <- tree_leaf("single", 42)
  expect_equal(fold_tree(tree, "sum"), 42)
  expect_equal(fold_tree(tree, "count"), 1)
  expect_equal(fold_tree(tree, "max"), 42)
  expect_equal(fold_tree(tree, "min"), 42)
})

test_that("Tree folding on data.tree objects matches nested list results", {
  skip_if_not_installed("data.tree")

  root_dt <- data.tree::Node$new("top")
  g1 <- root_dt$AddChild("g1")
  g1$AddChild("leaf1", value = 5)
  g1$AddChild("leaf2", value = 15)

  expect_equal(fold_tree(root_dt, "sum"), 20)
  expect_equal(fold_tree(root_dt, "count"), 2)
})
