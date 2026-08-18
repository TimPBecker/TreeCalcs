library(TreeCalcs)

test_that("Tree construction using helpers creates valid nested structure", {
  leaf1 <- tree_leaf("leaf_1", 10)
  leaf2 <- tree_leaf("leaf_2", 20)
  internal <- tree_node("internal_1", list(leaf1, leaf2))
  root <- tree_node("root_node", list(internal))

  expect_equal(root$name, "root_node")
  expect_equal(length(root$children), 1)
  expect_equal(root$children[[1]]$name, "internal_1")
  expect_equal(length(root$children[[1]]$children), 2)
  expect_equal(root$children[[1]]$children[[1]]$value, 10)
  expect_equal(root$children[[1]]$children[[2]]$value, 20)
})

test_that("Tree traversal extracts all leaves in correct order", {
  tree <- tree_node("root_node", list(
    tree_node("branchA", list(
      tree_leaf("a", 100),
      tree_leaf("b", 200)
    )),
    tree_node("branchB", list(
      tree_leaf("c", 300)
    ))
  ))

  df <- traverse_tree(tree)
  expect_s3_class(df, "data.frame")
  expect_equal(df$key, c("a", "b", "c"))
  expect_equal(df$value, c(100, 200, 300))
})

test_that("data.tree conversion works seamlessly both ways", {
  skip_if_not_installed("data.tree")

  root_dt <- data.tree::Node$new("top")
  b1 <- root_dt$AddChild("branch1")
  l1 <- b1$AddChild("leaf1", value = 42)
  l2 <- b1$AddChild("leaf2", value = 58)

  # Convert data.tree to list
  tree_list <- to_tree_list(root_dt)
  expect_type(tree_list, "list")
  expect_equal(tree_list$name, "top")

  # Traverse data.tree object directly
  df <- traverse_tree(root_dt)
  expect_equal(df$key, c("leaf1", "leaf2"))
  expect_equal(df$value, c(42, 58))
})
