library(TreeCalcs)

test_that("Tree permutation generates expected number of distinct permutations", {
  tree <- tree_node("root_node", list(
    tree_node("branch1", list(
      tree_leaf("a", 8),
      tree_leaf("b", 2)
    )),
    tree_node("branch2", list(
      tree_leaf("c", 5),
      tree_leaf("d", 1)
    ))
  ))

  perms <- permute_tree(tree, as_data_tree = FALSE)
  # Level 1 has 2 children (2! = 2)
  # Level 2 has two nodes with 2 children each (2! * 2! = 4)
  # Total permutations = 2 * 4 = 8
  expect_equal(length(perms), 8)

  # Check that each permuted tree has the same leaf sum
  sums <- sapply(perms, function(p) fold_tree(p, "sum"))
  expect_true(all(sums == 16))
})

test_that("Tree permutation works with identical leaf values (keyed differentiation)", {
  # Nodes have identical values but distinct keys
  tree <- tree_node("root_node", list(
    tree_node("branch1", list(
      tree_leaf("a", 10),
      tree_leaf("b", 10)
    )),
    tree_node("branch2", list(
      tree_leaf("c", 10),
      tree_leaf("d", 10)
    ))
  ))

  perms <- permute_tree(tree, as_data_tree = FALSE)
  expect_equal(length(perms), 8)
})

test_that("Tree permutation works with data.tree objects", {
  skip_if_not_installed("data.tree")

  root_dt <- data.tree::Node$new("top")
  b1 <- root_dt$AddChild("b1")
  b1$AddChild("x", value = 3)
  b1$AddChild("y", value = 7)

  perms_dt <- permute_tree(root_dt, as_data_tree = TRUE)
  expect_equal(length(perms_dt), 2)
  expect_true(all(sapply(perms_dt, function(p) inherits(p, "Node"))))
})
