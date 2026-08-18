# Example script demonstrating TreeCalcs usage
library(TreeCalcs)

cat("=====================================================\n")
cat("1. Building a Tree in R\n")
cat("=====================================================\n")
# Create root with 2 internal branches, each having 2 leaves
my_tree <- tree_node("root", list(
  tree_node("group_A", list(
    tree_leaf("item_1", 8),
    tree_leaf("item_2", 2)
  )),
  tree_node("group_B", list(
    tree_leaf("item_3", 5),
    tree_leaf("item_4", 1)
  ))
))

print_tree(my_tree)

cat("\n=====================================================\n")
cat("2. Generating All Tree Permutations (C++20)\n")
cat("=====================================================\n")
all_perms <- permute_tree(my_tree, as_data_tree = FALSE)
cat(sprintf("Generated %d distinct tree permutations:\n\n", length(all_perms)))

for (i in seq_along(all_perms)) {
  cat(sprintf("--- Permutation %d ---\n", i))
  print_tree(all_perms[[i]])
}

cat("\n=====================================================\n")
cat("3. Tree Traversal (Leaf extraction)\n")
cat("=====================================================\n")
df <- traverse_tree(my_tree)
print(df)

cat("\n=====================================================\n")
cat("4. Tree Folding / Reductions\n")
cat("=====================================================\n")
cat("Sum:   ", fold_tree(my_tree, "sum"), "\n")
cat("Max:   ", fold_tree(my_tree, "max"), "\n")
cat("Min:   ", fold_tree(my_tree, "min"), "\n")
cat("Count: ", fold_tree(my_tree, "count"), "\n")
cat("Prod:  ", fold_tree(my_tree, "prod"), "\n")

cat("\n=====================================================\n")
cat("5. Tree Propagation (computing internal node values)\n")
cat("=====================================================\n")
propagated <- propagate_tree(my_tree, op = "sum", as_data_tree = FALSE)
cat("Tree with auto-computed internal sums:\n")
print_tree(propagated)

cat("\nNote: When 'data.tree' is installed, all functions\n")
cat("seamlessly accept and return data.tree Node objects!\n")
