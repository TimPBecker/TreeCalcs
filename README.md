# TreeCalcs

An R package providing high-performance tree algorithms powered by C++20 concepts and Rcpp.

## Features

- **C++20 Engine**: Header-only template core in [`src/Tree.h`](file:///home/tim/Projects/TreeCalcs/src/Tree.h).
- **Tree Permutations**: Recursively permutes child subtrees at all internal node levels (`permute_tree`).
- **Tree Traversal**: Fast leaf traversal and extraction into R `data.frame` (`traverse_tree`).
- **Tree Propagation**: Computes internal node values recursively from leaf values (`propagate_tree`).
- **Tree Folding / Reduction**: Reduces tree values into scalar aggregations (`fold_tree`).
- **Hierarchical cShapley**: Fast tree-structured Shapley values with memoization (`cshapley`).
- **Dual Representation**: Supports standard nested R lists as well as seamless conversion with `data.tree` objects.

## Installation

From the project directory:

```r
# Build and install
install.packages(".", repos = NULL, type = "source")
```

Or using `devtools`:

```r
devtools::install()
```

## Quick Example

```r
library(TreeCalcs)

# 1. Construct a tree
tree <- tree_node("root", list(
  tree_node("group_A", list(
    tree_leaf("leaf_1", 8),
    tree_leaf("leaf_2", 2)
  )),
  tree_node("group_B", list(
    tree_leaf("leaf_3", 5),
    tree_leaf("leaf_4", 1)
  ))
))

# 2. Print tree structure
print_tree(tree)

# 3. Generate all tree permutations (C++20)
perms <- permute_tree(tree)
length(perms) # 8 distinct permutations

# 4. Propagate internal node values (summing children)
prop <- propagate_tree(tree, op = "sum")
print_tree(prop)

# 5. Fold tree
sum_val <- fold_tree(tree, "sum") # 16
```

## data.tree Integration

If `data.tree` is installed, all functions accept `data.tree` `Node` objects and automatically return `data.tree` objects:

```r
library(data.tree)
library(TreeCalcs)

root <- Node$new("top")
g1 <- root$AddChild("group_A")
g1$AddChild("leaf_1", value = 8)
g1$AddChild("leaf_2", value = 2)

# Permute directly on data.tree
perms <- permute_tree(root)
print(perms[[1]], "value")
```

## References & Acknowledgments

The hierarchical tree-structured Shapley value (`cshapley`) algorithm implemented in this package is based on:

- **Li, Yadong, Marco Naldi, Jeffrey Nisen, and Yixi Shi** (2016). *Organising the allocation*. Risk.
