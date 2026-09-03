#' Check if an object is a data.tree Node
#'
#' @param x An R object.
#' @return Logical indicating if x inherits from 'Node'.
#' @keywords internal
is_datatree_node <- function(x) {
  inherits(x, "Node")
}

#' Convert an object to a nested list suitable for C++ tree processing
#'
#' @param x A `data.tree` Node object or a nested list.
#' @return A nested list with `name`, `value`, and `children`.
#' @export
to_tree_list <- function(x) {
  if (is_datatree_node(x)) {
    if (!requireNamespace("data.tree", quietly = TRUE)) {
      stop("Package 'data.tree' is required to convert data.tree objects.")
    }
    return(data.tree::ToListExplicit(x, unname = TRUE))
  } else if (is.list(x)) {
    return(x)
  } else {
    stop("Input must be a data.tree Node or a nested list.")
  }
}

#' Convert a nested tree list into a data.tree Node (if data.tree is available)
#'
#' @param l A nested list.
#' @param as_data_tree Logical; if TRUE, converts to data.tree Node. If FALSE, returns list.
#'   If NULL (default), converts to data.tree if package is installed.
#' @return A `data.tree` Node or nested list.
#' @export
from_tree_list <- function(l, as_data_tree = NULL) {
  if (is.null(as_data_tree)) {
    as_data_tree <- requireNamespace("data.tree", quietly = TRUE)
  }
  
  if (as_data_tree) {
    if (!requireNamespace("data.tree", quietly = TRUE)) {
      stop("Package 'data.tree' is not installed.")
    }
    return(data.tree::as.Node(l, mode = "explicit"))
  }
  l
}

#' Helper to create an internal node list
#'
#' @param name Character string; key/name of the internal node.
#' @param children List of child nodes.
#' @param value Optional numeric value for the internal node.
#' @return A list representing an internal node.
#' @export
tree_node <- function(name, children = list(), value = NULL) {
  out <- list(name = as.character(name), children = children)
  if (!is.null(value)) {
    out$value <- as.numeric(value)
  }
  out
}

#' Helper to create a leaf node list
#'
#' @param name Character string; key/name of the leaf node.
#' @param value Numeric value stored in the leaf.
#' @return A list representing a leaf node.
#' @export
tree_leaf <- function(name, value = 0.0) {
  list(name = as.character(name), value = as.numeric(value))
}

#' Permute tree subtrees across all levels
#'
#' Generates all distinct tree permutations by sorting and permuting child subtrees
#' at every internal node level using C++20.
#'
#' @param tree A `data.tree` Node or nested tree list.
#' @param as_data_tree Logical; whether to return results as `data.tree` Node objects.
#'   Defaults to TRUE if input was a data.tree Node, or if data.tree is installed.
#' @return A list of permuted trees (`data.tree` nodes or nested lists).
#' @examples
#' \dontrun{
#' root <- tree_node("root", list(
#'   tree_node("group1", list(tree_leaf("a", 8), tree_leaf("b", 2))),
#'   tree_node("group2", list(tree_leaf("c", 5), tree_leaf("d", 1)))
#' ))
#' permuted <- permute_tree(root)
#' length(permuted) # 8 distinct trees
#' }
#' @export
permute_tree <- function(tree, as_data_tree = NULL) {
  input_is_dt <- is_datatree_node(tree)
  if (is.null(as_data_tree)) {
    as_data_tree <- input_is_dt || requireNamespace("data.tree", quietly = TRUE)
  }
  
  l <- to_tree_list(tree)
  res_lists <- cpp_permute_tree(l)
  
  if (as_data_tree && requireNamespace("data.tree", quietly = TRUE)) {
    lapply(res_lists, function(x) data.tree::as.Node(x, mode = "explicit"))
  } else {
    res_lists
  }
}

#' Propagate leaf values up through internal nodes
#'
#' Computes internal node values recursively from their children using C++.
#'
#' @param tree A `data.tree` Node or nested tree list.
#' @param op Aggregation operation: 'sum', 'prod', 'mean', 'max', or 'min'.
#' @param as_data_tree Logical; whether to return the result as a `data.tree` Node.
#' @return A tree with computed internal node values.
#' @examples
#' \dontrun{
#' root <- tree_node("root", list(
#'   tree_node("sub1", list(tree_leaf("x", 10), tree_leaf("y", 20))),
#'   tree_node("sub2", list(tree_leaf("z", 30)))
#' ))
#' prop <- propagate_tree(root, op = "sum")
#' }
#' @export
propagate_tree <- function(tree, op = c("sum", "prod", "mean", "max", "min"), as_data_tree = NULL) {
  op <- match.arg(op)
  input_is_dt <- is_datatree_node(tree)
  if (is.null(as_data_tree)) {
    as_data_tree <- input_is_dt || requireNamespace("data.tree", quietly = TRUE)
  }
  
  l <- to_tree_list(tree)
  res_list <- cpp_propagate_tree(l, op = op)
  from_tree_list(res_list, as_data_tree = as_data_tree)
}

#' Fold a tree to a single scalar value
#'
#' Reduces all values across the tree using a combine function in C++.
#'
#' @param tree A `data.tree` Node or nested tree list.
#' @param op Reduction operator: 'sum', 'count', 'max', 'min', or 'prod'.
#' @return A numeric scalar value.
#' @examples
#' \dontrun{
#' root <- tree_node("root", list(
#'   tree_leaf("a", 10),
#'   tree_leaf("b", 20)
#' ))
#' fold_tree(root, "sum") # 30
#' }
#' @export
fold_tree <- function(tree, op = c("sum", "count", "max", "min", "prod")) {
  op <- match.arg(op)
  l <- to_tree_list(tree)
  cpp_fold_tree(l, op = op)
}

#' Traverse a tree and return leaf keys and values
#'
#' @param tree A `data.tree` Node or nested tree list.
#' @return A `data.frame` containing leaf keys and values in traversal order.
#' @export
traverse_tree <- function(tree) {
  l <- to_tree_list(tree)
  cpp_traverse_tree(l)
}

#' Print ASCII visual representation of a tree
#'
#' @param tree A `data.tree` Node or nested tree list.
#' @return Invisibly returns the formatted string while printing to console.
#' @export
print_tree <- function(tree) {
  l <- to_tree_list(tree)
  str_repr <- cpp_tree_to_string(l)
  cat(str_repr)
  invisible(str_repr)
}

#' Compute hierarchical tree-structured Shapley values (cShapley)
#'
#' Computes the Shapley values of all leaf nodes subject to the hierarchical
#' grouping constraints imposed by the tree structure.
#'
#' @param tree A `data.tree` Node or nested tree list.
#' @param calc_func A function that accepts a data.frame with columns `key` and `value`
#'   representing a coalition of leaf nodes, and returns a single numeric value.
#'   Alternatively, a character string specifying `"sum"`, `"prod"`, `"mean"`, `"max"`, or `"min"`.
#' @return A named numeric vector of Shapley values for each leaf node.
#' @references
#' Li, Y., Naldi, M., Nisen, J., & Shi, Y. (2016). Organising the allocation. *Risk*, March 2016.
#' @examples
#' root <- tree_node("root_node", list(
#'   tree_node("group1", list(tree_leaf("a", 8), tree_leaf("b", 2))),
#'   tree_node("group2", list(tree_leaf("c", 5), tree_leaf("d", 1)))
#' ))
#' # Linear value function: sum of leaves
#' cshapley(root, "sum")
#'
#' # Non-linear custom value function: squared sum
#' cshapley(root, function(df) sum(df$value)^2)
#' @export
cshapley <- function(tree, calc_func = "sum") {
  if (is.character(calc_func)) {
    op <- match.arg(calc_func, c("sum", "prod", "mean", "max", "min"))
    calc_func <- switch(op,
      "sum" = function(df) if (nrow(df) == 0) 0 else sum(df$value),
      "prod" = function(df) if (nrow(df) == 0) 1 else prod(df$value),
      "mean" = function(df) if (nrow(df) == 0) 0 else mean(df$value),
      "max" = function(df) if (nrow(df) == 0) 0 else max(df$value),
      "min" = function(df) if (nrow(df) == 0) 0 else min(df$value)
    )
  }

  if (!is.function(calc_func)) {
    stop("calc_func must be an R function or one of 'sum', 'prod', 'mean', 'max', 'min'.")
  }

  l <- to_tree_list(tree)
  cpp_cshapley(l, calc_func)
}

#' Compute hierarchical tree-structured Shapley values via Monte Carlo simulation (cShapleyMC)
#'
#' Estimates hierarchical Shapley values of all leaf nodes by sampling random
#' tree permutations rather than enumerating all permutations.
#'
#' @param tree A `data.tree` Node or nested tree list.
#' @param calc_func A function that accepts a data.frame with columns `key` and `value`
#'   representing a coalition of leaf nodes, and returns a single numeric value.
#'   Alternatively, a character string specifying `"sum"`, `"prod"`, `"mean"`, `"max"`, or `"min"`.
#' @param n_samples Positive integer specifying the number of Monte Carlo permutation samples. Default is 1000.
#' @param seed Optional numeric seed for reproducible random sampling. Default is NULL.
#' @param ... Additional optional arguments (such as aliases `samples` or `nSamples`).
#' @return A named numeric vector of Shapley values for each leaf node.
#' @references
#' Li, Y., Naldi, M., Nisen, J., & Shi, Y. (2016). Organising the allocation. *Risk*, March 2016.
#' @examples
#' root <- tree_node("root_node", list(
#'   tree_node("group1", list(tree_leaf("a", 8), tree_leaf("b", 2))),
#'   tree_node("group2", list(tree_leaf("c", 5), tree_leaf("d", 1)))
#' ))
#' # Monte Carlo estimation with linear value function:
#' cShapleyMC(root, "sum", n_samples = 100, seed = 42)
#'
#' # Non-linear custom value function:
#' cShapleyMC(root, function(df) sum(df$value)^2, n_samples = 500, seed = 42)
#' @export
cShapleyMC <- function(tree, calc_func = "sum", n_samples = 1000, seed = NULL, ...) {
  dots <- list(...)
  if (missing(n_samples)) {
    if (!is.null(dots$samples)) n_samples <- dots$samples
    else if (!is.null(dots$nSamples)) n_samples <- dots$nSamples
    else if (!is.null(dots$n_samples)) n_samples <- dots$n_samples
  }
  if (missing(seed)) {
    if (!is.null(dots$seed)) seed <- dots$seed
  }

  if (is.numeric(calc_func) && missing(n_samples)) {
    n_samples <- calc_func
    calc_func <- "sum"
  }

  if (!is.numeric(n_samples) || length(n_samples) != 1 || is.na(n_samples) || n_samples < 1) {
    stop("n_samples must be a positive integer.")
  }
  n_samples <- as.integer(n_samples)

  if (!is.null(seed)) {
    if (!is.numeric(seed) || length(seed) != 1 || is.na(seed)) {
      stop("seed must be a single numeric value or NULL.")
    }
  }

  if (is.character(calc_func)) {
    op <- match.arg(calc_func, c("sum", "prod", "mean", "max", "min"))
    calc_func <- switch(op,
      "sum" = function(df) if (nrow(df) == 0) 0 else sum(df$value),
      "prod" = function(df) if (nrow(df) == 0) 1 else prod(df$value),
      "mean" = function(df) if (nrow(df) == 0) 0 else mean(df$value),
      "max" = function(df) if (nrow(df) == 0) 0 else max(df$value),
      "min" = function(df) if (nrow(df) == 0) 0 else min(df$value)
    )
  }

  if (!is.function(calc_func)) {
    stop("calc_func must be an R function or one of 'sum', 'prod', 'mean', 'max', 'min'.")
  }

  l <- to_tree_list(tree)
  cpp_cshapley_mc(l, calc_func, n_samples, seed)
}

#' @rdname cShapleyMC
#' @export
cshapleyMC <- cShapleyMC


