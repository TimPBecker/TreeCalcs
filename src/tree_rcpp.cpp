#include <Rcpp.h>
#include "Tree.h"
#include <numeric>
#include <cmath>

using KeyType = std::string;
using ValType = double;
using TreePtr = std::shared_ptr<Node<KeyType, ValType>>;

// [[Rcpp::plugins(cpp20)]]

// Helper: Convert an R list (compatible with data.tree ToListExplicit or nested lists) into C++ Node
TreePtr r_list_to_cpp_tree(Rcpp::List node_list) {
    if (!node_list.containsElementNamed("name") && !node_list.containsElementNamed("key")) {
        Rcpp::stop("Every node in the tree list must have a 'name' or 'key' field.");
    }
    
    std::string key = node_list.containsElementNamed("name") 
                        ? Rcpp::as<std::string>(node_list["name"]) 
                        : Rcpp::as<std::string>(node_list["key"]);
    
    // Check if children exist and is non-empty
    if (node_list.containsElementNamed("children")) {
        SEXP children_sexp = node_list["children"];
        if (!Rf_isNull(children_sexp)) {
            Rcpp::List children_list(children_sexp);
            if (children_list.size() > 0) {
                auto internal = std::make_shared<InternalNode<KeyType, ValType>>(key);
                if (node_list.containsElementNamed("value")) {
                    SEXP val_sexp = node_list["value"];
                    if (!Rf_isNull(val_sexp) && !Rcpp::NumericVector::is_na(Rcpp::as<double>(val_sexp))) {
                        internal->value = Rcpp::as<ValType>(val_sexp);
                    }
                }
                for (int i = 0; i < children_list.size(); ++i) {
                    Rcpp::List child_item = children_list[i];
                    internal->addChild(r_list_to_cpp_tree(child_item));
                }
                return internal;
            }
        }
    }
    
    // Leaf node: extract value
    ValType value = 0.0;
    if (node_list.containsElementNamed("value")) {
        SEXP val_sexp = node_list["value"];
        if (!Rf_isNull(val_sexp)) {
            value = Rcpp::as<ValType>(val_sexp);
        }
    }
    return std::make_shared<LeafNode<KeyType, ValType>>(key, value);
}

// Helper: Convert C++ Node to an R list (compatible with data.tree as.Node(..., mode = 'explicit'))
Rcpp::List cpp_tree_to_r_list(const TreePtr& node) {
    if (!node) return Rcpp::List::create();
    
    if (auto leaf = std::dynamic_pointer_cast<LeafNode<KeyType, ValType>>(node)) {
        return Rcpp::List::create(
            Rcpp::Named("name") = leaf->key,
            Rcpp::Named("value") = leaf->value
        );
    } else if (auto internal = std::dynamic_pointer_cast<InternalNode<KeyType, ValType>>(node)) {
        Rcpp::List children(internal->children.size());
        for (size_t i = 0; i < internal->children.size(); ++i) {
            children[i] = cpp_tree_to_r_list(internal->children[i]);
        }
        
        Rcpp::List out = Rcpp::List::create(
            Rcpp::Named("name") = internal->key,
            Rcpp::Named("children") = children
        );
        if (internal->value.has_value()) {
            out["value"] = internal->value.value();
        }
        return out;
    }
    return Rcpp::List::create();
}

//' Permute all subtrees of a tree in C++
//' 
//' Generates all valid permutations of children at each internal node level.
//' 
//' @param tree_list A nested list representing the tree hierarchy.
//' @return A list of nested lists representing all permuted trees.
//' @export
// [[Rcpp::export]]
Rcpp::List cpp_permute_tree(Rcpp::List tree_list) {
    auto cpp_root = r_list_to_cpp_tree(tree_list);
    auto internal = std::dynamic_pointer_cast<InternalNode<KeyType, ValType>>(cpp_root);
    if (!internal) {
        Rcpp::stop("Root must be an internal node with children to permute.");
    }
    
    auto permutations = internal->permuteTree();
    Rcpp::List out(permutations.size());
    for (size_t i = 0; i < permutations.size(); ++i) {
        out[i] = cpp_tree_to_r_list(permutations[i]);
    }
    return out;
}

//' Traverse a tree and extract leaf keys and values
//' 
//' Performs in-order type-aware traversal on the tree.
//' 
//' @param tree_list A nested list representing the tree hierarchy.
//' @return A DataFrame with columns 'key' and 'value'.
//' @export
// [[Rcpp::export]]
Rcpp::DataFrame cpp_traverse_tree(Rcpp::List tree_list) {
    auto cpp_root = r_list_to_cpp_tree(tree_list);
    std::vector<std::string> keys;
    std::vector<double> values;
    
    traverseTree<KeyType, ValType>(cpp_root, [&](const std::string& k, const double v) {
        keys.push_back(k);
        values.push_back(v);
    });
    
    return Rcpp::DataFrame::create(
        Rcpp::Named("key") = keys,
        Rcpp::Named("value") = values,
        Rcpp::Named("stringsAsFactors") = false
    );
}

//' Propagate values from leaves up to internal nodes
//' 
//' Computes internal node values by applying an aggregation operation
//' over children values recursively.
//' 
//' @param tree_list A nested list representing the tree hierarchy.
//' @param op Aggregation operator: 'sum', 'prod', 'mean', 'max', or 'min'.
//' @return A new nested list with internal node values populated.
//' @export
// [[Rcpp::export]]
Rcpp::List cpp_propagate_tree(Rcpp::List tree_list, std::string op = "sum") {
    auto cpp_root = r_list_to_cpp_tree(tree_list);
    TreePtr result;
    
    if (op == "sum") {
        result = propagateTree<KeyType, ValType>(cpp_root, [](const std::vector<ValType>& vals) {
            ValType total = 0.0;
            for (auto v : vals) total += v;
            return total;
        });
    } else if (op == "prod") {
        result = propagateTree<KeyType, ValType>(cpp_root, [](const std::vector<ValType>& vals) {
            ValType total = 1.0;
            for (auto v : vals) total *= v;
            return total;
        });
    } else if (op == "max") {
        result = propagateTree<KeyType, ValType>(cpp_root, [](const std::vector<ValType>& vals) {
            if (vals.empty()) return 0.0;
            return *std::max_element(vals.begin(), vals.end());
        });
    } else if (op == "min") {
        result = propagateTree<KeyType, ValType>(cpp_root, [](const std::vector<ValType>& vals) {
            if (vals.empty()) return 0.0;
            return *std::min_element(vals.begin(), vals.end());
        });
    } else if (op == "mean") {
        result = propagateTree<KeyType, ValType>(cpp_root, [](const std::vector<ValType>& vals) {
            if (vals.empty()) return 0.0;
            ValType total = 0.0;
            for (auto v : vals) total += v;
            return total / static_cast<ValType>(vals.size());
        });
    } else {
        Rcpp::stop("Unsupported propagation operator. Use 'sum', 'prod', 'mean', 'max', or 'min'.");
    }
    
    return cpp_tree_to_r_list(result);
}

//' Fold a tree using aggregation functions
//' 
//' Recursively reduces the tree from leaves to a single scalar result.
//' 
//' @param tree_list A nested list representing the tree hierarchy.
//' @param op Reduction operator: 'sum', 'prod', 'mean', 'max', 'min', or 'count'.
//' @return A numeric scalar with the folded result.
//' @export
// [[Rcpp::export]]
double cpp_fold_tree(Rcpp::List tree_list, std::string op = "sum") {
    auto cpp_root = r_list_to_cpp_tree(tree_list);
    
    if (op == "sum") {
        return foldTree<KeyType, ValType>(
            cpp_root,
            [](const ValType& val) { return val; },
            [](const std::vector<double>& results) {
                double total = 0;
                for (double r : results) total += r;
                return total;
            }
        );
    } else if (op == "count") {
        return foldTree<KeyType, ValType>(
            cpp_root,
            [](const ValType&) { return 1.0; },
            [](const std::vector<double>& results) {
                double total = 0;
                for (double r : results) total += r;
                return total;
            }
        );
    } else if (op == "max") {
        return foldTree<KeyType, ValType>(
            cpp_root,
            [](const ValType& val) { return val; },
            [](const std::vector<double>& results) {
                if (results.empty()) return 0.0;
                return *std::max_element(results.begin(), results.end());
            }
        );
    } else if (op == "min") {
        return foldTree<KeyType, ValType>(
            cpp_root,
            [](const ValType& val) { return val; },
            [](const std::vector<double>& results) {
                if (results.empty()) return 0.0;
                return *std::min_element(results.begin(), results.end());
            }
        );
    } else if (op == "prod") {
        return foldTree<KeyType, ValType>(
            cpp_root,
            [](const ValType& val) { return val; },
            [](const std::vector<double>& results) {
                double total = 1.0;
                for (double r : results) total *= r;
                return total;
            }
        );
    } else {
        Rcpp::stop("Unsupported fold operator. Use 'sum', 'count', 'max', 'min', or 'prod'.");
    }
}

//' Generate ASCII visual representation of a tree
//' 
//' @param tree_list A nested list representing the tree hierarchy.
//' @return A character string showing the ASCII tree.
//' @export
// [[Rcpp::export]]
std::string cpp_tree_to_string(Rcpp::List tree_list) {
    auto cpp_root = r_list_to_cpp_tree(tree_list);
    return treeToString<KeyType, ValType>(cpp_root);
}

//' Compute hierarchical cShapley values for tree leaves
//' 
//' @param tree_list A nested list representing the tree hierarchy.
//' @param calc_func An R function taking a DataFrame of leaves with columns 'key' and 'value'.
//' @return A named numeric vector of Shapley values for each leaf.
//' @export
// [[Rcpp::export]]
Rcpp::NumericVector cpp_cshapley(Rcpp::List tree_list, Rcpp::Function calc_func) {
    auto cpp_root = r_list_to_cpp_tree(tree_list);
    
    auto cpp_calc = [&](const std::vector<std::shared_ptr<LeafNode<KeyType, ValType>>>& leafSet) -> double {
        std::vector<std::string> keys;
        std::vector<double> values;
        keys.reserve(leafSet.size());
        values.reserve(leafSet.size());
        for (const auto& l : leafSet) {
            keys.push_back(l->key);
            values.push_back(l->value);
        }
        
        Rcpp::DataFrame df = Rcpp::DataFrame::create(
            Rcpp::Named("key") = keys,
            Rcpp::Named("value") = values,
            Rcpp::Named("stringsAsFactors") = false
        );
        
        SEXP res = calc_func(df);
        return Rcpp::as<double>(res);
    };
    
    auto shapleyMap = cShapley<KeyType, ValType>(cpp_root, cpp_calc);
    
    Rcpp::NumericVector out(shapleyMap.size());
    Rcpp::CharacterVector names(shapleyMap.size());
    size_t idx = 0;
    for (const auto& [k, v] : shapleyMap) {
        out[idx] = v;
        names[idx] = k;
        idx++;
    }
    out.attr("names") = names;
    return out;
}

//' Compute hierarchical cShapley values for tree leaves via Monte Carlo sampling
//' 
//' @param tree_list A nested list representing the tree hierarchy.
//' @param calc_func An R function taking a DataFrame of leaves with columns 'key' and 'value'.
//' @param n_samples Number of Monte Carlo permutation samples.
//' @param seed Optional random seed.
//' @return A named numeric vector of Shapley values for each leaf.
//' @export
// [[Rcpp::export]]
Rcpp::NumericVector cpp_cshapley_mc(Rcpp::List tree_list, Rcpp::Function calc_func, int n_samples, Rcpp::Nullable<double> seed = R_NilValue) {
    if (n_samples <= 0) {
        Rcpp::stop("n_samples must be a positive integer.");
    }
    auto cpp_root = r_list_to_cpp_tree(tree_list);
    
    auto cpp_calc = [&](const std::vector<std::shared_ptr<LeafNode<KeyType, ValType>>>& leafSet) -> double {
        std::vector<std::string> keys;
        std::vector<double> values;
        keys.reserve(leafSet.size());
        values.reserve(leafSet.size());
        for (const auto& l : leafSet) {
            keys.push_back(l->key);
            values.push_back(l->value);
        }
        
        Rcpp::DataFrame df = Rcpp::DataFrame::create(
            Rcpp::Named("key") = keys,
            Rcpp::Named("value") = values,
            Rcpp::Named("stringsAsFactors") = false
        );
        
        SEXP res = calc_func(df);
        return Rcpp::as<double>(res);
    };
    
    std::optional<uint64_t> cpp_seed = std::nullopt;
    if (seed.isNotNull()) {
        cpp_seed = static_cast<uint64_t>(Rcpp::as<double>(seed));
    } else {
        Rcpp::RNGScope scope;
        double r_unif = R::unif_rand();
        cpp_seed = static_cast<uint64_t>(r_unif * 9007199254740991.0);
    }
    
    auto shapleyMap = cShapleyMC<KeyType, ValType>(cpp_root, cpp_calc, static_cast<size_t>(n_samples), cpp_seed);
    
    Rcpp::NumericVector out(shapleyMap.size());
    Rcpp::CharacterVector names(shapleyMap.size());
    size_t idx = 0;
    for (const auto& [k, v] : shapleyMap) {
        out[idx] = v;
        names[idx] = k;
        idx++;
    }
    out.attr("names") = names;
    return out;
}

