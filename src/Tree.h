#ifndef TREE_H
#define TREE_H

#include <concepts>
#include <iostream>
#include <sstream>
#include <vector>
#include <memory>
#include <algorithm>
#include <functional>
#include <optional>
#include <string>
#include <map>
#include <unordered_map>
#include <cstdint>
#include <numeric>

template <typename K>
concept TreeKey = std::totally_ordered<K>;

template <typename V>
concept TreeValue = std::movable<V>;

// 1. ABSTRACT BASE CLASS
template <TreeKey K, TreeValue V>
struct Node {
    K key;
    virtual ~Node() = default; 
    explicit Node(K k) : key(std::move(k)) {}
};

// 2. LEAF NODE: Strictly holds data. (No children allowed)
template <TreeKey K, TreeValue V>
struct LeafNode : public Node<K, V> {
    V value;
    
    LeafNode(K k, V v) : Node<K, V>(std::move(k)), value(std::move(v)) {}
};

// 3. INTERNAL NODE: Can hold structure and optionally values
template <TreeKey K, TreeValue V>
struct InternalNode : public Node<K, V> {
    std::vector<std::shared_ptr<Node<K, V>>> children;
    std::optional<V> value;  // Optional value for internal nodes

    explicit InternalNode(K k) : Node<K, V>(std::move(k)), value(std::nullopt) {}

    void addChild(std::shared_ptr<Node<K, V>> child) { 
        children.push_back(child); 
    }
    
    bool hasValue() const {
        return value.has_value();
    }

    // Helper: Try to increment indices to next combination, returns true if successful
    template <typename Container>
    static bool nextCombination(std::vector<size_t>& indices, const Container& childPermutations) {
        if (childPermutations.empty()) return false;
        size_t pos = childPermutations.size() - 1;
        while (pos < childPermutations.size()) {
            indices[pos]++;
            if (indices[pos] < childPermutations[pos].size()) {
                return true;  // Successfully moved to next combination
            }
            indices[pos] = 0;
            if (pos == 0) {
                return false;  // No more combinations
            }
            pos--;
        }
        return false;
    }

    std::vector<std::shared_ptr<Node<K, V>>> permuteTree() {
        std::vector<std::shared_ptr<Node<K, V>>> permutedTrees;

        if (children.empty()) {
            return {std::make_shared<InternalNode<K, V>>(this->key)};
        }

        // Step 1: Get all permutations of each child's subtree
        std::vector<std::vector<std::shared_ptr<Node<K, V>>>> childPermutations;
        for (const auto& child : children) {
            if (auto internal = std::dynamic_pointer_cast<InternalNode<K, V>>(child)) {
                childPermutations.push_back(internal->permuteTree());
            } else {
                // Leaf node, just return itself
                childPermutations.push_back({child});
            }
        }

        // Step 2: Generate cartesian product of all child permutations
        std::vector<size_t> indices(childPermutations.size(), 0);
        
        // Comparator for sorting children by Key (nodes are distinguished by key)
        auto comp = [](const std::shared_ptr<Node<K, V>>& a, const std::shared_ptr<Node<K, V>>& b) {
            return a->key < b->key;
        };
        
        do {
            // Get current combination of children
            std::vector<std::shared_ptr<Node<K, V>>> currentChildren;
            for (size_t i = 0; i < childPermutations.size(); ++i) {
                currentChildren.push_back(childPermutations[i][indices[i]]);
            }

            // Step 3: Generate all orderings of children in current combination
            std::sort(currentChildren.begin(), currentChildren.end(), comp);

            do {
                auto newParent = std::make_shared<InternalNode<K, V>>(this->key);
                if (this->value.has_value()) {
                    newParent->value = this->value;
                }
                newParent->children = currentChildren;
                permutedTrees.push_back(newParent);
            } while (std::next_permutation(currentChildren.begin(), currentChildren.end(), comp));

        } while (nextCombination(indices, childPermutations));

        return permutedTrees;
    }
};

// --- TYPE-AWARE TRAVERSAL ---
template <TreeKey K, TreeValue V, typename LeafVisitor>
void traverseTree(const std::shared_ptr<Node<K, V>>& node, LeafVisitor visitLeaf) {
    if (!node) return;
    
    // If it's a leaf, extract the key and value and pass to visitor
    if (auto leaf = std::dynamic_pointer_cast<LeafNode<K, V>>(node)) {
        visitLeaf(leaf->key, leaf->value);
    } 
    // If it's an internal node, optionally visit its value, then dive deeper into children
    else if (auto internal = std::dynamic_pointer_cast<InternalNode<K, V>>(node)) {
        if (internal->value.has_value()) {
            visitLeaf(internal->key, internal->value.value());
        }
        for (const auto& child : internal->children) {
            traverseTree(child, visitLeaf);
        }
    }
}

// --- TREE PROPAGATION: Compute internal node values from children ---
template <TreeKey K, TreeValue V, typename F>
requires std::invocable<F, const std::vector<V>&> &&
         std::same_as<std::invoke_result_t<F, const std::vector<V>&>, V>
std::shared_ptr<Node<K, V>> propagateTree(
    const std::shared_ptr<Node<K, V>>& node,
    F aggregateFunc) {
    
    if (!node) return nullptr;
    
    // If it's a leaf, return it as-is (leaves already have values)
    if (auto leaf = std::dynamic_pointer_cast<LeafNode<K, V>>(node)) {
        return leaf;
    }
    
    // If it's an internal node, recursively propagate children and compute this node's value
    if (auto internal = std::dynamic_pointer_cast<InternalNode<K, V>>(node)) {
        auto newInternal = std::make_shared<InternalNode<K, V>>(internal->key);
        
        // Recursively propagate all children
        std::vector<V> childValues;
        for (const auto& child : internal->children) {
            auto propagatedChild = propagateTree(child, aggregateFunc);
            newInternal->addChild(propagatedChild);
            
            // Extract value from propagated child
            if (auto childLeaf = std::dynamic_pointer_cast<LeafNode<K, V>>(propagatedChild)) {
                childValues.push_back(childLeaf->value);
            } else if (auto childInternal = std::dynamic_pointer_cast<InternalNode<K, V>>(propagatedChild)) {
                if (childInternal->value.has_value()) {
                    childValues.push_back(childInternal->value.value());
                }
            }
        }
        
        // Compute this internal node's value from children
        if (!childValues.empty()) {
            newInternal->value = aggregateFunc(childValues);
        }
        
        return newInternal;
    }
    
    return nullptr;
}

// --- TREE FOLDING/AGGREGATION ---
template <TreeKey K, TreeValue V, typename LeafFunc, typename CombineFunc>
requires std::invocable<LeafFunc, const V&> &&
         std::invocable<CombineFunc, const std::vector<std::invoke_result_t<LeafFunc, const V&>>&> &&
         std::same_as<std::invoke_result_t<CombineFunc, const std::vector<std::invoke_result_t<LeafFunc, const V&>>&>, std::invoke_result_t<LeafFunc, const V&>>
auto foldTree(const std::shared_ptr<Node<K, V>>& node,
              LeafFunc leafFunc,
              CombineFunc combineFunc) {
    using R = std::invoke_result_t<LeafFunc, const V&>;
    if (!node) return R{};
    
    // If it's a leaf, apply leafFunc to the value
    if (auto leaf = std::dynamic_pointer_cast<LeafNode<K, V>>(node)) {
        return leafFunc(leaf->value);
    } 
    // If it's an internal node, recursively fold all children and combine results
    else if (auto internal = std::dynamic_pointer_cast<InternalNode<K, V>>(node)) {
        std::vector<R> childResults;
        // Include internal node's value if it has one
        if (internal->value.has_value()) {
            childResults.push_back(leafFunc(internal->value.value()));
        }
        for (const auto& child : internal->children) {
            childResults.push_back(foldTree(child, leafFunc, combineFunc));
        }
        return combineFunc(childResults);
    }
    
    return R{};
}

// --- GRAPHICAL TREE PRINTING (internal implementation) ---
template <TreeKey K, TreeValue V>
void _printTreeStream(const std::shared_ptr<Node<K, V>>& node, std::ostream& os, const std::string& prefix = "", bool isLast = true) {
    if (!node) return;
    
    os << prefix;
    os << (isLast ? "└── " : "├── ");
    
    if (auto leaf = std::dynamic_pointer_cast<LeafNode<K, V>>(node)) {
        os << "[Leaf: key=" << leaf->key << ", value=" << leaf->value << "]\n";
    } 
    else if (auto internal = std::dynamic_pointer_cast<InternalNode<K, V>>(node)) {
        os << "[Internal: key=" << internal->key;
        if (internal->value.has_value()) {
            os << ", value=" << internal->value.value();
        }
        os << "]\n";
        
        for (size_t i = 0; i < internal->children.size(); ++i) {
            bool isLastChild = (i == internal->children.size() - 1);
            std::string newPrefix = prefix + (isLast ? "    " : "│   ");
            _printTreeStream(internal->children[i], os, newPrefix, isLastChild);
        }
    }
}

template <TreeKey K, TreeValue V>
void _printTree(const std::shared_ptr<Node<K, V>>& node, const std::string& prefix = "", bool isLast = true) {
    _printTreeStream(node, std::cout, prefix, isLast);
}

// --- TREE PRINTING (public interface) ---
template <TreeKey K, TreeValue V>
void printTree(const std::shared_ptr<Node<K, V>>& node, std::ostream& os = std::cout) {
    _printTreeStream(node, os, "", true);
}

template <TreeKey K, TreeValue V>
std::string treeToString(const std::shared_ptr<Node<K, V>>& node) {
    std::ostringstream oss;
    _printTreeStream(node, oss, "", true);
    return oss.str();
}

// --- HELPER: EXTRACT LEAVES IN TRAVERSAL ORDER ---
template <TreeKey K, TreeValue V>
void _collectLeafNodes(const std::shared_ptr<Node<K, V>>& node, std::vector<std::shared_ptr<LeafNode<K, V>>>& leaves) {
    if (!node) return;
    if (auto leaf = std::dynamic_pointer_cast<LeafNode<K, V>>(node)) {
        leaves.push_back(leaf);
    } else if (auto internal = std::dynamic_pointer_cast<InternalNode<K, V>>(node)) {
        for (const auto& child : internal->children) {
            _collectLeafNodes(child, leaves);
        }
    }
}

template <TreeKey K, TreeValue V>
std::vector<std::shared_ptr<LeafNode<K, V>>> getLeafNodes(const std::shared_ptr<Node<K, V>>& node) {
    std::vector<std::shared_ptr<LeafNode<K, V>>> leaves;
    _collectLeafNodes(node, leaves);
    return leaves;
}

// --- HELPER: DIRECTLY EXTRACT LEAF PERMUTATIONS WITHOUT TREE ALLOCATIONS ---
template <TreeKey K, TreeValue V>
std::vector<std::vector<std::shared_ptr<LeafNode<K, V>>>> getLeafPermutations(const std::shared_ptr<Node<K, V>>& node) {
    if (!node) return {};
    
    if (auto leaf = std::dynamic_pointer_cast<LeafNode<K, V>>(node)) {
        return {{leaf}};
    }
    
    auto internal = std::dynamic_pointer_cast<InternalNode<K, V>>(node);
    if (!internal || internal->children.empty()) return {};
    
    // Step 1: Recursively get all leaf permutations of each child
    std::vector<std::vector<std::vector<std::shared_ptr<LeafNode<K, V>>>>> childPerms;
    childPerms.reserve(internal->children.size());
    for (const auto& child : internal->children) {
        childPerms.push_back(getLeafPermutations(child));
    }
    
    // Step 2: Cartesian product of child permutations combined with all orderings of children
    std::vector<size_t> indices(childPerms.size(), 0);
    std::vector<std::vector<std::shared_ptr<LeafNode<K, V>>>> result;
    
    std::vector<size_t> childOrder(internal->children.size());
    std::iota(childOrder.begin(), childOrder.end(), 0);
    
    auto comp = [&](size_t i, size_t j) {
        return internal->children[i]->key < internal->children[j]->key;
    };
    
    do {
        std::vector<size_t> currentOrder = childOrder;
        std::sort(currentOrder.begin(), currentOrder.end(), comp);
        
        do {
            std::vector<std::shared_ptr<LeafNode<K, V>>> seq;
            for (size_t cIdx : currentOrder) {
                const auto& childSeq = childPerms[cIdx][indices[cIdx]];
                seq.insert(seq.end(), childSeq.begin(), childSeq.end());
            }
            result.push_back(std::move(seq));
        } while (std::next_permutation(currentOrder.begin(), currentOrder.end(), comp));
        
    } while (internal->nextCombination(indices, childPerms));
    
    return result;
}

// --- CSHAPLEY: HIERARCHICAL TREE-STRUCTURED SHAPLEY VALUES (WITH MEMOIZATION) ---
// Based on: Li, Y., Naldi, M., Nisen, J., & Shi, Y. (2016). Organising the allocation. Risk.
template <TreeKey K, TreeValue V, typename CalcFunc>
requires std::invocable<CalcFunc, const std::vector<std::shared_ptr<LeafNode<K, V>>>&> &&
         std::convertible_to<std::invoke_result_t<CalcFunc, const std::vector<std::shared_ptr<LeafNode<K, V>>>&>, double>
std::map<K, double> cShapley(const std::shared_ptr<Node<K, V>>& root, CalcFunc calcFunc) {
    std::map<K, double> shapleyValues;
    if (!root) return shapleyValues;

    // Base case: root is a single LeafNode
    if (auto singleLeaf = std::dynamic_pointer_cast<LeafNode<K, V>>(root)) {
        std::vector<std::shared_ptr<LeafNode<K, V>>> emptySet;
        std::vector<std::shared_ptr<LeafNode<K, V>>> fullSet = {singleLeaf};
        double v0 = static_cast<double>(calcFunc(emptySet));
        double v1 = static_cast<double>(calcFunc(fullSet));
        shapleyValues[singleLeaf->key] = v1 - v0;
        return shapleyValues;
    }

    // Root is an InternalNode
    auto internal = std::dynamic_pointer_cast<InternalNode<K, V>>(root);
    if (!internal) return shapleyValues;

    // Extract unique leaves from root and assign each a canonical index
    auto initialLeaves = getLeafNodes(root);
    if (initialLeaves.empty()) return shapleyValues;

    std::map<K, size_t> keyToIndex;
    for (size_t i = 0; i < initialLeaves.size(); ++i) {
        keyToIndex[initialLeaves[i]->key] = i;
        shapleyValues[initialLeaves[i]->key] = 0.0;
    }

    size_t nLeaves = initialLeaves.size();

    // Directly generate leaf permutations (without allocating millions of tree objects)
    auto allLeafPerms = getLeafPermutations(root);
    if (allLeafPerms.empty()) return shapleyValues;

    // Memoized coalition evaluation:
    if (nLeaves <= 64) {
        // High-speed O(1) bitmask cache
        std::unordered_map<uint64_t, double> memoCache;
        
        auto getCoalitionValue = [&](uint64_t mask, const std::vector<std::shared_ptr<LeafNode<K, V>>>& coalition) -> double {
            auto it = memoCache.find(mask);
            if (it != memoCache.end()) {
                return it->second;
            }
            double val = static_cast<double>(calcFunc(coalition));
            memoCache[mask] = val;
            return val;
        };

        // Pre-evaluate empty coalition
        std::vector<std::shared_ptr<LeafNode<K, V>>> emptySet;
        getCoalitionValue(0ULL, emptySet);

        // Evaluate marginal contributions across all leaf permutations
        for (const auto& leaves : allLeafPerms) {
            std::vector<std::shared_ptr<LeafNode<K, V>>> currentCoalition;
            currentCoalition.reserve(leaves.size());
            
            uint64_t currentMask = 0ULL;
            double prevVal = getCoalitionValue(currentMask, currentCoalition);

            for (const auto& leaf : leaves) {
                size_t leafIdx = keyToIndex[leaf->key];
                currentMask |= (1ULL << leafIdx);
                currentCoalition.push_back(leaf);

                double currentVal = getCoalitionValue(currentMask, currentCoalition);
                double marginal = currentVal - prevVal;
                shapleyValues[leaf->key] += marginal;
                prevVal = currentVal;
            }
        }
    } else {
        // String bitmask cache for trees with > 64 leaves
        std::unordered_map<std::string, double> memoCache;

        auto getCoalitionValue = [&](const std::string& mask, const std::vector<std::shared_ptr<LeafNode<K, V>>>& coalition) -> double {
            auto it = memoCache.find(mask);
            if (it != memoCache.end()) {
                return it->second;
            }
            double val = static_cast<double>(calcFunc(coalition));
            memoCache[mask] = val;
            return val;
        };

        std::string emptyMask(nLeaves, '0');
        std::vector<std::shared_ptr<LeafNode<K, V>>> emptySet;
        getCoalitionValue(emptyMask, emptySet);

        for (const auto& leaves : allLeafPerms) {
            std::vector<std::shared_ptr<LeafNode<K, V>>> currentCoalition;
            currentCoalition.reserve(leaves.size());

            std::string currentMask(nLeaves, '0');
            double prevVal = getCoalitionValue(currentMask, currentCoalition);

            for (const auto& leaf : leaves) {
                size_t leafIdx = keyToIndex[leaf->key];
                currentMask[leafIdx] = '1';
                currentCoalition.push_back(leaf);

                double currentVal = getCoalitionValue(currentMask, currentCoalition);
                double marginal = currentVal - prevVal;
                shapleyValues[leaf->key] += marginal;
                prevVal = currentVal;
            }
        }
    }

    // Average marginal contributions across all permutations
    double numPermutations = static_cast<double>(allLeafPerms.size());
    for (auto& [key, val] : shapleyValues) {
        val /= numPermutations;
    }

    return shapleyValues;
}

// --- CONVENIENCE OVERLOADS FOR INTERNALNODE AND LEAFNODE POINTERS ---
template <TreeKey K, TreeValue V, typename LeafVisitor>
void traverseTree(const std::shared_ptr<InternalNode<K, V>>& node, LeafVisitor visitLeaf) {
    traverseTree(std::static_pointer_cast<Node<K, V>>(node), visitLeaf);
}

template <TreeKey K, TreeValue V, typename F>
auto propagateTree(const std::shared_ptr<InternalNode<K, V>>& node, F aggregateFunc) {
    return propagateTree(std::static_pointer_cast<Node<K, V>>(node), aggregateFunc);
}

template <TreeKey K, TreeValue V, typename LeafFunc, typename CombineFunc>
auto foldTree(const std::shared_ptr<InternalNode<K, V>>& node, LeafFunc leafFunc, CombineFunc combineFunc) {
    return foldTree(std::static_pointer_cast<Node<K, V>>(node), leafFunc, combineFunc);
}

template <TreeKey K, TreeValue V>
void printTree(const std::shared_ptr<InternalNode<K, V>>& node, std::ostream& os = std::cout) {
    printTree(std::static_pointer_cast<Node<K, V>>(node), os);
}

template <TreeKey K, TreeValue V>
std::string treeToString(const std::shared_ptr<InternalNode<K, V>>& node) {
    return treeToString(std::static_pointer_cast<Node<K, V>>(node));
}

template <TreeKey K, TreeValue V, typename CalcFunc>
auto cShapley(const std::shared_ptr<InternalNode<K, V>>& root, CalcFunc calcFunc) {
    return cShapley(std::static_pointer_cast<Node<K, V>>(root), calcFunc);
}


#endif // TREE_H
