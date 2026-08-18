# Performance Benchmark: Hierarchical cShapley on a Deep Tree with 1% VaR Calculation
library(TreeCalcs)

set.seed(1234)
n_scenarios <- 260

# 1. Build a deep 4-level hierarchy with 15 nodes (8 leaves + 7 internal nodes)
# Hierarchy:
# Bank (Level 0)
#  ├── Americas_Div (Level 1)
#  │    ├── US_Rates_Desk (Level 2)
#  │    │    ├── US_Treasury_10Y (Leaf)
#  │    │    └── US_Swaps_5Y (Leaf)
#  │    └── US_Credit_Desk (Level 2)
#  │         ├── US_IG_Bonds (Leaf)
#  │         └── US_HY_CDS (Leaf)
#  └── EMEA_Div (Level 1)
#       ├── UK_Gilt_Desk (Level 2)
#       │    ├── UK_Gilt_10Y (Leaf)
#       │    └── UK_Inflation_Swaps (Leaf)
#       └── EU_Equity_Desk (Level 2)
#            ├── EU_Index_Futures (Leaf)
#            └── EU_Single_Stock_Options (Leaf)

leaf_names <- c(
  "US_Treasury_10Y", "US_Swaps_5Y",
  "US_IG_Bonds", "US_HY_CDS",
  "UK_Gilt_10Y", "UK_Inflation_Swaps",
  "EU_Index_Futures", "EU_Single_Stock_Options"
)

# Generate realistic length-260 scenario vectors for each asset
scenarios <- list(
  US_Treasury_10Y          = rnorm(n_scenarios, mean = 0.5, sd = 12.0),
  US_Swaps_5Y              = rnorm(n_scenarios, mean = 0.3, sd = 10.5),
  US_IG_Bonds              = rnorm(n_scenarios, mean = 0.2, sd = 8.0),
  US_HY_CDS                = rnorm(n_scenarios, mean = -0.1, sd = 18.0),
  UK_Gilt_10Y              = rnorm(n_scenarios, mean = 0.4, sd = 11.0),
  UK_Inflation_Swaps       = rnorm(n_scenarios, mean = 0.1, sd = 9.5),
  EU_Index_Futures         = rnorm(n_scenarios, mean = 0.6, sd = 16.0),
  EU_Single_Stock_Options  = rnorm(n_scenarios, mean = 0.2, sd = 22.0)
)

deep_bank_tree <- tree_node("Bank_Root", list(
  tree_node("Americas_Division", list(
    tree_node("US_Rates_Desk", list(
      tree_leaf("US_Treasury_10Y"),
      tree_leaf("US_Swaps_5Y")
    )),
    tree_node("US_Credit_Desk", list(
      tree_leaf("US_IG_Bonds"),
      tree_leaf("US_HY_CDS")
    ))
  )),
  tree_node("EMEA_Division", list(
    tree_node("UK_Gilt_Desk", list(
      tree_leaf("UK_Gilt_10Y"),
      tree_leaf("UK_Inflation_Swaps")
    )),
    tree_node("EU_Equity_Desk", list(
      tree_leaf("EU_Index_Futures"),
      tree_leaf("EU_Single_Stock_Options")
    ))
  ))
))

cat("=========================================================================\n")
cat("                  DEEP TREE HIERARCHY (15 NODES)                         \n")
cat("=========================================================================\n")
print_tree(deep_bank_tree)

# 2. Define 1% percentile calculation function with invocation tracking
call_count <- 0
calc_quantile_1pct <- function(df) {
  call_count <<- call_count + 1
  if (nrow(df) == 0) {
    return(0.0)
  }
  active_assets <- df$key
  combined_pnl <- numeric(n_scenarios)
  for (asset in active_assets) {
    combined_pnl <- combined_pnl + scenarios[[asset]]
  }
  unname(quantile(combined_pnl, probs = 0.01))
}

# 3. Benchmark cShapley Execution
cat("\n=========================================================================\n")
cat("                       RUNNING PERFORMANCE BENCHMARK                     \n")
cat("=========================================================================\n")

# Calculate theoretical unmemoized calls:
# Tree Permutations = 2! (Divisions) * 2! (Americas Desks) * 2! (EMEA Desks) *
#                     2! (US Rates) * 2! (US Credit) * 2! (UK Gilt) * 2! (EU Equity)
#                   = 2 * 2 * 2 * 2 * 2 * 2 * 2 = 128 permutations
# Subsets evaluated per permutation = 9 (from size 0 to 8)
# Total unmemoized calls = 128 * 9 = 1,152 evaluations

timing <- system.time({
  shap_results <- cshapley(deep_bank_tree, calc_quantile_1pct)
})

cat(sprintf("Tree Permutations:                 128\n"))
cat(sprintf("Unmemoized Function Evaluations:   1,152\n"))
cat(sprintf("Actual Memoized Evaluations:       %d (%.1f%% reduction!)\n", 
            call_count, 100 * (1 - call_count / 1152)))
cat(sprintf("Execution Time:                    %.4f seconds\n", timing[["elapsed"]]))

# 4. Multi-iteration Benchmark for precise timing
n_runs <- 50
cat(sprintf("\nRunning %d benchmark iterations for timing distribution...\n", n_runs))
times <- numeric(n_runs)
for (i in 1:n_runs) {
  t <- system.time({
    cshapley(deep_bank_tree, calc_quantile_1pct)
  })
  times[i] <- t[["elapsed"]] * 1000 # ms
}

cat(sprintf("Mean Time per Run:                 %.2f ms\n", mean(times)))
cat(sprintf("Median Time per Run:               %.2f ms\n", median(times)))
cat(sprintf("Min / Max Time:                    %.2f ms / %.2f ms\n", min(times), max(times)))

# 5. Risk Attribution & Efficiency Validation
cat("\n=========================================================================\n")
cat("                     1% VaR ATTRIBUTION RESULTS                          \n")
cat("=========================================================================\n")
total_pnl <- Reduce(`+`, scenarios)
total_bank_var <- unname(quantile(total_pnl, probs = 0.01))

res_df <- data.frame(
  Asset = names(shap_results),
  VaR_Contribution = as.numeric(shap_results),
  Pct_Total = sprintf("%.2f%%", 100 * as.numeric(shap_results) / total_bank_var)
)
print(res_df, row.names = FALSE)

cat("-------------------------------------------------------------------------\n")
cat(sprintf("Sum of cShapley Attributions:      %.4f\n", sum(shap_results)))
cat(sprintf("Total Bank Portfolio 1%% VaR:      %.4f\n", total_bank_var))
cat(sprintf("Efficiency Match Discrepancy:      %.2e (Exact)\n", abs(sum(shap_results) - total_bank_var)))
cat("=========================================================================\n")
