library(TreeCalcs)

test_that("cShapley computes accurate linear Shapley values", {
  tree <- tree_node("root_node", list(
    tree_node("group1", list(
      tree_leaf("a", 8),
      tree_leaf("b", 2)
    )),
    tree_node("group2", list(
      tree_leaf("c", 5),
      tree_leaf("d", 1)
    ))
  ))

  shap <- cshapley(tree, "sum")
  expect_equal(shap[["a"]], 8)
  expect_equal(shap[["b"]], 2)
  expect_equal(shap[["c"]], 5)
  expect_equal(shap[["d"]], 1)
  expect_equal(sum(shap), 16)
})

test_that("cShapley satisfies efficiency property with non-linear value function", {
  tree <- tree_node("root_node", list(
    tree_node("group1", list(
      tree_leaf("a", 8),
      tree_leaf("b", 2)
    )),
    tree_node("group2", list(
      tree_leaf("c", 5),
      tree_leaf("d", 1)
    ))
  ))

  # Value function: (sum of leaf values)^2
  sq_calc <- function(df) {
    if (nrow(df) == 0) 0 else sum(df$value)^2
  }

  shap <- cshapley(tree, sq_calc)
  expect_equal(shap[["a"]], 128)
  expect_equal(shap[["b"]], 32)
  expect_equal(shap[["c"]], 80)
  expect_equal(shap[["d"]], 16)

  # Efficiency property: sum of Shapley values equals v(N) - v(empty) = 16^2 - 0 = 256
  expect_equal(sum(shap), 256)
})

test_that("cShapley works with bank/desk/trades hierarchy and max(0, sum(x)) payoff", {
  bank_tree <- tree_node("Bank", list(
    tree_node("Desk_A", list(
      tree_leaf("Trade_1", 12)
    )),
    tree_node("Desk_B", list(
      tree_leaf("Trade_2", 24),
      tree_leaf("Trade_3", -24)
    ))
  ))

  f_max_pos <- function(df) {
    if (nrow(df) == 0) {
      0
    } else {
      max(0, sum(df$value))
    }
  }

  shap <- cshapley(bank_tree, f_max_pos)

  expect_equal(shap[["Trade_1"]], 12)
  expect_equal(shap[["Trade_2"]], 15)
  expect_equal(shap[["Trade_3"]], -15)
  expect_equal(sum(shap), 12)
})

test_that("cShapley computes 1% percentile risk attribution with length-260 random vectors", {
  set.seed(42)
  n_scenarios <- 260

  scenarios <- list(
    Trade_1 = rnorm(n_scenarios, mean = 0, sd = 10),
    Trade_2 = rnorm(n_scenarios, mean = 0, sd = 15),
    Trade_3 = rnorm(n_scenarios, mean = 0, sd = 12)
  )

  bank_tree <- tree_node("Bank", list(
    tree_node("Desk_A", list(
      tree_leaf("Trade_1")
    )),
    tree_node("Desk_B", list(
      tree_leaf("Trade_2"),
      tree_leaf("Trade_3")
    ))
  ))

  calc_quantile_1pct <- function(df) {
    if (nrow(df) == 0) {
      return(0.0)
    }
    active_trades <- df$key
    combined_pnl <- rep(0, n_scenarios)
    for (t in active_trades) {
      combined_pnl <- combined_pnl + scenarios[[t]]
    }
    unname(quantile(combined_pnl, probs = 0.01))
  }

  shap <- cshapley(bank_tree, calc_quantile_1pct)

  expect_named(shap, c("Trade_1", "Trade_2", "Trade_3"), ignore.order = TRUE)

  full_bank_pnl <- scenarios$Trade_1 + scenarios$Trade_2 + scenarios$Trade_3
  full_bank_q01 <- unname(quantile(full_bank_pnl, probs = 0.01))

  expect_equal(sum(shap), full_bank_q01)
})


test_that("cShapley works with data.tree objects", {
  skip_if_not_installed("data.tree")

  root_dt <- data.tree::Node$new("top")
  g1 <- root_dt$AddChild("group1")
  g1$AddChild("x", value = 10)
  g1$AddChild("y", value = 20)

  shap_dt <- cshapley(root_dt, "sum")
  expect_equal(shap_dt[["x"]], 10)
  expect_equal(shap_dt[["y"]], 20)
  expect_equal(sum(shap_dt), 30)
})


test_that("cShapley satisfies associativity: node contributions equal sum of child node contributions across tree levels", {
  # 3-level tree structure: Root -> Region -> Desk -> Leaves
  full_tree <- tree_node("Bank", list(
    tree_node("Americas", list(
      tree_node("Americas_Rates", list(
        tree_leaf("US_10Y", 10),
        tree_leaf("US_5Y", 20)
      )),
      tree_node("Americas_Credit", list(
        tree_leaf("US_IG", 15),
        tree_leaf("US_HY", 5)
      ))
    )),
    tree_node("EMEA", list(
      tree_node("EMEA_Rates", list(
        tree_leaf("UK_Gilt", 8),
        tree_leaf("UK_Swap", 12)
      )),
      tree_node("EMEA_Equity", list(
        tree_leaf("EU_Index", 25),
        tree_leaf("EU_Stock", 5)
      ))
    ))
  ))

  # Non-linear payoff function: f(S) = (sum x)^2
  f_sq <- function(df) {
    if (nrow(df) == 0) 0 else sum(df$value)^2
  }

  shap_full <- cshapley(full_tree, f_sq)

  # Leaf contributions
  expect_equal(shap_full[["US_10Y"]], 1000)
  expect_equal(shap_full[["US_5Y"]], 2000)
  expect_equal(shap_full[["US_IG"]], 1500)
  expect_equal(shap_full[["US_HY"]], 500)
  expect_equal(shap_full[["UK_Gilt"]], 800)
  expect_equal(shap_full[["UK_Swap"]], 1200)
  expect_equal(shap_full[["EU_Index"]], 2500)
  expect_equal(shap_full[["EU_Stock"]], 500)

  # 1. Check Desk level sums (Child leaves -> Parent Desk)
  americas_rates_val <- shap_full[["US_10Y"]] + shap_full[["US_5Y"]]
  americas_credit_val <- shap_full[["US_IG"]] + shap_full[["US_HY"]]
  emea_rates_val <- shap_full[["UK_Gilt"]] + shap_full[["UK_Swap"]]
  emea_equity_val <- shap_full[["EU_Index"]] + shap_full[["EU_Stock"]]

  expect_equal(americas_rates_val, 3000)
  expect_equal(americas_credit_val, 2000)
  expect_equal(emea_rates_val, 2000)
  expect_equal(emea_equity_val, 3000)

  # 2. Check Region level sums (Child Desks -> Parent Region)
  americas_total <- americas_rates_val + americas_credit_val
  emea_total <- emea_rates_val + emea_equity_val

  expect_equal(americas_total, 5000)
  expect_equal(emea_total, 5000)

  # 3. Root total equals sum of regions
  expect_equal(americas_total + emea_total, 10000)
  expect_equal(sum(shap_full), 100^2)

  # 4. Compare with collapsed intermediate quotient tree at Desk level
  desk_tree <- tree_node("Bank", list(
    tree_node("Americas", list(
      tree_leaf("Americas_Rates", 30),
      tree_leaf("Americas_Credit", 20)
    )),
    tree_node("EMEA", list(
      tree_leaf("EMEA_Rates", 20),
      tree_leaf("EMEA_Equity", 30)
    ))
  ))
  shap_desk <- cshapley(desk_tree, f_sq)
  expect_equal(shap_desk[["Americas_Rates"]], americas_rates_val)
  expect_equal(shap_desk[["Americas_Credit"]], americas_credit_val)
  expect_equal(shap_desk[["EMEA_Rates"]], emea_rates_val)
  expect_equal(shap_desk[["EMEA_Equity"]], emea_equity_val)

  # 5. Compare with collapsed macro quotient tree at Region level
  region_tree <- tree_node("Bank", list(
    tree_leaf("Americas", 50),
    tree_leaf("EMEA", 50)
  ))
  shap_region <- cshapley(region_tree, f_sq)
  expect_equal(shap_region[["Americas"]], americas_total)
  expect_equal(shap_region[["EMEA"]], emea_total)
})

test_that("Tree permutations preserve per-node marginal contribution summation for all internal nodes", {
  tree <- tree_node("Root", list(
    tree_node("Branch1", list(
      tree_node("DeskA", list(
        tree_leaf("a1", 4),
        tree_leaf("a2", 6)
      )),
      tree_node("DeskB", list(
        tree_leaf("b1", 10)
      ))
    )),
    tree_node("Branch2", list(
      tree_node("DeskC", list(
        tree_leaf("c1", 3),
        tree_leaf("c2", 7)
      )),
      tree_node("DeskD", list(
        tree_leaf("d1", 5),
        tree_leaf("d2", 5)
      ))
    ))
  ))

  # Non-linear payoff: sqrt of sum + squared sum
  payoff <- function(df) {
    if (nrow(df) == 0) return(0.0)
    s <- sum(df$value)
    sqrt(s) + (s / 10)^2
  }

  perms <- permute_tree(tree, as_data_tree = FALSE)
  expect_gt(length(perms), 0)

  desk_a_leaves <- c("a1", "a2")
  desk_b_leaves <- c("b1")
  desk_c_leaves <- c("c1", "c2")
  desk_d_leaves <- c("d1", "d2")

  branch1_leaves <- c(desk_a_leaves, desk_b_leaves)
  branch2_leaves <- c(desk_c_leaves, desk_d_leaves)

  # In EVERY permutation, the sum of marginal contributions of children must equal parent contribution
  for (p in perms) {
    df <- traverse_tree(p)
    keys <- df$key

    # Calculate step-by-step marginal contributions
    marginals <- numeric(nrow(df))
    names(marginals) <- keys
    curr_set <- data.frame(key = character(), value = numeric())
    prev_val <- 0.0

    for (j in 1:nrow(df)) {
      curr_set <- rbind(curr_set, df[j, ])
      curr_val <- payoff(curr_set)
      marginals[j] <- curr_val - prev_val
      prev_val <- curr_val
    }

    # Marginal contributions for each Desk
    m_desk_a <- sum(marginals[desk_a_leaves])
    m_desk_b <- sum(marginals[desk_b_leaves])
    m_desk_c <- sum(marginals[desk_c_leaves])
    m_desk_d <- sum(marginals[desk_d_leaves])

    # Marginal contributions for each Branch
    m_branch1 <- sum(marginals[branch1_leaves])
    m_branch2 <- sum(marginals[branch2_leaves])

    # Assert that child contributions sum exactly to parent contribution in this permutation
    expect_equal(m_desk_a + m_desk_b, m_branch1, tolerance = 1e-10)
    expect_equal(m_desk_c + m_desk_d, m_branch2, tolerance = 1e-10)
    expect_equal(m_branch1 + m_branch2, payoff(df), tolerance = 1e-10)
  }
})

