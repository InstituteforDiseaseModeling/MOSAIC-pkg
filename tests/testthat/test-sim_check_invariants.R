# =============================================================================
# test-sim_check_invariants.R
#
# `sim_check_invariants()` is the engine's only correctness check that does not
# depend on the retired Python oracle: it catches the class of bug where R and
# Python agree because both are wrong. It runs on EVERY tick of every
# production run (config$check_invariants defaults to TRUE).
#
# In v0.70.0 its body was rewritten for speed -- one min() pass instead of
# anyNA() + any(v < 0), an accumulation loop instead of Reduce(`+`, lapply()),
# and a NULL skip instead of intersect(..., names(state)). It had no direct
# tests, so a rewrite could have silently turned any of these assertions into a
# no-op and every existing test would still have passed. These tests exist to
# make that impossible: each one asserts the check FIRES.
# =============================================================================

mk_state <- function(npatches = 3L, nticks = 2L) {
  st <- MOSAIC:::sim_alloc_state(nticks, npatches)
  row <- 2L   # tick 1
  for (nm in c("S", "E", "Isym", "Iasym", "R", "V1", "V2")) {
    st[[nm]][[row]] <- rep(10L, npatches)
  }
  st$N[[row]] <- rep(70L, npatches)   # 7 compartments x 10
  st
}
COMP <- c("S", "E", "Isym", "Iasym", "R", "V1", "V2")

test_that("a consistent state passes", {
  expect_true(sim_check_invariants(mk_state(), tick = 1L, compartments = COMP))
})

test_that("an NA in a compartment is caught and the patch is named", {
  st <- mk_state(); st$S[[2L]][2L] <- NA_integer_
  expect_error(sim_check_invariants(st, 1L, COMP), "S contains NA")
  expect_error(sim_check_invariants(st, 1L, COMP), "2")
})

test_that("a negative compartment is caught", {
  st <- mk_state(); st$E[[2L]][3L] <- -1L
  expect_error(sim_check_invariants(st, 1L, COMP), "E is negative")
})

test_that("N not equal to the sum of compartments is caught", {
  # The assertion that Reduce(`+`, lapply(...)) used to compute. If the
  # accumulation loop summed the wrong set, or short-circuited, this is the
  # only test that would notice.
  st <- mk_state(); st$N[[2L]][1L] <- 69L
  expect_error(sim_check_invariants(st, 1L, COMP), "N does not equal the sum")
})

test_that("the compartment sum uses exactly the compartments passed", {
  st <- mk_state()
  # Drop V2 from the list: the sum is then 60, not 70, so N = 70 must fail.
  expect_error(sim_check_invariants(st, 1L, setdiff(COMP, "V2")),
               "N does not equal the sum")
})

test_that("a non-finite continuous channel is caught, for each of the three", {
  for (nm in c("Lambda", "Psi", "W")) {
    st <- mk_state(); st[[nm]][[2L]][1L] <- Inf
    expect_error(sim_check_invariants(st, 1L, COMP),
                 sprintf("%s is not finite", nm))
    st <- mk_state(); st[[nm]][[2L]][1L] <- NaN
    expect_error(sim_check_invariants(st, 1L, COMP),
                 sprintf("%s is not finite", nm))
    st <- mk_state(); st[[nm]][[2L]][1L] <- -Inf
    expect_error(sim_check_invariants(st, 1L, COMP),
                 sprintf("%s is not finite", nm))
  }
})

test_that("a negative continuous channel is caught, for each of the three", {
  for (nm in c("Lambda", "Psi", "W")) {
    st <- mk_state(); st[[nm]][[2L]][2L] <- -1e-9
    expect_error(sim_check_invariants(st, 1L, COMP),
                 sprintf("%s is negative", nm))
  }
})

test_that("a state missing a continuous channel is skipped, not an error", {
  # The NULL skip that replaced intersect(c("Lambda","Psi","W"), names(state)).
  st <- mk_state(); rm("W", envir = st)
  expect_true(sim_check_invariants(st, 1L, COMP))
})

test_that("an empty compartment set is a no-op, as it was before", {
  st <- mk_state()
  expect_true(sim_check_invariants(st, 1L, character(0)))
})

test_that("the check is wired into the engine and on by default", {
  # Not a property of the function but of its call site: gating the engine's
  # only oracle-independent correctness check behind a flag that defaults off
  # would be indistinguishable from deleting it.
  par <- MOSAIC:::sim_params(list(
    date_start = "2023-01-01", date_stop = "2023-01-10",
    location_name = c("L1", "L2"),
    S_j_initial = rep(1000L, 2L),
    b_jt = matrix(3e-5, nrow = 2L, ncol = 10L),
    d_jt = matrix(3e-5, nrow = 2L, ncol = 10L)
  ), components = c("Susceptible", "Census"))
  expect_true(par$check_invariants)
})
