# =============================================================================
# test-review-runmosaic-git-provenance.R
#
# environment.json's git sha/branch described whichever repo the working
# directory was in (e.g. a country repo), not the MOSAIC code that ran.
# =============================================================================

.make_repo <- function(dir, branch) {
  system2("git", c("-C", shQuote(dir), "init", "-q", "-b", branch), stdout = FALSE, stderr = FALSE)
  writeLines("x", file.path(dir, "f.txt"))
  system2("git", c("-C", shQuote(dir), "add", "f.txt"), stdout = FALSE, stderr = FALSE)
  system2("git", c("-C", shQuote(dir), "-c", "user.email=t@t", "-c", "user.name=t",
                   "commit", "-q", "-m", "init"), stdout = FALSE, stderr = FALSE)
  system2("git", c("-C", shQuote(dir), "rev-parse", "--short", "HEAD"), stdout = TRUE)
}

test_that("the MOSAIC sha comes from the package checkout, the cwd repo is separate", {
  skip_if(!nzchar(Sys.which("git")), "git not available")
  pkg <- withr::local_tempdir(); cwd <- withr::local_tempdir()
  writeLines(c("Package: MOSAIC", "Version: 0.0.1"), file.path(pkg, "DESCRIPTION"))
  dir.create(file.path(pkg, "inst"))
  pkg_sha <- .make_repo(pkg, "pkgbranch"); cwd_sha <- .make_repo(cwd, "country")
  # Under load_all, system.file(package = "MOSAIC") is <src>/inst.
  g <- MOSAIC:::.mosaic_git_provenance(pkg_dir = file.path(pkg, "inst"), cwd = cwd,
                                       desc = list())
  expect_identical(g$source, "checkout")
  expect_identical(g$sha, pkg_sha)
  expect_identical(g$branch, "pkgbranch")
  expect_identical(g$cwd_sha, cwd_sha)
  expect_identical(g$cwd_branch, "country")
})

test_that("an installed package without a checkout records no MOSAIC sha", {
  skip_if(!nzchar(Sys.which("git")), "git not available")
  pkg <- withr::local_tempdir(); cwd <- withr::local_tempdir()
  cwd_sha <- .make_repo(cwd, "country")
  # An installed library inside some other repo (e.g. an renv project) is not
  # a MOSAIC checkout either.
  lib <- file.path(cwd, "renv", "library", "MOSAIC"); dir.create(lib, recursive = TRUE)
  g <- MOSAIC:::.mosaic_git_provenance(pkg_dir = pkg, cwd = cwd, desc = list())
  g2 <- MOSAIC:::.mosaic_git_provenance(pkg_dir = lib, cwd = cwd, desc = list())
  expect_identical(g2$source, "unknown")
  expect_identical(g$source, "unknown")
  expect_true(is.na(g$sha))
  expect_identical(g$cwd_sha, cwd_sha)
})

test_that("a remotes/pak install uses RemoteSha", {
  g <- MOSAIC:::.mosaic_git_provenance(
    pkg_dir = "", cwd = withr::local_tempdir(),
    desc = list(RemoteSha = "0123456789abcdef", RemoteRef = "main"))
  expect_identical(g$source, "remote")
  expect_identical(g$sha, "012345678")
  expect_identical(g$branch, "main")
})
