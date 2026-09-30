# =============================================================================
# test-review-runmosaic-git-provenance.R
#
# environment.json recorded only the working directory's repo (git$sha/branch),
# never the MOSAIC code that ran. git$sha/branch keep their meaning (the cwd
# repo, read downstream as the country-repo commit); the MOSAIC code sha is
# added as git$mosaic_sha/mosaic_branch/mosaic_source.
# =============================================================================

.make_repo <- function(dir, branch) {
  system2("git", c("-C", shQuote(dir), "init", "-q", "-b", branch), stdout = FALSE, stderr = FALSE)
  writeLines("x", file.path(dir, "f.txt"))
  system2("git", c("-C", shQuote(dir), "add", "f.txt"), stdout = FALSE, stderr = FALSE)
  system2("git", c("-C", shQuote(dir), "-c", "user.email=t@t", "-c", "user.name=t",
                   "commit", "-q", "-m", "init"), stdout = FALSE, stderr = FALSE)
  system2("git", c("-C", shQuote(dir), "rev-parse", "--short", "HEAD"), stdout = TRUE)
}

test_that("git$sha stays the cwd repo; the MOSAIC checkout sha is mosaic_sha", {
  skip_if(!nzchar(Sys.which("git")), "git not available")
  pkg <- withr::local_tempdir(); cwd <- withr::local_tempdir()
  writeLines(c("Package: MOSAIC", "Version: 0.0.1"), file.path(pkg, "DESCRIPTION"))
  dir.create(file.path(pkg, "inst"))
  pkg_sha <- .make_repo(pkg, "pkgbranch"); cwd_sha <- .make_repo(cwd, "country")
  # Under load_all, system.file(package = "MOSAIC") is <src>/inst.
  g <- MOSAIC:::.mosaic_git_provenance(pkg_dir = file.path(pkg, "inst"), cwd = cwd,
                                       desc = list())
  expect_identical(g$sha, cwd_sha)
  expect_identical(g$branch, "country")
  expect_identical(g$mosaic_source, "checkout")
  expect_identical(g$mosaic_sha, pkg_sha)
  expect_identical(g$mosaic_branch, "pkgbranch")
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
  expect_identical(g2$mosaic_source, "unknown")
  expect_identical(g$mosaic_source, "unknown")
  expect_true(is.na(g$mosaic_sha))
  expect_identical(g$sha, cwd_sha)
})

test_that("a non-git cwd omits git$sha, as before", {
  skip_if(!nzchar(Sys.which("git")), "git not available")
  g <- MOSAIC:::.mosaic_git_provenance(pkg_dir = "", cwd = withr::local_tempdir(),
                                       desc = list())
  # MOSAIC-OCV promote_model.R tests !is.null(env$git$sha).
  expect_null(g$sha)
  expect_identical(g$mosaic_source, "unknown")
})

test_that("a remotes/pak install uses RemoteSha for mosaic_sha", {
  g <- MOSAIC:::.mosaic_git_provenance(
    pkg_dir = "", cwd = withr::local_tempdir(),
    desc = list(RemoteSha = "0123456789abcdef", RemoteRef = "main"))
  expect_identical(g$mosaic_source, "remote")
  expect_identical(g$mosaic_sha, "012345678")
  expect_identical(g$mosaic_branch, "main")
})
