
# helpers ----------------------------------------------------------------

# creates an empty fake TauArgus.exe at <root>/<rel>
fake_exe <- function(root, rel) {
  path <- file.path(root, rel)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  file.create(path)
  path
}

# isolates the test from the user's environment and .Renviron
local_clean_tauargus_env <- function(env = parent.frame()) {
  old_exe <- Sys.getenv("TAUARGUS_EXE", unset = NA)
  old_renv <- Sys.getenv("R_ENVIRON_USER", unset = NA)
  old_opt <- getOption("rtauargus.tauargus_exe")
  renv <- tempfile(fileext = ".Renviron")
  Sys.unsetenv("TAUARGUS_EXE")
  Sys.setenv(R_ENVIRON_USER = renv)
  withr::defer({
    if (is.na(old_exe)) Sys.unsetenv("TAUARGUS_EXE") else Sys.setenv(TAUARGUS_EXE = old_exe)
    if (is.na(old_renv)) Sys.unsetenv("R_ENVIRON_USER") else Sys.setenv(R_ENVIRON_USER = old_renv)
    options(rtauargus.tauargus_exe = old_opt)
    unlink(c(renv, paste0(renv, ".bak")))
  }, envir = env)
  renv
}

# detection ----------------------------------------------------------------

test_that("detect_tauargus_exe finds TauArgus.exe in 'Argus' folders", {
  root <- tempfile("roots")
  on.exit(unlink(root, recursive = TRUE))
  exe <- fake_exe(root, "TauArgus/TauArgus4.2.3/TauArgus.exe")
  fake_exe(root, "Other/TauArgus.exe") # not in an "Argus" folder: ignored

  res <- detect_tauargus_exe(roots = root)
  expect_equal(normalizePath(res), normalizePath(exe))
})

test_that("detect_tauargus_exe returns the most recent version", {
  root <- tempfile("roots")
  on.exit(unlink(root, recursive = TRUE))
  fake_exe(root, "TauArgus/TauArgus4.2.3/TauArgus.exe")
  latest <- fake_exe(root, "TauArgus/TauArgus4.10.1/TauArgus.exe")
  fake_exe(root, "TauArgus4.2.4b1/TauArgus.exe")

  res <- detect_tauargus_exe(roots = root)
  expect_equal(normalizePath(res), normalizePath(latest))
})

test_that("detect_tauargus_exe searches up to three levels deep", {
  root <- tempfile("roots")
  on.exit(unlink(root, recursive = TRUE))
  exe <- fake_exe(root, "TauArgus4.2.3/TauArgus4.2.3/TauArgus/TauArgus.exe")

  expect_equal(
    normalizePath(detect_tauargus_exe(roots = root)),
    normalizePath(exe)
  )
})

test_that("detect_tauargus_exe returns '' when nothing is found", {
  root <- tempfile("roots")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE))
  expect_equal(detect_tauargus_exe(roots = root), "")
  expect_equal(detect_tauargus_exe(roots = character(0)), "")
})

# priority order -------------------------------------------------------------

test_that("environment variable, then .Renviron, then detection", {
  testthat::local_edition(3)
  renv <- local_clean_tauargus_env()
  local_mocked_bindings(detect_tauargus_exe = function(...) "C:/detected/TauArgus.exe")

  loc <- locate_tauargus_exe()
  expect_equal(as.character(loc), "C:/detected/TauArgus.exe")
  expect_equal(attr(loc, "source"), "detected")
  expect_equal(get_tauargus_exe(detect = FALSE), "")

  writeLines('TAUARGUS_EXE="C:/renviron/TauArgus.exe"', renv)
  loc <- locate_tauargus_exe()
  expect_equal(as.character(loc), "C:/renviron/TauArgus.exe")
  expect_equal(attr(loc, "source"), "renviron")

  Sys.setenv(TAUARGUS_EXE = "C:/env/TauArgus.exe")
  expect_equal(get_tauargus_exe(), "C:/env/TauArgus.exe")
})

# set_tauargus_exe -------------------------------------------------------------

test_that("set_tauargus_exe writes .Renviron only when asked", {
  renv <- local_clean_tauargus_env()

  suppressWarnings(set_tauargus_exe("C:/a/TauArgus.exe"))
  expect_equal(Sys.getenv("TAUARGUS_EXE"), "C:/a/TauArgus.exe")
  expect_equal(getOption("rtauargus.tauargus_exe"), "C:/a/TauArgus.exe")
  expect_false(file.exists(renv))

  suppressWarnings(suppressMessages(
    set_tauargus_exe("C:/a/TauArgus.exe", install = TRUE)
  ))
  expect_equal(readLines(renv), 'TAUARGUS_EXE="C:/a/TauArgus.exe"')

  expect_error(
    suppressWarnings(set_tauargus_exe("C:/b/TauArgus.exe", install = TRUE)),
    "overwrite"
  )
  suppressWarnings(suppressMessages(
    set_tauargus_exe("C:/b/TauArgus.exe", install = TRUE, overwrite = TRUE)
  ))
  expect_equal(readLines(renv), 'TAUARGUS_EXE="C:/b/TauArgus.exe"')
})

# interactive prompt -------------------------------------------------------------

test_that("choose_tauargus_exe does nothing in a non-interactive session", {
  testthat::local_edition(3)
  local_mocked_bindings(is_interactive = function() FALSE)
  expect_equal(choose_tauargus_exe(), "")
})

test_that("run_arb prompts for TauArgus.exe when it cannot be found", {
  testthat::local_edition(3)
  renv <- local_clean_tauargus_env()
  root <- tempfile("roots")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  exe <- fake_exe(root, "TauArgus/TauArgus.exe")

  local_mocked_bindings(
    detect_tauargus_exe = function(...) "",
    choose_tauargus_exe = function() {
      set_tauargus_exe(exe)
    }
  )

  # the prompt provides the exe; run_arb then fails on the (missing) batch file
  expect_error(
    run_arb("missing.arb", tauargus_exe = ""),
    "Batch file cannot be found"
  )
  expect_equal(Sys.getenv("TAUARGUS_EXE"), gsub("\\\\", "/", exe))

  # cancelled prompt: explicit error
  local_mocked_bindings(choose_tauargus_exe = function() "")
  Sys.unsetenv("TAUARGUS_EXE")
  expect_error(
    run_arb("missing.arb", tauargus_exe = ""),
    "location of Tau-Argus is not set"
  )
})
