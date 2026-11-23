
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

slash <- function(p) gsub("\\\\", "/", p)

test_that("environment variable, then .Renviron, then detection", {
  testthat::local_edition(3)
  renv <- local_clean_tauargus_env()
  root <- tempfile("roots")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  detected <- slash(fake_exe(root, "detected/TauArgus.exe"))
  from_renv <- slash(fake_exe(root, "renviron/TauArgus.exe"))
  from_env <- slash(fake_exe(root, "env/TauArgus.exe"))
  local_mocked_bindings(detect_tauargus_exe = function(...) detected)

  loc <- locate_tauargus_exe()
  expect_equal(as.character(loc), detected)
  expect_equal(attr(loc, "source"), "detected")
  expect_null(attr(loc, "stale"))
  expect_equal(get_tauargus_exe(detect = FALSE), "")

  writeLines(paste0('TAUARGUS_EXE="', from_renv, '"'), renv)
  loc <- locate_tauargus_exe()
  expect_equal(as.character(loc), from_renv)
  expect_equal(attr(loc, "source"), "renviron")

  Sys.setenv(TAUARGUS_EXE = from_env)
  expect_equal(get_tauargus_exe(), from_env)
})

# invalid stored location -------------------------------------------------------

test_that("a stored path to a missing file falls back to detection", {
  testthat::local_edition(3)
  local_clean_tauargus_env()
  root <- tempfile("roots")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  detected <- slash(fake_exe(root, "TauArgus/TauArgus.exe"))
  local_mocked_bindings(detect_tauargus_exe = function(...) detected)

  Sys.setenv(TAUARGUS_EXE = "C:/old/TauArgus4.2.3/TauArgus.exe")

  loc <- locate_tauargus_exe()
  expect_equal(as.character(loc), detected)
  expect_equal(attr(loc, "stale"), "C:/old/TauArgus4.2.3/TauArgus.exe")
  expect_message(
    res <- get_tauargus_exe(),
    "does not exist.*auto-detected"
  )
  expect_equal(res, detected)
})

test_that("a stored path to a missing file is kept if nothing is detected", {
  testthat::local_edition(3)
  local_clean_tauargus_env()
  local_mocked_bindings(detect_tauargus_exe = function(...) "")

  Sys.setenv(TAUARGUS_EXE = "C:/old/TauArgus.exe")
  expect_message(res <- get_tauargus_exe(), "does not exist")
  expect_equal(res, "C:/old/TauArgus.exe")
})

test_that("a stored folder is not accepted as TauArgus.exe", {
  testthat::local_edition(3)
  local_clean_tauargus_env()
  folder <- tempfile("TauArgus")
  dir.create(folder)
  on.exit(unlink(folder, recursive = TRUE), add = TRUE)
  local_mocked_bindings(detect_tauargus_exe = function(...) "")

  expect_false(is_valid_tauargus_exe(folder))
  expect_match(tauargus_exe_problem(folder), "is a folder")

  Sys.setenv(TAUARGUS_EXE = folder)
  expect_message(get_tauargus_exe(), "is a folder")
})

# set_tauargus_exe -------------------------------------------------------------

test_that("set_tauargus_exe writes .Renviron only when asked", {
  renv <- local_clean_tauargus_env()
  root <- tempfile("roots")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  a <- slash(fake_exe(root, "a/TauArgus.exe"))
  b <- slash(fake_exe(root, "b/TauArgus.exe"))

  set_tauargus_exe(a)
  expect_equal(Sys.getenv("TAUARGUS_EXE"), a)
  expect_equal(getOption("rtauargus.tauargus_exe"), a)
  expect_false(file.exists(renv))

  suppressMessages(set_tauargus_exe(a, install = TRUE))
  expect_equal(readLines(renv), paste0('TAUARGUS_EXE="', a, '"'))

  expect_error(set_tauargus_exe(b, install = TRUE), "overwrite")
  suppressMessages(set_tauargus_exe(b, install = TRUE, overwrite = TRUE))
  expect_equal(readLines(renv), paste0('TAUARGUS_EXE="', b, '"'))
})

test_that("set_tauargus_exe rejects missing files and folders", {
  renv <- local_clean_tauargus_env()
  folder <- tempfile("TauArgus")
  dir.create(folder)
  on.exit(unlink(folder, recursive = TRUE), add = TRUE)

  expect_error(set_tauargus_exe("C:/nowhere/TauArgus.exe"), "does not exist")
  expect_error(set_tauargus_exe(folder), "is a folder")
  expect_equal(Sys.getenv("TAUARGUS_EXE"), "")
  expect_false(file.exists(renv))

  # check = FALSE: stored anyway, with a warning
  expect_warning(
    set_tauargus_exe("Z:/network/TauArgus.exe", check = FALSE),
    "stored anyway"
  )
  expect_equal(Sys.getenv("TAUARGUS_EXE"), "Z:/network/TauArgus.exe")
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

test_that("run_arb rejects a folder given as tauargus_exe", {
  testthat::local_edition(3)
  local_clean_tauargus_env()
  folder <- tempfile("TauArgus")
  dir.create(folder)
  on.exit(unlink(folder, recursive = TRUE), add = TRUE)
  local_mocked_bindings(choose_tauargus_exe = function() "")

  expect_error(
    run_arb("missing.arb", tauargus_exe = folder),
    "is a folder"
  )
})
