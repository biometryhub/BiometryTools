skip_if(R.version$status == "Under development (unstable)")

test_that("Installation works", {
    expect_equal(install_asreml(), TRUE)
})

test_that("Installation provides output on success", {
    # skip_if(R.version$status == "Under development (unstable)")
    if("asreml" %in% installed.packages()[,1]){remove.packages("asreml")}
    expect_message(install_asreml(), "ASreml-R successfully installed!")
})

test_that("Returns true if asreml already installed", {
    # skip_if(R.version$status == "Under development (unstable)")
    install_asreml(quiet=T)
    expect_equal(install_asreml(), TRUE)
})

test_that("Returns message if asreml already installed", {
    # skip_if(R.version$status == "Under development (unstable)")
    install_asreml(quiet=T)
    expect_message(install_asreml(), "ASreml-R is already installed.")
})

test_that("Quiet returns no output", {
    skip_if(R.version$status == "Under development (unstable)")
    expect_invisible(install_asreml(quiet = T))
})

test_that("Force argument makes package install", {
    # skip_if(R.version$status == "Under development (unstable)")
    expect_equal(install_asreml(force = T), TRUE)
    expect_message(install_asreml(force = T), "ASreml-R successfully installed!")
})

test_that("keep_file = F doesn't keep file", {
    # skip_if(R.version$status == "Under development (unstable)")
    expect_file_2(install_asreml, list(force = T, keep_file = F),
                  pat = "asreml+(([a-zA-Z0-9_.\\-])*)+(.zip|.tar.gz|.tgz)", missing = T)
})

test_that("keep_file = T keeps file (in temp?)", {
    skip_if(R.version$status == "Under development (unstable)")
    expect_file_2(install_asreml, list(force = T, keep_file = T),
                  pat = "asreml+(([a-zA-Z0-9_.\\-])*)+(.zip|.tar.gz|.tgz)")
})


test_that("keep_file = 'data' keeps file in 'data'", {
    # skip_if(R.version$status == "Under development (unstable)")
    dir.create("data")
    expect_file_2(install_asreml, list(force = T, keep_file = "data"),
                  pat = "asreml+(([a-zA-Z0-9_.\\-])*)+(.zip|.tar.gz|.tgz)", dir = "data")
})

test_that("force and quiet work together", {
    # skip_if(R.version$status == "Under development (unstable)")
    expect_invisible(install_asreml(force = T, quiet = T))
})

test_that("Providing a non-existant directory fails", {
    # skip_if(R.version$status == "Under development (unstable)")
    expect_error(install_asreml(force = T, keep_file = "abc"))
})

test_that("Update function works", {
    # skip_if(R.version$status == "Under development (unstable)")
    expect_message(update_asreml(), "ASreml-R successfully installed!")
})
