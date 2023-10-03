expect_tz_warning <- function(object) {
    # FIXME when the tzone warning is fixed, delete this function,
    # use Replace all expect_tz_warning -> expect_no_warning
    # Issue #156
    expect_warning(object, regexp = "tzone")
}
