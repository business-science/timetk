expect_tz_warning <- function(object) {
    # FIXME when the tzone warning is fixed, delete this function,
    # use Replace all expect_tz_warning -> expect_no_warning
    # This is for R >= 4.3.0
    # Issue #156
    if (getRversion() >= "4.3.0") {
        expect_warning(object, regexp = "tzone")
    } else {
        expect_no_warning(object)
    }
}
