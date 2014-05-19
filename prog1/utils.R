getpadstring <- function(num) {
    if (num < 10) {
        "00"
    } else if (num < 100) {
        "0"
    } else {
        ""
    }
}