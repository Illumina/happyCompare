## internal funtions

substrRight = function(x, n){
    sapply(x, function(xx)
        substr(xx, (nchar(xx) - n + 1), nchar(xx))
    )
}