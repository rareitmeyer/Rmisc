

add_diffs <- function(data, colnames, op='-')
{
    opfn <- switch(op,
        '+'=`+`,
        '-'=`-`,
        '*'=`*`,
        '/'=`/`)
    stopifnot(!is.null(opfn))

    l <- combn(colnames, 2, simplify=F)
    values <- lapply(l, function(x) {
      opfn(data[,x[1]], data[,x[2]])
      })
    names(values) <- unlist(lapply(l, function(x){ paste(x, collapse=op) }))

    return(data.frame(values, check.names=F))
}

