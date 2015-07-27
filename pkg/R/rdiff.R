is.simple =
  function(x) {
    tryCatch({x[[1]]; FALSE}, error = function(e) TRUE) ||
      is.atomic(x) ||
      length(x) == 0 }

flatten = function(x) {if(is.simple(x)) x else {x = as.list(x); unlist(map(x, flatten))}}

flatlen = function(x) {
  x = flatten(x)
  length(x) + length(names(x))}

Diff =
  function(x, y){
    if(!identical(x, y))
      data.frame(
        x = paste(as.character(x), collapse = " "),
        y = paste(as.character(y), collapse = " "),
        d = max(flatlen(x), flatlen(y)))}

swapcol = function(diff) data.frame(x = diff$y, y = diff$x,  d = diff$d)

headl =
  function(x) {
    x = as.list(x)[1]
    nx = names(x)
    if(is.null(nx) || is.na(nx) || nx == "")
      x[[1]]
    else
      call("=", nx, x[[1]])}

taill =
  function(x) {
    y = as.list(x)[-1]
    if(length(y) > 0)
      y}

diffdist = function(x) sum(x$d)

mindiff =
  function(xx) {
    xx[[which.min(map(xx, diffdist))]]}

rdiff =
  function(x, y, verbose = FALSE) {
    if(is.character(x)) x = parse(x)
    if(is.character(y)) y = parse(y)
    rdiffi(x, y, verbose)}

rdiffi =
  memoise(
    function(x, y, verbose) {
      rdiffv = partial(rdiffi, verbose = verbose)
      z = {
        if(is.null(x) || is.null(y) || (is.simple(x) && is.simple(y)))
           Diff(x, y)
        else {
          mindiff(
            c(
              if(!is.simple(x))
                list(
                  rbind(rdiffv(headl(x), y), Diff(taill(x), NULL)),
                  rbind(Diff(headl(x), NULL), rdiffv(taill(x), y))),
              if(!is.simple(y))
                list(
                  rbind(rdiffv(x, headl(y)), Diff(NULL, taill(y))),
                  rbind(Diff(NULL, headl(y)), rdiffv(x, taill(y)))),
              if(!(is.simple(x) || is.simple(y)))
                list(
                  rbind(
                    rdiffv(headl(x), headl(y)),
                    rdiffv(taill(x), taill(y))))))}}
      if(verbose){
        cat("---\n")
        str(x)
        str(y)
        print(z)}
      z})
