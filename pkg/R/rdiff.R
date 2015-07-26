is.simple =
  function(x) {
    tryCatch({x[[1]]; FALSE}, error = function(e) TRUE) ||
      is.atomic(x) ||
      length(x) == 0 }

flatten = function(x) {if(is.simple(x)) x else {x = as.list(x); unlist(map(x, flatten))}}

flatlen = function(x) length(flatten(x))

Diff =
  function(x, y){
    if(!identical(x, y))
      data.frame(x = I(list(x)), y = I(list(y)), d = flatlen(x) + flatlen(y))}

swapcol = function(diff) data.frame(x = diff$y, y = diff$x,  d = diff$d)

headl = function(x) as.list(x)[[1]]

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
  function(x, y) {
    str(x)
    str(y)
    cat("---\n")
    if(is.simple(x) && is.simple(y)) Diff(x, y)
    else {
      mindiff(
        c(
          if(!is.simple(x))
            list(
              rbind(rdiff(headl(x), y), Diff(taill(x), NULL)),
              rbind(Diff(headl(x), NULL), rdiff(taill(x), y))),
          if(!is.simple(y))
            list(
              rbind(rdiff(x, headl(y)), Diff(NULL, taill(y))),
              rbind(Diff(NULL, headl(y)), rdiff(x, taill(y)))),
          if(!(is.simple(x) || is.simple(y)))
            list(
              rbind(rdiff(headl(x), headl(y)), rdiff(taill(x), taill(y))))))}}
