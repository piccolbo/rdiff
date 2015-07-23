
# diff for R code

strip.nulls =
  function(x)
    x[!sapply(x, is.null)]

diff.size =
  function(x, y = NULL) {
    if(is.null(y)) {
      if(is.null(x))
        0
      else {
        if(is.leaf(x)) 1
        else {
          diff.size(rhead(x)) +
            sum(unlist(sapply(rtail(x), diff.size)))}}}
    else{
      (diff.size(x) + diff.size(y))*
        if(class(x) == class(y))  .5 else 1 }}

order = function(x, ...) UseMethod("order")

order.default = base::order

order.expression =
  function(x)
    order(
      as.character(lapply(as.list(x), function(x) x[[1]])),
      as.character(lapply(as.list(x), function(x) x[[2]])),
      as.character(lapply(as.list(x), function(x) x[[3]])))

normalize = function(x, ...) UseMethod("normalize")

normalize.expression =
  function(x)
    as.expression(as.list(x)[order(x)])

normalize.default = identity

is.leaf =
  function(x)
    is.atomic(x) || is.symbol(x) || is.name(x) || length(x) == 0

Diff =
  function(left, right, dist = NULL)
    structure(
      data.frame(
        left = I(list(left)),
        right = I(list(right)),
        dist = {
          if(is.null(dist))
            diff.size(left, right)
          else dist}),
      class = c("Diff", "data.frame"))


`+.Diff` =
  function(d1, d2)
    rbind(d1, d2)

rdist = function(D) sum(D$dist)

min = function(..., na.rm = FALSE) UseMethod("min")
min.default = base::min
min.Diff =
  function(..., na.rm = FALSE)
    list(...)[[which.min(sapply(list(...), function(x) rdist(x)))]]

is.srcref = function(x) class(x) == "srcref"

rhead = function(x) x[[1]]
rtail = function(x) as.list(x[-1])
rdiff =
  memoise(
    function(x, y) {
      if(is.srcref(x) && is.srcref(y))
        NULL
      else {
        z = {
          if(identical(x,y)) NULL
          else {
            if(is.leaf(x) && is.leaf(y))
              Diff(x, y)
            else
              do.call(
                min,
                strip.nulls(
                  c(
                    if(!is.leaf(x) && !is.leaf(y))
                      list(
                        rdiff(rhead(x), rhead(y)) + rdiff(rtail(x), rtail(y)))
                    else NULL,
                    if(!is.leaf(x))
                      list(
                        Diff(rhead(x), NULL) + rdiff(rtail(x), y),
                        rdiff(rhead(x), y) + Diff(rtail(x), NULL))
                    else NULL,
                    if(!is.leaf(y))
                      list(
                        Diff(NULL, rhead(y)) + rdiff(x, rtail(y)),
                        rdiff(x, rhead(y)) + Diff(NULL, rtail(y)))
                    else NULL)))}}}
      Lstr(z)
      z})


reformat =
  function(x, indent = 0) {
    if(is.srcref(x) || is.null(x)) ""
    else {
      if(is.leaf(x))
        paste(
          paste0(rep(" ", 2 * indent), collapse = ""),
          paste0(as.character(x), collapse = "",
                 collapse = ""))
      else {
        paste(
          c(
            paste(reformat(rhead(x), indent), "("),
            sapply(rtail(x), reformat, indent = indent + 1)),
          collapse = "\n")}}}
