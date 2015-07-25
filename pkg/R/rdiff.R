
# diff for R code

strip.nulls =
  function(x) {
    x = x[!sapply(x, is.null)]
    if(length(x) == 0) NULL
    else
      x}

diff.size =
  function(x, y = NULL) {
    if(is.null(y)) {
      if(is.null(x))
        0
      else {
        if(is.leaf(x)) 1
        else {
          diff.size(x[[1]]) +
            sum(unlist(sapply(x[-1], diff.size)))}}}
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
    is.null(x) || is.atomic(x) || is.symbol(x) || is.name(x) || length(x) == 0

is.operator =
  function(x)
    x %in% c("+", "-", "*", "^", "%%", "%/%", "/",
             "==", ">", "<", "!=", "<=", ">=",
             "&", "|")

wrap  =
  function(x) {
    if(is.null(x)) x
    else {
    x = deparse(x)
    ux =
        paste0(
          strsplit(x, "")[[1]],
          intToUtf8(c(0x0332)),
          collapse = "")
    as.name(
      if(is.operator(x)) paste0("%", ux, "%")
      else (ux))}}


Diff =
  function(left, right, dist = NULL){
    is.diff = !identical(left, right)
    list(
      left  = if(is.diff) wrap(left) else left,
      right = if(is.diff) wrap(right) else right,
      report =
        if(is.diff)
          data.frame(
            left  = I(list(left)),
            right = I(list(right)),
            dist = {
              if(is.null(dist))
                diff.size(left, right)
              else dist}))}

rebuild =
  function(x) {
    x = strip.nulls(x)
    switch(
      length(x) + 1,
      NULL,
      x[[1]],
      as.call(x),
      as.call(x))}

cd =
  combine.diffs =
  function(...) {
    ds = list(...)
    left  = rebuild(map(ds, "left"))
    right = rebuild(map(ds, "right"))
    list(
      left   = left,
      right  = right,
      report = do.call(rbind, map(ds, "report")))}

rdist = function(D) sum(D$report$dist)


mindiff =
  function(..., na.rm = FALSE)
    list(...)[[which.min(sapply(list(...), function(x) rdist(x)))]]

is.srcref = function(x) class(x) == "srcref"

op = function(x) el(x, 1)
l = function(x) el(x, 2)
r = function(x) el(x, 3)
el = function(x, i) if(i <= length(x)) x[[i]]

rdiff =
  memoise(
    function(x, y) {
      str(x)
      str(y)
      if((is.null(x) || is.null(y)) ||
         (identical(x,y)) ||
         (is.leaf(x) && is.leaf(y))) Diff(x, y)
      else {
        if(is.srcref(x) && is.srcref(y)) NULL
        else {
          mindiff(
            cd(rdiff(op(x), op(y)) , rdiff(l(x), l(y)) , rdiff(r(x), r(y))),
            cd(rdiff(op(x), NULL)  , rdiff(l(x),   y)  , rdiff(r(x), NULL)),
            cd(rdiff(op(x), NULL)  , rdiff(l(x), NULL) , rdiff(r(x), y)),
            cd(rdiff(NULL,  op(y)) , rdiff(  x,  l(y)) , rdiff(NULL, r(y))),
            cd(rdiff(NULL,  op(y)) , rdiff(NULL, l(y)) , rdiff(  x,  r(y))))}}})


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
