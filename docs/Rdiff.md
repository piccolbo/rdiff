


*Unsatisfied with general purpose, syntax-oblivious diff tools I take the first step towards syntax-directed diffs for R.*

Like many developers, I use git to manage my source code and collaborate with others. One fundamental component of source code control is a tool to compare files, namely source code files. Most tools I am aware of are language-independent, which means the comparison algorithm has no knowledge of the syntax or semantics of the language. There are some exceptions in proprietary software, but I've never met anyone using them. On the one hand, this is an advantage because they work with any language, be it computer or natural. On the other, it's bad because they identify sets of differences that are bigger than one would naturally want. What "natural" means needs to be detailed a little better, but here is one example that occurs in practice and about which I expect most people will agree.

Let's say that the first version of a program was as follows:


```r
x = x + 1
x = x^2
y = x - 1
```

Later, I added an `if` statement "wrapped" around the entire source file and re-indented accordingly:


```r
if(!is.null(x)) {
  x = x + 1
  x = x^2
  y = x - 1
}
```

Then I run a traditional, syntax-oblivious diff tool, in preparation for committing the latest version.


```
# 1,3c1,5
# < x = x + 1
# < x = x^2
# < y = x - 1
# ---
# > if(!is.null(x)) {
# >   x = x + 1
# >   x = x^2
# >   y = x - 1
# > }
```

I got a very large diff, in fact one encompassing every line of code in both versions. This is the archetypal useless diff: it doesn't convey more information than, say, a hash function. It is terrible for identifying changes, fixing bugs introduced with a commit, evaluating contributions, you name it.
There are ways of telling diff to be a bit more flexible with white space, but they either are too conservative (`diff -B`) or too aggressive (`diff -w`). Moreover, in the best case diff can't identify a change below the resolution of a line, which is enough for short lines like these but not always. Ediff has a refine mode that can go beyond this limitation, but is not syntax oriented.

To address these problems, I am  going to write a function, `rdiff`, that detects changes at the token level. It ignores white space and other inessential differences, such as the use of single or double quotes for characters, by working on parsed expressions instead of text. It finds a smallest set of differences according to a reasonable weighting of the operations of insertion, deletion and substitution. Here is the output of `rdiff` on the above example:


```
#       x                     y d
# 1  NULL             symbol if 1
# 2  NULL  language !is.null(x) 5
# 3  NULL              symbol { 1
```

What `rdiff` returns is a data frame with three columns, one for the first argument to the comparison, one for the second and one for the edit distance. In this case, `rdiff` reports that there is a new `if` statement, a new bracket pair --- represented by just `{` --- and a new condition. The algorithm could probably work harder to report this as a single, complex difference, but let's not go for bells and whistles. The important thing is that the difference between the two versions has been correctly identified as one or more additions and none of the original lines is reported as changed --- they are just in a different context.

Let's look at a realistic example. the implementation of the famed ``magrittr::`%>%` ``:


```r
function (lhs, rhs) 
{
    parent <- parent.frame()
    env <- new.env(parent = parent)
    chain_parts <- split_chain(match.call(), env = env)
    pipes <- chain_parts[["pipes"]]
    rhss <- chain_parts[["rhss"]]
    lhs <- chain_parts[["lhs"]]
    env[["_function_list"]] <- lapply(1:length(rhss), function(i) wrap_function(rhss[[i]], 
        pipes[[i]], parent))
    env[["_fseq"]] <- `class<-`(eval(quote(function(value) freduce(value, 
        `_function_list`)), env, env), c("fseq", "function"))
    env[["freduce"]] <- freduce
    if (is_placeholder(lhs)) {
        env[["_fseq"]]
    }
    else {
        env[["_lhs"]] <- eval(lhs, parent, parent)
        result <- withVisible(eval(quote(`_fseq`(`_lhs`)), env, 
            env))
        if (is_compound_pipe(pipes[[1L]])) {
            eval(call("<-", lhs, result[["value"]]), parent, 
                parent)
        }
        else {
            if (result[["visible"]]) 
                result[["value"]]
            else invisible(result[["value"]])
        }
    }
}
```

A mischievous CamelCase troll decided to replace the two function arguments names with more "expressive" ones, yielding this version:


```r
function (leftHandSide, rightHandSide) 
{
    parent <- parent.frame()
    env <- new.env(parent = parent)
    chain_parts <- split_chain(match.call(), env = env)
    pipes <- chain_parts[["pipes"]]
    rightHandSides <- chain_parts[["rightHandSides"]]
    leftHandSide <- chain_parts[["leftHandSide"]]
    env[["_function_list"]] <- lapply(1:length(rightHandSides), function(i) wrap_function(rightHandSides[[i]],
        pipes[[i]], parent))
    env[["_fseq"]] <- `class<-`(eval(quote(function(value) freduce(value,
        `_function_list`)), env, env), c("fseq", "function"))
    env[["freduce"]] <- freduce
    if (is_placeholder(leftHandSide)) {
        env[["_fseq"]]
    }
    else {
        env[["_leftHandSide"]] <- eval(leftHandSide, parent, parent)
        result <- withVisible(eval(quote(`_fseq`(`_leftHandSide`)), env,
            env))
        if (is_compound_pipe(pipes[[1L]])) {
            eval(call("<-", leftHandSide, result[["value"]]), parent,
                parent)
        }
        else {
            if (result[["visible"]])
                result[["value"]]
            else invisible(result[["value"]])
        }
    }
}
```

The resulting diff is quite large, and it's hard to tell quickly and with certainty if there is anything else going on besides the variable renaming:


```
# 1c1
# < function (lhs, rhs) 
# ---
# > function (leftHandSide, rightHandSide) 
# 7,9c7,9
# <     rhss <- chain_parts[["rhss"]]
# <     lhs <- chain_parts[["lhs"]]
# <     env[["_function_list"]] <- lapply(1:length(rhss), function(i) wrap_function(rhss[[i]], 
# ---
# >     rightHandSides <- chain_parts[["rightHandSides"]]
# >     leftHandSide <- chain_parts[["leftHandSide"]]
# >     env[["_function_list"]] <- lapply(1:length(rightHandSides), function(i) wrap_function(rightHandSides[[i]],
# 14c14
# <     if (is_placeholder(lhs)) {
# ---
# >     if (is_placeholder(leftHandSide)) {
# 18,19c18,19
# <         env[["_lhs"]] <- eval(lhs, parent, parent)
# <         result <- withVisible(eval(quote(`_fseq`(`_lhs`)), env, 
# ---
# >         env[["_leftHandSide"]] <- eval(leftHandSide, parent, parent)
# >         result <- withVisible(eval(quote(`_fseq`(`_leftHandSide`)), env,
# 22c22
# <             eval(call("<-", lhs, result[["value"]]), parent, 
# ---
# >             eval(call("<-", leftHandSide, result[["value"]]), parent,
```

Instead, with `rdiff`:

```
#               x                      y d
# 1     chr "lhs"     chr "leftHandSide" 1
# 2     chr "rhs"    chr "rightHandSide" 1
# 3   symbol rhss  symbol rightHandSides 1
# 4    chr "rhss"   chr "rightHandSides" 1
# 5    symbol lhs    symbol leftHandSide 1
# 6     chr "lhs"     chr "leftHandSide" 1
# 7   symbol rhss  symbol rightHandSides 1
# 8   symbol rhss  symbol rightHandSides 1
# 9    symbol lhs    symbol leftHandSide 1
# 10   chr "_lhs"    chr "_leftHandSide" 1
# 11   symbol lhs    symbol leftHandSide 1
# 12  symbol _lhs   symbol _leftHandSide 1
# 13   symbol lhs    symbol leftHandSide 1
```

This is a much more focused set of differences. In fact, we can think of summarizing it in an alternative way where identical rows are condensed to one, extended with the number of occurrences.


Now let's lift the covers on the algorithm that performs this magic. The function `rdiff` is just a wrapper that does argument processing and then calls `rdiffi`:

<html>
<head>
<style type="text/css">
.number{
	color: rgb(21,20,181) ;
}

.functioncall{
	color: red ;
}

.string{
	color: rgb(153,153,255) ;
}

.keyword{
	color: black;
}

.argument{
	color: rgb( 177,63,5) ;
}

.comment{
	color: rgb( 204,204,204) ;
}

.roxygencomment{
	color: rgb(0,151,255);
}

.formalargs{
	color: rgb(18,182,18);
}

.eqformalargs{
	color: rgb(18,182,18);
}

.assignement{
	color: rgb(55,55,98);
}

.package{
	color: rgb(150,182,37);
}

.slot{
	font-style:italic;
}

.symbol{
	color: black ;
}

.prompt{
	color: black ;
}

.line{
    color: gray ;   
}
</style>
</head>
<body>
<pre>
<span class="line">61  </span><span class="1">rdiffi</span> <span class="1">=</span>
<span class="line">62  </span>  <span class="1">memoise</span><span class="1">(</span>
<span class="line">63  </span>    <span class="1">function</span><span class="1">(</span><span class="1">x</span><span class="1">,</span> <span class="1">y</span><span class="1">,</span> <span class="1">verbose</span><span class="1">)</span> <span class="1">{</span>
<span class="line">64  </span>      <span class="1">rdiffv</span> <span class="1">=</span> <span class="1">partial</span><span class="1">(</span><span class="1">rdiffi</span><span class="1">,</span> <span class="1">verbose</span> <span class="1">=</span> <span class="1">verbose</span><span class="1">)</span>
<span class="line">65  </span>      <span class="1">if</span><span class="1">(</span><span class="1">class</span><span class="1">(</span><span class="1">x</span><span class="1">)</span> <span class="1">==</span> <span class="1">"srcref"</span><span class="1">)</span> <span class="1">x</span> <span class="1">=</span> <span class="1">NULL</span>
<span class="line">66  </span>      <span class="1">if</span><span class="1">(</span><span class="1">class</span><span class="1">(</span><span class="1">y</span><span class="1">)</span> <span class="1">==</span> <span class="1">"srcref"</span><span class="1">)</span> <span class="1">y</span> <span class="1">=</span> <span class="1">NULL</span>
<span class="line">67  </span>      <span class="1">z</span> <span class="1">=</span> <span class="1">{</span>
<span class="line">68  </span>        <span class="1">if</span><span class="1">(</span><span class="1">is.simple</span><span class="1">(</span><span class="1">x</span><span class="1">)</span> <span class="1">&amp;&amp;</span> <span class="1">is.simple</span><span class="1">(</span><span class="1">y</span><span class="1">)</span><span class="1">)</span>
<span class="line">69  </span>          <span class="1">Diff</span><span class="1">(</span><span class="1">x</span><span class="1">,</span> <span class="1">y</span><span class="1">)</span>
<span class="line">70  </span>        <span class="1">else</span> <span class="1">{</span>
<span class="line">71  </span>          <span class="1">mindiff</span><span class="1">(</span>
<span class="line">72  </span>            <span class="1">c</span><span class="1">(</span>
<span class="line">73  </span>              <span class="1">if</span><span class="1">(</span><span class="1">!</span><span class="1">is.simple</span><span class="1">(</span><span class="1">x</span><span class="1">)</span><span class="1">)</span>
<span class="line">74  </span>                <span class="1">list</span><span class="1">(</span>
<span class="line">75  </span>                  <span class="1">rbind</span><span class="1">(</span><span class="1">rdiffv</span><span class="1">(</span><span class="1">headl</span><span class="1">(</span><span class="1">x</span><span class="1">)</span><span class="1">,</span> <span class="1">y</span><span class="1">)</span><span class="1">,</span> <span class="1">Diff</span><span class="1">(</span><span class="1">taill</span><span class="1">(</span><span class="1">x</span><span class="1">)</span><span class="1">,</span> <span class="1">NULL</span><span class="1">)</span><span class="1">)</span><span class="1">,</span>
<span class="line">76  </span>                  <span class="1">rbind</span><span class="1">(</span><span class="1">Diff</span><span class="1">(</span><span class="1">headl</span><span class="1">(</span><span class="1">x</span><span class="1">)</span><span class="1">,</span> <span class="1">NULL</span><span class="1">)</span><span class="1">,</span> <span class="1">rdiffv</span><span class="1">(</span><span class="1">taill</span><span class="1">(</span><span class="1">x</span><span class="1">)</span><span class="1">,</span> <span class="1">y</span><span class="1">)</span><span class="1">)</span><span class="1">)</span><span class="1">,</span>
<span class="line">77  </span>              <span class="1">if</span><span class="1">(</span><span class="1">!</span><span class="1">is.simple</span><span class="1">(</span><span class="1">y</span><span class="1">)</span><span class="1">)</span>
<span class="line">78  </span>                <span class="1">list</span><span class="1">(</span>
<span class="line">79  </span>                  <span class="1">rbind</span><span class="1">(</span><span class="1">rdiffv</span><span class="1">(</span><span class="1">x</span><span class="1">,</span> <span class="1">headl</span><span class="1">(</span><span class="1">y</span><span class="1">)</span><span class="1">)</span><span class="1">,</span> <span class="1">Diff</span><span class="1">(</span><span class="1">NULL</span><span class="1">,</span> <span class="1">taill</span><span class="1">(</span><span class="1">y</span><span class="1">)</span><span class="1">)</span><span class="1">)</span><span class="1">,</span>
<span class="line">80  </span>                  <span class="1">rbind</span><span class="1">(</span><span class="1">Diff</span><span class="1">(</span><span class="1">NULL</span><span class="1">,</span> <span class="1">headl</span><span class="1">(</span><span class="1">y</span><span class="1">)</span><span class="1">)</span><span class="1">,</span> <span class="1">rdiffv</span><span class="1">(</span><span class="1">x</span><span class="1">,</span> <span class="1">taill</span><span class="1">(</span><span class="1">y</span><span class="1">)</span><span class="1">)</span><span class="1">)</span><span class="1">)</span><span class="1">,</span>
<span class="line">81  </span>              <span class="1">if</span><span class="1">(</span><span class="1">!</span><span class="1">(</span><span class="1">is.simple</span><span class="1">(</span><span class="1">x</span><span class="1">)</span> <span class="1">||</span> <span class="1">is.simple</span><span class="1">(</span><span class="1">y</span><span class="1">)</span><span class="1">)</span><span class="1">)</span>
<span class="line">82  </span>                <span class="1">list</span><span class="1">(</span>
<span class="line">83  </span>                  <span class="1">rbind</span><span class="1">(</span>
<span class="line">84  </span>                    <span class="1">rdiffv</span><span class="1">(</span><span class="1">headl</span><span class="1">(</span><span class="1">x</span><span class="1">)</span><span class="1">,</span> <span class="1">headl</span><span class="1">(</span><span class="1">y</span><span class="1">)</span><span class="1">)</span><span class="1">,</span>
<span class="line">85  </span>                    <span class="1">rdiffv</span><span class="1">(</span><span class="1">taill</span><span class="1">(</span><span class="1">x</span><span class="1">)</span><span class="1">,</span> <span class="1">taill</span><span class="1">(</span><span class="1">y</span><span class="1">)</span><span class="1">)</span><span class="1">)</span><span class="1">)</span><span class="1">)</span><span class="1">)</span><span class="1">}</span><span class="1">}</span>
<span class="line">86  </span>      <span class="1">if</span><span class="1">(</span><span class="1">verbose</span><span class="1">)</span><span class="1">{</span>
<span class="line">87  </span>        <span class="1">cat</span><span class="1">(</span><span class="1">"---\n"</span><span class="1">)</span>
<span class="line">88  </span>        <span class="1">str</span><span class="1">(</span><span class="1">x</span><span class="1">)</span>
<span class="line">89  </span>        <span class="1">str</span><span class="1">(</span><span class="1">y</span><span class="1">)</span>
<span class="line">90  </span>        <span class="1">print</span><span class="1">(</span><span class="1">z</span><span class="1">)</span><span class="1">}</span>
<span class="line">91  </span>      <span class="1">z</span><span class="1">}</span><span class="1">)</span>
</pre>
</body>
</html>

The line numbers are brittle to any upstream change in the file, so at the time of this writing the listing starts at line 61. `rdiffi` is just the *memoized* version of another function. More on *memoization* and why it's necessary in this case later. `rdiffi` accepts as arguments two unevaluated R expressions, the sort of objects created by parsing a file or calling `quote`.  At line 64 I am just taking the `verbose` argument out of the way by partial application to create `rdiffv`. This is nothing fundamental, but the `verbose` feature was an invaluable debugging tool and can be handy to understand the algorithm and maybe improve it later. Just remember to clear memoization data with `memoise::forget(rdiff:::rdiffi)` if you want to see the full search tree that the algorithm explores. The following lines are used to ignore any `srcref` attribute, as it seems best excluded from any source comparison (it provides the relation between R code and the source text it was parsed from) 

At line 68 the real action starts, with a distinction between *simple* and *complex* expressions. Simple expressions can not be broken down into smaller elements: names, literals and so forth. The only action the algorithm needs to take for two simple expression is pricing their difference and keeping track of what caused it. This is done by the function `Diff`, which is responsible for creating one row of the output data frame. If the arguments are identical, then there is nothing to report and the output of `Diff` is `NULL`. If one of the two arguments to `rdiff` is empty, we can also just call `Diff` and move on: there is no way decomposing a complex expression will make it a better match with an empty expression. If neither is empty and at least one of the two is complex, we move to lines 72--85 where a recursive decomposition happens.
We have a list of five different ways of decomposing a pair of expressions and comparing them. Not all make sense when one of the two arguments is simple, hence the `if` statements that check for that. When one of the arguments is simple, the list of possible decompositions has actually only two elements, with only two recursive calls to `rdiffv`. When both are complex, the list grows to five and the number of recursive calls to six.
The decomposition is performed by the functions `headl` and `taill`, which transform an expression into a list and then take its first or all its other elements, respectively. When the list is down to a single element, `headl` will return that element and `taill` will return `NULL`. There is an additional case for lists of arguments, which become named lists, but there is no need to burden you with all the details. The idea is to decompose complex expressions into their first element and the rest and then try all possible ways of comparing the pieces. Head vs. head and tail vs. tail may be the most obvious, but we need to include also head vs. everything and tail vs. `NULL`, head vs. NULL and tail vs. everything and the latter two with the roles of the two arguments reversed. Using tree terminology, sometimes the head corresponds to the root of a parse subtree, sometimes it corresponds to the first of a list of subtrees. In either case we are asking the question: what if the root or first subtree has been changed or deleted? What if it stayed the same? Then we continue to explore the same possibilities on the remaining subtrees. Once the results are in, in the form of `Diff`-generated data frames, we can just combine them with rbind, and `mindiff` has the role of finding the option that returned the lowest cost diff --- the cost of a diff is just the sum of the thrid column containing the cost for each change.

As you may have already realized, this is an application of divide and conquer strategy, but with multiple ways of subdividing an instance, each of which needs to be evaluated. Precise calculations are beyond the scope of this post, but since we have 6 recursive calls in the worst case scenario and we can hope at best to halve the complexity of the expressions under examinations at each level of the recursion, we can expect $\sim 6^h$ calls where $h$ is the height of the largest parse tree or $\sim N^{log_2(6)} \simeq N^{2.6}$ where $N$ is its size. In case of unbalanced trees, things can get much worse, like $\sim 5^N$. I would have to bring a lot more math to bear to support this analysis, but this suggests that it's going to be a slow to completely impractical algorithms. One thing we can do to improve on it is to use memoization, that is store the results of each comparison of subtrees, and R has a package just for that, `memoize`. Since there are at most $N$ nodes, we can only perform $N^2$ comparisons, each of which takes only a finite number of steps in addition to the comparisons of the subtrees, which are also stored. By this simple step we have lowered the complexity to $N^2$ in all cases and simplified the analysis! Albeit some try to identify some minor differences with dynamic programming, recursion with memoization is pretty much the same. A more complete analysis sould take into account not only the depth but also the degree of each node, and most results concerning the comparison of lableled trees, the theoretical version of this problem, contain a dependency on the highest degree (a [survey](https://scholar.google.com/scholar?cluster=10857095216044257415&hl=en&as_sdt=0,5&as_vis=1) of algorithms for this and related problems is available). Here I am assuming the degree to be bounded by a small constant. It may be an appropriate assumption when dealing with code snippets, but less than adequate when comparing complete programs.

Where do we go from here? An $N^2$ algorithm is not going to become a workhorse for source code control. Diffing the above `magrittr` function takes several minutes. Diffing is a common operation and performance needs to scale well with program size. The aforementioned survey describes advanced algorithms that chip at the complexity a little, but they don't change the big picture. A faster language rewrite is always an option, but the asymptotic complexity would be the same. A more promising approach may be to focus on the specific case of large programs with small differences, which is the most common usage scenario for this type of programs and I am working on an idea related to the *branch and bound* technique. Speed is not quite enough, though. We also need to produce output in a well known format for integration with other components in a versioning system. A possible candidate is the [unified diff format](https://en.wikipedia.org/wiki/Diff_utility#Unified_format), which is in widespread use but can only report changes at the line level, negating some of the advantages of the new algorithm. If you have any suggestions, particularly on this issue, they are [welcome](https://github.com/piccolbo/rdiff/issues), as are [pull requests](https://github.com/piccolbo/rdiff).
