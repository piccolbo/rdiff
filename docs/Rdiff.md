

# Syntax-directed diffs for R in R

*Unsatisfied with general purpose, syntax-oblivious diff tools we take the first step towards syntax-directed diffs for R.*

Like many developers, I use git to manage my source code and collaborate with others. One fundamental component of source code control is a tool to compare files, namely source code files. Most tools I am aware of are language-independent, which means the comparison algorithm has no knowledge of the syntax or semantics of the language. There are some exceptions in proprietary software, but I've never met anyone using them. On the one hand, this is good because they work with any language, be it computer or natural. On the other, it's bad because they identify sets of differences that are bigger than one would naturally want. What "natural" means needs to be detailed a little better, but here is one example that occurs in practice and about which I expect most people will agree.

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


```r
1,3c1,5
< x = x + 1
< x = x^2
< y = x - 1
---
> if(!is.null(x)) {
>   x = x + 1
>   x = x^2
>   y = x - 1
> }
```

I got a very large diff, in fact one encompassing every line of code in both versions. This is the archetypal useless diff: it doesn't convey more information than the dates on the files. It is terrible for identifying changes, fixing bugs introduced with a commit, evaluating contributions, you name it.
There are ways of telling diff to be a bit more flexible with white space, but they either are too conservative (`diff -B`) or too aggressive (`diff -w`). Moreover, in the best case diff can't identify a change below the resolution of a line, which is enough for short lines like these but not always.

To address these problems, we are going to write a program that detects changes at the token level. It ignores white space and other inessential differences, such as the use of single or double quotes for characters, by working on parsed expressions instead of text. It finds a smallest set of differences according to a reasonable weighting of the operations of insertion, deletion and substitution. One additional requirement aimed at achieving compatibility with tools like git would be the ability to output GNU unified diff format, but we won't cover this in the current post. This is not only in the interest of brevity, but also because such a format is incapable of representing token level changes. Suggestions on how to move past this hurdle are welcome.




```r
rdiff("snippet1.R", "snippet2.R")
```

```
#   x            y d
# 1             if 1
# 2   ! is.null(x) 3
# 3              { 1
```

What `rdiff` returns is a data frame with three columns, one for the first argument to the comparison, one for the second and one for an edit distance. Each row represents an individual difference: one element from the left argument, one from the right argument and their edit distance. In this case, `rdiff` reports that there is a new `if` statement, a new bracket pair --- represented by just `{` --- and a new condition. The algorithm could probably work harder to report this as a single, complex difference, but let's not go for bells and whistles. The important thing is that the difference between the two versions has been correctly identified as one or more additions and none of the original lines is reported as changed --- they are just in a different context.

Now let's lift the covers on the algorithm that performs this magic. The function `rdiff` is not particularly noteworthy:


```r
rdiff
```

```
# function(x, y, verbose = FALSE) {
#     if(is.character(x)) x = parse(x)
#     if(is.character(y)) y = parse(y)
#     rdiffi(x, y, verbose)}
# <environment: namespace:rdiff>
```

It just checks if the arguments are strings and parses the corresponding file in. Otherwise arguments are expected to be unevaluated R expressions, something you can generate also with `quote` or `substitute`. `rdiff` then calls `rdiffi`:

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
<span class="line">50  </span><span class="1">rdiffi</span> <span class="1">=</span>
<span class="line">51  </span>  <span class="1">memoise</span><span class="1">(</span>
<span class="line">52  </span>    <span class="1">function</span><span class="1">(</span><span class="1">x</span><span class="1">,</span> <span class="1">y</span><span class="1">,</span> <span class="1">verbose</span><span class="1">)</span> <span class="1">{</span>
<span class="line">53  </span>      <span class="1">rdiffv</span> <span class="1">=</span> <span class="1">partial</span><span class="1">(</span><span class="1">rdiffi</span><span class="1">,</span> <span class="1">verbose</span> <span class="1">=</span> <span class="1">verbose</span><span class="1">)</span>
<span class="line">54  </span>      <span class="1">z</span> <span class="1">=</span> <span class="1">{</span>
<span class="line">55  </span>        <span class="1">if</span><span class="1">(</span><span class="1">is.null</span><span class="1">(</span><span class="1">x</span><span class="1">)</span> <span class="1">||</span> <span class="1">is.null</span><span class="1">(</span><span class="1">y</span><span class="1">)</span> <span class="1">||</span> <span class="1">(</span><span class="1">is.simple</span><span class="1">(</span><span class="1">x</span><span class="1">)</span> <span class="1">&amp;&amp;</span> <span class="1">is.simple</span><span class="1">(</span><span class="1">y</span><span class="1">)</span><span class="1">)</span><span class="1">)</span>
<span class="line">56  </span>           <span class="1">Diff</span><span class="1">(</span><span class="1">x</span><span class="1">,</span> <span class="1">y</span><span class="1">)</span>
<span class="line">57  </span>        <span class="1">else</span> <span class="1">{</span>
<span class="line">58  </span>          <span class="1">mindiff</span><span class="1">(</span>
<span class="line">59  </span>            <span class="1">c</span><span class="1">(</span>
<span class="line">60  </span>              <span class="1">if</span><span class="1">(</span><span class="1">!</span><span class="1">is.simple</span><span class="1">(</span><span class="1">x</span><span class="1">)</span><span class="1">)</span>
<span class="line">61  </span>                <span class="1">list</span><span class="1">(</span>
<span class="line">62  </span>                  <span class="1">rbind</span><span class="1">(</span><span class="1">rdiffv</span><span class="1">(</span><span class="1">headl</span><span class="1">(</span><span class="1">x</span><span class="1">)</span><span class="1">,</span> <span class="1">y</span><span class="1">)</span><span class="1">,</span> <span class="1">Diff</span><span class="1">(</span><span class="1">taill</span><span class="1">(</span><span class="1">x</span><span class="1">)</span><span class="1">,</span> <span class="1">NULL</span><span class="1">)</span><span class="1">)</span><span class="1">,</span>
<span class="line">63  </span>                  <span class="1">rbind</span><span class="1">(</span><span class="1">Diff</span><span class="1">(</span><span class="1">headl</span><span class="1">(</span><span class="1">x</span><span class="1">)</span><span class="1">,</span> <span class="1">NULL</span><span class="1">)</span><span class="1">,</span> <span class="1">rdiffv</span><span class="1">(</span><span class="1">taill</span><span class="1">(</span><span class="1">x</span><span class="1">)</span><span class="1">,</span> <span class="1">y</span><span class="1">)</span><span class="1">)</span><span class="1">)</span><span class="1">,</span>
<span class="line">64  </span>              <span class="1">if</span><span class="1">(</span><span class="1">!</span><span class="1">is.simple</span><span class="1">(</span><span class="1">y</span><span class="1">)</span><span class="1">)</span>
<span class="line">65  </span>                <span class="1">list</span><span class="1">(</span>
<span class="line">66  </span>                  <span class="1">rbind</span><span class="1">(</span><span class="1">rdiffv</span><span class="1">(</span><span class="1">x</span><span class="1">,</span> <span class="1">headl</span><span class="1">(</span><span class="1">y</span><span class="1">)</span><span class="1">)</span><span class="1">,</span> <span class="1">Diff</span><span class="1">(</span><span class="1">NULL</span><span class="1">,</span> <span class="1">taill</span><span class="1">(</span><span class="1">y</span><span class="1">)</span><span class="1">)</span><span class="1">)</span><span class="1">,</span>
<span class="line">67  </span>                  <span class="1">rbind</span><span class="1">(</span><span class="1">Diff</span><span class="1">(</span><span class="1">NULL</span><span class="1">,</span> <span class="1">headl</span><span class="1">(</span><span class="1">y</span><span class="1">)</span><span class="1">)</span><span class="1">,</span> <span class="1">rdiffv</span><span class="1">(</span><span class="1">x</span><span class="1">,</span> <span class="1">taill</span><span class="1">(</span><span class="1">y</span><span class="1">)</span><span class="1">)</span><span class="1">)</span><span class="1">)</span><span class="1">,</span>
<span class="line">68  </span>              <span class="1">if</span><span class="1">(</span><span class="1">!</span><span class="1">(</span><span class="1">is.simple</span><span class="1">(</span><span class="1">x</span><span class="1">)</span> <span class="1">||</span> <span class="1">is.simple</span><span class="1">(</span><span class="1">y</span><span class="1">)</span><span class="1">)</span><span class="1">)</span>
<span class="line">69  </span>                <span class="1">list</span><span class="1">(</span>
<span class="line">70  </span>                  <span class="1">rbind</span><span class="1">(</span>
<span class="line">71  </span>                    <span class="1">rdiffv</span><span class="1">(</span><span class="1">headl</span><span class="1">(</span><span class="1">x</span><span class="1">)</span><span class="1">,</span> <span class="1">headl</span><span class="1">(</span><span class="1">y</span><span class="1">)</span><span class="1">)</span><span class="1">,</span>
<span class="line">72  </span>                    <span class="1">rdiffv</span><span class="1">(</span><span class="1">taill</span><span class="1">(</span><span class="1">x</span><span class="1">)</span><span class="1">,</span> <span class="1">taill</span><span class="1">(</span><span class="1">y</span><span class="1">)</span><span class="1">)</span><span class="1">)</span><span class="1">)</span><span class="1">)</span><span class="1">)</span><span class="1">}</span><span class="1">}</span>
<span class="line">73  </span>      <span class="1">if</span><span class="1">(</span><span class="1">verbose</span><span class="1">)</span><span class="1">{</span>
<span class="line">74  </span>        <span class="1">cat</span><span class="1">(</span><span class="1">"---\n"</span><span class="1">)</span>
<span class="line">75  </span>        <span class="1">str</span><span class="1">(</span><span class="1">x</span><span class="1">)</span>
<span class="line">76  </span>        <span class="1">str</span><span class="1">(</span><span class="1">y</span><span class="1">)</span>
<span class="line">77  </span>        <span class="1">print</span><span class="1">(</span><span class="1">z</span><span class="1">)</span><span class="1">}</span>
<span class="line">78  </span>      <span class="1">z</span><span class="1">}</span><span class="1">)</span>
</pre>
</body>
</html>

The line numbers are brittle to any upstream change in the file, so at the time of this writing the listing starts at 50. `rdiffi` is just the *memoized* version of another function. More on *memoization* and why it's necessary in this case later. At line 53 we are just taking the `verbose` argument out of the way by partial application to create `rdiffv`. This is nothing fundamental, but the `verbose` feature was an invaluable debugging tool and can be handy to understand the algorithm and maybe improve it later. Just remember to clear memoization data with `memoise::forget(rdiff:::rdiffi)` if you want to see the full search tree that the algorithm explores.

At line 55 the real action starts, with a distinction between *simple* and *complex* expressions. Without going into details, simple expressions can not be broken down into smaller elements: names, literals and so forth. Then the only action the algorithm can take is pricing that difference and keeping track of what caused it. This is done by the function `Diff`, which is responsible for creating one row of the output data frame. If the arguments are identical, then there is nothing to report and the output of `Diff` is `NULL`. If one of the two arguments to `rdiff` is empty, we can also just call `Diff` and move on: there is no way decomposing a complex expression will make it a better match with an empty expression. Likewise if both arguments are simple. If neither is empty and at least one of the two is complex, we move to lines 58-72 where a recursive decomposition happens.
We have a list of five different ways of decomposing a pair of complex expressions and comparing them. Not all make sense when one of the two arguments is simple, hence the `if` statements that check for that. When one of the arguments is simple, the list of possible decompositions has actually only two elements, with only two recursive calls to `rdiffv`. When both are complex, the list grows to five and the number of recursive calls to six.
The decomposition is performed by the functions `headl` and `taill`, which transform an expression into a list and then take its first or all its other elements, respectively. When the list is down to a single element, `headl` will return the contents of that element with the `[[]]` operator and `taill` will return `NULL`. There is an additional case for list of arguments, which become named lists, but there is not need to burden you with all the details. The idea is to decompose complex expressions into their first element and the rest and then try all possible ways of comparing the pieces. Head vs head and tail vs tail may be the most obvious, but we need to include also head vs everything and tail vs `NULL`, head vs NULL and tail vs everything and the latter two with the roles of the two arguments reversed. Using tree terminology, sometimes the head corresponds to the root of a parse subtree, sometimes it corresponds to the first of a list of subtrees. In either case we are asking the question: what if the root or first subtree has been changed or deleted? What if it stayed the same? Then we continue to explore the same possibilities on the remaining subtrees Once the results are in, in the form of `Diff`-generated data frames, we can just combine them with rbind, and `mindiff` has the role of finding the option that returned the lowest cost diff - the cost of a diff is just the sum of the thrid column containing the weight for each change. This is an application of a divide and conquer strategy, but with multiple ways of subdividing an instance, each of which needs to be evaluated. Precise calculations are beyond the scope of this post, but since we have 6 recursive calls in the worst case scenario and we can hope at best to halve the complexity of the expressions under examinations at each level of the recursion, we can expect $\sim 6^h$ calls where $h$ is the height of the largest parse tree or $\sim N^{log_2(6)} \simeq N^{2.6}$ where $N$ is its size. I would have to bring a lot more math to bear to prove this result with any degree of confidence, but this suggests that it's going to be a fairly slow algorithm. One thing we can do to improve on it is to use memoization, that is store the results of each comparison of subtrees. Since there are at most $N$ nodes, we can only perform $N^2$ comparisons, each of which takes only a finite number of steps in addition to the comparisons of the subtrees, which are also stored. By just doing that we have shed 0.6 in the exponent of our preliminary analysis, and we have a stronger analysis for good measure! Albeit some try to identify some minor differences with dynamic programming, recursion with memoization is pretty much the same.
