GNU Find for GNU R
================

## Purpose

This package provides a simple wrapper using `system2` for calling [GNU
Find](https://www.gnu.org/software/findutils/).

## Hello World

Let’s see all the R files we’ve modified in the last hour. But we don’t
want to see any the were generated in .Rcheck folders.

``` r
library(gnufind)
fs <- gnu_find(search_path = "~", 
               name = "*.R", 
               type = "f", 
               newermt = "1 hour ago",
               cmp = not(path = "*Rcheck*"))
```

`cmp` is a shorthand for making a `compound` statement. The three kinds
of compound statements built-in are `or`, `and`, and `not`. New compound
statements can be generated using `compound`. And you can use more than
one compound statement. For example, suppose you want to exclude a few
directories from your search path:

``` r
os <- gnu_find(search_path = "~", 
               cmp = or(name = c("*.R", "*.py")), 
               cmp = and(not(path = "*lib*"), 
                         not(path = "*cache*")))
```

In addition to compound statements, there are also switches. We might
use the `-empty` switch to find all empty directories.

``` r
ed <- gnu_find(search_path = "~", type = "d", sw = "empty")
```
