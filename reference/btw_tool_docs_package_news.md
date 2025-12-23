# Tool: Package Release Notes

Include release notes for a package, either the release notes for the
most recent package release or release notes matching a search term.

## Usage

``` r
btw_tool_docs_package_news(package_name, search_term = "", `_intent` = "")
```

## Arguments

- package_name:

  The name of the package as a string, e.g. `"shiny"`.

- search_term:

  A regular expression to search for in the NEWS entries. If empty, the
  release notes of the current installed version is included.

- \_intent:

  An optional string describing the intent of the tool use. When the
  tool is used by an LLM, the model will use this argument to explain
  why it called the tool.

## Value

Returns the release notes for the currently installed version of the
package, or the release notes matching the search term.

## See also

[`btw_tools()`](https://posit-dev.github.io/btw/reference/btw_tools.md)

Other docs tools:
[`btw_tool_package_docs`](https://posit-dev.github.io/btw/reference/btw_tool_package_docs.md)

## Examples

``` r
# Copy release notes to the clipboard for use in any AI app
btw("@news dplyr", clipboard = FALSE)
#> ## Context
#> 
#> ### dplyr v1.1.4
#> 
#> - `join_by()` now allows its helper functions to be namespaced with
#>   `dplyr::`, like `join_by(dplyr::between(x, lower, upper))` (#6838).
#> 
#> - `left_join()` and friends now return a specialized error message if
#>   they detect that your join would return more rows than dplyr can
#>   handle (#6912).
#> 
#> - `slice_*()` now throw the correct error if you forget to name `n`
#>   while also prefixing the call with `dplyr::` (#6946).
#> 
#> - `dplyr_reconstruct()`\'s default method has been rewritten to avoid
#>   materializing duckplyr queries too early (#6947).
#> 
#> - Updated the `storms` data to include 2022 data (#6937, \@steveharoz).
#> 
#> - Updated the `starwars` data to use a new API, because the old one is
#>   defunct. There are very minor changes to the data itself (#6938,
#>   \@steveharoz).

btw("@news dplyr join_by", clipboard = FALSE)
#> ## Context
#> 
#> ### dplyr v1.1.4
#> 
#> `join_by()` now allows its helper functions to be namespaced with
#> `dplyr::`, like `join_by(dplyr::between(x, lower, upper))` (#6838).
#> 
#> ### dplyr v1.1.0
#> 
#> #### New features
#> 
#> Joins have been completely overhauled to enable more flexible join
#> operations and provide more tools for quality control. Many of these
#> changes are inspired by data.table\'s join syntax (#5914, #5661, #5413,
#> #2240).
#> 
#> - A *join specification* can now be created through `join_by()`. This
#>   allows you to specify both the left and right hand side of a join
#>   using unquoted column names, such as
#>   `join_by(sale_date == commercial_date)`. Join specifications can be
#>   supplied to any `*_join()` function as the `by` argument.
#> 
#> - Join specifications allow for new types of joins:
#> 
#>   - Equality joins: The most common join, specified by `==`. For
#>     example, `join_by(sale_date == commercial_date)`.
#> 
#>   - Inequality joins: For joining on inequalities, i.e.`>=`, `>`, `<`,
#>     and `<=`. For example, use `join_by(sale_date >= commercial_date)`
#>     to find every commercial that aired before a particular sale.
#> 
#>   - Rolling joins: For \"rolling\" the closest match forward or
#>     backwards when there isn\'t an exact match, specified by using the
#>     rolling helper, `closest()`. For example,
#>     `join_by(closest(sale_date >= commercial_date))` to find only the
#>     most recent commercial that aired before a particular sale.
#> 
#>   - Overlap joins: For detecting overlaps between sets of columns,
#>     specified by using one of the overlap helpers: `between()`,
#>     `within()`, or `overlaps()`. For example, use
#>     `join_by(between(commercial_date, sale_date_lower, sale_date))` to
#>     find commercials that aired before a particular sale, as long as
#>     they occurred after some lower bound, such as 40 days before the
#>     sale was made.
#> 
#>   Note that you cannot use arbitrary expressions in the join conditions,
#>   like `join_by(sale_date - 40 >= commercial_date)`. Instead, use
#>   `mutate()` to create a new column containing the result of
#>   `sale_date - 40` and refer to that by name in `join_by()`.
#> 
#> - `multiple` is a new argument for controlling what happens when a row
#>   in `x` matches multiple rows in `y`. For equality joins and rolling
#>   joins, where this is usually surprising, this defaults to signalling a
#>   `"warning"`, but still returns all of the matches. For inequality
#>   joins, where multiple matches are usually expected, this defaults to
#>   returning `"all"` of the matches. You can also return only the
#>   `"first"` or `"last"` match, `"any"` of the matches, or you can
#>   `"error"`.
#> 
#> - `keep` now defaults to `NULL` rather than `FALSE`. `NULL` implies
#>   `keep = FALSE` for equality conditions, but `keep = TRUE` for
#>   inequality conditions, since you generally want to preserve both sides
#>   of an inequality join.
#> 
#> - `unmatched` is a new argument for controlling what happens when a row
#>   would be dropped because it doesn\'t have a match. For backwards
#>   compatibility, the default is `"drop"`, but you can also choose to
#>   `"error"` if dropped rows would be surprising.

if (interactive()) { # can be slow
  if (R.version$major == 4 && R.version$minor > "2.0") {
    # Search through R's release notes.
    # This should find a NEWS entry from R 4.2
    btw("@news R dynamic rd content", clipboard = FALSE)
  }
}

# Tool use by LLMs via ellmer or MCP ----
btw_tool_docs_package_news("dplyr")
#> <btw::BtwPackageNewsToolResult>
#>  @ value  : chr "### dplyr v1.1.4\n\n- `join_by()` now allows its helper functions to be namespaced with\n  `dplyr::`, like `joi"| __truncated__
#>  @ error  : NULL
#>  @ extra  :List of 1
#>  .. $ display:List of 1
#>  ..  ..$ markdown: chr "### dplyr v1.1.4\n\n- `join_by()` now allows its helper functions to be namespaced with\n  `dplyr::`, like `joi"| __truncated__
#>  @ request: NULL

btw_tool_docs_package_news("dplyr", "join_by")
#> <btw::BtwPackageNewsToolResult>
#>  @ value  : chr "### dplyr v1.1.4\n\n`join_by()` now allows its helper functions to be namespaced with\n`dplyr::`, like `join_by"| __truncated__
#>  @ error  : NULL
#>  @ extra  :List of 1
#>  .. $ display:List of 1
#>  ..  ..$ markdown: chr "### dplyr v1.1.4\n\n`join_by()` now allows its helper functions to be namespaced with\n`dplyr::`, like `join_by"| __truncated__
#>  @ request: NULL
```
