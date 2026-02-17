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

[`btw_tools()`](https://posit-dev.github.io/btw/dev/reference/btw_tools.md)

Other docs tools:
[`btw_tool_package_docs`](https://posit-dev.github.io/btw/dev/reference/btw_tool_package_docs.md)

## Examples

``` r
# Copy release notes to the clipboard for use in any AI app
btw("@news dplyr", clipboard = FALSE)
#> ## Context
#> 
#> ### dplyr v1.2.0
#> 
#> #### New features
#> 
#> - New `filter_out()` companion to `filter()`.
#> 
#>   - Use `filter()` when specifying rows to *keep*.
#> 
#>   - Use `filter_out()` when specifying rows to *drop*.
#> 
#>   `filter_out()` simplifies cases where you would have previously used a
#>   `filter()` to drop rows. It is particularly useful when missing values
#>   are involved. For example, to drop rows where the `count` is zero:
#> 
#>   ``` r
#>   df |> filter(count != 0 | is.na(count))
#> 
#>   df |> filter_out(count == 0)
#>   ```
#> 
#>   With `filter()`, you must provide a \"negative\" condition of `!= 0`
#>   and must explicitly guard against accidentally dropping rows with
#>   `NA`. With `filter_out()`, you directly specify rows to drop and you
#>   don\'t have to guard against dropping rows with `NA`, which tends to
#>   result in much clearer code.
#> 
#>   This work is a result of [Tidyup 8: Expanding the `filter()`
#>   family](https://github.com/tidyverse/tidyups/pull/30), with a lot of
#>   great feedback from the community (#6560, #6891).
#> 
#> - New `when_any()` and `when_all()`, which are elementwise versions of
#>   `any()` and `all()`. Alternatively, you can think of them as
#>   performing repeated `|` and `&` on any number of inputs, for example:
#> 
#>   - `when_any(x, y, z)` is equivalent to `x | y | z`.
#> 
#>   - `when_all(x, y, z)` is equivalent to `x & y & z`.
#> 
#>   `when_any()` is particularly useful within `filter()` and
#>   `filter_out()` to specify comma separated conditions combined with `|`
#>   rather than `&`, like:
#> 
#>   ``` r
#>   # With `|`
#>   countries |>
#>     filter(
#>       (name %in% c("US", "CA") & between(score, 200, 300)) |
#>         (name %in% c("PR", "RU") & between(score, 100, 200))
#>     )
#> 
#>   # With `when_any()`, you drop the explicit `|`, the extra `()`, and your
#>   # conditions are all indented to the same level
#>   countries |>
#>     filter(when_any(
#>       name %in% c("US", "CA") & between(score, 200, 300),
#>       name %in% c("PR", "RU") & between(score, 100, 200)
#>     ))
#> 
#>   # To drop these rows instead, use `filter_out()`
#>   countries |>
#>     filter_out(when_any(
#>       name %in% c("US", "CA") & between(score, 200, 300),
#>       name %in% c("PR", "RU") & between(score, 100, 200)
#>     ))
#>   ```
#> 
#>   This work is a result of [Tidyup 8: Expanding the `filter()`
#>   family](https://github.com/tidyverse/tidyups/pull/30).
#> 
#> - `case_when()` is now part of a family of 4 related functions, 3 of
#>   which are new:
#> 
#>   - Use `case_when()` to create a new vector based on logical
#>     conditions.
#>   - Use `replace_when()` to update an existing vector based on logical
#>     conditions.
#>   - Use `recode_values()` to create a new vector by mapping all old
#>     values to new values.
#>   - Use `replace_values()` to update an existing vector by mapping some
#>     old values to new values.
#> 
#>   Learn all about these in a new vignette,
#>   `vignette("recoding-replacing")`.
#> 
#>   `replace_when()` is particularly useful for conditionally mutating
#>   rows within one or more columns, and can be thought of as an enhanced
#>   version of `base::replace()`.
#> 
#>   `recode_values()` and `replace_values()` have the familiar
#>   `case_when()`-style formula interface for easy interactive use, but
#>   also have `from` and `to` arguments as a way for you to incorporate a
#>   pre-built lookup table, making them more holistic replacements for
#>   both `case_match()` and `recode()`.
#> 
#>   This work is a result of [Tidyup 7: Recoding and replacing values in
#>   the
#>   tidyverse](https://github.com/tidyverse/tidyups/blob/main/007-tidyverse-recoding-and-replacing.md),
#>   with a lot of great
#>   [feedback](https://github.com/tidyverse/tidyups/pull/29) from the
#>   community (#7728, #7729).
#> 
#> - `case_when()` has gained a new `.unmatched` argument. For extra
#>   safety, set `.unmatched = "error"` rather than providing a `.default`
#>   when you believe that you\'ve handled every possible case, and it will
#>   error if a case is left unhandled. The new `recode_values()` also has
#>   this argument (#7653).
#> 
#> - `if_else()`, `case_when()`, and `coalesce()` have gotten significantly
#>   faster and use much less memory due to a rewrite in C via vctrs
#>   (#7723, #7725, #7727).
#> 
#> - New `ptype` argument for `between()`, allowing users to specify the
#>   desired output type. This is particularly useful for ordered factors
#>   and other complex types where the default common type behavior might
#>   not be ideal (#6906, \@JamesHWade).
#> 
#> - New `rbind()` method for `rowwise_df` to avoid creating corrupt
#>   rowwise data frames (r-lib/vctrs#1935).
#> 
#> #### Lifecycle changes
#> 
#> ### Newly stable
#> 
#> - `.by` has moved from experimental to stable (#7762).
#> 
#> - `reframe()` has moved from experimental to stable (#7713,
#>   \@VisruthSK).
#> 
#> ### Newly breaking
#> 
#> - `if_else()` no longer allows `condition` to be a logical array. It
#>   must be a logical vector with no `dim` attribute (#7723).
#> 
#> ### Newly deprecated
#> 
#> - `case_match()` is soft-deprecated, and is fully replaced by
#>   `recode_values()` and `replace_values()`, which are more flexible,
#>   more powerful, and have much better names.
#> 
#> - In `case_when()`, supplying all size 1 LHS inputs along with a size
#>   \>1 RHS input is now soft-deprecated. This is an improper usage of
#>   `case_when()` that should instead be a series of if statements, like:
#> 
#>   ``` r
#>   # Scalars!
#>   code <- 1L
#>   flavor <- "vanilla"
#> 
#>   # Improper usage:
#>   case_when(
#>     code == 1L && flavor == "chocolate" ~ x,
#>     code == 1L && flavor == "vanilla" ~ y,
#>     code == 2L && flavor == "vanilla" ~ z,
#>     .default = default
#>   )
#> 
#>   # Recommended:
#>   if (code == 1L && flavor == "chocolate") {
#>     x
#>   } else if (code == 1L && flavor == "vanilla") {
#>     y
#>   } else if (code == 2L && flavor == "vanilla") {
#>     z
#>   } else {
#>     default
#>   }
#>   ```
#> 
#>   The recycling behavior that allows this style of `case_when()` to work
#>   is unsafe, and can result in silent bugs that we\'d like to guard
#>   against with an error in the future (#7082).
#> 
#> - The `dplyr.legacy_locale` global option is soft-deprecated. If you
#>   used this to affect the ordering of `arrange()`, use
#>   `arrange(.locale =)` instead. If you used this to affect the ordering
#>   of `group_by() |> summarise()`, follow up with an additional call to
#>   `arrange(.locale =)` instead (#7760).
#> 
#> - Passing `size` to `if_else()` is now deprecated. The output size is
#>   always taken from the `condition` (#7722).
#> 
#> ### Other deprecation advancements
#> 
#> - The following were already deprecated, and are now defunct and throw
#>   an error:
#> 
#>   - All underscored standard evaluation versions of major dplyr verbs.
#>     Deprecated in 0.7.0 (Jun 2017), use the non-underscored version of
#>     the verb with unquoting instead, see `vignette("programming")`. This
#>     includes:
#> 
#>     - `add_count_()`
#>     - `add_tally_()`
#>     - `arrange_()`
#>     - `count_()`
#>     - `distinct_()`
#>     - `do_()`
#>     - `filter_()`
#>     - `funs_()`
#>     - `group_by_()`
#>     - `group_indices_()`
#>     - `mutate_()`
#>     - `tally_()`
#>     - `transmute_()`
#>     - `rename_()`
#>     - `select_()`
#>     - `slice_()`
#>     - `summarise_()`
#>     - `summarize_()`
#> 
#>   - `mutate_each()`, `mutate_each_()`, `summarise_each()`, and
#>     `summarise_each_()`. Deprecated in 0.7.0 (Jun 2017), use `across()`
#>     instead.
#> 
#>   - Returning more or less than 1 row per group in `summarise()`.
#>     Deprecated in 1.1.0 (Jan 2023), use `reframe()` instead.
#> 
#>   - `combine()`. Deprecated in 1.0.0 (May 2020), use `c()` or
#>     `vctrs::vec_c()` instead.
#> 
#>   - `src_mysql()`, `src_postgres()`, `src_sqlite()`, `src_local()`, and
#>     `src_df()`. Deprecated in 1.0.0 (May 2020), use `tbl()` instead.
#> 
#>   - `tbl_df()` and `as.tbl()`. Deprecated in 1.0.0 (May 2020), use
#>     `tibble::as_tibble()` instead.
#> 
#>   - `add_rownames()`. Deprecated in 1.0.0 (May 2020), use
#>     `tibble::rownames_to_column()` instead.
#> 
#>   - The `.drop` argument of `add_count()`. Deprecated in 1.0.0 (May
#>     2020), had no effect.
#> 
#>   - The `add` argument of `group_by()` and `group_by_prepare()`.
#>     Deprecated in 1.0.0 (May 2020), use `.add` instead.
#> 
#>   - The `.dots` argument of `group_by()` and `group_by_prepare()`.
#>     Deprecated in 1.0.0 (May 2020).
#> 
#>   - The `...` argument of `group_keys()` and `group_indices()`.
#>     Deprecated in 1.0.0 (May 2020), use `group_by()` first.
#> 
#>   - The `keep` argument of `group_map()`, `group_modify()`, and
#>     `group_split()`. Deprecated in 1.0.0 (May 2020), use `.keep`
#>     instead.
#> 
#>   - Using `across()` and data frames in `filter()`. Deprecated in 1.0.8
#>     (Feb 2022), use `if_any()` or `if_all()` instead.
#> 
#>   - `multiple = NULL` in joins. Deprecated in 1.1.1 (Mar 2023), use
#>     `multiple = "all"` instead.
#> 
#>   - `multiple = "error" / "warning"` in joins. Deprecated in 1.1.1 (Mar
#>     2023), use `relationship = "many-to-one"` instead.
#> 
#>   - The `vars` argument of `group_cols()`. Deprecated in 1.0.0 (Jan
#>     2023).
#> 
#> - The following were already deprecated, and now warn unconditionally if
#>   used:
#> 
#>   - `all_equal()`. Deprecated in 1.1.0 (Jan 2023), use `all.equal()`
#>     instead.
#> 
#>   - `progress_estimated()`. Deprecated in 1.0.0 (May 2020).
#> 
#>   - `filter()` with a 1 column matrix. Deprecated in 1.1.0 (Jan 2023),
#>     use a vector instead.
#> 
#>   - `slice()` with a 1 column matrix. Deprecated in 1.1.0 (Jan 2023),
#>     use a vector instead.
#> 
#>   - Not supplying the `.cols` argument of `across()`. Deprecated in
#>     1.1.0 (Jan 2023).
#> 
#>   - `group_indices()` with no arguments. Deprecated in 1.0.0 (May 2020),
#>     use `cur_group_id()` instead.
#> 
#> - The following were already soft-deprecated, and now warn once per
#>   session if used:
#> 
#>   - `cur_data()` and `cur_data_all()`. Deprecated in 1.1.0 (Jan 2023),
#>     use `pick()` instead.
#> 
#>   - The `...` argument of `across()`. Deprecated in 1.1.0 (Jan 2023),
#>     use an anonymous function instead.
#> 
#>   - Using `by = character()` to perform a cross join. Deprecated in
#>     1.1.0 (Jan 2023), use `cross_join()` instead.
#> 
#> ### Removed
#> 
#> The following were already defunct, and have been removed:
#> 
#> - `id()`. Deprecated in 0.5.0 (Jun 2016), use `vctrs::vec_group_id()`
#>   instead. If your package uses NSE and implicitly relied on the
#>   variable `id` being available, you now need to put
#>   `utils::globalVariables("id")` inside one of your package files to
#>   tell R that `id` is a column name.
#> 
#> - `failwith()`. Deprecated in 0.7.0 (Jun 2017), use `purrr::possibly()`
#>   instead.
#> 
#> - `select_vars()` and `select_vars_()`. Deprecated in 0.8.4 (Jan 2020),
#>   use `tidyselect::vars_select()` instead.
#> 
#> - `rename_vars()` and `rename_vars_()`. Deprecated in 0.8.4 (Jan 2020),
#>   use `tidyselect::vars_rename()` instead.
#> 
#> - `select_var()`. Deprecated in 0.8.4 (Jan 2020), use
#>   `tidyselect::vars_pull()` instead.
#> 
#> - `current_vars()`. Deprecated in 0.8.4 (Jan 2020), use
#>   `tidyselect::peek_vars()` instead.
#> 
#> - `bench_tbls()`, `compare_tbls()`, `compare_tbls2()`, `eval_tbls()`,
#>   and `eval_tbls2()`. Deprecated in 1.0.0 (May 2020).
#> 
#> - `location()` and `changes()`. Deprecated in 1.0.0 (May 2020), use
#>   `lobstr::ref()` instead.
#> 
#> #### Minor improvements and bug fixes
#> 
#> - The base pipe is now used throughout the documentation (#7711).
#> 
#> - The superseded `recode()` now has updated documentation showing how to
#>   migrate to `recode_values()` and `replace_values()`.
#> 
#> - The `.groups` message emitted by `summarise()` is hopefully more clear
#>   now (#6986).
#> 
#> - `storms` has been updated to include 2023 and 2024 data (#7111,
#>   \@tomalrussell).
#> 
#> - `if_any()` and `if_all()` are now more consistent in all use cases
#>   (#7059, #7077, #7746, \@jrwinget). In particular:
#> 
#>   - When called with zero inputs, `if_any()` returns `FALSE` and
#>     `if_all()` returns `TRUE`.
#> 
#>   - When called with one input, both now return logical vectors rather
#>     than the original column.
#> 
#>   - The result of applying `.fns` now must be a logical vector.
#> 
#> - `tally_n()` creates fully qualified funciton calls for duckplyr
#>   compatibility (#7046)
#> 
#> - Empty `rowwise()` list-column elements now resolve to `logical()`
#>   rather than a random logical of length 1 (#7710).
#> 
#> - `last_dplyr_warnings()` no longer prevents objects from being garbage
#>   collected (#7649).
#> 
#> - `case_when()` now throws correctly indexed errors when `NULL`s are
#>   supplied in `...` (#7739).
#> 
#> - `case_when()` now throws a better error if one of the conditions is an
#>   array (#6862, \@ilovemane).
#> 
#> - `bind_rows()` now replaces empty (or `NA`) element names in a list
#>   with its numeric index while preserving existing names (#7719,
#>   \@Meghansaha).
#> 
#> - New `slice_sample()` example showing how to use it to shuffle rows
#>   (#7707, \@Hzanib).
#> 
#> - Updated `across()` examples to include an example using `everything()`
#>   (#7621, \@JBrandenburg02).
#> 
#> - Clarified how `slice_min()` and `slice_max()` work in the introduction
#>   vignette (#7717, \@ccani007).
#> 
#> - Fixed an edge case when coercing data frames to matrices (#7004).
#> 
#> - Fixed an issue where duckplyr\'s ALTREP data frames were being
#>   materialized early due to internal usage of `ncol()` (#7049).
#> 
#> - Progress towards making dplyr conformant with the public C API of R
#>   (#7741, #7797).
#> 
#> - R \>=4.1.0 is now required, in line with the [tidyverse
#>   standard](https://tidyverse.org/blog/2019/04/r-version-support/) of
#>   supporting the previous 5 minor releases of R (#7711).

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
#>  @ value  : chr "### dplyr v1.2.0\n\n#### New features\n\n- New `filter_out()` companion to `filter()`.\n\n  - Use `filter()` wh"| __truncated__
#>  @ error  : NULL
#>  @ extra  :List of 1
#>  .. $ display:List of 1
#>  ..  ..$ markdown: chr "### dplyr v1.2.0\n\n#### New features\n\n- New `filter_out()` companion to `filter()`.\n\n  - Use `filter()` wh"| __truncated__
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
