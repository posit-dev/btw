# Tool: Run R code

**\[experimental\]** This tool runs R code and returns results as a list
of
[`ellmer::Content()`](https://ellmer.tidyverse.org/reference/Content.html)
objects. It captures text output, plots, messages, warnings, and errors.
Code execution stops on the first error, returning all results up to
that point.

## Usage

``` r
btw_tool_run_r(code, `_intent` = "")
```

## Arguments

- code:

  A character string containing R code to run.

- \_intent:

  An optional string describing the intent of the tool use. When the
  tool is used by an LLM, the model will use this argument to explain
  why it called the tool.

## Value

A list of ellmer Content objects:

- `ContentText`: visible return values and text output

- `ContentMessage`: messages from
  [`message()`](https://rdrr.io/r/base/message.html)

- `ContentWarning`: warnings from
  [`warning()`](https://rdrr.io/r/base/warning.html)

- `ContentError`: errors from
  [`stop()`](https://rdrr.io/r/base/stop.html)

- `ContentImageInline`: plots created during execution

## Details

### Configuration Options

The behavior of the `btw_tool_run_r` tool can be customized using the
following R options:

- `btw.run_r.graphics_device`: A function that creates a graphics device
  used for rendering plots. By default, it uses
  [`ragg::agg_png()`](https://ragg.r-lib.org/reference/agg_png.html) if
  the `ragg` package is installed, otherwise it falls back to
  [`grDevices::png()`](https://rdrr.io/r/grDevices/png.html).

- `btw.run_r.plot_aspect_ratio`: Aspect ratio for plots created during
  code execution. Can be a character string of the form `"w:h"` (e.g.,
  `"16:9"`) or a numeric value representing width/height (e.g., `16/9`).
  Default is `"3:2"`.

- `btw.run_r.plot_size`: Integer pixel size for the longest side of
  plots. Default is `768L`. This image size was selected to match
  [OpenAI's image resizing
  rules](https://platform.openai.com/docs/guides/images-vision?api-mode=responses),
  where images are resized such that the largest size is 768px. Another
  common choice is 512px. Larger images may be used but will result in
  increased token sizes.

- `btw.run_r.enabled`: Logical flag to enable or disable the tool
  globally.

These values can be set using
[`options()`](https://rdrr.io/r/base/options.html) in your R session or
`.Rprofile` or in a [btw.md
file](https://posit-dev.github.io/btw/dev/reference/use_btw_md.md) under
the `options` section.

    ---
    options:
     run_r:
       enabled: true
       plot_aspect_ratio: "16:9"
       plot_size: 512
    ---

## Security Considerations

Executing arbitrary R code can pose significant security risks,
especially in shared or multi-user environments. Furthermore, neither
shinychat (as of v0.4.0) or nor ellmer (as of v0.4.0) provide a
mechanism to review and reject the code before execution. Even more, the
code is executed in the global environment and does not have any
sandboxing or R code limitations applied.

It is your responsibility to ensure that you are taking appropriate
measures to reduce the risk of the LLM writing arbitrary code. Most
often, this means not prompting the model to take large or potentially
destructive actions. At this time, we do not recommend that you enable
this tool in a publicly- available environment without strong safeguards
in place.

That said, this tool is very powerful and can greatly enhance the
capabilities of your btw chatbots. Please use it responsibly! If you'd
like to enable the tool, please read the instructions below.

## Enabling this tool

This tool is not enabled by default in
[`btw_tools()`](https://posit-dev.github.io/btw/dev/reference/btw_tools.md),
[`btw_app()`](https://posit-dev.github.io/btw/dev/reference/btw_client.md)
or
[`btw_client()`](https://posit-dev.github.io/btw/dev/reference/btw_client.md).
To enable the function, you have a few options:

1.  Set the `btw.run_r.enabled` option to `TRUE` in your R session, or
    in your `.Rprofile` file to enable it globally.

2.  Set the `BTW_RUN_R_ENABLED` environment variable to `true` in your
    `.Renviron` file or your system environment.

3.  Explicitly include the tool when calling `btw_tools("run")` (unless
    the above options disable it).

In your [btw.md
file](https://posit-dev.github.io/btw/dev/reference/use_btw_md.md), you
can explicitly enable the tool by naming it in the tools option

    ---
    tools:
      - run_r
    ---

or you can enable the tool by setting the `btw.run_r.enabled` option
from the `options` list in `btw.md` (this approach is useful if you've
globally disabled the tool but want to enable it for a specific btw
chat):

    ---
    options:
      run_r:
        enabled: true
    ---

## See also

[`btw_tools()`](https://posit-dev.github.io/btw/dev/reference/btw_tools.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Simple calculation
btw_tool_run_r("2 + 2")

# Code with plot
btw_tool_run_r("hist(rnorm(100))")

# Code with warning
btw_tool_run_r("mean(c(1, 2, NA))")
} # }
```
