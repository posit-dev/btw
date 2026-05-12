# btw_tool_docs_help_page() works

    Code
      cli::cat_line(res@value)
    Output
      ## `help(package = "stats", "rnorm")`
      
      ### The Normal Distribution
      
      #### Description
      
      Density, distribution function, quantile function and random generation
      for the normal distribution with mean equal to `mean` and standard
      deviation equal to `sd`.
      
      #### Usage
      
      ``` R
      dnorm(x, mean = 0, sd = 1, log = FALSE)
      pnorm(q, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
      qnorm(p, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
      rnorm(n, mean = 0, sd = 1)
      ```
      
      #### Arguments
      
      ##### `x`
      
      vector of quantiles.
      
      ##### `p`
      
      vector of probabilities.
      
      ##### `n`
      
      number of observations. If `length(n) > 1`, the length is taken to be
      the number required.
      
      ##### `mean`
      
      vector of means.
      
      ##### `sd`
      
      vector of standard deviations.
      
      ##### `log`
      
      logical; if `TRUE`, probabilities/densities are given as logarithms.
      
      ##### `lower.tail`
      
      logical; if `TRUE` (default), probabilities are `P[X \le x]`, otherwise,
      `P[X > x]`.
      
      #### Details
      
      If `mean` or `sd` are not specified they assume the default values of
      `0` and `1`, respectively.
      
      The normal distribution has density
      
      ` f(x) = \frac{1}{\sqrt{2\pi}\sigma} e^{-(x-\mu)^2/2\sigma^2}`
      
      where `\mu` is the mean of the distribution and `\sigma` the standard
      deviation.
      
      #### Value
      
      `dnorm` gives the density, `pnorm` is the cumulative distribution
      function, and `qnorm` is the quantile function of the normal
      distribution. `rnorm` generates random deviates.
      
      The length of the result is determined by `n` for `rnorm`, and is the
      maximum of the lengths of the numerical arguments for the other
      functions.
      
      The numerical arguments other than `n` are recycled to the length of the
      result. Only the first elements of the logical arguments are used.
      
      For `sd = 0` this gives the limit as `sd` decreases to 0, a point mass
      at `mu`. `sd < 0` is an error and returns `NaN`.
      
      #### Source
      
      For `pnorm`, based on
      <a href="#reference+Normal.Rd+R+3ACody+3A1993" class="citation">Cody
      (1993)</a>.
      
      For `qnorm`, the code is based on a C translation of
      <a href="#reference+Normal.Rd+R+3AWichura+3A1988"
      class="citation">Wichura (1988)</a> which provides precise results up to
      about 16 digits for `log.p=FALSE`. For log scale probabilities in the
      extreme tails, since <span class="rlang">**R**</span> version 4.1.0,
      extensively since 4.3.0, asymptotic expansions are used which have been
      derived and explored in
      <a href="#reference+Normal.Rd+R+3AMaechler+3A2022"
      class="citation">Mächler (2022)</a>.
      
      For `rnorm`, see RNG for how to select the algorithm and for references
      to the supplied methods.
      
      #### References
      
      <span id="reference+Normal.Rd+R+3ABecker+2BChambers+2BWilks+3A1988"></span>
      
      Becker RA, Chambers JM, Wilks AR (1988). *The New S Language*. Chapman
      and Hall/CRC, London. ISBN 053409192X.
      
      <span id="reference+Normal.Rd+R+3ACody+3A1993"></span>Cody WJ (1993).
      “Algorithm 715: SPECFUN—A Portable FORTRAN Package of Special Function
      Routines and Test Drivers.” *ACM Transactions on Mathematical Software*,
      **19**(1), 22–30.
      [doi:10.1145/151271.151273](https://doi.org/10.1145/151271.151273).
      
      <span id="reference+Normal.Rd+R+3AJohnson+2BKotz+2BBalakrishnan+3A1994"></span>Johnson
      NL, Kotz S, Balakrishnan N (1994). *Continuous Univariate
      Distributions*, volume 1. Wiley, New York. ISBN 978-0-471-58495-7.  
      Chapter 13.
      
      <span id="reference+Normal.Rd+R+3AMaechler+3A2022"></span>Mächler M
      (2022). *Asymptotic Tail Formulas for Gaussian Quantiles*. Vignette,
      CRAN package <span class="pkg">DPQ</span>,
      <https://CRAN.R-project.org/package=DPQ/vignettes/qnorm-asymp.pdf>.
      
      <span id="reference+Normal.Rd+R+3AWichura+3A1988"></span>Wichura MJ
      (1988). “Algorithm AS 241: The Percentage Points of the Normal
      Distribution.” *Applied Statistics*, **37**(3), 477.
      [doi:10.2307/2347330](https://doi.org/10.2307/2347330).
      
      #### See Also
      
      Distributions for other standard distributions, including `dlnorm` for
      the *log*-normal distribution.
      
      #### Examples
      
      ``` R
      require(graphics)
      
      dnorm(0) == 1/sqrt(2*pi)
      dnorm(1) == exp(-1/2)/sqrt(2*pi)
      dnorm(1) == 1/sqrt(2*pi*exp(1))
      
      ## Using "log = TRUE" for an extended range :
      par(mfrow = c(2,1))
      plot(function(x) dnorm(x, log = TRUE), -60, 50,
           main = "log { Normal density }")
      curve(log(dnorm(x)), add = TRUE, col = "red", lwd = 2)
      mtext("dnorm(x, log=TRUE)", adj = 0)
      mtext("log(dnorm(x))", col = "red", adj = 1)
      
      plot(function(x) pnorm(x, log.p = TRUE), -50, 10,
           main = "log { Normal Cumulative }")
      curve(log(pnorm(x)), add = TRUE, col = "red", lwd = 2)
      mtext("pnorm(x, log=TRUE)", adj = 0)
      mtext("log(pnorm(x))", col = "red", adj = 1)
      
      ## if you want the so-called 'error function'
      erf <- function(x) 2 * pnorm(x * sqrt(2)) - 1
      ## (see Abramowitz and Stegun 29.2.29)
      ## and the so-called 'complementary error function'
      erfc <- function(x) 2 * pnorm(x * sqrt(2), lower = FALSE)
      ## and the inverses
      erfinv <- function (x) qnorm((1 + x)/2)/sqrt(2)
      erfcinv <- function (x) qnorm(x/2, lower = FALSE)/sqrt(2)
      ```

# btw_tool_docs_help_page() with unknown topic

    Code
      btw_tool_docs_help_page("unknown-topic", "dplyr")
    Condition
      Error in `btw_tool_docs_help_page()`:
      ! No help page found for topic "unknown-topic" in package dplyr.
      i To search in all packages, call `btw_tool_docs_help_page()` with an empty string for `package_name`.
    Code
      btw_tool_docs_help_page("unknown-topic")
    Condition
      Error in `btw_tool_docs_help_page()`:
      ! No help page found for topic "unknown-topic" in all installed packages.

# btw_tool_docs_help_page() with multiple help topics

    Code
      btw_tool_docs_help_page("filter")
    Condition
      Error in `btw_tool_docs_help_page()`:
      ! Topic "filter" matched 2 different topics.
      i Choose one or submit individual tool calls for each topic.
      * {"topic":"filter", "package_name":"dplyr"}
      * {"topic":"filter", "package_name":"stats"}

