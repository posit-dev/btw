# md_table()

    Code
      cat(md_table(mtcars[1:4, 1:4]))
    Output
      | mpg | cyl | disp | hp |
      |-----|-----|------|----|
      | 21 | 6 | 160 | 110 |
      | 21 | 6 | 160 | 110 |
      | 22.8 | 4 | 108 | 93 |
      | 21.4 | 6 | 258 | 110 |

# md_code_block()

    Code
      md_code_block("markdown", c("```r", "runif(1)", "```"))
    Output
      [1] "````markdown\n```r\nrunif(1)\n```\n````"

---

    Code
      md_code_block("markdown", c("````r", "runif(1)", "````"))
    Output
      [1] "`````markdown\n````r\nrunif(1)\n````\n`````"

