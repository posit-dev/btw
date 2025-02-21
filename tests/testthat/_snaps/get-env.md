# get_environment works

    Code
      cat(get_environment(env))
    Output
      mtcars
      #> $ mpg  <dbl> 21.0, 21.0, 22.8, 21.4, 18.7
      #> $ cyl  <dbl> 6, 6, 4, 6, 8
      #> $ disp <dbl> 160, 160, 108, 258, 360
      #> $ hp   <dbl> 110, 110, 93, 110, 175
      #> $ drat <dbl> 3.90, 3.90, 3.85, 3.08, 3.15
      #> $ wt   <dbl> 2.620, 2.875, 2.320, 3.215, 3.440
      #> $ qsec <dbl> 16.46, 17.02, 18.61, 19.44, 17.02
      #> $ vs   <dbl> 0, 0, 1, 1, 0
      #> $ am   <dbl> 1, 1, 1, 0, 0
      #> $ gear <dbl> 4, 4, 4, 3, 3
      #> $ carb <dbl> 4, 4, 1, 1, 2
      
      
      boop
      #> [1] "bop"
      

---

    Code
      cat(get_environment(env, items = "mtcars"))
    Output
      mtcars
      #> $ mpg  <dbl> 21.0, 21.0, 22.8, 21.4, 18.7
      #> $ cyl  <dbl> 6, 6, 4, 6, 8
      #> $ disp <dbl> 160, 160, 108, 258, 360
      #> $ hp   <dbl> 110, 110, 93, 110, 175
      #> $ drat <dbl> 3.90, 3.90, 3.85, 3.08, 3.15
      #> $ wt   <dbl> 2.620, 2.875, 2.320, 3.215, 3.440
      #> $ qsec <dbl> 16.46, 17.02, 18.61, 19.44, 17.02
      #> $ vs   <dbl> 0, 0, 1, 1, 0
      #> $ am   <dbl> 1, 1, 1, 0, 0
      #> $ gear <dbl> 4, 4, 4, 3, 3
      #> $ carb <dbl> 4, 4, 1, 1, 2
      

---

    Code
      cat(get_environment(env, items = "boop"))
    Output
      boop
      #> [1] "bop"
      

---

    Code
      cat(get_environment(env, items = character(0)))

