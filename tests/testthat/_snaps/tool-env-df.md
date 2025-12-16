# btw_this.data.frame() works

    Code
      cli::cat_line(btw_this(mtcars))
    Output
      ```json
      {"n_cols":11,"n_rows":32,"groups":[],"class":"data.frame","columns":{"mpg":{"variable":"mpg","type":"numeric","mean":20.0906,"sd":6.0269,"p0":10.4,"p25":15.425,"p50":19.2,"p75":22.8,"p100":33.9},"cyl":{"variable":"cyl","type":"numeric","mean":6.1875,"sd":1.7859,"p0":4,"p25":4,"p50":6,"p75":8,"p100":8},"disp":{"variable":"disp","type":"numeric","mean":230.7219,"sd":123.9387,"p0":71.1,"p25":120.825,"p50":196.3,"p75":326,"p100":472},"hp":{"variable":"hp","type":"numeric","mean":146.6875,"sd":68.5629,"p0":52,"p25":96.5,"p50":123,"p75":180,"p100":335},"drat":{"variable":"drat","type":"numeric","mean":3.5966,"sd":0.5347,"p0":2.76,"p25":3.08,"p50":3.695,"p75":3.92,"p100":4.93},"wt":{"variable":"wt","type":"numeric","mean":3.2172,"sd":0.9785,"p0":1.513,"p25":2.5812,"p50":3.325,"p75":3.61,"p100":5.424},"qsec":{"variable":"qsec","type":"numeric","mean":17.8487,"sd":1.7869,"p0":14.5,"p25":16.8925,"p50":17.71,"p75":18.9,"p100":22.9},"vs":{"variable":"vs","type":"numeric","mean":0.4375,"sd":0.504,"p0":0,"p25":0,"p50":0,"p75":1,"p100":1},"am":{"variable":"am","type":"numeric","mean":0.4062,"sd":0.499,"p0":0,"p25":0,"p50":0,"p75":1,"p100":1},"gear":{"variable":"gear","type":"numeric","mean":3.6875,"sd":0.7378,"p0":3,"p25":3,"p50":4,"p75":4,"p100":5},"carb":{"variable":"carb","type":"numeric","mean":2.8125,"sd":1.6152,"p0":1,"p25":2,"p50":2,"p75":4,"p100":8}}}
      ```

---

    Code
      cli::cat_line(btw_this(mtcars, max_rows = Inf, max_cols = Inf))
    Output
      ```json
      {"n_cols":11,"n_rows":32,"groups":[],"class":"data.frame","columns":{"mpg":{"variable":"mpg","type":"numeric","mean":20.0906,"sd":6.0269,"p0":10.4,"p25":15.425,"p50":19.2,"p75":22.8,"p100":33.9},"cyl":{"variable":"cyl","type":"numeric","mean":6.1875,"sd":1.7859,"p0":4,"p25":4,"p50":6,"p75":8,"p100":8},"disp":{"variable":"disp","type":"numeric","mean":230.7219,"sd":123.9387,"p0":71.1,"p25":120.825,"p50":196.3,"p75":326,"p100":472},"hp":{"variable":"hp","type":"numeric","mean":146.6875,"sd":68.5629,"p0":52,"p25":96.5,"p50":123,"p75":180,"p100":335},"drat":{"variable":"drat","type":"numeric","mean":3.5966,"sd":0.5347,"p0":2.76,"p25":3.08,"p50":3.695,"p75":3.92,"p100":4.93},"wt":{"variable":"wt","type":"numeric","mean":3.2172,"sd":0.9785,"p0":1.513,"p25":2.5812,"p50":3.325,"p75":3.61,"p100":5.424},"qsec":{"variable":"qsec","type":"numeric","mean":17.8487,"sd":1.7869,"p0":14.5,"p25":16.8925,"p50":17.71,"p75":18.9,"p100":22.9},"vs":{"variable":"vs","type":"numeric","mean":0.4375,"sd":0.504,"p0":0,"p25":0,"p50":0,"p75":1,"p100":1},"am":{"variable":"am","type":"numeric","mean":0.4062,"sd":0.499,"p0":0,"p25":0,"p50":0,"p75":1,"p100":1},"gear":{"variable":"gear","type":"numeric","mean":3.6875,"sd":0.7378,"p0":3,"p25":3,"p50":4,"p75":4,"p100":5},"carb":{"variable":"carb","type":"numeric","mean":2.8125,"sd":1.6152,"p0":1,"p25":2,"p50":2,"p75":4,"p100":8}}}
      ```

---

    Code
      cli::cat_line(btw_this(mtcars, format = "glimpse"))
    Output
      Rows: 32
      Columns: 11
      $ mpg  <dbl> 21.0, 21.0, 22.8, 21.4, 18.7, 18.1, 14.3, 24.4, 22.8, 19.2, 17.8,~
      $ cyl  <dbl> 6, 6, 4, 6, 8, 6, 8, 4, 4, 6, 6, 8, 8, 8, 8, 8, 8, 4, 4, 4, 4, 8,~
      $ disp <dbl> 160.0, 160.0, 108.0, 258.0, 360.0, 225.0, 360.0, 146.7, 140.8, 16~
      $ hp   <dbl> 110, 110, 93, 110, 175, 105, 245, 62, 95, 123, 123, 180, 180, 180~
      $ drat <dbl> 3.90, 3.90, 3.85, 3.08, 3.15, 2.76, 3.21, 3.69, 3.92, 3.92, 3.92,~
      $ wt   <dbl> 2.620, 2.875, 2.320, 3.215, 3.440, 3.460, 3.570, 3.190, 3.150, 3.~
      $ qsec <dbl> 16.46, 17.02, 18.61, 19.44, 17.02, 20.22, 15.84, 20.00, 22.90, 18~
      $ vs   <dbl> 0, 0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0,~
      $ am   <dbl> 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0,~
      $ gear <dbl> 4, 4, 4, 3, 3, 3, 3, 4, 4, 4, 4, 3, 3, 3, 3, 3, 3, 4, 4, 4, 3, 3,~
      $ carb <dbl> 4, 4, 1, 1, 2, 1, 4, 2, 2, 4, 4, 3, 3, 3, 4, 4, 4, 1, 2, 1, 1, 2,~

---

    Code
      cli::cat_line(btw_this(mtcars, format = "glimpse", max_rows = Inf, max_cols = Inf))
    Output
      Rows: 32
      Columns: 11
      $ mpg  <dbl> 21.0, 21.0, 22.8, 21.4, 18.7, 18.1, 14.3, 24.4, 22.8, 19.2, 17.8,~
      $ cyl  <dbl> 6, 6, 4, 6, 8, 6, 8, 4, 4, 6, 6, 8, 8, 8, 8, 8, 8, 4, 4, 4, 4, 8,~
      $ disp <dbl> 160.0, 160.0, 108.0, 258.0, 360.0, 225.0, 360.0, 146.7, 140.8, 16~
      $ hp   <dbl> 110, 110, 93, 110, 175, 105, 245, 62, 95, 123, 123, 180, 180, 180~
      $ drat <dbl> 3.90, 3.90, 3.85, 3.08, 3.15, 2.76, 3.21, 3.69, 3.92, 3.92, 3.92,~
      $ wt   <dbl> 2.620, 2.875, 2.320, 3.215, 3.440, 3.460, 3.570, 3.190, 3.150, 3.~
      $ qsec <dbl> 16.46, 17.02, 18.61, 19.44, 17.02, 20.22, 15.84, 20.00, 22.90, 18~
      $ vs   <dbl> 0, 0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0,~
      $ am   <dbl> 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0,~
      $ gear <dbl> 4, 4, 4, 3, 3, 3, 3, 4, 4, 4, 4, 3, 3, 3, 3, 3, 3, 4, 4, 4, 3, 3,~
      $ carb <dbl> 4, 4, 1, 1, 2, 1, 4, 2, 2, 4, 4, 3, 3, 3, 4, 4, 4, 1, 2, 1, 1, 2,~

---

    Code
      cli::cat_line(btw_this(mtcars, format = "print"))
    Output
          mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
        <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
      1  21       6   160   110  3.9   2.62  16.5     0     1     4     4
      2  21       6   160   110  3.9   2.88  17.0     0     1     4     4
      3  22.8     4   108    93  3.85  2.32  18.6     1     1     4     1
      4  21.4     6   258   110  3.08  3.22  19.4     1     0     3     1
      5  18.7     8   360   175  3.15  3.44  17.0     0     0     3     2

---

    Code
      cli::cat_line(btw_this(mtcars, format = "print", max_rows = Inf, max_cols = Inf))
    Output
           mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
         <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
       1  21       6  160    110  3.9   2.62  16.5     0     1     4     4
       2  21       6  160    110  3.9   2.88  17.0     0     1     4     4
       3  22.8     4  108     93  3.85  2.32  18.6     1     1     4     1
       4  21.4     6  258    110  3.08  3.22  19.4     1     0     3     1
       5  18.7     8  360    175  3.15  3.44  17.0     0     0     3     2
       6  18.1     6  225    105  2.76  3.46  20.2     1     0     3     1
       7  14.3     8  360    245  3.21  3.57  15.8     0     0     3     4
       8  24.4     4  147.    62  3.69  3.19  20       1     0     4     2
       9  22.8     4  141.    95  3.92  3.15  22.9     1     0     4     2
      10  19.2     6  168.   123  3.92  3.44  18.3     1     0     4     4
      # i 22 more rows

---

    Code
      cli::cat_line(btw_this(mtcars, format = "json"))
    Output
      ```json
      [
        {"mpg":21,"cyl":6,"disp":160,"hp":110,"drat":3.9,"wt":2.62,"qsec":16.46,"vs":0,"am":1,"gear":4,"carb":4},
        {"mpg":21,"cyl":6,"disp":160,"hp":110,"drat":3.9,"wt":2.875,"qsec":17.02,"vs":0,"am":1,"gear":4,"carb":4},
        {"mpg":22.8,"cyl":4,"disp":108,"hp":93,"drat":3.85,"wt":2.32,"qsec":18.61,"vs":1,"am":1,"gear":4,"carb":1},
        {"mpg":21.4,"cyl":6,"disp":258,"hp":110,"drat":3.08,"wt":3.215,"qsec":19.44,"vs":1,"am":0,"gear":3,"carb":1},
        {"mpg":18.7,"cyl":8,"disp":360,"hp":175,"drat":3.15,"wt":3.44,"qsec":17.02,"vs":0,"am":0,"gear":3,"carb":2}
      ]
      ```

---

    Code
      cli::cat_line(btw_this(mtcars, format = "json", max_rows = Inf, max_cols = Inf))
    Output
      ```json
      [
        {"mpg":21,"cyl":6,"disp":160,"hp":110,"drat":3.9,"wt":2.62,"qsec":16.46,"vs":0,"am":1,"gear":4,"carb":4},
        {"mpg":21,"cyl":6,"disp":160,"hp":110,"drat":3.9,"wt":2.875,"qsec":17.02,"vs":0,"am":1,"gear":4,"carb":4},
        {"mpg":22.8,"cyl":4,"disp":108,"hp":93,"drat":3.85,"wt":2.32,"qsec":18.61,"vs":1,"am":1,"gear":4,"carb":1},
        {"mpg":21.4,"cyl":6,"disp":258,"hp":110,"drat":3.08,"wt":3.215,"qsec":19.44,"vs":1,"am":0,"gear":3,"carb":1},
        {"mpg":18.7,"cyl":8,"disp":360,"hp":175,"drat":3.15,"wt":3.44,"qsec":17.02,"vs":0,"am":0,"gear":3,"carb":2},
        {"mpg":18.1,"cyl":6,"disp":225,"hp":105,"drat":2.76,"wt":3.46,"qsec":20.22,"vs":1,"am":0,"gear":3,"carb":1},
        {"mpg":14.3,"cyl":8,"disp":360,"hp":245,"drat":3.21,"wt":3.57,"qsec":15.84,"vs":0,"am":0,"gear":3,"carb":4},
        {"mpg":24.4,"cyl":4,"disp":146.7,"hp":62,"drat":3.69,"wt":3.19,"qsec":20,"vs":1,"am":0,"gear":4,"carb":2},
        {"mpg":22.8,"cyl":4,"disp":140.8,"hp":95,"drat":3.92,"wt":3.15,"qsec":22.9,"vs":1,"am":0,"gear":4,"carb":2},
        {"mpg":19.2,"cyl":6,"disp":167.6,"hp":123,"drat":3.92,"wt":3.44,"qsec":18.3,"vs":1,"am":0,"gear":4,"carb":4},
        {"mpg":17.8,"cyl":6,"disp":167.6,"hp":123,"drat":3.92,"wt":3.44,"qsec":18.9,"vs":1,"am":0,"gear":4,"carb":4},
        {"mpg":16.4,"cyl":8,"disp":275.8,"hp":180,"drat":3.07,"wt":4.07,"qsec":17.4,"vs":0,"am":0,"gear":3,"carb":3},
        {"mpg":17.3,"cyl":8,"disp":275.8,"hp":180,"drat":3.07,"wt":3.73,"qsec":17.6,"vs":0,"am":0,"gear":3,"carb":3},
        {"mpg":15.2,"cyl":8,"disp":275.8,"hp":180,"drat":3.07,"wt":3.78,"qsec":18,"vs":0,"am":0,"gear":3,"carb":3},
        {"mpg":10.4,"cyl":8,"disp":472,"hp":205,"drat":2.93,"wt":5.25,"qsec":17.98,"vs":0,"am":0,"gear":3,"carb":4},
        {"mpg":10.4,"cyl":8,"disp":460,"hp":215,"drat":3,"wt":5.424,"qsec":17.82,"vs":0,"am":0,"gear":3,"carb":4},
        {"mpg":14.7,"cyl":8,"disp":440,"hp":230,"drat":3.23,"wt":5.345,"qsec":17.42,"vs":0,"am":0,"gear":3,"carb":4},
        {"mpg":32.4,"cyl":4,"disp":78.7,"hp":66,"drat":4.08,"wt":2.2,"qsec":19.47,"vs":1,"am":1,"gear":4,"carb":1},
        {"mpg":30.4,"cyl":4,"disp":75.7,"hp":52,"drat":4.93,"wt":1.615,"qsec":18.52,"vs":1,"am":1,"gear":4,"carb":2},
        {"mpg":33.9,"cyl":4,"disp":71.1,"hp":65,"drat":4.22,"wt":1.835,"qsec":19.9,"vs":1,"am":1,"gear":4,"carb":1},
        {"mpg":21.5,"cyl":4,"disp":120.1,"hp":97,"drat":3.7,"wt":2.465,"qsec":20.01,"vs":1,"am":0,"gear":3,"carb":1},
        {"mpg":15.5,"cyl":8,"disp":318,"hp":150,"drat":2.76,"wt":3.52,"qsec":16.87,"vs":0,"am":0,"gear":3,"carb":2},
        {"mpg":15.2,"cyl":8,"disp":304,"hp":150,"drat":3.15,"wt":3.435,"qsec":17.3,"vs":0,"am":0,"gear":3,"carb":2},
        {"mpg":13.3,"cyl":8,"disp":350,"hp":245,"drat":3.73,"wt":3.84,"qsec":15.41,"vs":0,"am":0,"gear":3,"carb":4},
        {"mpg":19.2,"cyl":8,"disp":400,"hp":175,"drat":3.08,"wt":3.845,"qsec":17.05,"vs":0,"am":0,"gear":3,"carb":2},
        {"mpg":27.3,"cyl":4,"disp":79,"hp":66,"drat":4.08,"wt":1.935,"qsec":18.9,"vs":1,"am":1,"gear":4,"carb":1},
        {"mpg":26,"cyl":4,"disp":120.3,"hp":91,"drat":4.43,"wt":2.14,"qsec":16.7,"vs":0,"am":1,"gear":5,"carb":2},
        {"mpg":30.4,"cyl":4,"disp":95.1,"hp":113,"drat":3.77,"wt":1.513,"qsec":16.9,"vs":1,"am":1,"gear":5,"carb":2},
        {"mpg":15.8,"cyl":8,"disp":351,"hp":264,"drat":4.22,"wt":3.17,"qsec":14.5,"vs":0,"am":1,"gear":5,"carb":4},
        {"mpg":19.7,"cyl":6,"disp":145,"hp":175,"drat":3.62,"wt":2.77,"qsec":15.5,"vs":0,"am":1,"gear":5,"carb":6},
        {"mpg":15,"cyl":8,"disp":301,"hp":335,"drat":3.54,"wt":3.57,"qsec":14.6,"vs":0,"am":1,"gear":5,"carb":8},
        {"mpg":21.4,"cyl":4,"disp":121,"hp":109,"drat":4.11,"wt":2.78,"qsec":18.6,"vs":1,"am":1,"gear":4,"carb":2}
      ]
      ```

# btw_tool_env_describe_data_frame() checks that the package is installed

    Code
      btw_tool_env_describe_data_frame("skibidi::ohio")
    Condition
      Error in `btw_tool_env_describe_data_frame()`:
      ! Package skibidi is not installed.
      i Did you mean "Kifidi", "simIDM", "bib2df", "BiBitR", or "Bioi"?

# btw_this.tbl() works

    Code
      cli::cat_line(btw_this(tbl_data))
    Output
      | x | y |
      |---|---|
      | 1 | a |
      | 2 | b |
      | 3 | c |
      | 4 | d |
      | 5 | e |

---

    Code
      cli::cat_line(btw_this(tbl_data, format = "glimpse"))
    Output
      | x | y |
      |---|---|
      | 1 | a |
      | 2 | b |
      | 3 | c |
      | 4 | d |
      | 5 | e |

---

    Code
      cli::cat_line(btw_this(tbl_data, format = "print"))
    Output
      | x | y |
      |---|---|
      | 1 | a |
      | 2 | b |
      | 3 | c |
      | 4 | d |
      | 5 | e |

---

    Code
      cli::cat_line(btw_this(tbl_data, format = "json"))
    Output
      ```json
      [
        {"x":1,"y":"a"},
        {"x":2,"y":"b"},
        {"x":3,"y":"c"},
        {"x":4,"y":"d"},
        {"x":5,"y":"e"}
      ]
      ```

# btw_tool_env_describe_data_frame() errors when data frame not found

    Code
      btw_tool_env_describe_data_frame("nonexistent_dataframe")
    Condition
      Error in `btw_tool_env_describe_data_frame()`:
      ! The data frame "nonexistent_dataframe" was not found in the environment.
      i If the data is from a package, use `package` to specify the package name.

# describe_data_frame_skim handles character columns with few unique values

    Code
      cli::cat_line(result)
    Output
      | x | category |
      |---|----------|
      |  1 | A |
      |  2 | B |
      |  3 | C |
      |  4 | A |
      |  5 | B |
      |  6 | C |
      |  7 | A |
      |  8 | B |
      |  9 | C |
      | 10 | A |

# describe_data_frame_skim truncates long character values

    Code
      cli::cat_line(result)
    Output
      ```json
      {"n_cols":2,"n_rows":34,"groups":[],"class":"data.frame","columns":{"text":{"variable":"text","type":"character","min":1,"max":200,"empty":0,"n_unique":10,"values":["aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa ...","bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb ...","a","b","c","d","e","f","g","h"]},"id":{"variable":"id","type":"numeric","mean":34,"sd":0,"p0":34,"p25":34,"p50":34,"p75":34,"p100":34}}}
      ```

# btw_this() handles factor columns

    Code
      cli::cat_line(result)
    Output
      | x | category |
      |---|----------|
      | 1 | low |
      | 2 | medium |
      | 3 | high |
      | 4 | medium |
      | 5 | low |

