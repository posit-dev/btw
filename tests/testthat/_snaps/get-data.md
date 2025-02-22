# get_data_frame works

    Code
      cat(get_data_frame(mtcars))
    Output
      {"name":"mtcars","n_cols":11,"n_rows":32,"groups":[],"class":"data.frame","columns":{"mpg":{"variable":"mpg","type":"numeric","mean":20.0906,"sd":6.0269,"p0":10.4,"p25":15.425,"p50":19.2,"p75":22.8,"p100":33.9},"cyl":{"variable":"cyl","type":"numeric","mean":6.1875,"sd":1.7859,"p0":4,"p25":4,"p50":6,"p75":8,"p100":8},"disp":{"variable":"disp","type":"numeric","mean":230.7219,"sd":123.9387,"p0":71.1,"p25":120.825,"p50":196.3,"p75":326,"p100":472},"hp":{"variable":"hp","type":"numeric","mean":146.6875,"sd":68.5629,"p0":52,"p25":96.5,"p50":123,"p75":180,"p100":335},"drat":{"variable":"drat","type":"numeric","mean":3.5966,"sd":0.5347,"p0":2.76,"p25":3.08,"p50":3.695,"p75":3.92,"p100":4.93},"wt":{"variable":"wt","type":"numeric","mean":3.2172,"sd":0.9785,"p0":1.513,"p25":2.5812,"p50":3.325,"p75":3.61,"p100":5.424},"qsec":{"variable":"qsec","type":"numeric","mean":17.8487,"sd":1.7869,"p0":14.5,"p25":16.8925,"p50":17.71,"p75":18.9,"p100":22.9},"vs":{"variable":"vs","type":"numeric","mean":0.4375,"sd":0.504,"p0":0,"p25":0,"p50":0,"p75":1,"p100":1},"am":{"variable":"am","type":"numeric","mean":0.4062,"sd":0.499,"p0":0,"p25":0,"p50":0,"p75":1,"p100":1},"gear":{"variable":"gear","type":"numeric","mean":3.6875,"sd":0.7378,"p0":3,"p25":3,"p50":4,"p75":4,"p100":5},"carb":{"variable":"carb","type":"numeric","mean":2.8125,"sd":1.6152,"p0":1,"p25":2,"p50":2,"p75":4,"p100":8}}}

---

    Code
      cat(get_data_frame(mtcars, dims = c(Inf, Inf)))
    Output
      {"name":"mtcars","n_cols":11,"n_rows":32,"groups":[],"class":"data.frame","columns":{"mpg":{"variable":"mpg","type":"numeric","mean":20.0906,"sd":6.0269,"p0":10.4,"p25":15.425,"p50":19.2,"p75":22.8,"p100":33.9},"cyl":{"variable":"cyl","type":"numeric","mean":6.1875,"sd":1.7859,"p0":4,"p25":4,"p50":6,"p75":8,"p100":8},"disp":{"variable":"disp","type":"numeric","mean":230.7219,"sd":123.9387,"p0":71.1,"p25":120.825,"p50":196.3,"p75":326,"p100":472},"hp":{"variable":"hp","type":"numeric","mean":146.6875,"sd":68.5629,"p0":52,"p25":96.5,"p50":123,"p75":180,"p100":335},"drat":{"variable":"drat","type":"numeric","mean":3.5966,"sd":0.5347,"p0":2.76,"p25":3.08,"p50":3.695,"p75":3.92,"p100":4.93},"wt":{"variable":"wt","type":"numeric","mean":3.2172,"sd":0.9785,"p0":1.513,"p25":2.5812,"p50":3.325,"p75":3.61,"p100":5.424},"qsec":{"variable":"qsec","type":"numeric","mean":17.8487,"sd":1.7869,"p0":14.5,"p25":16.8925,"p50":17.71,"p75":18.9,"p100":22.9},"vs":{"variable":"vs","type":"numeric","mean":0.4375,"sd":0.504,"p0":0,"p25":0,"p50":0,"p75":1,"p100":1},"am":{"variable":"am","type":"numeric","mean":0.4062,"sd":0.499,"p0":0,"p25":0,"p50":0,"p75":1,"p100":1},"gear":{"variable":"gear","type":"numeric","mean":3.6875,"sd":0.7378,"p0":3,"p25":3,"p50":4,"p75":4,"p100":5},"carb":{"variable":"carb","type":"numeric","mean":2.8125,"sd":1.6152,"p0":1,"p25":2,"p50":2,"p75":4,"p100":8}}}

---

    Code
      cat(get_data_frame(mtcars, format = "print"))
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
      cat(get_data_frame(mtcars, format = "print", dims = c(Inf, Inf)))
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
      cat(get_data_frame(mtcars, format = "json"))
    Output
      [
        {
          "mpg": 21,
          "cyl": 6,
          "disp": 160,
          "hp": 110,
          "drat": 3.9,
          "wt": 2.62,
          "qsec": 16.46,
          "vs": 0,
          "am": 1,
          "gear": 4,
          "carb": 4,
          "_row": "Mazda RX4"
        },
        {
          "mpg": 21,
          "cyl": 6,
          "disp": 160,
          "hp": 110,
          "drat": 3.9,
          "wt": 2.875,
          "qsec": 17.02,
          "vs": 0,
          "am": 1,
          "gear": 4,
          "carb": 4,
          "_row": "Mazda RX4 Wag"
        },
        {
          "mpg": 22.8,
          "cyl": 4,
          "disp": 108,
          "hp": 93,
          "drat": 3.85,
          "wt": 2.32,
          "qsec": 18.61,
          "vs": 1,
          "am": 1,
          "gear": 4,
          "carb": 1,
          "_row": "Datsun 710"
        },
        {
          "mpg": 21.4,
          "cyl": 6,
          "disp": 258,
          "hp": 110,
          "drat": 3.08,
          "wt": 3.215,
          "qsec": 19.44,
          "vs": 1,
          "am": 0,
          "gear": 3,
          "carb": 1,
          "_row": "Hornet 4 Drive"
        },
        {
          "mpg": 18.7,
          "cyl": 8,
          "disp": 360,
          "hp": 175,
          "drat": 3.15,
          "wt": 3.44,
          "qsec": 17.02,
          "vs": 0,
          "am": 0,
          "gear": 3,
          "carb": 2,
          "_row": "Hornet Sportabout"
        }
      ] 

---

    Code
      cat(get_data_frame(mtcars, format = "json", dims = c(Inf, Inf)))
    Output
      [
        {
          "mpg": 21,
          "cyl": 6,
          "disp": 160,
          "hp": 110,
          "drat": 3.9,
          "wt": 2.62,
          "qsec": 16.46,
          "vs": 0,
          "am": 1,
          "gear": 4,
          "carb": 4,
          "_row": "Mazda RX4"
        },
        {
          "mpg": 21,
          "cyl": 6,
          "disp": 160,
          "hp": 110,
          "drat": 3.9,
          "wt": 2.875,
          "qsec": 17.02,
          "vs": 0,
          "am": 1,
          "gear": 4,
          "carb": 4,
          "_row": "Mazda RX4 Wag"
        },
        {
          "mpg": 22.8,
          "cyl": 4,
          "disp": 108,
          "hp": 93,
          "drat": 3.85,
          "wt": 2.32,
          "qsec": 18.61,
          "vs": 1,
          "am": 1,
          "gear": 4,
          "carb": 1,
          "_row": "Datsun 710"
        },
        {
          "mpg": 21.4,
          "cyl": 6,
          "disp": 258,
          "hp": 110,
          "drat": 3.08,
          "wt": 3.215,
          "qsec": 19.44,
          "vs": 1,
          "am": 0,
          "gear": 3,
          "carb": 1,
          "_row": "Hornet 4 Drive"
        },
        {
          "mpg": 18.7,
          "cyl": 8,
          "disp": 360,
          "hp": 175,
          "drat": 3.15,
          "wt": 3.44,
          "qsec": 17.02,
          "vs": 0,
          "am": 0,
          "gear": 3,
          "carb": 2,
          "_row": "Hornet Sportabout"
        },
        {
          "mpg": 18.1,
          "cyl": 6,
          "disp": 225,
          "hp": 105,
          "drat": 2.76,
          "wt": 3.46,
          "qsec": 20.22,
          "vs": 1,
          "am": 0,
          "gear": 3,
          "carb": 1,
          "_row": "Valiant"
        },
        {
          "mpg": 14.3,
          "cyl": 8,
          "disp": 360,
          "hp": 245,
          "drat": 3.21,
          "wt": 3.57,
          "qsec": 15.84,
          "vs": 0,
          "am": 0,
          "gear": 3,
          "carb": 4,
          "_row": "Duster 360"
        },
        {
          "mpg": 24.4,
          "cyl": 4,
          "disp": 146.7,
          "hp": 62,
          "drat": 3.69,
          "wt": 3.19,
          "qsec": 20,
          "vs": 1,
          "am": 0,
          "gear": 4,
          "carb": 2,
          "_row": "Merc 240D"
        },
        {
          "mpg": 22.8,
          "cyl": 4,
          "disp": 140.8,
          "hp": 95,
          "drat": 3.92,
          "wt": 3.15,
          "qsec": 22.9,
          "vs": 1,
          "am": 0,
          "gear": 4,
          "carb": 2,
          "_row": "Merc 230"
        },
        {
          "mpg": 19.2,
          "cyl": 6,
          "disp": 167.6,
          "hp": 123,
          "drat": 3.92,
          "wt": 3.44,
          "qsec": 18.3,
          "vs": 1,
          "am": 0,
          "gear": 4,
          "carb": 4,
          "_row": "Merc 280"
        },
        {
          "mpg": 17.8,
          "cyl": 6,
          "disp": 167.6,
          "hp": 123,
          "drat": 3.92,
          "wt": 3.44,
          "qsec": 18.9,
          "vs": 1,
          "am": 0,
          "gear": 4,
          "carb": 4,
          "_row": "Merc 280C"
        },
        {
          "mpg": 16.4,
          "cyl": 8,
          "disp": 275.8,
          "hp": 180,
          "drat": 3.07,
          "wt": 4.07,
          "qsec": 17.4,
          "vs": 0,
          "am": 0,
          "gear": 3,
          "carb": 3,
          "_row": "Merc 450SE"
        },
        {
          "mpg": 17.3,
          "cyl": 8,
          "disp": 275.8,
          "hp": 180,
          "drat": 3.07,
          "wt": 3.73,
          "qsec": 17.6,
          "vs": 0,
          "am": 0,
          "gear": 3,
          "carb": 3,
          "_row": "Merc 450SL"
        },
        {
          "mpg": 15.2,
          "cyl": 8,
          "disp": 275.8,
          "hp": 180,
          "drat": 3.07,
          "wt": 3.78,
          "qsec": 18,
          "vs": 0,
          "am": 0,
          "gear": 3,
          "carb": 3,
          "_row": "Merc 450SLC"
        },
        {
          "mpg": 10.4,
          "cyl": 8,
          "disp": 472,
          "hp": 205,
          "drat": 2.93,
          "wt": 5.25,
          "qsec": 17.98,
          "vs": 0,
          "am": 0,
          "gear": 3,
          "carb": 4,
          "_row": "Cadillac Fleetwood"
        },
        {
          "mpg": 10.4,
          "cyl": 8,
          "disp": 460,
          "hp": 215,
          "drat": 3,
          "wt": 5.424,
          "qsec": 17.82,
          "vs": 0,
          "am": 0,
          "gear": 3,
          "carb": 4,
          "_row": "Lincoln Continental"
        },
        {
          "mpg": 14.7,
          "cyl": 8,
          "disp": 440,
          "hp": 230,
          "drat": 3.23,
          "wt": 5.345,
          "qsec": 17.42,
          "vs": 0,
          "am": 0,
          "gear": 3,
          "carb": 4,
          "_row": "Chrysler Imperial"
        },
        {
          "mpg": 32.4,
          "cyl": 4,
          "disp": 78.7,
          "hp": 66,
          "drat": 4.08,
          "wt": 2.2,
          "qsec": 19.47,
          "vs": 1,
          "am": 1,
          "gear": 4,
          "carb": 1,
          "_row": "Fiat 128"
        },
        {
          "mpg": 30.4,
          "cyl": 4,
          "disp": 75.7,
          "hp": 52,
          "drat": 4.93,
          "wt": 1.615,
          "qsec": 18.52,
          "vs": 1,
          "am": 1,
          "gear": 4,
          "carb": 2,
          "_row": "Honda Civic"
        },
        {
          "mpg": 33.9,
          "cyl": 4,
          "disp": 71.1,
          "hp": 65,
          "drat": 4.22,
          "wt": 1.835,
          "qsec": 19.9,
          "vs": 1,
          "am": 1,
          "gear": 4,
          "carb": 1,
          "_row": "Toyota Corolla"
        },
        {
          "mpg": 21.5,
          "cyl": 4,
          "disp": 120.1,
          "hp": 97,
          "drat": 3.7,
          "wt": 2.465,
          "qsec": 20.01,
          "vs": 1,
          "am": 0,
          "gear": 3,
          "carb": 1,
          "_row": "Toyota Corona"
        },
        {
          "mpg": 15.5,
          "cyl": 8,
          "disp": 318,
          "hp": 150,
          "drat": 2.76,
          "wt": 3.52,
          "qsec": 16.87,
          "vs": 0,
          "am": 0,
          "gear": 3,
          "carb": 2,
          "_row": "Dodge Challenger"
        },
        {
          "mpg": 15.2,
          "cyl": 8,
          "disp": 304,
          "hp": 150,
          "drat": 3.15,
          "wt": 3.435,
          "qsec": 17.3,
          "vs": 0,
          "am": 0,
          "gear": 3,
          "carb": 2,
          "_row": "AMC Javelin"
        },
        {
          "mpg": 13.3,
          "cyl": 8,
          "disp": 350,
          "hp": 245,
          "drat": 3.73,
          "wt": 3.84,
          "qsec": 15.41,
          "vs": 0,
          "am": 0,
          "gear": 3,
          "carb": 4,
          "_row": "Camaro Z28"
        },
        {
          "mpg": 19.2,
          "cyl": 8,
          "disp": 400,
          "hp": 175,
          "drat": 3.08,
          "wt": 3.845,
          "qsec": 17.05,
          "vs": 0,
          "am": 0,
          "gear": 3,
          "carb": 2,
          "_row": "Pontiac Firebird"
        },
        {
          "mpg": 27.3,
          "cyl": 4,
          "disp": 79,
          "hp": 66,
          "drat": 4.08,
          "wt": 1.935,
          "qsec": 18.9,
          "vs": 1,
          "am": 1,
          "gear": 4,
          "carb": 1,
          "_row": "Fiat X1-9"
        },
        {
          "mpg": 26,
          "cyl": 4,
          "disp": 120.3,
          "hp": 91,
          "drat": 4.43,
          "wt": 2.14,
          "qsec": 16.7,
          "vs": 0,
          "am": 1,
          "gear": 5,
          "carb": 2,
          "_row": "Porsche 914-2"
        },
        {
          "mpg": 30.4,
          "cyl": 4,
          "disp": 95.1,
          "hp": 113,
          "drat": 3.77,
          "wt": 1.513,
          "qsec": 16.9,
          "vs": 1,
          "am": 1,
          "gear": 5,
          "carb": 2,
          "_row": "Lotus Europa"
        },
        {
          "mpg": 15.8,
          "cyl": 8,
          "disp": 351,
          "hp": 264,
          "drat": 4.22,
          "wt": 3.17,
          "qsec": 14.5,
          "vs": 0,
          "am": 1,
          "gear": 5,
          "carb": 4,
          "_row": "Ford Pantera L"
        },
        {
          "mpg": 19.7,
          "cyl": 6,
          "disp": 145,
          "hp": 175,
          "drat": 3.62,
          "wt": 2.77,
          "qsec": 15.5,
          "vs": 0,
          "am": 1,
          "gear": 5,
          "carb": 6,
          "_row": "Ferrari Dino"
        },
        {
          "mpg": 15,
          "cyl": 8,
          "disp": 301,
          "hp": 335,
          "drat": 3.54,
          "wt": 3.57,
          "qsec": 14.6,
          "vs": 0,
          "am": 1,
          "gear": 5,
          "carb": 8,
          "_row": "Maserati Bora"
        },
        {
          "mpg": 21.4,
          "cyl": 4,
          "disp": 121,
          "hp": 109,
          "drat": 4.11,
          "wt": 2.78,
          "qsec": 18.6,
          "vs": 1,
          "am": 1,
          "gear": 4,
          "carb": 2,
          "_row": "Volvo 142E"
        }
      ] 

