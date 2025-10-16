# btw_tool_github_issue_get() works

    Code
      cli::cat_line(result@value)
    Output
      # Issue #1: feat: Add `get_data_frame_skim()`
      
      **State:** closed
      **Author:** gadenbuie
      **Labels:** none
      **Created:** TIMESTAMP
      
      ## Description
      
      Adds a new data frame summary format based on skimr, with a few customizations. It's output as a JSON object, which makes it very dense summary format but less readable for humans. (As a tool call I think that's a good tradeoff.)
      
      ``` r
      pkgload::load_all()
      #> ℹ Loading btw
      
      films <- starwarsdb::films
      get_data_frame(films, format = "skim")
      #> [1] "{\"name\":\"films\",\"n_cols\":6,\"n_rows\":6,\"groups\":[],\"class\":[\"tbl_df\",\"tbl\",\"data.frame\"],\"columns\":{\"release_date\":{\"variable\":\"release_date\",\"type\":\"Date\",\"min\":\"1977-05-25\",\"max\":\"2005-05-19\",\"median\":\"1991-05-22\",\"n_unique\":6},\"title\":{\"variable\":\"title\",\"type\":\"character\",\"min\":10,\"max\":23,\"empty\":0,\"n_unique\":6,\"values\":[\"A New Hope\",\"The Empire Strikes Back\",\"Return of the Jedi\",\"The Phantom Menace\",\"Attack of the Clones\",\"Revenge of the Sith\"]},\"opening_crawl\":{\"variable\":\"opening_crawl\",\"type\":\"character\",\"min\":478,\"max\":522,\"empty\":0,\"n_unique\":6,\"values\":[\"It is a period of civil war.\\r\\nRebel spaceships, striking\\r\\nfrom a hidden base, have won\\r\\ntheir first victory against\\r\\nthe evil Galactic Empir ...\",\"It is a dark time for the\\r\\nRebellion. Although the Death\\r\\nStar has been destroyed,\\r\\nImperial troops have driven the\\r\\nRebel forces from their ...\",\"Luke Skywalker has returned to\\r\\nhis home planet of Tatooine in\\r\\nan attempt to rescue his\\r\\nfriend Han Solo from the\\r\\nclutches of the vile gan ...\",\"Turmoil has engulfed the\\r\\nGalactic Republic. The taxation\\r\\nof trade routes to outlying star\\r\\nsystems is in dispute.\\r\\n\\r\\nHoping to resolve the ...\",\"There is unrest in the Galactic\\r\\nSenate. Several thousand solar\\r\\nsystems have declared their\\r\\nintentions to leave the Republic.\\r\\n\\r\\nThis sepa ...\",\"War! The Republic is crumbling\\r\\nunder attacks by the ruthless\\r\\nSith Lord, Count Dooku.\\r\\nThere are heroes on both sides.\\r\\nEvil is everywhere. ...\"]},\"director\":{\"variable\":\"director\",\"type\":\"character\",\"min\":12,\"max\":16,\"empty\":0,\"n_unique\":3,\"values\":[\"George Lucas\",\"Irvin Kershner\",\"Richard Marquand\"]},\"producer\":{\"variable\":\"producer\",\"type\":\"character\",\"min\":13,\"max\":48,\"empty\":0,\"n_unique\":3,\"values\":[\"Gary Kurtz, Rick McCallum\",\"Howard G. Kazanjian, George Lucas, Rick McCallum\",\"Rick McCallum\"]},\"episode_id\":{\"variable\":\"episode_id\",\"type\":\"numeric\",\"mean\":3.5,\"sd\":1.8708,\"p0\":1,\"p25\":2.25,\"p50\":3.5,\"p75\":4.75,\"p100\":6}}}"
      
      con <- starwarsdb::starwars_connect()
      get_data_frame(dplyr::tbl(con, "people"), format = "skim")
      #> [1] "{\"name\":\"dplyr::tbl(con, \\\"people\\\")\",\"n_cols\":11,\"n_rows\":82,\"groups\":[],\"class\":[\"tbl_duckdb_connection\",\"tbl_dbi\",\"tbl_sql\",\"tbl_lazy\",\"tbl\"],\"columns\":{\"name\":{\"variable\":\"name\",\"type\":\"character\",\"min\":4,\"max\":21,\"empty\":0,\"n_unique\":82,\"values\":[\"R2-D2\",\"R5-D4\",\"Han Solo\",\"Greedo\",\"Luke Skywalker\",\"Owen Lars\",\"Boba Fett\",\"Lobot\",\"Ackbar\",\"Eeth Koth\"]},\"hair_color\":{\"variable\":\"hair_color\",\"type\":\"character\",\"min\":4,\"max\":13,\"empty\":0,\"n_unique\":11,\"values\":[\"auburn, grey\",\"auburn, white\",\"grey\",\"blonde\",\"auburn\",\"brown, grey\",\"blond\",\"white\",null,\"black\"]},\"skin_color\":{\"variable\":\"skin_color\",\"type\":\"character\",\"min\":3,\"max\":19,\"empty\":0,\"n_unique\":29,\"values\":[\"grey, red\",\"red\",\"green, grey\",\"brown, white\",\"grey, blue\",\"green-tan, brown\",\"brown mottle\",\"mottled green\",\"grey, green, yellow\",\"red, blue, white\"]},\"eye_color\":{\"variable\":\"eye_color\",\"type\":\"character\",\"min\":3,\"max\":13,\"empty\":0,\"n_unique\":13,\"values\":[\"pink\",\"red, blue\",\"gold\",\"blue-gray\",\"green, yellow\",\"white\",null,\"hazel\",\"red\",\"orange\"]},\"gender\":{\"variable\":\"gender\",\"type\":\"character\",\"min\":8,\"max\":9,\"empty\":0,\"n_unique\":2,\"values\":[\"masculine\",\"feminine\"]},\"homeworld\":{\"variable\":\"homeworld\",\"type\":\"character\",\"min\":4,\"max\":14,\"empty\":0,\"n_unique\":48,\"values\":[\"Endor\",\"Troiken\",\"Champala\",\"Geonosis\",\"Serenno\",\"Utapau\",\"Eriadu\",\"Rodia\",\"Socorro\",\"Mon Cala\"]},\"species\":{\"variable\":\"species\",\"type\":\"character\",\"min\":3,\"max\":14,\"empty\":0,\"n_unique\":37,\"values\":[\"Xexto\",\"Trandoshan\",\"Mon Calamari\",\"Neimodian\",\"Iktotchi\",\"Chagrian\",\"Skakoan\",\"Aleena\",\"Cerean\",\"Hutt\"]},\"sex\":{\"variable\":\"sex\",\"type\":\"character\",\"min\":4,\"max\":14,\"empty\":0,\"n_unique\":4,\"values\":[\"male\",\"female\",\"hermaphroditic\",\"none\"]},\"height\":{\"variable\":\"height\",\"type\":\"numeric\",\"mean\":174.6049,\"sd\":34.7742,\"p0\":66,\"p25\":167,\"p50\":180,\"p75\":191,\"p100\":264},\"mass\":{\"variable\":\"mass\",\"type\":\"numeric\",\"mean\":97.3119,\"sd\":169.4572,\"p0\":15,\"p25\":55.6,\"p50\":79,\"p75\":84.5,\"p100\":1358},\"birth_year\":{\"variable\":\"birth_year\",\"type\":\"numeric\",\"mean\":87.5651,\"sd\":154.6914,\"p0\":8,\"p25\":35,\"p50\":52,\"p75\":72,\"p100\":896}}}"
      ```
      

# btw_tool_github_issue_thread() works

    Code
      cli::cat_line(result@value)
    Output
      # Issue #1: feat: Add `get_data_frame_skim()`
      
      **State:** closed
      **Author:** gadenbuie
      **Labels:** none
      **Created:** TIMESTAMP
      **Comments:** 0
      
      ## Description
      
      Adds a new data frame summary format based on skimr, with a few customizations. It's output as a JSON object, which makes it very dense summary format but less readable for humans. (As a tool call I think that's a good tradeoff.)
      
      ``` r
      pkgload::load_all()
      #> ℹ Loading btw
      
      films <- starwarsdb::films
      get_data_frame(films, format = "skim")
      #> [1] "{\"name\":\"films\",\"n_cols\":6,\"n_rows\":6,\"groups\":[],\"class\":[\"tbl_df\",\"tbl\",\"data.frame\"],\"columns\":{\"release_date\":{\"variable\":\"release_date\",\"type\":\"Date\",\"min\":\"1977-05-25\",\"max\":\"2005-05-19\",\"median\":\"1991-05-22\",\"n_unique\":6},\"title\":{\"variable\":\"title\",\"type\":\"character\",\"min\":10,\"max\":23,\"empty\":0,\"n_unique\":6,\"values\":[\"A New Hope\",\"The Empire Strikes Back\",\"Return of the Jedi\",\"The Phantom Menace\",\"Attack of the Clones\",\"Revenge of the Sith\"]},\"opening_crawl\":{\"variable\":\"opening_crawl\",\"type\":\"character\",\"min\":478,\"max\":522,\"empty\":0,\"n_unique\":6,\"values\":[\"It is a period of civil war.\\r\\nRebel spaceships, striking\\r\\nfrom a hidden base, have won\\r\\ntheir first victory against\\r\\nthe evil Galactic Empir ...\",\"It is a dark time for the\\r\\nRebellion. Although the Death\\r\\nStar has been destroyed,\\r\\nImperial troops have driven the\\r\\nRebel forces from their ...\",\"Luke Skywalker has returned to\\r\\nhis home planet of Tatooine in\\r\\nan attempt to rescue his\\r\\nfriend Han Solo from the\\r\\nclutches of the vile gan ...\",\"Turmoil has engulfed the\\r\\nGalactic Republic. The taxation\\r\\nof trade routes to outlying star\\r\\nsystems is in dispute.\\r\\n\\r\\nHoping to resolve the ...\",\"There is unrest in the Galactic\\r\\nSenate. Several thousand solar\\r\\nsystems have declared their\\r\\nintentions to leave the Republic.\\r\\n\\r\\nThis sepa ...\",\"War! The Republic is crumbling\\r\\nunder attacks by the ruthless\\r\\nSith Lord, Count Dooku.\\r\\nThere are heroes on both sides.\\r\\nEvil is everywhere. ...\"]},\"director\":{\"variable\":\"director\",\"type\":\"character\",\"min\":12,\"max\":16,\"empty\":0,\"n_unique\":3,\"values\":[\"George Lucas\",\"Irvin Kershner\",\"Richard Marquand\"]},\"producer\":{\"variable\":\"producer\",\"type\":\"character\",\"min\":13,\"max\":48,\"empty\":0,\"n_unique\":3,\"values\":[\"Gary Kurtz, Rick McCallum\",\"Howard G. Kazanjian, George Lucas, Rick McCallum\",\"Rick McCallum\"]},\"episode_id\":{\"variable\":\"episode_id\",\"type\":\"numeric\",\"mean\":3.5,\"sd\":1.8708,\"p0\":1,\"p25\":2.25,\"p50\":3.5,\"p75\":4.75,\"p100\":6}}}"
      
      con <- starwarsdb::starwars_connect()
      get_data_frame(dplyr::tbl(con, "people"), format = "skim")
      #> [1] "{\"name\":\"dplyr::tbl(con, \\\"people\\\")\",\"n_cols\":11,\"n_rows\":82,\"groups\":[],\"class\":[\"tbl_duckdb_connection\",\"tbl_dbi\",\"tbl_sql\",\"tbl_lazy\",\"tbl\"],\"columns\":{\"name\":{\"variable\":\"name\",\"type\":\"character\",\"min\":4,\"max\":21,\"empty\":0,\"n_unique\":82,\"values\":[\"R2-D2\",\"R5-D4\",\"Han Solo\",\"Greedo\",\"Luke Skywalker\",\"Owen Lars\",\"Boba Fett\",\"Lobot\",\"Ackbar\",\"Eeth Koth\"]},\"hair_color\":{\"variable\":\"hair_color\",\"type\":\"character\",\"min\":4,\"max\":13,\"empty\":0,\"n_unique\":11,\"values\":[\"auburn, grey\",\"auburn, white\",\"grey\",\"blonde\",\"auburn\",\"brown, grey\",\"blond\",\"white\",null,\"black\"]},\"skin_color\":{\"variable\":\"skin_color\",\"type\":\"character\",\"min\":3,\"max\":19,\"empty\":0,\"n_unique\":29,\"values\":[\"grey, red\",\"red\",\"green, grey\",\"brown, white\",\"grey, blue\",\"green-tan, brown\",\"brown mottle\",\"mottled green\",\"grey, green, yellow\",\"red, blue, white\"]},\"eye_color\":{\"variable\":\"eye_color\",\"type\":\"character\",\"min\":3,\"max\":13,\"empty\":0,\"n_unique\":13,\"values\":[\"pink\",\"red, blue\",\"gold\",\"blue-gray\",\"green, yellow\",\"white\",null,\"hazel\",\"red\",\"orange\"]},\"gender\":{\"variable\":\"gender\",\"type\":\"character\",\"min\":8,\"max\":9,\"empty\":0,\"n_unique\":2,\"values\":[\"masculine\",\"feminine\"]},\"homeworld\":{\"variable\":\"homeworld\",\"type\":\"character\",\"min\":4,\"max\":14,\"empty\":0,\"n_unique\":48,\"values\":[\"Endor\",\"Troiken\",\"Champala\",\"Geonosis\",\"Serenno\",\"Utapau\",\"Eriadu\",\"Rodia\",\"Socorro\",\"Mon Cala\"]},\"species\":{\"variable\":\"species\",\"type\":\"character\",\"min\":3,\"max\":14,\"empty\":0,\"n_unique\":37,\"values\":[\"Xexto\",\"Trandoshan\",\"Mon Calamari\",\"Neimodian\",\"Iktotchi\",\"Chagrian\",\"Skakoan\",\"Aleena\",\"Cerean\",\"Hutt\"]},\"sex\":{\"variable\":\"sex\",\"type\":\"character\",\"min\":4,\"max\":14,\"empty\":0,\"n_unique\":4,\"values\":[\"male\",\"female\",\"hermaphroditic\",\"none\"]},\"height\":{\"variable\":\"height\",\"type\":\"numeric\",\"mean\":174.6049,\"sd\":34.7742,\"p0\":66,\"p25\":167,\"p50\":180,\"p75\":191,\"p100\":264},\"mass\":{\"variable\":\"mass\",\"type\":\"numeric\",\"mean\":97.3119,\"sd\":169.4572,\"p0\":15,\"p25\":55.6,\"p50\":79,\"p75\":84.5,\"p100\":1358},\"birth_year\":{\"variable\":\"birth_year\",\"type\":\"numeric\",\"mean\":87.5651,\"sd\":154.6914,\"p0\":8,\"p25\":35,\"p50\":52,\"p75\":72,\"p100\":896}}}"
      ```
      

# btw_tool_github_pull_request_get() works

    Code
      cli::cat_line(result@value)
    Output
      # Pull Request #1: feat: Add `get_data_frame_skim()`
      
      **State:** closed
      **Author:** gadenbuie
      **Created:** TIMESTAMP
      **Base:** main
      **Head:** feat/data-skim
      **Mergeable:** unknown
      **Changed Files:** 15 (+195, -90)
      
      ## Description
      
      Adds a new data frame summary format based on skimr, with a few customizations. It's output as a JSON object, which makes it very dense summary format but less readable for humans. (As a tool call I think that's a good tradeoff.)
      
      ``` r
      pkgload::load_all()
      #> ℹ Loading btw
      
      films <- starwarsdb::films
      get_data_frame(films, format = "skim")
      #> [1] "{\"name\":\"films\",\"n_cols\":6,\"n_rows\":6,\"groups\":[],\"class\":[\"tbl_df\",\"tbl\",\"data.frame\"],\"columns\":{\"release_date\":{\"variable\":\"release_date\",\"type\":\"Date\",\"min\":\"1977-05-25\",\"max\":\"2005-05-19\",\"median\":\"1991-05-22\",\"n_unique\":6},\"title\":{\"variable\":\"title\",\"type\":\"character\",\"min\":10,\"max\":23,\"empty\":0,\"n_unique\":6,\"values\":[\"A New Hope\",\"The Empire Strikes Back\",\"Return of the Jedi\",\"The Phantom Menace\",\"Attack of the Clones\",\"Revenge of the Sith\"]},\"opening_crawl\":{\"variable\":\"opening_crawl\",\"type\":\"character\",\"min\":478,\"max\":522,\"empty\":0,\"n_unique\":6,\"values\":[\"It is a period of civil war.\\r\\nRebel spaceships, striking\\r\\nfrom a hidden base, have won\\r\\ntheir first victory against\\r\\nthe evil Galactic Empir ...\",\"It is a dark time for the\\r\\nRebellion. Although the Death\\r\\nStar has been destroyed,\\r\\nImperial troops have driven the\\r\\nRebel forces from their ...\",\"Luke Skywalker has returned to\\r\\nhis home planet of Tatooine in\\r\\nan attempt to rescue his\\r\\nfriend Han Solo from the\\r\\nclutches of the vile gan ...\",\"Turmoil has engulfed the\\r\\nGalactic Republic. The taxation\\r\\nof trade routes to outlying star\\r\\nsystems is in dispute.\\r\\n\\r\\nHoping to resolve the ...\",\"There is unrest in the Galactic\\r\\nSenate. Several thousand solar\\r\\nsystems have declared their\\r\\nintentions to leave the Republic.\\r\\n\\r\\nThis sepa ...\",\"War! The Republic is crumbling\\r\\nunder attacks by the ruthless\\r\\nSith Lord, Count Dooku.\\r\\nThere are heroes on both sides.\\r\\nEvil is everywhere. ...\"]},\"director\":{\"variable\":\"director\",\"type\":\"character\",\"min\":12,\"max\":16,\"empty\":0,\"n_unique\":3,\"values\":[\"George Lucas\",\"Irvin Kershner\",\"Richard Marquand\"]},\"producer\":{\"variable\":\"producer\",\"type\":\"character\",\"min\":13,\"max\":48,\"empty\":0,\"n_unique\":3,\"values\":[\"Gary Kurtz, Rick McCallum\",\"Howard G. Kazanjian, George Lucas, Rick McCallum\",\"Rick McCallum\"]},\"episode_id\":{\"variable\":\"episode_id\",\"type\":\"numeric\",\"mean\":3.5,\"sd\":1.8708,\"p0\":1,\"p25\":2.25,\"p50\":3.5,\"p75\":4.75,\"p100\":6}}}"
      
      con <- starwarsdb::starwars_connect()
      get_data_frame(dplyr::tbl(con, "people"), format = "skim")
      #> [1] "{\"name\":\"dplyr::tbl(con, \\\"people\\\")\",\"n_cols\":11,\"n_rows\":82,\"groups\":[],\"class\":[\"tbl_duckdb_connection\",\"tbl_dbi\",\"tbl_sql\",\"tbl_lazy\",\"tbl\"],\"columns\":{\"name\":{\"variable\":\"name\",\"type\":\"character\",\"min\":4,\"max\":21,\"empty\":0,\"n_unique\":82,\"values\":[\"R2-D2\",\"R5-D4\",\"Han Solo\",\"Greedo\",\"Luke Skywalker\",\"Owen Lars\",\"Boba Fett\",\"Lobot\",\"Ackbar\",\"Eeth Koth\"]},\"hair_color\":{\"variable\":\"hair_color\",\"type\":\"character\",\"min\":4,\"max\":13,\"empty\":0,\"n_unique\":11,\"values\":[\"auburn, grey\",\"auburn, white\",\"grey\",\"blonde\",\"auburn\",\"brown, grey\",\"blond\",\"white\",null,\"black\"]},\"skin_color\":{\"variable\":\"skin_color\",\"type\":\"character\",\"min\":3,\"max\":19,\"empty\":0,\"n_unique\":29,\"values\":[\"grey, red\",\"red\",\"green, grey\",\"brown, white\",\"grey, blue\",\"green-tan, brown\",\"brown mottle\",\"mottled green\",\"grey, green, yellow\",\"red, blue, white\"]},\"eye_color\":{\"variable\":\"eye_color\",\"type\":\"character\",\"min\":3,\"max\":13,\"empty\":0,\"n_unique\":13,\"values\":[\"pink\",\"red, blue\",\"gold\",\"blue-gray\",\"green, yellow\",\"white\",null,\"hazel\",\"red\",\"orange\"]},\"gender\":{\"variable\":\"gender\",\"type\":\"character\",\"min\":8,\"max\":9,\"empty\":0,\"n_unique\":2,\"values\":[\"masculine\",\"feminine\"]},\"homeworld\":{\"variable\":\"homeworld\",\"type\":\"character\",\"min\":4,\"max\":14,\"empty\":0,\"n_unique\":48,\"values\":[\"Endor\",\"Troiken\",\"Champala\",\"Geonosis\",\"Serenno\",\"Utapau\",\"Eriadu\",\"Rodia\",\"Socorro\",\"Mon Cala\"]},\"species\":{\"variable\":\"species\",\"type\":\"character\",\"min\":3,\"max\":14,\"empty\":0,\"n_unique\":37,\"values\":[\"Xexto\",\"Trandoshan\",\"Mon Calamari\",\"Neimodian\",\"Iktotchi\",\"Chagrian\",\"Skakoan\",\"Aleena\",\"Cerean\",\"Hutt\"]},\"sex\":{\"variable\":\"sex\",\"type\":\"character\",\"min\":4,\"max\":14,\"empty\":0,\"n_unique\":4,\"values\":[\"male\",\"female\",\"hermaphroditic\",\"none\"]},\"height\":{\"variable\":\"height\",\"type\":\"numeric\",\"mean\":174.6049,\"sd\":34.7742,\"p0\":66,\"p25\":167,\"p50\":180,\"p75\":191,\"p100\":264},\"mass\":{\"variable\":\"mass\",\"type\":\"numeric\",\"mean\":97.3119,\"sd\":169.4572,\"p0\":15,\"p25\":55.6,\"p50\":79,\"p75\":84.5,\"p100\":1358},\"birth_year\":{\"variable\":\"birth_year\",\"type\":\"numeric\",\"mean\":87.5651,\"sd\":154.6914,\"p0\":8,\"p25\":35,\"p50\":52,\"p75\":72,\"p100\":896}}}"
      ```
      

# btw_tool_github_pull_request_diff() works

    Code
      cli::cat_line(result@value)
    Output
      # Pull Request #1 File Changes
      
      **Total Files Changed:** 15
      
      ## DESCRIPTION
      
      **Status:** modified | **Changes:** +21 -18
      
      ```diff
      @@ -4,33 +4,36 @@ Version: 0.0.0.9001
       Authors@R: c(
           person("Simon", "Couch", , "simon.couch@posit.co", role = c("aut", "cre"),
                  comment = c(ORCID = "0000-0001-5676-5107")),
      -    person("Garrick", "Aden-Buie", , "garrick@adenbuie.com", role = c("aut")),
      -    person("Joe", "Cheng", , "joe@posit.co", role = c("aut")),
      -    person(given = "Posit Software, PBC", role = c("cph", "fnd"))
      +    person("Garrick", "Aden-Buie", , "garrick@adenbuie.com", role = "aut"),
      +    person("Joe", "Cheng", , "joe@posit.co", role = "aut"),
      +    person("Posit Software, PBC", role = c("cph", "fnd"))
         )
       Description: Provides a number of utilities for describing R objects and
      -    package documentation in plain text. For interactive use, this is especially
      -    powerful for describing relevant pieces of context to large language models.
      -    When used programmatically, these utilities can be registered with 'ellmer'
      -    chats as tool calls, enabling language models to peruse package 
      -    documentation and explore your computational environment.
      +    package documentation in plain text. For interactive use, this is
      +    especially powerful for describing relevant pieces of context to large
      +    language models.  When used programmatically, these utilities can be
      +    registered with 'ellmer' chats as tool calls, enabling language models
      +    to peruse package documentation and explore your computational
      +    environment.
       License: MIT + file LICENSE
      -Suggests: 
      -    testthat (>= 3.0.0)
      -Config/testthat/edition: 3
      -Encoding: UTF-8
      -Roxygen: list(markdown = TRUE)
      -RoxygenNote: 7.3.2
      -Imports: 
      +URL: https://github.com/simonpcouch/btw,
      +    https://simonpcouch.github.io/btw/
      +BugReports: https://github.com/simonpcouch/btw/issues
      +Imports:
           cli,
           clipr,
           dplyr,
           ellmer,
           jsonlite,
      -    rmarkdown,
           rlang (>= 1.1.0),
      +    rmarkdown,
      +    skimr,
           tibble,
           withr
      -URL: https://github.com/simonpcouch/btw, https://simonpcouch.github.io/btw/
      -BugReports: https://github.com/simonpcouch/btw/issues
      +Suggests:
      +    testthat (>= 3.0.0)
       Config/Needs/website: tidyverse/tidytemplate
      +Config/testthat/edition: 3
      +Encoding: UTF-8
      +Roxygen: list(markdown = TRUE)
      +RoxygenNote: 7.3.2
      ```
      
      ## R/get-data.R
      
      **Status:** modified | **Changes:** +89 -18
      
      ```diff
      @@ -4,7 +4,10 @@
       #' the data frame itself.
       # TODO: should it be a different function name when there's no `get()`ting
       # happening?
      -#' @param format One of `"glimpse"`, `"print"`, or `"json"`.
      +#' @param format One of `"skim"`, `"glimpse"`, `"print"`, or `"json"`.
      +#' * `"skim"` is the most information-dense format for describing the data. It
      +#'   uses and returns the same information as [skimr::skim()] but formatting as
      +#'   a JSON object that describes the dataset.
       #' * To glimpse the data column-by-column, use `"glimpse"`. This is
       #'   particularly helpful for getting a sense of data frame column names,
       #'   types, and distributions, when pairings of entries in individual rows
      @@ -32,28 +35,33 @@
       #'
       #' @export
       get_data_frame <- function(
      -    data_frame,
      -    format = c("glimpse", "print", "json"),
      -    dims = c(5, 100)
      +  data_frame,
      +  format = c("skim", "glimpse", "print", "json"),
      +  dims = c(5, 100)
       ) {
         format <- rlang::arg_match(format)
         check_inherits(dims, "numeric")
      +  .data_name <- deparse(substitute(data_frame))
       
         # models have likely the seen the "object ___ not found" quite a bit,
         # so no need to rethrow / handle errors nicely
      -  if (!inherits(data_frame, "data.frame")) {
      +  if (inherits(data_frame, "character")) {
      +    .data_name <- data_frame
           data_frame <- get(data_frame)
         }
       
      -  n_row <- min(dims[1], nrow(data_frame))
      -  n_col <- min(dims[2], ncol(data_frame))
      -  data_frame_small <- data_frame[seq_len(n_row), seq_len(n_col), drop = FALSE]
      +  if (format %in% c("print", "json")) {
      +    n_row <- min(dims[1], nrow(data_frame))
      +    n_col <- min(dims[2], ncol(data_frame))
      +    data_frame_small <- data_frame[seq_len(n_row), seq_len(n_col), drop = FALSE]
      +  }
       
         res <- switch(
           format,
      -    glimpse = get_data_frame_glimpse(x = data_frame_small),
      +    glimpse = get_data_frame_glimpse(x = data_frame),
           print = get_data_frame_print(x = data_frame_small),
      -    json = get_data_frame_json(x = data_frame_small)
      +    json = get_data_frame_json(x = data_frame_small),
      +    skim = get_data_frame_skim(data_frame, .data_name)
         )
       
         paste0(res, collapse = "\n")
      @@ -62,20 +70,27 @@ get_data_frame <- function(
       tool_get_data_frame <- function() {
         ellmer::tool(
           get_data_frame,
      -    "Function to extract or manipulate a data frame with various formatting
      -  options.",
      +    "Function to extract or manipulate a data frame with various formatting options.",
           data_frame = ellmer::type_string(
             "The name of the data frame to be described."
           ),
           format = ellmer::type_string(
      -      "The output format of the data frame. Options are \"glimpse\",
      -  \"print\", or \"json\". Defaults to \"glimpse\".",
      +      paste(
      +        "The output format of the data frame: 'skim', 'glimpse', 'print', or 'json'. Default 'skim'.",
      +        "",
      +        "* skim: Returns a JSON object with information about every column in the table.",
      +        "* glimpse: Returns the number of rows, columns, column names and types and the first values of each column",
      +        "* print: Prints the data frame",
      +        "* json: Returns the data frame as JSON",
      +        sep = "\n"
      +      ),
             required = FALSE
           ),
           dims = ellmer::type_array(
      -      "Dimensions for printing the data frame. A numeric vector of length
      -  2, where the first element represents the number of rows and the second
      -  the number of columns. Defaults to `c(5, 100)`.",
      +      paste(
      +        'Dimensions of the data frame to use for the "print" or "json" format.',
      +        "A numeric vector of length 2 as number of rows and columns. Default `c(5, 100)`."
      +      ),
             items = ellmer::type_integer(),
             required = FALSE
           )
      @@ -90,10 +105,66 @@ get_data_frame_glimpse <- function(x, x_name) {
       get_data_frame_print <- function(x) {
         withr::local_options(pillar.advice = FALSE, pillar.min_title_chars = Inf)
       
      -  res <- cli::ansi_strip(capture.output(tibble::as_tibble(x, width = 1000, n = Inf)))
      +  res <- cli::ansi_strip(
      +    capture.output(tibble::as_tibble(x, width = 1000, n = Inf))
      +  )
         res[2:length(res)]
       }
       
       get_data_frame_json <- function(x) {
         capture.output(jsonlite::toJSON(x, auto_unbox = TRUE, pretty = TRUE))
       }
      +
      +get_data_frame_skim <- function(df, .data_name = NULL) {
      +  if (is.null(.data_name)) {
      +    .data_name <- deparse(substitute(df))
      +  }
      +  cols <- skimr::skim(df, .data_name = .data_name)
      +
      +  attrs <- attributes(cols)[c("df_name", "data_cols", "data_rows", "groups")]
      +  names(attrs) <- c("name", "n_cols", "n_rows", "groups")
      +  attrs[["class"]] <- class(df)
      +
      +  # Move variable to the front
      +  cols <- cols[c("skim_variable", setdiff(names(cols), "skim_variable"))]
      +
      +  # Drop histogram and whitespace stats
      +  # TODO: Others
      +  cols <- cols[!grepl("[.](whitespace|hist)$", names(cols))]
      +
      +  # Transpose the list into row-major (purrr::transpose() in base)
      +  cols <- do.call(mapply, c(FUN = list, cols, SIMPLIFY = FALSE))
      +
      +  cols <- lapply(cols, function(col) {
      +    keep <- sprintf("^(skim_|%s.)", col$skim_type)
      +    col <- col[grepl(keep, names(col))]
      +    names(col) <- sub("^skim_", "", names(col))
      +    names(col) <- sub(paste0(col$type, "."), "", names(col), fixed = TRUE)
      +
      +    if (col$type == "character") {
      +      var <- rlang::sym(col$variable)
      +
      +      if (col$n_unique <= 10) {
      +        col$values <- dplyr::pull(dplyr::distinct(df, !!var))
      +      } else {
      +        counts <- dplyr::count(df, !!var)
      +        counts <- dplyr::mutate(counts, rank = dplyr::row_number(.data$n))
      +        counts <- dplyr::filter(counts, .data$rank <= 11)
      +        values <- dplyr::pull(counts, !!var)
      +        col$values <- values[seq_len(min(length(values), 10))]
      +      }
      +      col$values <- vapply(col$values, FUN.VALUE = character(1), function(x) {
      +        if (is.na(x) || nchar(x) <= 140) return(x)
      +        paste(substring(x, 1, 140), "...")
      +      })
      +    } else if (col$type == "factor") {
      +      col$levels <- levels(df[[col$variable]])
      +    }
      +
      +    col
      +  })
      +
      +  attrs$columns <- cols
      +
      +  jsonlite::toJSON(attrs, auto_unbox = TRUE)
      +}
      ```
      
      ## R/get-env.R
      
      **Status:** modified | **Changes:** +12 -3
      
      ```diff
      @@ -19,15 +19,17 @@ get_environment <- function(environment = global_env(), items = NULL) {
         }
       
         res <- character()
      -  env_item_names <- names(environment)
      +  env_item_names <- ls(environment)
         if (!is.null(items)) {
           env_item_names <- env_item_names[env_item_names %in% items]
         }
       
         for (item_name in env_item_names) {
           item <- env_get(environment, item_name)
       
      -    if (inherits(item, "data.frame")) {
      +    if (inherits(item, "Chat")) next
      +
      +    if (inherits(item, c("data.frame", "tbl"))) {
             item_desc <- strsplit(get_data_frame(item), "\n")[[1]]
           } else if (inherits(item, "function")) {
             # TODO: this should be a `get_function()` or something
      @@ -37,7 +39,7 @@ get_environment <- function(environment = global_env(), items = NULL) {
               error = function(e) capture.output(item)
             )
           } else {
      -      item_desc <- capture.output(item)
      +      item_desc <- capture_print(item)
           }
       
           item_res <- c(item_name, paste0("#> ", item_desc), "\n")
      @@ -58,3 +60,10 @@ tool_get_environment <- function() {
           )
         )
       }
      +
      +capture_print <- function(item) {
      +  out <- capture.output(print(item))
      +  if (length(out) && nzchar(out)) return(out)
      +
      +  capture.output(print(item), type = "message")
      +}
      ```
      
      ## R/get-pkg.R
      
      **Status:** modified | **Changes:** +10 -9
      
      ```diff
      @@ -77,15 +77,16 @@ get_package_help <- function(package_name) {
           ignore.case = TRUE
         )
       
      -  res <- help_db$matches |>
      -    dplyr::group_by(Name) |>
      -    dplyr::summarize(
      -      topic_id = dplyr::first(Name),
      -      title = dplyr::first(Entry[Field == "Title"]),
      -      aliases = list(I(Entry[Field == "alias"]))
      -    ) |>
      -    dplyr::ungroup() |>
      -    dplyr::select(topic_id, title, aliases)
      +  res <- help_db$matches
      +  res <- dplyr::group_by(res, Name)
      +  res <- dplyr::summarize(
      +    res,
      +    topic_id = dplyr::first(Name),
      +    title = dplyr::first(Entry[Field == "Title"]),
      +    aliases = list(I(Entry[Field == "alias"]))
      +  )
      +  res <- dplyr::ungroup(res)
      +  res <- dplyr::select(res, topic_id, title, aliases)
       
         get_data_frame(res, format = "json", dims = c(Inf, Inf))
       }
      ```
      
      ## man/btw-package.Rd
      
      **Status:** modified | **Changes:** +1 -1
      
      ```diff
      @@ -5,7 +5,7 @@
       \alias{btw-package}
       \title{btw: Describe R Stuff to Large Language Models}
       \description{
      -Provides a number of utilities for describing R objects and package documentation in plain text. For interactive use, this is especially powerful for describing relevant pieces of context to large language models. When used programmatically, these utilities can be registered with 'ellmer' chats as tool calls, enabling language models to peruse package documentation and explore your R environment.
      +Provides a number of utilities for describing R objects and package documentation in plain text. For interactive use, this is especially powerful for describing relevant pieces of context to large language models. When used programmatically, these utilities can be registered with 'ellmer' chats as tool calls, enabling language models to peruse package documentation and explore your computational environment.
       }
       \seealso{
       Useful links:
      ```
      
      ## man/get_data_frame.Rd
      
      **Status:** modified | **Changes:** +5 -2
      
      ```diff
      @@ -6,16 +6,19 @@
       \usage{
       get_data_frame(
         data_frame,
      -  format = c("glimpse", "print", "json"),
      +  format = c("skim", "glimpse", "print", "json"),
         dims = c(5, 100)
       )
       }
       \arguments{
       \item{data_frame}{A single string naming a data frame or, alternatively,
       the data frame itself.}
       
      -\item{format}{One of \code{"glimpse"}, \code{"print"}, or \code{"json"}.
      +\item{format}{One of \code{"skim"}, \code{"glimpse"}, \code{"print"}, or \code{"json"}.
       \itemize{
      +\item \code{"skim"} is the most information-dense format for describing the data. It
      +uses and returns the same information as \code{\link[skimr:skim]{skimr::skim()}} but formatting as
      +a JSON object that describes the dataset.
       \item To glimpse the data column-by-column, use \code{"glimpse"}. This is
       particularly helpful for getting a sense of data frame column names,
       types, and distributions, when pairings of entries in individual rows
      ```
      
      ## tests/testthat/_snaps/btw.md
      
      **Status:** modified | **Changes:** +1 -1
      
      ```diff
      @@ -3,5 +3,5 @@
           Code
             print(btw(mtcars))
           Output
      -      [1] "mtcars\n#> $ mpg  <dbl> 21.0, 21.0, 22.8, 21.4, 18.7\n#> $ cyl  <dbl> 6, 6, 4, 6, 8\n#> $ disp <dbl> 160, 160, 108, 258, 360\n#> $ hp   <dbl> 110, 110, 93, 110, 175\n#> $ drat <dbl> 3.90, 3.90, 3.85, 3.08, 3.15\n#> $ wt   <dbl> 2.620, 2.875, 2.320, 3.215, 3.440\n#> $ qsec <dbl> 16.46, 17.02, 18.61, 19.44, 17.02\n#> $ vs   <dbl> 0, 0, 1, 1, 0\n#> $ am   <dbl> 1, 1, 1, 0, 0\n#> $ gear <dbl> 4, 4, 4, 3, 3\n#> $ carb <dbl> 4, 4, 1, 1, 2\n\n"
      +      [1] "mtcars\n#> {\"name\":\"item\",\"n_cols\":11,\"n_rows\":32,\"groups\":[],\"class\":\"data.frame\",\"columns\":{\"mpg\":{\"variable\":\"mpg\",\"type\":\"numeric\",\"mean\":20.0906,\"sd\":6.0269,\"p0\":10.4,\"p25\":15.425,\"p50\":19.2,\"p75\":22.8,\"p100\":33.9},\"cyl\":{\"variable\":\"cyl\",\"type\":\"numeric\",\"mean\":6.1875,\"sd\":1.7859,\"p0\":4,\"p25\":4,\"p50\":6,\"p75\":8,\"p100\":8},\"disp\":{\"variable\":\"disp\",\"type\":\"numeric\",\"mean\":230.7219,\"sd\":123.9387,\"p0\":71.1,\"p25\":120.825,\"p50\":196.3,\"p75\":326,\"p100\":472},\"hp\":{\"variable\":\"hp\",\"type\":\"numeric\",\"mean\":146.6875,\"sd\":68.5629,\"p0\":52,\"p25\":96.5,\"p50\":123,\"p75\":180,\"p100\":335},\"drat\":{\"variable\":\"drat\",\"type\":\"numeric\",\"mean\":3.5966,\"sd\":0.5347,\"p0\":2.76,\"p25\":3.08,\"p50\":3.695,\"p75\":3.92,\"p100\":4.93},\"wt\":{\"variable\":\"wt\",\"type\":\"numeric\",\"mean\":3.2172,\"sd\":0.9785,\"p0\":1.513,\"p25\":2.5812,\"p50\":3.325,\"p75\":3.61,\"p100\":5.424},\"qsec\":{\"variable\":\"qsec\",\"type\":\"numeric\",\"mean\":17.8487,\"sd\":1.7869,\"p0\":14.5,\"p25\":16.8925,\"p50\":17.71,\"p75\":18.9,\"p100\":22.9},\"vs\":{\"variable\":\"vs\",\"type\":\"numeric\",\"mean\":0.4375,\"sd\":0.504,\"p0\":0,\"p25\":0,\"p50\":0,\"p75\":1,\"p100\":1},\"am\":{\"variable\":\"am\",\"type\":\"numeric\",\"mean\":0.4062,\"sd\":0.499,\"p0\":0,\"p25\":0,\"p50\":0,\"p75\":1,\"p100\":1},\"gear\":{\"variable\":\"gear\",\"type\":\"numeric\",\"mean\":3.6875,\"sd\":0.7378,\"p0\":3,\"p25\":3,\"p50\":4,\"p75\":4,\"p100\":5},\"carb\":{\"variable\":\"carb\",\"type\":\"numeric\",\"mean\":2.8125,\"sd\":1.6152,\"p0\":1,\"p25\":2,\"p50\":2,\"p75\":4,\"p100\":8}}}\n\n"
       
      ```
      
      ## tests/testthat/_snaps/get-data.md
      
      **Status:** modified | **Changes:** +25 -11
      
      ```diff
      @@ -3,22 +3,36 @@
           Code
             cat(get_data_frame(mtcars))
           Output
      -      $ mpg  <dbl> 21.0, 21.0, 22.8, 21.4, 18.7
      -      $ cyl  <dbl> 6, 6, 4, 6, 8
      -      $ disp <dbl> 160, 160, 108, 258, 360
      -      $ hp   <dbl> 110, 110, 93, 110, 175
      -      $ drat <dbl> 3.90, 3.90, 3.85, 3.08, 3.15
      -      $ wt   <dbl> 2.620, 2.875, 2.320, 3.215, 3.440
      -      $ qsec <dbl> 16.46, 17.02, 18.61, 19.44, 17.02
      -      $ vs   <dbl> 0, 0, 1, 1, 0
      -      $ am   <dbl> 1, 1, 1, 0, 0
      -      $ gear <dbl> 4, 4, 4, 3, 3
      -      $ carb <dbl> 4, 4, 1, 1, 2
      +      {"name":"mtcars","n_cols":11,"n_rows":32,"groups":[],"class":"data.frame","columns":{"mpg":{"variable":"mpg","type":"numeric","mean":20.0906,"sd":6.0269,"p0":10.4,"p25":15.425,"p50":19.2,"p75":22.8,"p100":33.9},"cyl":{"variable":"cyl","type":"numeric","mean":6.1875,"sd":1.7859,"p0":4,"p25":4,"p50":6,"p75":8,"p100":8},"disp":{"variable":"disp","type":"numeric","mean":230.7219,"sd":123.9387,"p0":71.1,"p25":120.825,"p50":196.3,"p75":326,"p100":472},"hp":{"variable":"hp","type":"numeric","mean":146.6875,"sd":68.5629,"p0":52,"p25":96.5,"p50":123,"p75":180,"p100":335},"drat":{"variable":"drat","type":"numeric","mean":3.5966,"sd":0.5347,"p0":2.76,"p25":3.08,"p50":3.695,"p75":3.92,"p100":4.93},"wt":{"variable":"wt","type":"numeric","mean":3.2172,"sd":0.9785,"p0":1.513,"p25":2.5812,"p50":3.325,"p75":3.61,"p100":5.424},"qsec":{"variable":"qsec","type":"numeric","mean":17.8487,"sd":1.7869,"p0":14.5,"p25":16.8925,"p50":17.71,"p75":18.9,"p100":22.9},"vs":{"variable":"vs","type":"numeric","mean":0.4375,"sd":0.504,"p0":0,"p25":0,"p50":0,"p75":1,"p100":1},"am":{"variable":"am","type":"numeric","mean":0.4062,"sd":0.499,"p0":0,"p25":0,"p50":0,"p75":1,"p100":1},"gear":{"variable":"gear","type":"numeric","mean":3.6875,"sd":0.7378,"p0":3,"p25":3,"p50":4,"p75":4,"p100":5},"carb":{"variable":"carb","type":"numeric","mean":2.8125,"sd":1.6152,"p0":1,"p25":2,"p50":2,"p75":4,"p100":8}}}
       
       ---
       
           Code
             cat(get_data_frame(mtcars, dims = c(Inf, Inf)))
      +    Output
      +      {"name":"mtcars","n_cols":11,"n_rows":32,"groups":[],"class":"data.frame","columns":{"mpg":{"variable":"mpg","type":"numeric","mean":20.0906,"sd":6.0269,"p0":10.4,"p25":15.425,"p50":19.2,"p75":22.8,"p100":33.9},"cyl":{"variable":"cyl","type":"numeric","mean":6.1875,"sd":1.7859,"p0":4,"p25":4,"p50":6,"p75":8,"p100":8},"disp":{"variable":"disp","type":"numeric","mean":230.7219,"sd":123.9387,"p0":71.1,"p25":120.825,"p50":196.3,"p75":326,"p100":472},"hp":{"variable":"hp","type":"numeric","mean":146.6875,"sd":68.5629,"p0":52,"p25":96.5,"p50":123,"p75":180,"p100":335},"drat":{"variable":"drat","type":"numeric","mean":3.5966,"sd":0.5347,"p0":2.76,"p25":3.08,"p50":3.695,"p75":3.92,"p100":4.93},"wt":{"variable":"wt","type":"numeric","mean":3.2172,"sd":0.9785,"p0":1.513,"p25":2.5812,"p50":3.325,"p75":3.61,"p100":5.424},"qsec":{"variable":"qsec","type":"numeric","mean":17.8487,"sd":1.7869,"p0":14.5,"p25":16.8925,"p50":17.71,"p75":18.9,"p100":22.9},"vs":{"variable":"vs","type":"numeric","mean":0.4375,"sd":0.504,"p0":0,"p25":0,"p50":0,"p75":1,"p100":1},"am":{"variable":"am","type":"numeric","mean":0.4062,"sd":0.499,"p0":0,"p25":0,"p50":0,"p75":1,"p100":1},"gear":{"variable":"gear","type":"numeric","mean":3.6875,"sd":0.7378,"p0":3,"p25":3,"p50":4,"p75":4,"p100":5},"carb":{"variable":"carb","type":"numeric","mean":2.8125,"sd":1.6152,"p0":1,"p25":2,"p50":2,"p75":4,"p100":8}}}
      +
      +---
      +
      +    Code
      +      cat(get_data_frame(mtcars, format = "glimpse"))
      +    Output
      +      $ mpg  <dbl> 21.0, 21.0, 22.8, 21.4, 18.7, 18.1, 14.3, 24.4, 22.8, 19.2, 17.8,~
      +      $ cyl  <dbl> 6, 6, 4, 6, 8, 6, 8, 4, 4, 6, 6, 8, 8, 8, 8, 8, 8, 4, 4, 4, 4, 8,~
      +      $ disp <dbl> 160.0, 160.0, 108.0, 258.0, 360.0, 225.0, 360.0, 146.7, 140.8, 16~
      +      $ hp   <dbl> 110, 110, 93, 110, 175, 105, 245, 62, 95, 123, 123, 180, 180, 180~
      +      $ drat <dbl> 3.90, 3.90, 3.85, 3.08, 3.15, 2.76, 3.21, 3.69, 3.92, 3.92, 3.92,~
      +      $ wt   <dbl> 2.620, 2.875, 2.320, 3.215, 3.440, 3.460, 3.570, 3.190, 3.150, 3.~
      +      $ qsec <dbl> 16.46, 17.02, 18.61, 19.44, 17.02, 20.22, 15.84, 20.00, 22.90, 18~
      +      $ vs   <dbl> 0, 0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0,~
      +      $ am   <dbl> 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0,~
      +      $ gear <dbl> 4, 4, 4, 3, 3, 3, 3, 4, 4, 4, 4, 3, 3, 3, 3, 3, 3, 4, 4, 4, 3, 3,~
      +      $ carb <dbl> 4, 4, 1, 1, 2, 1, 4, 2, 2, 4, 4, 3, 3, 3, 4, 4, 4, 1, 2, 1, 1, 2,~
      +
      +---
      +
      +    Code
      +      cat(get_data_frame(mtcars, format = "glimpse", dims = c(Inf, Inf)))
           Output
             $ mpg  <dbl> 21.0, 21.0, 22.8, 21.4, 18.7, 18.1, 14.3, 24.4, 22.8, 19.2, 17.8,~
             $ cyl  <dbl> 6, 6, 4, 6, 8, 6, 8, 4, 4, 6, 6, 8, 8, 8, 8, 8, 8, 4, 4, 4, 4, 8,~
      ```
      
      ## tests/testthat/_snaps/get-env.md
      
      **Status:** modified | **Changes:** +5 -25
      
      ```diff
      @@ -3,41 +3,21 @@
           Code
             cat(get_environment(env))
           Output
      -      mtcars
      -      #> $ mpg  <dbl> 21.0, 21.0, 22.8, 21.4, 18.7
      -      #> $ cyl  <dbl> 6, 6, 4, 6, 8
      -      #> $ disp <dbl> 160, 160, 108, 258, 360
      -      #> $ hp   <dbl> 110, 110, 93, 110, 175
      -      #> $ drat <dbl> 3.90, 3.90, 3.85, 3.08, 3.15
      -      #> $ wt   <dbl> 2.620, 2.875, 2.320, 3.215, 3.440
      -      #> $ qsec <dbl> 16.46, 17.02, 18.61, 19.44, 17.02
      -      #> $ vs   <dbl> 0, 0, 1, 1, 0
      -      #> $ am   <dbl> 1, 1, 1, 0, 0
      -      #> $ gear <dbl> 4, 4, 4, 3, 3
      -      #> $ carb <dbl> 4, 4, 1, 1, 2
      -      
      -      
             boop
             #> [1] "bop"
             
      +      
      +      mtcars
      +      #> {"name":"item","n_cols":11,"n_rows":32,"groups":[],"class":"data.frame","columns":{"mpg":{"variable":"mpg","type":"numeric","mean":20.0906,"sd":6.0269,"p0":10.4,"p25":15.425,"p50":19.2,"p75":22.8,"p100":33.9},"cyl":{"variable":"cyl","type":"numeric","mean":6.1875,"sd":1.7859,"p0":4,"p25":4,"p50":6,"p75":8,"p100":8},"disp":{"variable":"disp","type":"numeric","mean":230.7219,"sd":123.9387,"p0":71.1,"p25":120.825,"p50":196.3,"p75":326,"p100":472},"hp":{"variable":"hp","type":"numeric","mean":146.6875,"sd":68.5629,"p0":52,"p25":96.5,"p50":123,"p75":180,"p100":335},"drat":{"variable":"drat","type":"numeric","mean":3.5966,"sd":0.5347,"p0":2.76,"p25":3.08,"p50":3.695,"p75":3.92,"p100":4.93},"wt":{"variable":"wt","type":"numeric","mean":3.2172,"sd":0.9785,"p0":1.513,"p25":2.5812,"p50":3.325,"p75":3.61,"p100":5.424},"qsec":{"variable":"qsec","type":"numeric","mean":17.8487,"sd":1.7869,"p0":14.5,"p25":16.8925,"p50":17.71,"p75":18.9,"p100":22.9},"vs":{"variable":"vs","type":"numeric","mean":0.4375,"sd":0.504,"p0":0,"p25":0,"p50":0,"p75":1,"p100":1},"am":{"variable":"am","type":"numeric","mean":0.4062,"sd":0.499,"p0":0,"p25":0,"p50":0,"p75":1,"p100":1},"gear":{"variable":"gear","type":"numeric","mean":3.6875,"sd":0.7378,"p0":3,"p25":3,"p50":4,"p75":4,"p100":5},"carb":{"variable":"carb","type":"numeric","mean":2.8125,"sd":1.6152,"p0":1,"p25":2,"p50":2,"p75":4,"p100":8}}}
      +      
       
       ---
       
           Code
             cat(get_environment(env, items = "mtcars"))
           Output
             mtcars
      -      #> $ mpg  <dbl> 21.0, 21.0, 22.8, 21.4, 18.7
      -      #> $ cyl  <dbl> 6, 6, 4, 6, 8
      -      #> $ disp <dbl> 160, 160, 108, 258, 360
      -      #> $ hp   <dbl> 110, 110, 93, 110, 175
      -      #> $ drat <dbl> 3.90, 3.90, 3.85, 3.08, 3.15
      -      #> $ wt   <dbl> 2.620, 2.875, 2.320, 3.215, 3.440
      -      #> $ qsec <dbl> 16.46, 17.02, 18.61, 19.44, 17.02
      -      #> $ vs   <dbl> 0, 0, 1, 1, 0
      -      #> $ am   <dbl> 1, 1, 1, 0, 0
      -      #> $ gear <dbl> 4, 4, 4, 3, 3
      -      #> $ carb <dbl> 4, 4, 1, 1, 2
      +      #> {"name":"item","n_cols":11,"n_rows":32,"groups":[],"class":"data.frame","columns":{"mpg":{"variable":"mpg","type":"numeric","mean":20.0906,"sd":6.0269,"p0":10.4,"p25":15.425,"p50":19.2,"p75":22.8,"p100":33.9},"cyl":{"variable":"cyl","type":"numeric","mean":6.1875,"sd":1.7859,"p0":4,"p25":4,"p50":6,"p75":8,"p100":8},"disp":{"variable":"disp","type":"numeric","mean":230.7219,"sd":123.9387,"p0":71.1,"p25":120.825,"p50":196.3,"p75":326,"p100":472},"hp":{"variable":"hp","type":"numeric","mean":146.6875,"sd":68.5629,"p0":52,"p25":96.5,"p50":123,"p75":180,"p100":335},"drat":{"variable":"drat","type":"numeric","mean":3.5966,"sd":0.5347,"p0":2.76,"p25":3.08,"p50":3.695,"p75":3.92,"p100":4.93},"wt":{"variable":"wt","type":"numeric","mean":3.2172,"sd":0.9785,"p0":1.513,"p25":2.5812,"p50":3.325,"p75":3.61,"p100":5.424},"qsec":{"variable":"qsec","type":"numeric","mean":17.8487,"sd":1.7869,"p0":14.5,"p25":16.8925,"p50":17.71,"p75":18.9,"p100":22.9},"vs":{"variable":"vs","type":"numeric","mean":0.4375,"sd":0.504,"p0":0,"p25":0,"p50":0,"p75":1,"p100":1},"am":{"variable":"am","type":"numeric","mean":0.4062,"sd":0.499,"p0":0,"p25":0,"p50":0,"p75":1,"p100":1},"gear":{"variable":"gear","type":"numeric","mean":3.6875,"sd":0.7378,"p0":3,"p25":3,"p50":4,"p75":4,"p100":5},"carb":{"variable":"carb","type":"numeric","mean":2.8125,"sd":1.6152,"p0":1,"p25":2,"p50":2,"p75":4,"p100":8}}}
             
       
       ---
      ```
      
      ## tests/testthat/helpers.R
      
      **Status:** added | **Changes:** +5 -0
      
      ```diff
      @@ -0,0 +1,5 @@
      +skip_if_not_macos <- function() {
      +  # Skip helper: snapshots only on macos for now
      +  skip_on_os("windows")
      +  skip_on_os("linux")
      +}
      ```
      
      ## tests/testthat/test-btw.R
      
      **Status:** modified | **Changes:** +2 -0
      
      ```diff
      @@ -1,3 +1,5 @@
      +skip_if_not_macos()
      +
       test_that("btw works", {
         local_mocked_bindings(interactive = function() FALSE)
         # have to `print()` as the result is returned invisibly
      ```
      
      ## tests/testthat/test-get-data.R
      
      **Status:** modified | **Changes:** +13 -2
      
      ```diff
      @@ -1,10 +1,21 @@
      +skip_if_not_macos()
      +
       test_that("get_data_frame works", {
         expect_snapshot(cat(get_data_frame(mtcars)))
         expect_snapshot(cat(get_data_frame(mtcars, dims = c(Inf, Inf))))
       
      +  expect_snapshot(cat(get_data_frame(mtcars, format = "glimpse")))
      +  expect_snapshot(
      +    cat(get_data_frame(mtcars, format = "glimpse", dims = c(Inf, Inf)))
      +  )
      +
         expect_snapshot(cat(get_data_frame(mtcars, format = "print")))
      -  expect_snapshot(cat(get_data_frame(mtcars, format = "print", dims = c(Inf, Inf))))
      +  expect_snapshot(
      +    cat(get_data_frame(mtcars, format = "print", dims = c(Inf, Inf)))
      +  )
       
         expect_snapshot(cat(get_data_frame(mtcars, format = "json")))
      -  expect_snapshot(cat(get_data_frame(mtcars, format = "json", dims = c(Inf, Inf))))
      +  expect_snapshot(
      +    cat(get_data_frame(mtcars, format = "json", dims = c(Inf, Inf)))
      +  )
       })
      ```
      
      ## tests/testthat/test-get-env.R
      
      **Status:** modified | **Changes:** +2 -0
      
      ```diff
      @@ -1,3 +1,5 @@
      +skip_if_not_macos()
      +
       test_that("get_environment works", {
         env <- new_environment(list(mtcars = mtcars, boop = "bop"))
       
      ```
      
      ## tests/testthat/test-get-pkg.R
      
      **Status:** modified | **Changes:** +2 -0
      
      ```diff
      @@ -13,6 +13,8 @@ test_that("get_package_help() works", {
       })
       
       test_that("get_help_page() works", {
      +  skip_if_not_macos()
      +  
         res <- get_help_page("stats", "rnorm")
         
         expect_snapshot(res)
      ```
      
      ## tests/testthat/test-utils.R
      
      **Status:** modified | **Changes:** +2 -0
      
      ```diff
      @@ -4,6 +4,8 @@ test_that("check_installed() works", {
         expect_invisible(check_installed("somepackage"))
         expect_null(check_installed("somepackage"))
       
      +  skip_if_not_macos()
      +
         # informative error if not installed
         testthat::local_mocked_bindings(is_installed = function(x) FALSE)
         expect_snapshot(check_installed("somepackage"), error = TRUE)
      ```
      
      

# btw_tool_github_issues_list() works

    Code
      cli::cat_line(result@value)
    Output
      # Issues in posit-dev/btw
      
      **Total:** 5
      
      ## #104: feat: Add git tools
      
      **State:** closed | **Author:** gadenbuie
      
      Adds seven new git tools for basic repository operations using the {gert} package, enhancing functionality for status, diffs, commits, and branch management. 
      
      ## #103: docs: Add return value where missing
      
      **State:** closed | **Author:** gadenbuie
      
      Fixes a common CRAN incoming check issue
      
      (All clear with these checks: https://gist.github.com/gadenbuie/39d6bde907ef20a4252c93adca44cb2a)
      
      ## #102: tests: Refactor `with_retry()` to keep `test_that()` outer call
      
      **State:** closed | **Author:** gadenbuie
      
      (no description)
      
      ## #101: test: Try tests with retry for flaky chromote tests
      
      **State:** closed | **Author:** gadenbuie
      
      (no description)
      
      ## #100: feat: Add `btw_task_create_readme()`
      
      **State:** closed | **Author:** gadenbuie
      
      New task agent to help create a README in the current project
      
      

# btw_tool_github_issues_list() handles no results

    Code
      cli::cat_line(result@value)
    Output
      No issues found matching filters: state=open, labels=nonexistent-label-xyz123

# btw_tool_github_pull_requests_list() works

    Code
      cli::cat_line(result@value)
    Output
      # Pull Requests in posit-dev/btw
      
      **Total:** 5
      
      ## #104: feat: Add git tools
      
      **State:** closed | **Author:** gadenbuie
      
      Adds seven new git tools for basic repository operations using the {gert} package, enhancing functionality for status, diffs, commits, and branch management. 
      
      ## #103: docs: Add return value where missing
      
      **State:** closed | **Author:** gadenbuie
      
      Fixes a common CRAN incoming check issue
      
      (All clear with these checks: https://gist.github.com/gadenbuie/39d6bde907ef20a4252c93adca44cb2a)
      
      ## #102: tests: Refactor `with_retry()` to keep `test_that()` outer call
      
      **State:** closed | **Author:** gadenbuie
      
      (no description)
      
      ## #101: test: Try tests with retry for flaky chromote tests
      
      **State:** closed | **Author:** gadenbuie
      
      (no description)
      
      ## #100: feat: Add `btw_task_create_readme()`
      
      **State:** closed | **Author:** gadenbuie
      
      New task agent to help create a README in the current project
      
      

# get_github_repo() errors when no repo detected

    Code
      get_github_repo()
    Condition
      Error in `get_github_repo()`:
      ! Could not detect GitHub repository.
      i Provide `owner` and `repo` parameters, or run from within a git repository with a GitHub remote.

