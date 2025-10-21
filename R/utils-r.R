eval_limited_r_code <- function(
  code,
  ...,
  .error_extra = NULL,
  .error_eval = "Error evaluating R code.",
  .parent_env = emptyenv()
) {
  env <- env_base_r_limited(..., .parent_env = .parent_env)

  tryCatch(
    eval(parse(text = code), envir = env),
    error = function(e) {
      cld_not_find <- gettext("could not find function", domain = "R")
      e_msg <- conditionMessage(e)
      if (grepl(cld_not_find, e_msg, fixed = TRUE)) {
        e_msg <- sub(cld_not_find, "", e_msg, fixed = TRUE)
        e_msg <- trimws(e_msg)
        cli::cli_abort(c(
          "Function not allowed or not found: {e_msg}",
          i = .error_extra
        ))
      } else {
        cli::cli_abort(.error_eval, parent = e)
      }
    }
  )
}

env_base_r_limited <- function(..., .parent_env = emptyenv()) {
  # Create a new environment with a limited set of base R functions that are
  # considered safe for evaluation. You can expand this list as needed, or
  # remove functions that you deem unsafe by setting them to `NULL` in `...`.

  new_environment(
    parent = .parent_env,
    data = list(
      # Assignment and subsetting
      `<-` = base::`<-`,
      `=` = base::`=`,
      `[` = base::`[`,
      `[[` = base::`[[`,
      `$` = base::`$`,
      `[<-` = base::`[<-`,
      `[[<-` = base::`[[<-`,
      `$<-` = base::`$<-`,

      # Logical and comparison operators
      `!` = base::`!`,
      `&` = base::`&`,
      `|` = base::`|`,
      `&&` = base::`&&`,
      `||` = base::`||`,
      `==` = base::`==`,
      `!=` = base::`!=`,
      `<` = base::`<`,
      `>` = base::`>`,
      `<=` = base::`<=`,
      `>=` = base::`>=`,
      `%in%` = base::`%in%`,

      # Arithmetic operators
      `+` = base::`+`,
      `-` = base::`-`,
      `*` = base::`*`,
      `/` = base::`/`,
      `^` = base::`^`,
      `%%` = base::`%%`,
      `%/%` = base::`%/%`,

      # Mathematical functions
      abs = base::abs,
      sign = base::sign,
      sqrt = base::sqrt,
      exp = base::exp,
      log = base::log,
      log10 = base::log10,
      log2 = base::log2,
      ceiling = base::ceiling,
      floor = base::floor,
      trunc = base::trunc,
      round = base::round,
      signif = base::signif,
      min = base::min,
      max = base::max,
      sum = base::sum,
      prod = base::prod,
      mean = base::mean,
      median = stats::median,
      range = base::range,

      # Type checking
      is.null = base::is.null,
      is.na = base::is.na,
      is.nan = base::is.nan,
      is.finite = base::is.finite,
      is.infinite = base::is.infinite,
      is.logical = base::is.logical,
      is.numeric = base::is.numeric,
      is.integer = base::is.integer,
      is.double = base::is.double,
      is.character = base::is.character,
      is.factor = base::is.factor,
      is.list = base::is.list,
      is.data.frame = base::is.data.frame,
      is.matrix = base::is.matrix,
      is.array = base::is.array,

      # Type coercion
      as.logical = base::as.logical,
      as.numeric = base::as.numeric,
      as.integer = base::as.integer,
      as.double = base::as.double,
      as.character = base::as.character,
      as.factor = base::as.factor,
      as.data.frame = base::as.data.frame,
      as.matrix = base::as.matrix,

      # Vector/list operations
      c = base::c,
      list = base::list,
      length = base::length,
      names = base::names,
      `names<-` = base::`names<-`,
      seq = base::seq,
      seq_len = base::seq_len,
      seq_along = base::seq_along,
      rep = base::rep,
      rev = base::rev,
      head = utils::head,
      tail = utils::tail,
      which = base::which,
      match = base::match,
      duplicated = base::duplicated,
      unique = base::unique,
      order = base::order,
      sort = base::sort,
      rank = base::rank,
      append = base::append,
      unlist = base::unlist,

      # Data structure manipulation
      data.frame = base::data.frame,
      nrow = base::nrow,
      ncol = base::ncol,
      dim = base::dim,
      colnames = base::colnames,
      rownames = base::rownames,
      `colnames<-` = base::`colnames<-`,
      `rownames<-` = base::`rownames<-`,
      `dim<-` = base::`dim<-`,
      matrix = base::matrix,
      array = base::array,
      t = base::t,

      # Functional programming
      lapply = base::lapply,
      vapply = base::vapply,
      sapply = base::sapply,
      Map = base::Map,
      Reduce = base::Reduce,
      Filter = base::Filter,

      # String operations
      paste = base::paste,
      paste0 = base::paste0,
      nchar = base::nchar,
      substr = base::substr,
      substring = base::substring,
      strsplit = base::strsplit,
      grep = base::grep,
      grepl = base::grepl,
      sub = base::sub,
      gsub = base::gsub,
      tolower = base::tolower,
      toupper = base::toupper,

      # Special values and NA handling
      anyNA = base::anyNA,

      # Added functions
      ...
    )
  )
}
