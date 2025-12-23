mock_chat <- function() {
  structure(
    list(
      messages = list(),
      system_prompt = NULL,
      tools = list()
    ),
    class = "Chat"
  )
}

test_that("generate_session_id() creates valid IDs", {
  id1 <- generate_session_id()
  id2 <- generate_session_id()

  expect_match(id1, "^[a-z]+_[a-z]+$")
  expect_match(id2, "^[a-z]+_[a-z]+$")

  # IDs should be different (probabilistically, could fail occasionally but unlikely)
  ids <- replicate(10, generate_session_id())
  expect_true(length(unique(ids)) > 5)
})

test_that("generate_session_id() checks for uniqueness", {
  clear_all_subagent_sessions()

  id1 <- generate_session_id()
  store_session(id1, mock_chat())

  ids <- replicate(20, generate_session_id())
  expect_false(id1 %in% ids)

  clear_all_subagent_sessions()
})

test_that("store_session() and retrieve_session() work", {
  clear_all_subagent_sessions()

  session_id <- "test_session"
  chat <- mock_chat()

  result <- store_session(session_id, chat)
  expect_equal(result, session_id)

  session <- retrieve_session(session_id)
  expect_type(session, "list")
  expect_equal(session$id, session_id)
  expect_equal(session$chat, chat)
  expect_s3_class(session$created, "POSIXct")
  expect_null(session$last_used)

  clear_all_subagent_sessions()
})

test_that("store_session() requires Chat object", {
  expect_error(
    store_session("test", "not a chat object"),
    "must be a.*Chat"
  )
})

test_that("retrieve_session() returns NULL for nonexistent session", {
  clear_all_subagent_sessions()
  session <- retrieve_session("nonexistent_session")
  expect_null(session)
})

test_that("store_session() can include metadata", {
  clear_all_subagent_sessions()

  session_id <- "test_with_metadata"
  chat <- mock_chat()
  metadata <- list(custom_field = "custom_value")

  store_session(session_id, chat, metadata)
  session <- retrieve_session(session_id)

  expect_equal(session$custom_field, "custom_value")

  clear_all_subagent_sessions()
})

test_that("list_subagent_sessions() works with no sessions", {
  clear_all_subagent_sessions()

  result <- list_subagent_sessions()
  expect_type(result, "list")
  expect_equal(length(result), 0)
})

test_that("list_subagent_sessions() lists all sessions", {
  clear_all_subagent_sessions()

  store_session("session_1", mock_chat())
  store_session("session_2", mock_chat())
  store_session("session_3", mock_chat())

  result <- list_subagent_sessions()
  expect_type(result, "list")
  expect_equal(length(result), 3)

  session_ids <- names(result)
  expect_true("session_1" %in% session_ids)
  expect_true("session_2" %in% session_ids)
  expect_true("session_3" %in% session_ids)

  expect_equal(result$session_1$id, "session_1")
  expect_equal(result$session_2$id, "session_2")
  expect_equal(result$session_3$id, "session_3")

  clear_all_subagent_sessions()
})

test_that("clear_subagent_session() removes a session", {
  clear_all_subagent_sessions()

  session_id <- "test_clear"
  store_session(session_id, mock_chat())

  expect_false(is.null(retrieve_session(session_id)))

  result <- clear_subagent_session(session_id)
  expect_true(result)

  expect_null(retrieve_session(session_id))
})

test_that("clear_subagent_session() returns FALSE for nonexistent session", {
  clear_all_subagent_sessions()
  result <- clear_subagent_session("nonexistent")
  expect_false(result)
})

test_that("clear_all_subagent_sessions() clears all sessions", {
  clear_all_subagent_sessions()

  store_session("session_1", mock_chat())
  store_session("session_2", mock_chat())
  store_session("session_3", mock_chat())

  expect_equal(length(list_subagent_sessions()), 3)

  count <- clear_all_subagent_sessions()
  expect_equal(count, 3)

  expect_equal(length(list_subagent_sessions()), 0)
})

test_that("clear_all_subagent_sessions() returns 0 when no sessions", {
  clear_all_subagent_sessions()
  count <- clear_all_subagent_sessions()
  expect_equal(count, 0)
})

test_that("btw_subagent_client_config() uses default tools", {
  withr::local_options(
    btw.subagent.tools_default = NULL,
    btw.tools = NULL
  )

  chat <- btw_subagent_client_config()

  expect_true(inherits(chat, "Chat"))
  expect_true(length(chat$get_tools()) > 0)
})

test_that("btw_subagent_client_config() respects tool filtering", {
  chat <- btw_subagent_client_config(tools = c("docs"))

  expect_true(inherits(chat, "Chat"))
  expect_true(length(chat$get_tools()) > 0)
})

test_that("btw_subagent_client_config() follows client precedence", {
  skip_if_not_installed("ellmer")

  withr::local_options(
    btw.subagent.client = "anthropic/claude-sonnet-4-20250514",
    btw.client = "anthropic/claude-opus-4-20241120"
  )

  chat <- btw_subagent_client_config()
  expect_true(inherits(chat, "Chat"))

  chat_obj <- ellmer::chat_anthropic()
  chat2 <- btw_subagent_client_config(client = chat_obj)
  expect_identical(chat2, chat_obj)
})

test_that("btw_subagent_client_config() clones clients from options", {
  skip_if_not_installed("ellmer")

  chat_obj <- ellmer::chat_anthropic()

  withr::local_options(btw.subagent.client = chat_obj)

  chat1 <- btw_subagent_client_config()
  chat2 <- btw_subagent_client_config()

  expect_false(identical(chat1, chat2))
  expect_false(identical(chat1, chat_obj))
})

test_that("build_subagent_description() includes tool groups", {
  desc <- build_subagent_description()

  expect_type(desc, "character")
  expect_match(desc, "Delegate a task")
  expect_match(desc, "Available tool groups")
  expect_true(any(grepl("docs|env|search|github", desc)))
})

test_that("build_subagent_description() includes basic text", {
  desc <- build_subagent_description()

  expect_type(desc, "character")
  expect_match(desc, "Delegate a task")
  expect_match(desc, "subagent")
})

test_that("btw_tool_subagent is registered in btw_tools", {
  all_tools <- btw_tools()

  tool_names <- sapply(all_tools, function(t) t@name)
  expect_true("btw_tool_subagent" %in% tool_names)

  subagent_tool <- all_tools[[which(tool_names == "btw_tool_subagent")]]

  expect_equal(subagent_tool@name, "btw_tool_subagent")
  expect_type(subagent_tool@description, "character")
  expect_match(subagent_tool@description, "Delegate a task")
  expect_true(length(subagent_tool@arguments) > 0)
})

test_that("BtwSubagentResult inherits from BtwToolResult", {
  result <- BtwSubagentResult(
    value = "test response",
    session_id = "test_id",
    extra = list()
  )

  expect_true(S7::S7_inherits(result, BtwSubagentResult))
  expect_true(S7::S7_inherits(result, BtwToolResult))
  expect_equal(result@value, "test response")
  expect_equal(result@session_id, "test_id")
})

# Tests for new btw.subagent.tools_default and btw.subagent.tools_allowed options

test_that("btw_subagent_client_config() uses tools_default when tools is NULL", {
  withr::local_options(
    btw.subagent.tools_default = c("docs"),
    btw.tools = NULL
  )

  chat <- btw_subagent_client_config(tools = NULL)

  expect_true(inherits(chat, "Chat"))
  tool_names <- map_chr(chat$get_tools(), function(t) t@name)
  expect_true(all(grepl("^btw_tool_docs_", tool_names)))
})

test_that("btw_subagent_client_config() falls back through precedence chain", {
  # Test fallback: tools_default -> btw.tools -> btw_tools()

  # Test fallback to btw.tools
  withr::local_options(
    btw.subagent.tools_default = NULL,
    btw.tools = c("search")
  )

  chat <- btw_subagent_client_config(tools = NULL)

  tool_names <- map_chr(chat$get_tools(), function(t) t@name)
  expect_true(all(grepl("^btw_tool_search_", tool_names)))

  # Test fallback to btw_tools()
  withr::local_options(
    btw.subagent.tools_default = NULL,
    btw.tools = NULL
  )

  chat2 <- btw_subagent_client_config(tools = NULL)

  tool_names2 <- sapply(chat2$get_tools(), function(t) t@name)
  expect_true(length(tool_names2) > 0) # Should get all btw_tools()
})

test_that("btw_subagent_client_config() filters tools with tools_allowed", {
  withr::local_options(
    btw.subagent.tools_allowed = c("docs"),
    btw.subagent.tools_default = c("docs", "files")
  )

  chat <- btw_subagent_client_config(tools = NULL)

  tool_names <- map_chr(chat$get_tools(), function(t) t@name)
  expect_true(all(grepl("^btw_tool_docs_", tool_names)))
  expect_false(any(grepl("^btw_tool_files_", tool_names)))
})

test_that("btw_subagent_client_config() errors on disallowed tools", {
  withr::local_options(
    btw.subagent.tools_allowed = c("docs")
  )

  expect_error(
    btw_subagent_client_config(tools = c("files")),
    "Subagent requested disallowed tools"
  )

  expect_error(
    btw_subagent_client_config(tools = c("files")),
    "btw.subagent.tools_allowed"
  )
})

test_that("btw_subagent_client_config() allows tools within whitelist", {
  withr::local_options(
    btw.subagent.tools_allowed = c("docs", "files")
  )

  # Should not error
  chat <- btw_subagent_client_config(tools = c("docs"))

  tool_names <- map_chr(chat$get_tools(), function(t) t@name)
  expect_true(all(grepl("^btw_tool_docs_", tool_names)))
})

test_that("btw_subagent_client_config() filters explicit tools against tools_allowed", {
  withr::local_options(
    btw.subagent.tools_allowed = c("docs", "search")
  )

  # Requesting tools partially in whitelist should error
  expect_error(
    btw_subagent_client_config(tools = c("docs", "files")),
    "disallowed tools"
  )

  # Requesting only allowed tools should work
  chat <- btw_subagent_client_config(tools = c("docs", "search"))

  tool_names <- map_chr(chat$get_tools(), function(t) t@name)
  expect_true(any(grepl("^btw_tool_docs_", tool_names)))
  expect_true(any(grepl("^btw_tool_search_", tool_names)))
})

test_that("btw_subagent_client_config() works without tools_allowed set", {
  withr::local_options(
    btw.subagent.tools_allowed = NULL,
    btw.subagent.tools_default = c("files")
  )

  # Should work with any tools when tools_allowed is NULL
  chat <- btw_subagent_client_config(tools = c("docs"))

  tool_names <- map_chr(chat$get_tools(), function(t) t@name)
  expect_true(all(grepl("^btw_tool_docs_", tool_names)))
})

test_that("btw_subagent_client_config() precedence: explicit tools > tools_default", {
  withr::local_options(
    btw.subagent.tools_default = c("docs"),
    btw.subagent.tools_allowed = c("docs", "files")
  )

  # Explicit tools argument should override tools_default
  chat <- btw_subagent_client_config(tools = c("files"))

  tool_names <- map_chr(chat$get_tools(), function(t) t@name)
  expect_true(all(grepl("^btw_tool_files_", tool_names)))
  expect_false(any(grepl("^btw_tool_docs_", tool_names)))
})

test_that("btw_subagent_client_config() tools_allowed filters defaults", {
  withr::local_options(
    btw.subagent.tools_allowed = c("docs"),
    btw.subagent.tools_default = c("docs", "files", "search")
  )

  chat <- btw_subagent_client_config(tools = NULL)

  tool_names <- map_chr(chat$get_tools(), function(t) t@name)
  expect_true(all(grepl("^btw_tool_docs_", tool_names)))
  expect_false(any(grepl("^btw_tool_files_", tool_names)))
  expect_false(any(grepl("^btw_tool_search_", tool_names)))
})

test_that("btw_subagent_client_config() error message is helpful", {
  withr::local_options(
    btw.subagent.tools_allowed = c("docs")
  )

  expect_error(
    btw_subagent_client_config(tools = c("files", "github")),
    "btw_tool_files_"
  )

  expect_error(
    btw_subagent_client_config(tools = c("files", "github")),
    "btw_tool_github"
  )

  expect_error(
    btw_subagent_client_config(tools = c("files")),
    "Set.*btw.subagent.tools_allowed = NULL"
  )
})

test_that("btw_subagent_client_config() tools_allowed works with specific tool names", {
  withr::local_options(
    btw.subagent.tools_allowed = c(
      "btw_tool_docs_help_page",
      "btw_tool_files_read_text_file"
    )
  )

  # Should work with specific allowed tools
  chat <- btw_subagent_client_config(tools = c("btw_tool_docs_help_page"))

  tool_names <- map_chr(chat$get_tools(), function(t) t@name)
  expect_true("btw_tool_docs_help_page" %in% tool_names)
  expect_equal(length(tool_names), 1)

  # Should error with disallowed specific tool
  expect_error(
    btw_subagent_client_config(tools = c("search_packages")),
    "disallowed tools"
  )
})

# Tests for subagent tool filtering (prevents recursive subagents)

test_that("btw_tool_subagent is filtered out when explicitly requested", {
  # Explicitly request the subagent tool
  expect_warning(
    chat <- btw_subagent_client_config(tools = c("btw_tool_subagent", "docs")),
    "btw_tool_subagent"
  )

  tool_names <- map_chr(chat$get_tools(), function(t) t@name)

  # Should not include btw_tool_subagent
  expect_false("btw_tool_subagent" %in% tool_names)

  # Should still include other requested tools
  expect_true(any(grepl("^btw_tool_docs_", tool_names)))
})

test_that("btw_tool_subagent is filtered out from default tools", {
  withr::local_options(
    btw.subagent.tools_default = NULL,
    btw.tools = NULL,
    btw.subagent.tools_allowed = NULL
  )

  # Use default tools (btw_tools())
  chat <- btw_subagent_client_config(tools = NULL)

  tool_names <- map_chr(chat$get_tools(), function(t) t@name)

  # btw_tool_subagent should not be in the tools
  expect_false("btw_tool_subagent" %in% tool_names)

  # But other tools should be present
  expect_true(length(tool_names) > 0)
})

test_that("btw_tool_subagent is filtered out from 'agent' tool group", {
  # Request the 'agent' tool group which includes btw_tool_subagent
  expect_warning(
    chat <- btw_subagent_client_config(tools = c("agent")),
    "btw_tool_subagent"
  )

  tool_names <- map_chr(chat$get_tools(), function(t) t@name)

  # btw_tool_subagent should be filtered out
  expect_false("btw_tool_subagent" %in% tool_names)
})

test_that("btw_tool_subagent is filtered out even when in tools_allowed", {
  withr::local_options(
    btw.subagent.tools_allowed = c("agent", "docs")
  )

  # Request agent group (which includes subagent tool)
  expect_warning(
    chat <- btw_subagent_client_config(tools = c("agent", "docs")),
    "btw_tool_subagent"
  )

  tool_names <- map_chr(chat$get_tools(), function(t) t@name)

  # btw_tool_subagent should still be filtered out
  expect_false("btw_tool_subagent" %in% tool_names)

  # But other tools should be present
  expect_true(any(grepl("^btw_tool_docs_", tool_names)))
})

test_that("btw_tool_subagent never appears in chat$get_tools() for subagent", {
  # Test multiple scenarios to ensure subagent tool never appears

  # Scenario 1: Explicit request
  expect_warning(
    chat1 <- btw_subagent_client_config(tools = c("btw_tool_subagent")),
    "btw_tool_subagent"
  )
  expect_false(
    "btw_tool_subagent" %in% sapply(chat1$get_tools(), function(t) t@name)
  )

  # Scenario 2: Via tool group
  expect_warning(
    chat2 <- btw_subagent_client_config(tools = c("agent")),
    "btw_tool_subagent"
  )

  expect_false(
    "btw_tool_subagent" %in% sapply(chat2$get_tools(), function(t) t@name)
  )

  # Scenario 3: Default tools
  withr::local_options(
    btw.subagent.tools_default = NULL,
    btw.tools = NULL
  )
  chat3 <- btw_subagent_client_config(tools = NULL)
  expect_false(
    "btw_tool_subagent" %in% sapply(chat3$get_tools(), function(t) t@name)
  )

  # Scenario 4: Mixed with other tools
  expect_warning(
    chat4 <- btw_subagent_client_config(
      tools = c("btw_tool_subagent", "docs", "files")
    ),
    "btw_tool_subagent"
  )
  expect_false(
    "btw_tool_subagent" %in% sapply(chat4$get_tools(), function(t) t@name)
  )
})

test_that("subagent tool filtering happens after tools_allowed filtering", {
  withr::local_options(
    btw.subagent.tools_allowed = c("btw_tool_subagent", "docs")
  )

  # Even if subagent tool is in allowed list, it should be filtered out
  expect_warning(
    chat <- btw_subagent_client_config(tools = c("btw_tool_subagent", "docs")),
    "btw_tool_subagent"
  )

  tool_names <- map_chr(chat$get_tools(), function(t) t@name)

  # Subagent tool should be filtered out
  expect_false("btw_tool_subagent" %in% tool_names)

  # Docs tools should remain
  expect_true(any(grepl("^btw_tool_docs_", tool_names)))
})
