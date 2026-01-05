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

# Internal session management functions (subagent_new_session_id, subagent_store_session, etc.)
# are tested through the public API via subagent_get_or_create_session()
# See behavioral tests at the end of this file.

test_that("subagent_client() uses default tools", {
  withr::local_options(
    btw.subagent.tools_default = NULL,
    btw.tools = NULL
  )

  chat <- subagent_client()

  expect_true(inherits(chat, "Chat"))
  expect_true(length(chat$get_tools()) > 0)
})

test_that("subagent_client() respects tool filtering", {
  chat <- subagent_client(tools = c("docs"))

  expect_true(inherits(chat, "Chat"))
  expect_true(length(chat$get_tools()) > 0)
})

test_that("subagent_client() follows client precedence", {
  skip_if_not_installed("ellmer")

  withr::local_options(
    btw.subagent.client = "anthropic/claude-sonnet-4-20250514",
    btw.client = "anthropic/claude-opus-4-20241120"
  )

  chat <- subagent_client()
  expect_true(inherits(chat, "Chat"))

  chat_obj <- ellmer::chat_anthropic()
  chat2 <- subagent_client(client = chat_obj)
  expect_identical(chat2, chat_obj)
})

test_that("subagent_client() clones clients from options", {
  skip_if_not_installed("ellmer")

  chat_obj <- ellmer::chat_anthropic()

  withr::local_options(btw.subagent.client = chat_obj)

  chat1 <- subagent_client()
  chat2 <- subagent_client()

  expect_false(identical(chat1, chat2))
  expect_false(identical(chat1, chat_obj))
})

# subagent_build_description() is internal - description content is tested
# through btw_tool_agent_subagent registration tests below

test_that("btw_tool_agent_subagent is registered in btw_tools", {
  all_tools <- btw_tools()

  tool_names <- sapply(all_tools, function(t) t@name)
  expect_true("btw_tool_agent_subagent" %in% tool_names)

  subagent_tool <- all_tools[[which(tool_names == "btw_tool_agent_subagent")]]

  expect_equal(subagent_tool@name, "btw_tool_agent_subagent")
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

test_that("subagent_client() uses tools_default when tools is NULL", {
  withr::local_options(
    btw.subagent.tools_default = c("docs"),
    btw.tools = NULL
  )

  chat <- subagent_client(tools = NULL)

  expect_true(inherits(chat, "Chat"))
  tool_names <- map_chr(chat$get_tools(), function(t) t@name)
  expect_true(all(grepl("^btw_tool_docs_", tool_names)))
})

test_that("subagent_client() falls back through precedence chain", {
  # Test fallback: tools_default -> btw.tools -> btw_tools()

  # Test fallback to btw.tools
  withr::local_options(
    btw.subagent.tools_default = NULL,
    btw.tools = c("search")
  )

  chat <- subagent_client(tools = NULL)

  tool_names <- map_chr(chat$get_tools(), function(t) t@name)
  expect_true(all(grepl("^btw_tool_search_", tool_names)))

  # Test fallback to btw_tools()
  withr::local_options(
    btw.subagent.tools_default = NULL,
    btw.tools = NULL
  )

  chat2 <- subagent_client(tools = NULL)

  tool_names2 <- sapply(chat2$get_tools(), function(t) t@name)
  expect_true(length(tool_names2) > 0) # Should get all btw_tools()
})

test_that("subagent_client() filters tools with tools_allowed", {
  withr::local_options(
    btw.subagent.tools_allowed = c("docs"),
    btw.subagent.tools_default = c("docs", "files")
  )

  chat <- subagent_client(tools = NULL)

  tool_names <- map_chr(chat$get_tools(), function(t) t@name)
  expect_true(all(grepl("^btw_tool_docs_", tool_names)))
  expect_false(any(grepl("^btw_tool_files_", tool_names)))
})

test_that("subagent_client() errors on disallowed tools", {
  withr::local_options(
    btw.subagent.tools_allowed = c("docs")
  )

  expect_error(
    subagent_client(tools = c("files")),
    "Subagent requested disallowed tools"
  )

  expect_error(
    subagent_client(tools = c("files")),
    "btw.subagent.tools_allowed"
  )
})

test_that("subagent_client() allows tools within whitelist", {
  withr::local_options(
    btw.subagent.tools_allowed = c("docs", "files")
  )

  # Should not error
  chat <- subagent_client(tools = c("docs"))

  tool_names <- map_chr(chat$get_tools(), function(t) t@name)
  expect_true(all(grepl("^btw_tool_docs_", tool_names)))
})

test_that("subagent_client() filters explicit tools against tools_allowed", {
  withr::local_options(
    btw.subagent.tools_allowed = c("docs", "search")
  )

  # Requesting tools partially in whitelist should error
  expect_error(
    subagent_client(tools = c("docs", "files")),
    "disallowed tools"
  )

  # Requesting only allowed tools should work
  chat <- subagent_client(tools = c("docs", "search"))

  tool_names <- map_chr(chat$get_tools(), function(t) t@name)
  expect_true(any(grepl("^btw_tool_docs_", tool_names)))
  expect_true(any(grepl("^btw_tool_search_", tool_names)))
})

test_that("subagent_client() works without tools_allowed set", {
  withr::local_options(
    btw.subagent.tools_allowed = NULL,
    btw.subagent.tools_default = c("files")
  )

  # Should work with any tools when tools_allowed is NULL
  chat <- subagent_client(tools = c("docs"))

  tool_names <- map_chr(chat$get_tools(), function(t) t@name)
  expect_true(all(grepl("^btw_tool_docs_", tool_names)))
})

test_that("subagent_client() precedence: explicit tools > tools_default", {
  withr::local_options(
    btw.subagent.tools_default = c("docs"),
    btw.subagent.tools_allowed = c("docs", "files")
  )

  # Explicit tools argument should override tools_default
  chat <- subagent_client(tools = c("files"))

  tool_names <- map_chr(chat$get_tools(), function(t) t@name)
  expect_true(all(grepl("^btw_tool_files_", tool_names)))
  expect_false(any(grepl("^btw_tool_docs_", tool_names)))
})

test_that("subagent_client() tools_allowed filters defaults", {
  withr::local_options(
    btw.subagent.tools_allowed = c("docs"),
    btw.subagent.tools_default = c("docs", "files", "search")
  )

  chat <- subagent_client(tools = NULL)

  tool_names <- map_chr(chat$get_tools(), function(t) t@name)
  expect_true(all(grepl("^btw_tool_docs_", tool_names)))
  expect_false(any(grepl("^btw_tool_files_", tool_names)))
  expect_false(any(grepl("^btw_tool_search_", tool_names)))
})

test_that("subagent_client() error message is helpful", {
  withr::local_options(
    btw.subagent.tools_allowed = c("docs")
  )

  expect_error(
    subagent_client(tools = c("files")),
    "btw_tool_files_"
  )

  expect_error(
    subagent_client(tools = c("env")),
    "btw_tool_env_describe_data_frame"
  )

  expect_error(
    subagent_client(tools = c("files")),
    "Set.*btw.subagent.tools_allowed = NULL"
  )
})

test_that("subagent_client() tools_allowed works with specific tool names", {
  withr::local_options(
    btw.subagent.tools_allowed = c(
      "btw_tool_docs_help_page",
      "btw_tool_files_read_text_file"
    )
  )

  # Should work with specific allowed tools
  chat <- subagent_client(tools = c("btw_tool_docs_help_page"))

  tool_names <- map_chr(chat$get_tools(), function(t) t@name)
  expect_true("btw_tool_docs_help_page" %in% tool_names)
  expect_equal(length(tool_names), 1)

  # Should error with disallowed specific tool
  expect_error(
    subagent_client(tools = c("search_packages")),
    "disallowed tools"
  )
})

# Tests for subagent tool filtering (prevents recursive subagents)

test_that("btw_tool_agent_subagent errors when explicitly requested", {
  # Explicitly requesting the subagent tool now throws an error
  expect_error(
    subagent_client(tools = c("btw_tool_agent_subagent", "docs")),
    "Subagents cannot spawn other subagents"
  )

  # Same for short name
  expect_error(
    subagent_client(tools = c("subagent", "docs")),
    "Subagents cannot spawn other subagents"
  )
})

test_that("btw_tool_agent_subagent is filtered out from default tools", {
  withr::local_options(
    btw.subagent.tools_default = NULL,
    btw.tools = NULL,
    btw.subagent.tools_allowed = NULL
  )

  # Use default tools (btw_tools())
  chat <- subagent_client(tools = NULL)

  tool_names <- map_chr(chat$get_tools(), function(t) t@name)

  # btw_tool_agent_subagent should not be in the tools
  expect_false("btw_tool_agent_subagent" %in% tool_names)

  # But other tools should be present
  expect_true(length(tool_names) > 0)
})

test_that("btw_tool_agent_subagent is silently filtered out from 'agent' tool group", {
  # Request the 'agent' tool group which includes btw_tool_agent_subagent
  # The subagent tool is silently filtered via can_register (no warning)
  chat <- subagent_client(tools = c("agent"))

  tool_names <- map_chr(chat$get_tools(), function(t) t@name)

  # btw_tool_agent_subagent should be filtered out
  expect_false("btw_tool_agent_subagent" %in% tool_names)
})

test_that("btw_tool_agent_subagent is silently filtered out even when in tools_allowed", {
  withr::local_options(
    btw.subagent.tools_allowed = c("agent", "docs")
  )

  # Request agent group (which includes subagent tool)
  # The subagent tool is silently filtered via can_register
  chat <- subagent_client(tools = c("agent", "docs"))

  tool_names <- map_chr(chat$get_tools(), function(t) t@name)

  # btw_tool_agent_subagent should still be filtered out
  expect_false("btw_tool_agent_subagent" %in% tool_names)

  # But other tools should be present
  expect_true(any(grepl("^btw_tool_docs_", tool_names)))
})

test_that("btw_tool_agent_subagent never appears in chat$get_tools() for subagent", {
  # Test multiple scenarios to ensure subagent tool never appears

  # Scenario 1: Explicit request → throws error
  expect_error(
    subagent_client(tools = c("btw_tool_agent_subagent")),
    "Subagents cannot spawn other subagents"
  )

  # Scenario 2: Via tool group → silently filtered
  chat2 <- subagent_client(tools = c("agent"))
  expect_false(
    "btw_tool_agent_subagent" %in% sapply(chat2$get_tools(), function(t) t@name)
  )

  # Scenario 3: Default tools → silently filtered
  withr::local_options(
    btw.subagent.tools_default = NULL,
    btw.tools = NULL
  )
  chat3 <- subagent_client(tools = NULL)
  expect_false(
    "btw_tool_agent_subagent" %in% sapply(chat3$get_tools(), function(t) t@name)
  )

  # Scenario 4: Mixed explicit with other tools → throws error
  expect_error(
    subagent_client(
      tools = c("btw_tool_agent_subagent", "docs", "files")
    ),
    "Subagents cannot spawn other subagents"
  )
})

test_that("subagent tool errors even when in tools_allowed", {
  withr::local_options(
    btw.subagent.tools_allowed = c("btw_tool_agent_subagent", "docs")
  )

  # Even if subagent tool is in allowed list, explicit request throws error
  expect_error(
    subagent_client(tools = c("btw_tool_agent_subagent", "docs")),
    "Subagents cannot spawn other subagents"
  )

  # But requesting via group doesn't error - silently filters
  chat <- subagent_client(tools = c("agent", "docs"))
  tool_names <- map_chr(chat$get_tools(), function(t) t@name)

  # Subagent tool should be filtered out
  expect_false("btw_tool_agent_subagent" %in% tool_names)

  # Docs tools should remain
  expect_true(any(grepl("^btw_tool_docs_", tool_names)))
})

# ---- Chat Client Configuration ----------------------------------------------

test_that("subagent_client creates chat with filtered tools", {
  chat <- subagent_client(tools = "files")

  expect_true(inherits(chat, "Chat"))

  tool_names <- map_chr(chat$get_tools(), function(t) t@name)
  expect_true(all(grepl("^btw_tool_files_", tool_names)))
  expect_false(any(grepl("^btw_tool_docs_", tool_names)))
})

test_that("subagent_client respects explicit client parameter", {
  custom_client <- ellmer::chat_anthropic(model = "claude-opus-4-20241120")

  chat <- subagent_client(client = custom_client)

  expect_identical(chat, custom_client)
})

test_that("subagent_client includes base subagent prompt", {
  chat <- subagent_client()

  system_prompt <- chat$get_system_prompt()

  expect_match(system_prompt, "Task Execution Guidelines")
  expect_match(system_prompt, "Work Efficiently")
  expect_true(nchar(system_prompt) > 0)
})

# ---- Session Management (via helpers) ---------------------------------------

test_that("subagent_get_or_create_session creates new session when ID is NULL", {
  subagent_clear_all_sessions()

  result <- subagent_get_or_create_session(
    session_id = NULL,
    create_chat_fn = function() mock_chat()
  )

  expect_type(result, "list")
  expect_false(is.null(result$session_id))
  expect_match(result$session_id, "^[a-z]+_[a-z]+$")
  expect_true(result$is_new)
  expect_true(inherits(result$chat, "Chat"))

  subagent_clear_all_sessions()
})

test_that("subagent_get_or_create_session retrieves existing session", {
  subagent_clear_all_sessions()

  # Create a session first
  session_id <- subagent_new_session_id()
  chat <- mock_chat()
  subagent_store_session(session_id, chat)

  # Retrieve it
  result <- subagent_get_or_create_session(
    session_id = session_id,
    create_chat_fn = function() stop("Should not be called")
  )

  expect_equal(result$session_id, session_id)
  expect_identical(result$chat, chat)
  expect_false(result$is_new)

  subagent_clear_all_sessions()
})

test_that("subagent_get_or_create_session errors helpfully for invalid session", {
  subagent_clear_all_sessions()

  expect_error(
    subagent_get_or_create_session(
      session_id = "nonexistent_badger_wombat",
      create_chat_fn = function() mock_chat()
    ),
    regexp = "Session not found.*nonexistent_badger_wombat"
  )

  expect_error(
    subagent_get_or_create_session(
      session_id = "nonexistent",
      create_chat_fn = function() mock_chat()
    ),
    regexp = "Omit.*session_id.*to start a new session"
  )
})

# ---- Tool Filtering and Restrictions ----------------------------------------

test_that("tools_allowed option filters configured tools", {
  withr::local_options(
    btw.subagent.tools_allowed = c("docs"),
    btw.subagent.tools_default = c("docs", "files")
  )

  chat <- subagent_client(tools = NULL)

  tool_names <- map_chr(chat$get_tools(), function(t) t@name)

  expect_true(all(grepl("^btw_tool_docs_", tool_names)))
  expect_false(any(grepl("^btw_tool_files_", tool_names)))
})

test_that("subagent recursion is prevented in default tools", {
  withr::local_options(
    btw.subagent.tools_default = NULL,
    btw.tools = NULL,
    btw.subagent.tools_allowed = NULL
  )

  chat <- subagent_client(tools = NULL)

  tool_names <- map_chr(chat$get_tools(), function(t) t@name)

  # Subagent tool should be filtered out
  expect_false("btw_tool_agent_subagent" %in% tool_names)
  # But other tools should be present
  expect_true(length(tool_names) > 0)
})
