# Tests for subagent tool and session management

# Helper to create a mock Chat object for testing
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

# ---- Session ID Generation ----

test_that("generate_session_id() creates valid IDs", {
  id1 <- generate_session_id()
  id2 <- generate_session_id()

  # Check format: word_word (underscore separator)
  expect_match(id1, "^[a-z]+_[a-z]+$")
  expect_match(id2, "^[a-z]+_[a-z]+$")

  # IDs should be different (probabilistically)
  # This could fail occasionally but very unlikely
  ids <- replicate(10, generate_session_id())
  expect_true(length(unique(ids)) > 5)
})

test_that("generate_session_id() checks for uniqueness", {
  # Clear any existing sessions
  clear_all_subagent_sessions()

  # Generate first ID
  id1 <- generate_session_id()

  # Store a session with this ID
  store_session(id1, mock_chat())

  # Generate more IDs - they should all be different from id1
  ids <- replicate(20, generate_session_id())
  expect_false(id1 %in% ids)

  # Clean up
  clear_all_subagent_sessions()
})

# ---- Session Storage and Retrieval ----

test_that("store_session() and retrieve_session() work", {
  clear_all_subagent_sessions()

  session_id <- "test_session"
  chat <- mock_chat()

  # Store session
  result <- store_session(session_id, chat)
  expect_equal(result, session_id)

  # Retrieve session
  session <- retrieve_session(session_id)
  expect_type(session, "list")
  expect_equal(session$id, session_id)
  expect_equal(session$chat, chat)
  expect_s3_class(session$created, "POSIXct")

  # Initial session should NOT have last_used field
  expect_null(session$last_used)

  # Clean up
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

# ---- Session Listing ----

test_that("list_subagent_sessions() works with no sessions", {
  clear_all_subagent_sessions()

  result <- list_subagent_sessions()
  expect_type(result, "list")
  expect_equal(length(result), 0)
})

test_that("list_subagent_sessions() lists all sessions", {
  clear_all_subagent_sessions()

  # Create multiple sessions
  store_session("session_1", mock_chat())
  store_session("session_2", mock_chat())
  store_session("session_3", mock_chat())

  result <- list_subagent_sessions()
  expect_type(result, "list")
  expect_equal(length(result), 3)
  
  # Check that session IDs are present
  session_ids <- names(result)
  expect_true("session_1" %in% session_ids)
  expect_true("session_2" %in% session_ids)
  expect_true("session_3" %in% session_ids)
  
  # Each session should be a list with expected fields
  expect_equal(result$session_1$id, "session_1")
  expect_equal(result$session_2$id, "session_2")
  expect_equal(result$session_3$id, "session_3")

  clear_all_subagent_sessions()
})

# ---- Session Clearing ----

test_that("clear_subagent_session() removes a session", {
  clear_all_subagent_sessions()

  session_id <- "test_clear"
  store_session(session_id, mock_chat())

  # Verify it exists
  expect_false(is.null(retrieve_session(session_id)))

  # Clear it
  result <- clear_subagent_session(session_id)
  expect_true(result)

  # Verify it's gone
  expect_null(retrieve_session(session_id))
})

test_that("clear_subagent_session() returns FALSE for nonexistent session", {
  clear_all_subagent_sessions()
  result <- clear_subagent_session("nonexistent")
  expect_false(result)
})

test_that("clear_all_subagent_sessions() clears all sessions", {
  clear_all_subagent_sessions()

  # Create multiple sessions
  store_session("session_1", mock_chat())
  store_session("session_2", mock_chat())
  store_session("session_3", mock_chat())

  # Verify they exist
  expect_equal(length(list_subagent_sessions()), 3)

  # Clear all
  count <- clear_all_subagent_sessions()
  expect_equal(count, 3)

  # Verify they're all gone
  expect_equal(length(list_subagent_sessions()), 0)
})

test_that("clear_all_subagent_sessions() returns 0 when no sessions", {
  clear_all_subagent_sessions()
  count <- clear_all_subagent_sessions()
  expect_equal(count, 0)
})

# ---- Client Configuration ----

test_that("btw_subagent_client_config() uses default tools", {
  withr::local_options(
    btw.subagent.tools = NULL,
    btw.tools = NULL
  )

  chat <- btw_subagent_client_config()

  expect_true(inherits(chat, "Chat"))
  # Chat should have tools configured
  expect_true(length(chat$get_tools()) > 0)
})

test_that("btw_subagent_client_config() respects tool filtering", {
  # Test with character vector
  chat <- btw_subagent_client_config(tools = c("docs"))

  expect_true(inherits(chat, "Chat"))
  # Should have some docs tools
  expect_true(length(chat$get_tools()) > 0)
})

test_that("btw_subagent_client_config() follows client precedence", {
  skip_if_not_installed("ellmer")

  # Test option precedence
  withr::local_options(
    btw.subagent.client = "anthropic/claude-sonnet-4-20250514",
    btw.client = "anthropic/claude-opus-4-20241120"
  )

  chat <- btw_subagent_client_config()
  expect_true(inherits(chat, "Chat"))

  # Test argument precedence
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

  # Should be different objects (cloned)
  expect_false(identical(chat1, chat2))
  expect_false(identical(chat1, chat_obj))
})

# ---- Tool Description ----

test_that("build_subagent_description() includes tool groups", {
  desc <- build_subagent_description()

  expect_type(desc, "character")
  expect_match(desc, "Delegate a task")
  expect_match(desc, "Available tool groups")

  # Should mention at least one tool group (e.g., docs, env, etc.)
  expect_true(
    any(grepl("docs|env|search|github", desc))
  )
})

test_that("build_subagent_description() includes basic text", {
  desc <- build_subagent_description()

  expect_type(desc, "character")
  expect_match(desc, "Delegate a task")
  expect_match(desc, "subagent")
})

# ---- Tool Registration ----

test_that("btw_tool_subagent is registered in btw_tools", {
  all_tools <- btw_tools()

  tool_names <- sapply(all_tools, function(t) t@name)
  expect_true("btw_tool_subagent" %in% tool_names)

  # Get the specific tool
  subagent_tool <- all_tools[[which(tool_names == "btw_tool_subagent")]]

  # Check properties
  expect_equal(subagent_tool@name, "btw_tool_subagent")
  expect_type(subagent_tool@description, "character")
  expect_match(subagent_tool@description, "Delegate a task")

  # Check it has arguments
  expect_true(length(subagent_tool@arguments) > 0)
})

# ---- BtwSubagentResult Class ----

test_that("BtwSubagentResult inherits from BtwToolResult", {
  result <- BtwSubagentResult(
    value = "test response",
    session_id = "test_id",
    extra = list()
  )

  # Check S7 class hierarchy
  expect_true(S7::S7_inherits(result, BtwSubagentResult))
  expect_true(S7::S7_inherits(result, BtwToolResult))
  expect_equal(result@value, "test response")
  expect_equal(result@session_id, "test_id")
})
