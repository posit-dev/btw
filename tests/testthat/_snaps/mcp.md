# btw_mcp_server errors informatively with bad `tools`

    Code
      btw_mcp_server(tools = "bop")
    Condition
      Error in `btw::btw_tools()`:
      ! `tools` must be one of "env", "docs", "files", "ide", "search", "session", "web", "env_describe_data_frame", "docs_package_news", "docs_package_help_topics", "docs_help_page", "docs_available_vignettes", "docs_vignette", "env_describe_environment", "files_list_files", "files_read_text_file", "files_write_text_file", "ide_read_current_editor", "search_packages", "search_package_info", "session_check_package_installed", "session_platform_info", "session_package_info", or "web_read_url", not "bop".

