#' Where btw looks for configuration
#'
#' @description
#' \pkg{btw} reads configuration -- context files (`btw.md`, `AGENTS.md`,
#' `CLAUDE.md`), skills, and btw-style agent definitions -- from a combination
#' of project-level and user-level (global) locations. This page describes
#' where btw looks for each type of configuration and in what order of
#' priority.
#'
#' @section User (global) locations:
#'
#' User-level (global) configuration is discovered from the following
#' directories, in decreasing order of priority:
#'
#' 1. `~/.btw/` (recommended)
#' 2. `~/.config/btw/`
#' 3. The platform-specific R user config directory,
#'    `tools::R_user_dir("btw")`
#'
#' Skills are discovered in the `skills/` subdirectory of each of these
#' directories (e.g. `~/.btw/skills/`), and btw-style agents are discovered as
#' `agent-*.md` files directly inside each directory (e.g.
#' `~/.btw/agent-my_agent.md`).
#'
#' For the loose `btw.md` configuration file only, btw additionally checks for
#' a `btw.md` file directly in your home directory (`~/btw.md`) as a
#' convenience, though `~/.btw/btw.md` is the preferred, canonical location.
#'
#' On Windows, R's notion of your home directory (`fs::path_home_r()`,
#' typically your `Documents` folder) can differ from your user profile
#' directory (`fs::path_home()`); btw searches both.
#'
#' @section Project locations:
#'
#' Project-level configuration is discovered by walking up from the current
#' working directory to the project root, where the project root is
#' identified by the presence of a `DESCRIPTION`, `.git`, `.vscode`, `.here`,
#' or `*.Rproj` file.
#'
#' * **Config file**: the nearest `btw.md` file, or if none is found, the
#'   nearest `AGENTS.md` file, or if neither is found, the nearest `CLAUDE.md`
#'   file. (`btw.md` > `AGENTS.md` > `CLAUDE.md`)
#' * **btw-style agents**: `.btw/agent-*.md`
#' * **Skills**: `.btw/skills/` or `.agents/skills/`
#'
#' @section Claude Code agents:
#'
#' As an intentional special case -- distinct from, and not derived from, the
#' user (global) locations described above -- btw also discovers
#' [Claude Code sub-agent](https://docs.claude.com/en/docs/claude-code/sub-agents)
#' definitions from:
#'
#' * `.claude/agents/` in the project
#' * `~/.claude/agents/` for the user
#'
#' This deliberately mirrors Claude Code's own convention for locating
#' sub-agents, rather than following btw's usual user-level directory
#' priority.
#'
#' @name btw-config
#' @rdname btw-config
NULL
