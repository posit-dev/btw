#' Agent: A Tool for Researching R Tasks
#'
#' An agent that researches R packages and provides example code for specific R
#' tasks. This agent is designed to help users find the best R packages for
#' specific tasks, list key functions and arguments, and return runnable
#' example code.
#'
#' @param client An [ellmer::Chat] client, defaults to [btw_client()].
#' @inheritParams btw_tool_agent
#'
#' @returns Returns an aync [ellmer::tool()] that can be used to invoke the
#'   R task research agent.
#'
#' @family agents
#' @export
btw_agent_researcher_r_task <- function(
  client = btw_client(tools = FALSE),
  tools = c("docs", "search", "session")
) {
  btw_tool_agent(
    client = client,
    tools = tools,
    turns = FALSE, # Don't use initial turns, this agent is stateless
    name = "btw_agent_researcher_r_task",
    title = "R Task Research Agent",
    description = r"(Research an R programming task.

Rapidly researches a single, narrowly-scoped R-programming task. Finds the best CRAN (or locally installed) package(s), lists key functions/arguments, and returns runnable example code.

INPUT
A unique, non-overlapping question such as “Draw a clustered heat-map”, “Fit a beta-regression”, or “Read a parquet file”.

PARALLEL TASKS
Prefer batching distinct research tasks only when they are clearly orthogonal.

GUIDELINES
– Break the user’s goal into the small, independent R tasks.
– Avoid issuing more than one call for the same or highly similar question.
– Limit parallel calls to reduce duplication and API load.
– Do not use this tool for broad or overlapping questions.
- Only use this tool when you do not already know the answer.
    )",
    system_prompt = r"---(
MISSION
Treat the input as an explicit research question about R packages.
Search CRAN docs, vignettes, help pages, and other provided tools to answer that question.
Return the answer in a compact format that explains your findings and provides example code that can be run in R.

RESPONSE FORMAT
Your response should include (one block per topic or package)

* Task
* Package(s): pkg1, pkg2, …
* Why these packages? 1-sentence rationale.
* Key functions & arguments related to the task as a bullet list.
* Example code in a fenced R snippet that can be run as-is.
* Citations: minimal list of URLs or help files consulted.

GUIDELINES
* Be factual; no speculation.
* Prefer base CRAN packages unless the question implies otherwise.
* Keep prose succinct; let code illustrate usage.
* If multiple packages solve the task, list the best 2-3 and show code for the top choice.
* If no suitable package exists, say so and suggest an alternative strategy.

Return only the summarized research.)---",
  )
}
