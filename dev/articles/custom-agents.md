# Custom Agents

As a conversation with an LLM gets longer, everything in it shapes the
next response: the files it has read, its earlier answers, the direction
the work has taken. That helps when you’re building on earlier work. It
gets in the way when you want an independent check of something, or when
one job needs different tools or a different model than the rest of the
conversation. You could open a new chat for each of those jobs, but then
you’d rewrite the instructions every time and copy material between
chats by hand.

**Custom agents** let your chat run those jobs for you. A custom agent
is a Markdown file that describes one job: its instructions, the tools
it can use, and, optionally, the model it runs on. Your chat can hand
off that job whenever it’s needed, and the result comes back into the
conversation. Because the files live in your project, you write each
job’s instructions once and reuse them.

Behind the scenes, btw turns each agent file into a tool for your main
[`btw_app()`](https://posit-dev.github.io/btw/dev/reference/btw_client.md)
or
[`btw_client()`](https://posit-dev.github.io/btw/dev/reference/btw_client.md)
chat. When the main chat client calls the tool, it writes a prompt
describing the task. btw then starts a *subagent*, which is a separate
chat session configured by the agent file. The subagent sees only its
own instructions, its tools, and that prompt. Its final response comes
back to the main chat as the tool’s result.

In this vignette, we’ll write three custom agents, make them available
to a btw chat, and ask that chat to delegate work to them. Agent files
use the same format as `btw.md`; if you haven’t written a `btw.md` file
before, start with
[`vignette("btw-md")`](https://posit-dev.github.io/btw/dev/articles/btw-md.md).
To follow along, you’ll need btw, an
[ellmer](https://ellmer.tidyverse.org/) chat provider you can use, and
[shinychat](https://posit-dev.github.io/shinychat/) if you want to run
the chat app.

## An example: reviewing a report

Our example borrows from the way people review each other’s work.
Suppose you’ve written a research report and want a second opinion
before sharing it. You might ask three colleagues to read it: one to
check where the data came from, another to reproduce the numerical
claims, and a third to check whether the conclusions survive both
reviews. We’ll build a custom agent for each of those three jobs.

The analogy only goes so far. The agents don’t work together the way
colleagues would. Each review is a separate call from the main chat to
an agent’s tool, and each call runs in its own session, so the sessions
never see each other. Anything one agent needs from another’s review has
to be written into its prompt by the main chat. With custom agents, the
main chat makes the calls and collects the results, and the agent files
stay in your project for the next report.

For this review, we’ll create a `btw.md` file and three files in the
project’s `.btw/agents/` directory:

```
btw.md
.btw/agents/
  researcher.md
  statistician.md
  fact_checker.md
```

Once those files exist, you can start up
[`btw_app()`](https://posit-dev.github.io/btw/dev/reference/btw_client.md)
and ask the main chat to delegate the review to all three agents and
summarize what they found.

## Give the main chat access to agents

Starting from the `btw.md` file in
[`vignette("btw-md")`](https://posit-dev.github.io/btw/dev/articles/btw-md.md),
we make one change: add the `agent` group to `tools`. This file is saved
at the project root (or your working directory).

```
---
client: posit
tools: [files, docs, sessioninfo, run, agent]
---

You are a research data science assistant.
Inspect the project files before answering.
```

When the `agent` tool group is included, btw adds one tool for each
agent file btw finds. If you list `tools` in `btw.md` but leave out
`agent`, the main chat won’t receive those tools, even when the agent
files exist. The other groups in `tools` here let the main chat inspect
the report, consult documentation, and run R. They don’t determine what
an agent can use; each agent has its own tool list.

From the project directory, launch the app with
[`btw_app()`](https://posit-dev.github.io/btw/dev/reference/btw_client.md)
or, for programmatic use, call
[`btw_client()`](https://posit-dev.github.io/btw/dev/reference/btw_client.md).

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`btw`](https://github.com/posit-dev/btw)`)`\
[`btw_app`](https://posit-dev.github.io/btw/dev/reference/btw_client.md)`(``)`

## Create an agent file

An agent file starts out looking like any other `btw.md` file. Here’s
one for the statistician. Its frontmatter chooses tools for reading
files, looking up documentation, and running R, and its body describes
the job:

```
---
tools: [files_read, docs, run]
---

You are a consulting statistician. Read the report named in the task.
Use R to check its sample size, model, and reported estimates.
Separate what the analysis establishes from what the report concludes.
Do not edit files.

Return a memo with the data used, checks run, discrepancies, and
recommended corrections.
```

If you saved this as the project’s `btw.md`, every chat in the project
would get the statistician’s instructions and tools. To turn these
instructions into a subagent, we need two more fields: `name` identifies
the agent, and `description` tells the main chat when to use it. We’ll
also add an optional `title` for display.

```
---
name: statistician
title: Consulting statistician
description: Reproduce and critique the numerical claims in a research report.
tools: [files_read, docs, run]
---

You are a consulting statistician. Read the report named in the task.
Use R to check its sample size, model, and reported estimates.
Separate what the analysis establishes from what the report concludes.
Do not edit files.

Return a memo with the data used, checks run, discrepancies, and
recommended corrections.
```

Save this file as `.btw/agents/statistician.md`. The location is what
makes it an agent: btw looks for Markdown files in `.btw/agents/` and
offers each one to the main chat as a tool. The filename can be anything
ending in `.md`; the `name` field identifies the agent.

Each field in the frontmatter controls one part of the agent:

- `name: statistician` produces a tool named
  `btw_tool_agent_statistician`. Names can contain letters, numbers, and
  underscores.
- `description` is written for the main chat, which reads it to decide
  when to delegate a task to this agent.
- `title` is the name shown to you in the app.
- `tools` works the same way as in `btw.md`, but it limits what the
  *agent* can use. The statistician gets `run` so it can reproduce a
  count or a model fit.

The body becomes the agent’s instructions, added after btw’s general
instructions for subagents. `files_read` is the only file tool in the
list, so the statistician can’t write files through a btw file tool. R
code can write files, though, so with `run` in the list, the “Do not
edit files” instruction is a request, not a sandbox.

You don’t have to register agent files by hand:
[`btw_tools()`](https://posit-dev.github.io/btw/dev/reference/btw_tools.md)
finds the files in `.btw/agents/` whenever it assembles the chat’s tools
and the `"agent"` tool group is included. To check that btw found the
statistician, run this from your project directory:

\
[`names`](https://rdrr.io/r/base/names.html)`(`[`btw_tools`](https://posit-dev.github.io/btw/dev/reference/btw_tools.md)`(``"agent"``)``)`

The result should include `btw_tool_agent_statistician`, the tool the
main chat will call. It also includes `btw_tool_agent_subagent`, btw’s
built-in tool for one-off delegation, and any agents you’ve defined at
the user level. If the statistician is missing, check that the file is
in `.btw/agents/` and that its frontmatter has a `name` field. If you
already have an ellmer chat and want to register a particular file
yourself, `btw_agent_tool(".btw/agents/statistician.md")` creates its
tool definition directly.

## Add the other two agents

The researcher and the fact checker use the same format. Their jobs only
require reading, so their `tools` lists leave out `run`. Create
`.btw/agents/researcher.md`:

```
---
name: researcher
title: Researcher
description: Review a study's question, data provenance, design, and causal claims.
tools: [files_read, files_search, docs]
---

You are reviewing a research report. Read the report named in the task.
Check the research question, where the data came from, the study design,
and whether the conclusions claim more than that design supports.

Do not edit files. Return a short memo with the report's strengths,
concerns, and claims the author should revise. Cite the passages you checked.
```

Then create `.btw/agents/fact_checker.md`, with the same tools and a
different job:

```
---
name: fact_checker
title: Fact checker
description: Check the final report against the source and earlier review memos.
tools: [files_read, files_search, docs]
---

Read the report named in the task and compare its claims with the
researcher and statistician memos included in your task. Flag unsupported
or contradictory claims, citing the report and the relevant memo.
Do not edit files. Return a concise list of corrections.
```

The fact checker’s instructions refer to memos “included in your task”,
and that wording matters. The fact checker’s session has no access to
the researcher’s or statistician’s sessions. Their memos reached the
main chat as tool results. For the fact checker to compare the report
with them, the main chat has to copy the memos into the prompt when it
calls `btw_tool_agent_fact_checker`.

## Give an agent its own model

An agent file can have a `client` field, written the same way as in
`btw.md`. Use it when a job suits a different model than the main
chat’s. For example, the researcher’s job is narrow, so you could give
it a smaller, faster model by adding a `client` line to its frontmatter
and leaving the body as it was:

```
---
name: researcher
title: Researcher
description: Review a study's question, data provenance, design, and causal claims.
client: posit/zai-org/GLM-5.3-Flash
tools: [files_read, files_search, docs]
---
```

Without a `client` field, an agent uses the `btw.subagent.client` option
if you’ve set it, for example in the `options` of your `btw.md` file.
Otherwise, it uses the client from the `btw.client` option or from your
`btw.md` file.

Agent files can also live at the user level, for example in
`~/.btw/agents/`, when you want to reuse an agent across projects. When
a project agent and a user-level agent have the same name, btw uses the
project agent.

## Run the review

With the three files in place, give the main chat a report path and the
order of the calls. This is an example prompt to type into
[`btw_app()`](https://posit-dev.github.io/btw/dev/reference/btw_client.md);
replace `penguin-study.qmd` with a report in your project.

> Review `penguin-study.qmd` using the custom agents. Call `researcher`
> to assess the question, provenance, design, and causal language. Call
> `statistician` to reproduce the sample-size count and model and check
> the numerical claims. Then call `fact_checker`, including the complete
> researcher and statistician memos in its task. Summarize the
> corrections I should make to the report, distinguishing verified
> results from unresolved questions.

If the model follows these instructions, the main chat makes three tool
calls, one after another. It calls `btw_tool_agent_researcher` with a
prompt naming the report, and the researcher’s memo comes back as the
result. It calls `btw_tool_agent_statistician` the same way. Then it
calls `btw_tool_agent_fact_checker` with a prompt that includes both
memos. With all three results in its context, the main chat writes the
summary you asked for.

Each call starts a new session unless the main chat passes the
`session_id` from an earlier call to continue that session. A tool
result is text, not an edit to the report. The main chat can use its own
tools to check the memos or act on them.

The prompt we used here was overly explicit; in practice I’d include
coordinating instructions for when and how to call the subagents in the
project’s `btw.md` file. In general, you usually don’t need to be as
explicit, the main agent will call the appropriate subagent according to
the instructions in the agent’s `description`.

## Work with an agent directly

Because an agent file uses the `btw.md` format, it can also configure a
chat on its own. To chat with the statistician without asking the main
chat to delegate, pass its file as `path_btw`:

\
[`btw_app`](https://posit-dev.github.io/btw/dev/reference/btw_client.md)`(`\
`  client ``=`` ``ellmer``::`[`chat_posit`](https://ellmer.tidyverse.org/reference/chat_posit.html)`(``)``,`\
`  path_btw ``=`` ``".btw/agents/statistician.md"`\
`)`

btw reads the file the way it reads `btw.md`: the file’s `tools` become
the chat’s tools, and its body is added to the chat’s system prompt.
This is a regular
[`btw_app()`](https://posit-dev.github.io/btw/dev/reference/btw_client.md)
conversation, not a subagent session that reports back to another chat.
The file takes the place of the project’s `btw.md`, so the project’s
`client` setting doesn’t apply; that’s why this call passes `client`,
though a `client` field in the agent file would work too. Your
user-level `btw.md`, if you have one, still applies as usual. The
`name`, `title`, and `description` fields only matter when btw offers
the file as a tool, so the chat ignores them.

## Recap

We started from a `btw.md` file that configures the chat you talk to. By
adding a `name` and a `description` and saving the file in
`.btw/agents/`, you can turn the same format into an agent that the main
chat can call. When the main chat calls an agent’s tool, btw runs a new
session with that agent’s tools, instructions, and optionally its own
model, and returns the session’s final response as the tool result.
Apart from the agent’s own instructions, the main chat’s prompt is the
only input the session gets, and the tool result is the only output the
main chat sees.

You can also delegate a one-off task without making an agent file:
`btw_tool_agent_subagent` lets the main chat supply a prompt and tool
list for that call. Use a custom agent file when the job, instructions,
and tool limits should be repeatable or shared with the project.
