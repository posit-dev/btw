# btw_tool_env_query_data_frame() works

    Code
      btw_tool_env_query_data_frame("SELECT mpg FROM mtcars LIMIT 5;", "mtcars")
    Output
      [1] "```json"                                                                                       
      [2] "[\n  {\"mpg\":21},\n  {\"mpg\":21},\n  {\"mpg\":22.8},\n  {\"mpg\":21.4},\n  {\"mpg\":18.7}\n]"
      [3] "```"                                                                                           

---

    Code
      btw_tool_env_query_data_frame("SELECT mpg FROM mtcars LIMIT 5;", "mtcars")
    Output
      [1] "```json"                                                                                       
      [2] "[\n  {\"mpg\":21},\n  {\"mpg\":21},\n  {\"mpg\":22.8},\n  {\"mpg\":21.4},\n  {\"mpg\":18.7}\n]"
      [3] "```"                                                                                           

