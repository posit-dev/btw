# btw_tool_files_patch_impl: success output for mixed add/update/delete

    Code
      cat(result@value)
    Output
      Applied patch with 3 operations.
        - Added: added.txt
        - Updated: update_me.txt
        - Deleted: delete_me.txt

# btw_tool_files_patch_impl: failure output when hunk context not found

    Code
      cat(conditionMessage(result@error))
    Output
      Hunk context not found in 'target.txt'.
      i First context/delete line: "nonexistent line"

