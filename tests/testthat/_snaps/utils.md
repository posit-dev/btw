# check_installed() works

    Code
      check_installed("somepackage")
    Condition
      Error:
      ! Package somepackage is not installed.
      i Did you mean "doudpackage", "datapackage", "findPackage", "FSTpackage", or "somspace"?

# path_find_user() prefers ~/btw.md and warns on multiple configs

    Code
      result <- path_find_user("btw.md")
    Condition
      Warning:
      ! Found more than one user-level 'btw.md' config file.
      i Using '~/btw.md'.
      i Ignoring lower-priority: '~/.btw/btw.md'.
      i Consider consolidating your btw configuration into '~/.btw/btw.md'.
      This warning is displayed once per session.

