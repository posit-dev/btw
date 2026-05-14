# btw --help shows top-level help

    Code
      run_btw("--help")
    Output
      Usage: btw [OPTIONS] <COMMAND>
      
      Describe R objects, documentation, and workspace state in LLM-friendly text.
      Wraps btw package tools for docs, pkg, info, and cran operations.
      
      Commands:
        docs                Access R documentation
        pkg                 Work with an R package under development
        info                [Deprecated] Inspect the R session and environment
        system-info         Show platform and R session info
        check-installed     Check if packages are installed
        installed-packages  Show installed package information
        cran                Query CRAN package metadata
        skills              Manage btw skills
        help                Show btw CLI usage guide for AI agents
        app                 Run btw_app() in the current directory
      
      Options:
        --version / --no-version  Print btw version and exit. [default: false]
                                  Enable with `--version`.
      
      For help with a specific command, run: `btw <command> --help`.

# btw docs --help shows docs group help

    Code
      run_btw("docs", "--help")
    Output
      Access R documentation
      
      Usage: btw docs [OPTIONS] <COMMAND>
      
      Commands:
        topics    List help topics and vignettes for a package
        help      Show help for a topic or package
        vignette  Read a package vignette
        news      Show package NEWS
      
      Global options:
        --version / --no-version  Print btw version and exit. [default: false]
                                  Enable with `--version`.
      
      For help with a specific command, run: `btw docs <command> --help`.

# btw docs help --help shows help subcommand usage

    Code
      run_btw("docs", "help", "--help")
    Output
      Show help for a topic or package
      
      Usage: btw docs help [OPTIONS] <TOPIC>
      
      Options:
        -p, --package <PACKAGE>  Package name to scope the help topic.
                                 [default: ""] [type: string]
      
      Global options:
        --version / --no-version  Print btw version and exit. [default: false]
                                  Enable with `--version`.
      
      Arguments:
        <TOPIC>  Help topic, or pkg::topic to scope to a specific package.

# btw pkg --help shows pkg group help

    Code
      run_btw("pkg", "--help")
    Output
      Work with an R package under development
      
      Usage: btw pkg [OPTIONS] <COMMAND>
      
      Commands:
        document  Generate package documentation
        check     Run R CMD check
        test      Run package tests
        load      Load package with pkgload
        coverage  Measure package test coverage
      
      Options:
        --path <PATH>  Path to package directory. [default: "."] [type: string]
      
      Global options:
        --version / --no-version  Print btw version and exit. [default: false]
                                  Enable with `--version`.
      
      For help with a specific command, run: `btw pkg <command> --help`.

# btw cran --help shows cran group help

    Code
      run_btw("cran", "--help")
    Output
      Query CRAN package metadata
      
      Usage: btw cran [OPTIONS] <COMMAND>
      
      Commands:
        search  Search CRAN for packages
        info    Show CRAN metadata for a package
      
      Options:
        --json / --no-json  Output as JSON. [default: false] Enable with `--json`.
      
      Global options:
        --version / --no-version  Print btw version and exit. [default: false]
                                  Enable with `--version`.
      
      For help with a specific command, run: `btw cran <command> --help`.

