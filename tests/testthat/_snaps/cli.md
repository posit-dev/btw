# btw --help shows top-level help

    Code
      run_btw("--help")
    Output
      Usage: btw [OPTIONS] <COMMAND>
      
      Describe R objects, documentation, and workspace state in LLM-friendly text.
      Wraps btw package tools for docs, pkg, info, and cran operations.
      
      Commands:
        docs  
        pkg   
        info  
        cran  
      
      Options:
        --version / --no-version  Print btw version and exit. [default: false]
                                  Enable with `--version`.
      
      For help with a specific command, run: `btw <command> --help`.

# btw docs --help shows docs group help

    Code
      run_btw("docs", "--help")
    Output
      Usage: btw docs [OPTIONS] <COMMAND>
      
      docs command
      
      Commands:
        help      
        vignette  
        news      
      
      Global options:
        --version / --no-version  Print btw version and exit. [default: false]
                                  Enable with `--version`.
      
      For help with a specific command, run: `btw docs <command> --help`.

# btw docs help --help shows help subcommand usage

    Code
      run_btw("docs", "help", "--help")
    Output
      Usage: btw docs help [OPTIONS] <TOPIC>
      
      help command
      
      Options:
        -p, --package <PACKAGE>  Package name to scope the help topic.
                                 [default: "NA"] [type: string]
      
      Global options:
        --version / --no-version  Print btw version and exit. [default: false]
                                  Enable with `--version`.
      
      Arguments:
        <TOPIC>  Help topic or package name.

# btw pkg --help shows pkg group help

    Code
      run_btw("pkg", "--help")
    Output
      Usage: btw pkg [OPTIONS] <COMMAND>
      
      pkg command
      
      Commands:
        document  
        check     
        test      
        load      
        coverage  
      
      Options:
        --path <PATH>  Path to package directory. [default: "."] [type: string]
      
      Global options:
        --version / --no-version  Print btw version and exit. [default: false]
                                  Enable with `--version`.
      
      For help with a specific command, run: `btw pkg <command> --help`.

# btw info --help shows info group help

    Code
      run_btw("info", "--help")
    Output
      Usage: btw info [OPTIONS] <COMMAND>
      
      info command
      
      Commands:
        platform  
        packages  
      
      Global options:
        --version / --no-version  Print btw version and exit. [default: false]
                                  Enable with `--version`.
      
      For help with a specific command, run: `btw info <command> --help`.

# btw cran --help shows cran group help

    Code
      run_btw("cran", "--help")
    Output
      Usage: btw cran [OPTIONS] <COMMAND>
      
      cran command
      
      Commands:
        search  
        info    
      
      Global options:
        --version / --no-version  Print btw version and exit. [default: false]
                                  Enable with `--version`.
      
      For help with a specific command, run: `btw cran <command> --help`.

