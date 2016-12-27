
[This document is formatted with GitHub-Flavored Markdown.    ]:#
[For better viewing, including hyperlinks, read it online at  ]:#
[https://github.com/WarpEngineer/escript_boilerplate/blob/master/README.md]:#

* [Overview](#overview)
* [Features](#features)
* [Installation](#installation)
* [Author](#author)
* [License](#license)

## Overview

<!--more-->

This script is a good template to write Erlang (escript) scripts.  It's based on the original [Bash3
Boilerplate](https://github.com/kvz/bash3boilerplate). The most common things needed in a command-line script are
handled and all that's needed is to fill in the actual runtime code.

Just as with Bash3 Boilerplate, the script is Delete-Key-**Friendly**. The one and only script, `main.escript`(https://github.com/WarpEngineer/escript_boilerplate/blob/master/main.escript), can be used by removing the parts you don't need. To maintain **Portability**, the script does not use anything fancy so it should work with any recent version of Erlang.

## Features

- Configuration by environment variables
- Simple command-line argument parsing that requires no external dependencies. Definitions are optionally parsed from help info, ensuring there will be no duplication
- Helpful magic variables like `__file` and `__dir`
- Logging that supports colors and is compatible with [Syslog Severity levels](http://en.wikipedia.org/wiki/Syslog#Severity_levels)

## Installation

All you need is the main.escript file.  Use curl or wget to download the source and save it as your script. Then you can start deleting the unwanted bits, and adding your own logic.

```bash
wget https://github.com/WarpEngineer/escript_boilerplate/blob/master/main.escript
vim main.escript
```

## Author

- [@WarpEngineer](https://github.com/WarpEngineer)

## License

Copyright (c) 2016 A. G. Madi.
Licensed under [GPL](https://github.com/WarpEngineer/escript_boilerplate/blob/master/LICENSE).

