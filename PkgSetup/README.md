## Setup

### Git Submodule

To ensure `ODCdqaFunctions` contains the same functions as `ODCdqa`, git submodules is used. A git post-commit hook is used to copy and replace `functions.R`.

To setup the post-commit hook:

1. Navigate to `.git/hook` inside the repo
2. Create a file called `post-commit` (no extension) containing the following

```
#!/bin/bash

cd ~/ODCdqaFunctions/
cp -rf ./ODCdqa/src/functions.R ./R/functions.R
```

3. Convert to executable using `chmod +x FILE`

#### Usage

The post-commit hook runs after a commit is made. A second commit needs to be made to track the changes to `functions.R` file.

Commit 1: Update to latest version of main `ODCdqa` repository.
Commit 2: Copy and commit changes.