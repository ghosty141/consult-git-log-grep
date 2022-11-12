# consult-git-log-grep.el

consult-git-log-grep provides an interactive way to search the git log using [consult](https://github.com/minad/consult).
## Installation and Configuration

This package is available on MELPA and can be installed using ```M-x package-install``` or if use-package is installed:
```elisp
(use-package consult-git-log-grep)
```

By default selecting a commit displays the output of "git show <SHA>" in a new buffer. This may not be desired if magit is installed.
To use magit to show the commit, set `consult-git-log-grep-open-function` to `#'magit-show-commit`

```elisp
(use-package consult-git-log-grep
  :custom 
  (consult-git-log-grep-open-function #'magit-show-commit))
```

## Usage

`M-x consult-git-log-grep`

