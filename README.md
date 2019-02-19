# jaro
**jaro** is just another resource opener<sup name="a1">[1](#fn1)</sup>. It uses a configuration file to determine which application should be used to open a file. Configuration file is actually a _Scheme_ file which you can use anything that Scheme offers. You don't need to know Scheme to use _jaro_, configuration is pretty intuitive.

# Usage
To open some file using _jaro_, just run:
```sh
jaro /path/to/some/file
```

# Installation
Only dependency is `guile`. Install it from your package manager. Then just put `jaro` script somewhere in your path. You can also replace `xdg-open` script with `jaro`, if you know what you are doing.

# Configuration
_jaro_ looks for the file `~/.config/associations` and loads it. This file contains multiple `(assoc ...)` definitions and arbitrary _Scheme_ conde. `jaro` will try to match the given URI with each association in order. I'll go trough some examples that shows you associating files/uris with programs.

- Here is a pretty basic rule for associating all image types with `sxiv`:
```scheme
(assoc
  #:pattern "image/.*"
  #:program "sxiv %f")
```

- Here is another example that opens all _Youtube_ links with `mpv`:
```scheme
(assoc
  #:pattern "^https?://(www.)?youtube.com/watch\\?.*v="
  #:program "mpv %f")
```

- This association opens given any URL that starts with `http` or `https` in _qutebrowser_ if it's already open. If _qutebrowser_ is not open, it'll simply open the URL in Firefox.
```scheme
(assoc
  #:pattern "^https?://.*"
  #:program "qutebrowser %f"
  #:test "pgrep qutebrowser"
  #:on-fail "firefox %f")
```

- This example associates all text files with `vim`. If you call `jaro` from command-line it'll simply open `vim` with given file, but if `jaro` is called from another GUI application it'll open `vim` in a new terminal. In this example `st` is the terminal emulator, so that if `jaro` is called from a GUI application it'll append the `#:term` parameter before the `#:program` parameter which will end up like `st -e vim %f`.
```scheme
(assoc
  #:pattern "^text/.*"
  #:program "vim %f"
  #:term "st -e")
```

- This association opens given directory in `ranger`, like the previous example it runs ranger within a terminal if it's needed. But also if `jaro` is called within a `tmux` session, it splits the window and opens `ranger` in another split.
```scheme
(assoc
  #:pattern "inode/directory"
  #:program "ranger %f"
  #:term "st -e"
  #:tmux "tmux split-window -h")
```

An association file may contain all of these associations and `jaro` will try to match one of them.

## Options
### #:pattern
A regular expression to match with mimetype or path. This is required. `jaro` will try to match the pattern with mimetype first, if it fails it'll try to match for the path. This can also be a list of regular expressions, if one of them matches the association will be used. Lists are only for convenience, you can always use grouping with `|` in a single regular expression.

Examples:
```scheme
#:pattern "^image/png$"                                     ;; png files
#:pattern "^https?://\\w+\\.com"                            ;; .com websites
#:pattern "\\.mp3"                                          ;; mp3 files
#:pattern '("^application/(x-)?(pdf)$" "^text/postscript$") ;; pdf and postscript files
```

### #:program
The program to open with. This is required. This can either be a string or a list of strings. If it's a string, it will be run in a shell, like `/bin/sh`. If it's a list of strings, the program will be run directly. Before running, some substitution happens:
- `%f` is replaced with given relative path, it will be the path that you called `jaro` with.
- `%F` is replaced with given path but this will be the absolute path.
- `%U` is replaced with given path but in URI form. This is useful if the application only accepts URIs (like `file:///home/user/file.txt`).

Examples:
```scheme
#:program "vim %f"                               ;; open in vim
#:program "echo %F | xclip -selection clipboard" ;; copy full path to clipboard
#:program '("mpv" "--autofit=1920x1080" "%f")
```

### #:term
The program to run `#:program` with, if `jaro` is called from outside of a terminal. This command will be prepended in front of `#:program` before calling. If `#:standalone` is set to `#t`, instead of appending before `#:program`, `#:term` will be called standalone. Like `#:program`, this should be either a string or list of strings.

Examples:
```scheme
;; This will open all text files in vim. When called from outside of terminal,
;; it will open vim in a new terminal.
(assoc
  #:pattern "text/"
  #:program "vim %f"
  #:term "st -e")

;; This will open all text files in vim if it is called in a terminal, but if
;; it is called from outside of a terminal, it will open files in gedit.
;; (Yeah, the name #:term looks confusing here.)
(assoc
  #:pattern "text/"
  #:program "vim %f"
  #:term "gedit %f"
  #:standalone #t)
```

### #:tmux
Command to run if `jaro` is called inside a `tmux` session. This command will be prepended in front of `#:program` before calling. If `#:standalone` is set to `#t`, instead of appending before `#:program`, `#:tmux` will be called directly. Like `#:program`, this should be either a string or list of strings.

Examples:
```scheme
;; Open directories in ranger. If jaro is called inside a tmux session,
;; split window and open ranger in that split.
(assoc
  #:pattern "inode/directory"
  #:program "ranger %f"
  #:tmux "tmux split-window")

;; Open http(s) links in firefox. If jaro is called inside a tmux session,
;; use w3m to open links in a new tmux split.
(assoc
  #:pattern "https?://"
  #:program "firefox %f"
  #:tmux "tmux split-window w3m %f"
  #:standalone #t)
```

### #:screen
Like `#:tmux` but for `screen`.

### #:standalone
If this is set to `#t`; `#:term`, `#:tmux`, `#:screen` will be run standalone, instead of appending before `#:program`. This is useful for using different programs in different working environments.

### #:test
A test command. If `#:pattern` is matched and `#:test` is present, before calling the `#:program`, `jaro` will run `#:test`. If command exits with `0`, `#:program` will be run. Otherwise `jaro` will skip this association like it hasn't been matched. Like `#:program`, this should be either a string or list of strings.

### #:on-fail
Command to run if the `#:test` fails. If `#:test` fails and `#:on-fail` is present, `jaro` will execute `#:on-fail` and stop trying next association.

Example:
```scheme
;; Test if qutebrowser is running. If it is, then open the url in qutebrowser,
;; if it is not, then open the url in firefox.
(assoc
  #:pattern "^https?://.*"
  #:program "qutebrowser %f"
  #:test "pgrep qutebrowser"
  #:on-fail "firefox %f")
```

### #:on-error
Command to run if `#:program` (or `#:on-fail`, `#:term`, `#:tmux`, `#:screen`) exits with something other than `0`.

Example:
```scheme
;; Open images with sxiv. If it fails to open image, opens the image with feh.
(assoc
  #:pattern '"^image/.*$"
  #:program "sxiv %f"
  #:on-error "feh %f")
```

### #:on-success
Command to run if `#:program` (or `#:on-fail`, `#:term`, `#:tmux`, `#:screen`) exits with `0`.

### Arbitrary options and different opening modes
You can define arbitrary options (like `#:edit`, `#:view` etc. Anything but the ones explained above.) and start `jaro` with a method to use that option. For example:
```scheme
(assoc
  #:pattern "^text/.*"
  #:program "vim %f"
  #:view "cat %f")
```

When `jaro` is started with `--method=view`, it will open given text file in `cat` instead of `#:program`. You can add arbitrary amount of opening modes. These modes, like `#:program` option, should be either a string or a list of strings (or a scheme procedure).

Another example:
```scheme
(assoc
  #:pattern "image/.*"
  #:program "sxiv %f"
  #:view "sxiv %f"
  #:edit "gimp %f")
```

Now you can use `jaro --method=view path/to/file` or `jaro --method=edit path/to/file` to open an image. You can define aliases in your shell for these different opening modes, like for bash:
```sh
alias edit="jaro --method=edit"
alias view="jaro --method=view"
# ...
```

### More advanced usage
`#:program` or derivatives can also be a lambda. You need to return `#t` or `#f` to make things work properly.

## A selection menu for non-matched
Add this association to end of your associations file. If nothing has been matched, this association will run and present you a dialog to select which application to use. It will display programs that supports opening mimetype of given file and all the binaries in your system.

```scheme
;; If jaro is called inside a terminal, it will use fzf for selecting the
;; alternative, otherwise it will use dmenu. `select-alternative-with` function
;; simply pipes alternatives to given program, so you can use any other
;; dmenu-like program.
(assoc
  #:pattern ".*"
  #:program (select-alternative-with "fzf")
  #:term (select-alternative-with "dmenu")
  #:standalone #t)

;; This one uses dmenu all the time
(assoc
  #:pattern ".*"
  #:program (select-alternative-with "dmenu")
  #:standalone #t)
```

---

<a name="fn1">1</a>: I'm bad at naming stuff [↩](#a1)
