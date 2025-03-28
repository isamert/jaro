# jaro

_jaro_ is a *j*ust *a*nother *r*esource *o*pener. It runs the appropriate application to open a given file or URL based on given configurations.

## Usage

Run `jaro` followed by the file path or URI you wish to open. If configured, jaro will select the appropriate application automatically:

```shell
jaro ~/document.pdf
jaro https://example.com
```

jaro can also read from stdin.

```shell
echo "/path/to/file" | jaro
```

You can disable stdin feature with passing `--no-stdin` parameter.

General usage:

```bash
jaro [OPTIONS] <URI>
```

### Command Line Options

- `-t`, `--mime-type`: Print the MIME type of the URI.
- `-c`, `--cold-run`: Simulate the actions without executing them.
- `-f`, `--binding-file=FILE`: Use a specific binding configuration file (default: `~/.config/associations`).
- `-m`, `--method=METHOD`: Use a specific method to run the application.
- `-N`, `--no-stdin`: Do not read URI from standard input. This is helpful if you want to use jaro in a pipeline but you don't care about the results of the earlier steps.
- `-h`, `--help`: Display help information.

## Installation

To use _jaro_, ensure you have [Guile](https://www.gnu.org/software/guile/) (>= `1.8`) installed on your system. Place the `jaro` script in a directory that is in your system's `PATH`.

``` sh
# Fedora
sudo dnf install guile

# Debian/Ubuntu etc.
sudo apt-get install guile-3.0

# Arch
sudo pacman -S guile
```

```sh
curl -L -o /usr/local/bin/jaro https://raw.githubusercontent.com/isamert/jaro/refs/heads/master/jaro
chmod +x /usr/local/bin/jaro
```

For enhanced mimetype detection, install `Perl MimeInfo`, otherwise _jaro_ will fallback to standard `file` utility for mimetype detection, which is far more inferior. To install it:

```shell
# Fedora
sudo dnf install perl-File-MimeInfo

# Debian/Ubuntu etc.
sudo apt-get install libfile-mimeinfo-perl

# Arch
sudo pacman -S perl-file-mimeinfo
```

## Replacing `xdg-open`

I simply recommend replacing `xdg-open` with `jaro` so that all file/URL opening requests are redirected to `jaro` instead of `xdg-open`. Easiest way to do this would be shadowing the real `xdg-open` binary.

Create a symbolic link named `xdg-open` pointing to `jaro` in a directory that precedes the `xdg-open` binary's directory in the PATH. Assume `jaro` is located at `/usr/local/bin/jaro` and the actual `xdg-open` is at `/usr/bin/xdg-open`.

```shell
ln -s /usr/local/bin/jaro /usr/local/bin/xdg-open
```

Assuming `/usr/local/bin/` precedes `/usr/bin` in the PATH variable, you will successfully shadow `xdg-open` with `jaro`. This method is preferable to simply removing `xdg-open` and replacing it with `jaro`, as it prevents disruptions to system packages.

Check your PATH and ensure `xdg-open` points to `jaro`:

```shell
# Ensure that the xdg-open that you created is inside a folder that
# precedes the real xdg-open's directory:
echo $PATH

# Should output the link you created, instead of the real xdg-open:
which xdg-open

# Ensure that xdg-open link you've created points to jaro:
stat $(which xdg-open)
```

## Configuration

jaro, by default, looks for the file `~/.config/associations` and loads it. This file contains multiple `(bind ...)` definitions and (optionally) some arbitrary Guile Scheme code. jaro will try to match the given URI with each binding in order. I'll go trough some examples that shows you binding files/URIs with programs.

Only hard dependency is Guile. For enhanced mimetype detection, install `mimetype`, otherwise _jaro_ will fallback to standard `file` utility for mimetype detection, which is far more inferior.

# Configuration

_jaro_ looks for the file `~/.config/associations` and loads it. This file contains multiple `(assoc ...)` definitions and arbitrary _Scheme_ code. `jaro` will try to match the given URI with each association in order. I'll go trough some examples that shows you associating files/uris with programs.

```scheme
(bind
 #:pattern "image/.*"
 #:program '(sxiv %f))
```

Here is another example that opens all YouTube links with `mpv`:

```scheme
(bind
 #:pattern "^https?://(www.)?youtube.com/watch\\?.*v="
 #:program '(mpv %f))
```

Let's go back to first example, and make a small addition:

```scheme
(bind
 #:pattern "image/.*"
 #:program '(sxiv %f)
 #:gallery '(nomacs %f))
```

When you run `jaro an-image.png`, this does exactly the same thing as the first binding. When you run `jaro --method=gallery an-image.png` however, instead of opening the image with `sxiv`, jaro uses `nomacs` now.

Some of the keywords (things that start with `#:`) have a reserved meaning in jaro. In addition to them, you can define arbitrary *methods* like the `#:gallery` example from above. The following keywords are reserved:

- `#:name`: Assigns a unique identifier to a binding, which can be referenced by other bindings or methods.
- `#:program`: Specifies the command or application to be executed when the pattern matches. Can be a string, list, or Scheme procedure.
- `#:pattern`: Defines the regular expression(s) or list of expressions that determine which files or URIs the binding applies to. The match is done against the file/URI or the mimetype.
- `#:test`: An optional command or procedure to run before executing the main program. If this test passes, the main program is run; otherwise, it triggers the `#:on-fail` method.
- `#:on-fail`: Specifies an alternative command or procedure to execute if the `#:test` fails.
- `#:on-success`: Defines a command or procedure to run if the `#:program` executes successfully.
- `#:on-error`: Specifies a command or procedure to execute if the `#:program` fails. This can also be set to 'continue' to try alternative bindings.

These are discussed in detail in [Configuration Reference](#configuration-reference).

## A more advanced configuration example

Here is a commented configuration that illustrates advanced features of `jaro`:

```scheme
;; -*- mode: scheme; -*-

;;; Configuration

;; Optional, for dynamically selecting programs/methods on runtime:
(set!
 dynamic-menu-program
 (oscase
  #:darwin "choose"
  #:gnu/linux "rofi -dmenu"))

;; An example conditional runner definition for detecting the Kitty
;; terminal
(define-conditional-runner (kitty _)
  (getenv "KITTY_PID"))

;;; Bindings

;; Open pdf/ps/epub etc. with zathura, based on mimetype
(bind
 #:pattern '("(application|text)/(x-)?(pdf|postscript|ps|epub.*)" "image/(x-)?eps")
 #:program '(zathura %f))

;; Open torrent files and magnet links using qBittorrent
(bind
 #:pattern '("^magnet:" "\\.torrent$")
 #:program '(qbittorrent --skip-dialog=false %f))

;; Open images using `imv` program
(bind
 #:pattern "^image/.*"
 ;; imv does not directly load images of the directory, so we start
 ;; imv in the directory or our image and set the first image to the
 ;; image that we want to open
 #:program '(imv -n %f %d)
 ;; If we are inside the Kitty terminal, simply use it's ability to
 ;; show images instead of using an external program
 #:kitty '(kitty +kitten icat %f)
 ;; If the jaro is started with --method=gallery option, then defer
 ;; opening this file to nomacs definition down below
 #:gallery 'nomacs)

;; Open a Zoom link. Extract the meeting number and password from the
;; link using regexp capture groups and feed it into the zoom app
;; using t %1 and %2
(bind
 #:pattern "https://.*zoom\\.us/j/(\\w+)\\?pwd=(\\w+)"
 #:program '(zoom zoommtg://zoom.us/join?confno=%1&pwd=%2))

(bind
 #:pattern "https://.*zoom\\.us/j/(\\w+)\\?pwd=(\\w+)"
 #:program '(zoom zoommtg://zoom.us/join?confno=%1&pwd=%2))

;; If a compressed file is opened with jaro, then display a menu using
;; rofi (on Linux) or choose (on MacOS) to as user what to do with
;; this file. See beginning of this example file for menu program
;; configuration.
(bind
 ;; Give this binding a name, which we will utilize later
 #:name 'archive
 #:pattern "^application/(x-)?(tar|gzip|bzip2|lzma|xz|compress|7z|rar|gtar|zip)(-compressed)?"
 ;; Instead of doing something directly, let user select one of the
 ;; methods (#:unpack, #:unpack-to-directory, #:view) of this binding.
 #:program (select-one-of #:methods)
 ;; Unpack the archive using atool
 #:unpack '(atool --extract %f)
 ;; Let user select a directory with `zenity` to extract the archive
 ;; into, using atool again
 #:unpack-to-directory "atool --extract-to=$(zenity --file-selection --title='Choose a directory' --directory) %f"
 ;; Open the archive using `file-roller`.
 #:view '(file-roller %f))

(bind
 ;; Given a jar or apk file...
 #:pattern ".(jar|apk)$"
 ;; ...show a menu of: run, archive.unpack, archive.unpack-to-directory
 #:program (select-one-of #:methods 'archive.view 'archive.unpack 'archive.unpack-to-directory)
 ;; ^^ #:methods refers the methods of this binding. There is only one: "run"
 ;; ^^ 'archive.<method> refers to the methods of 'archive binding.

 ;; Here, instead of directly running an external command we use the
 ;; "program" syntax. It simply let's us run arbitrary Guile scheme
 ;; code. Inside "program", the variables %1 %2 %3... etc are bound to
 ;; the capture groups from the #:pattern and the "run" let's you run
 ;; external programs using the syntax that you are familiar from the
 ;; earlier bindings.
 #:run (program
         (match %1
           ["jar" (run (java -jar %f))]
           ["apk" (run (notify-send "Can't run APK files. Install an Android Emulator?"))])))

;; A named binding, referenced above
(bind
 #:name 'nomacs
 #:pattern "^image/.*"
 #:program '(nomacs %f))
```

## Configuration reference

### #:pattern

Defines regular expressions to match against URIs or MIME types. Can be:
- Single regex string (`"image/.*"`)
- Compiled regex object
- List of patterns (`'("\.txt$" "text/.*")`)

Patterns are checked against both the input URI and its detected MIME type. Capture groups can be referenced in commands using `%1`, `%2`, etc.

Example:

```scheme
(bind
 #:pattern "^https://example.com/(\\w+)/"
 #:program '(open-section %1))  ; Capture path component
```

### #:program and other methods

The primary command to execute when the pattern matches. Can be:
- String (`"sxiv %f"`)
- List of arguments (`'(sxiv %f)`)
- Scheme procedure
- Reference to another binding (`'nomacs`)

Additional methods can be defined as arbitrary keywords (e.g., `#:gallery`) for alternative opening modes. These are invoked with `--method=METHOD`.

Example:

```scheme
(bind
 #:pattern "\\.md$"
 #:program '(glow %f)          ; Default method
 #:preview '(mdcat %f))        ; Custom preview mode
```

### #:name

Assigns a unique identifier to a binding for cross-referencing. Named bindings can be invoked using:

```scheme
(bind #:pattern ... #:program 'named-binding)
```

or reference specific methods:

```scheme
(bind #:pattern ... #:program 'named-binding.method)
```

Example:

```scheme
(bind
 #:name 'pdf-viewer
 #:pattern "\\.pdf$"
 #:program '(zathura %f)
 #:edit '(xournalpp %f))

(bind
 #:pattern "\\.ps$"
 #:program 'pdf-viewer
 #:edit 'pdf-viewer.edit)
```

### #:test

Optional precondition check that must succeed before running the main program. If the test fails, triggers `#:on-fail`:

Example:

```scheme
(bind
 #:name 'browser
 #:pattern '("^https?://.*" "^.*\\.html?(#[\\w_-]+)?")
 #:test '(pgrep qutebrowser)   ; Check if qutebrowser is running or not
 #:program '(qutebrowser %f)   ; If it's running, open the url with it
 #:on-fail '(firefox %f))      ; If not, fallback to Firefox
```

### #:on-error

Specifies fallback behavior when the main program fails. Special values:
- `'continue`: Try subsequent bindings
- Procedure or command list: Execute custom error handling

Example:

```scheme
(bind
 #:pattern "\\.mkv$"
 #:program '(mpv --hwdec %f)
 #:on-error '(vlc %f))         ; Fallback player
```

### #:on-success

Runs after successful execution of the main program. Useful for cleanup or notifications:

Example:

```scheme
(bind
 #:pattern "\\.enc$"
 #:program "decrypt-file %f"
 #:on-success "rm %f.enc")     ; Cleanup after success
```

### #:continue-on-error

Boolean flag (default: `#f`) that when true, continues to subsequent bindings after any error in the current binding.

Example:

```scheme
(bind
 #:pattern "\\.jpg$"
 #:program "non-existing-program %f"  ; This binding will fail because the program does not exist.
 #:continue-on-error #t)

;; The next matching pattern will be used.

(bind
 #:pattern "image/*"
 #:program "imv %f")
```

### Arbitrary options and different opening modes

Define custom methods for context-specific opening:

```scheme
(bind
 #:pattern "image/.*"
 #:program '(imv %f)
 #:edit '(gimp %f))
```

and run this:

```sh
jaro --method=edit photo.jpg
```

Also consider aliasing your common use-cases:

``` sh
alias open="jaro"
alias edit="jaro --method=edit"
alias view="jaro --method=view"
alias gallery="jaro --method=gallery"
```

Now you can do the following instead:

```sh
edit photo.jpg
view photo.jpg
open photo.jpg
```

### Environment detection, conditional runners

Automatically select methods based on runtime environment using `define-conditional-runner`:

Built-in conditionals: `emacs`, `tmux`, `term`, `vim`. Conditional runners are already defined for these environments but you can override them as well.

```scheme
(define-conditional-runner (kitty _)
  (getenv "KITTY_PID"))

;; Open images using `imv` program
(bind
 #:pattern "^image/.*"
 #:program '(imv %f)
 ;; If we are inside the Kitty terminal, simply use it's ability to
 ;; show images instead of using an external program.
 ;; #:kitty keyword is introduced by the define-conditional-runner
 ;; call above
 #:kitty '(kitty +kitten icat %f))
```

### `select-one-of`

Interactive selection menu for multiple options. Supported selectors:

- `#:methods`: Current binding's own methods
- `#:alternatives`: MIME type associations from system
- `#:binaries`: Installed system commands/binaries
- `#:bindings`: All named bindings
- `'<binding-name>.<method-name>`: Direct reference to methods of other bindings.

Example offering extraction options for archives:

```scheme
;; For select-one-of to work, you need to set the dynamic-menu-program
;; Here we use choose[1] for macOS, rofi[2] for GNU/Linux.
;;
;; [1]: https://github.com/chipsenkbeil/choose
;; [2]: https://github.com/davatorium/rofi
(set!
 dynamic-menu-program
 (oscase
  #:darwin "choose"
  #:gnu/linux "rofi -dmenu"))

(bind
 #:pattern "\\.tar\\..*"
 ;; Following shows a menu consisting of following items:
 ;; - view
 ;; - extract
 ;; - ...all other programs that can open a tar files in your system
 #:program (select-one-of #:methods #:alternatives)
 ;; Runs when "view" is selected
 #:view '(tar tvf %f)
 ;; Runs when "extract" is selected
 #:extract '(tar xvf %f))
```

### Other user level functions

- `(open-with 'binding-name)`: Reference other bindings
- `(program ...)`: Scheme procedure wrapper with access to:
  - `$input`: Original URI
  - `$mimetype`: Detected type
  - `$matches`: Regex capture groups
  - `%1`-`%5`: Individual capture groups
  - `(run (...))`: Program runner. It takes a parameter like what you supply to `#:program` and runs it.

```scheme
;; open-with
(bind
 #:pattern "\\.txt$"
 #:program (open-with 'editor))     ; Delegate to 'editor binding

(bind
 #:name 'editor
 #:pattern "^text/"
 #:term '(vim %f))

;; program
(bind
 #:pattern "https://github.com/([^/]+)/([^/]+)/?"
 #:program (program
            (if (string-suffix? ".git" $input)
                (run (git clone %f))      ; Clone the repo if the URL ends with .git
                (run (xdg-open %f)))))    ; Open it otherwise
```

### Running elisp

Execute Emacs Lisp code directly through `emacsclient`, using the `elisp` form:

```scheme
(bind
 #:pattern "\\.org$"
 #:program (elisp (find-file "%F")))
```

Supports all URI formatting placeholders (`%f`, `%F`, `%U`).

## Contributing

Contributions are welcome! Please make sure to include tests with any major changes.

While developing, I tend use the `run-tests.sh` script to continuously execute my tests upon file modifications; they run fairly quickly. You need the `entr` utility for this script to function. Alternatively you can simply use `guile tests.scm`.
