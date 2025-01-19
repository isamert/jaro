# jaro

_jaro_ is a *j*ust *a*nother *r*esource *o*pener. It runs the appropriate application to open a given file or URL based on customizable configurations. These configurations are written in Guile Scheme, offering powerful extensions for advanced users while maintaining simplicity and ease of use for everyone. No prior knowledge of Scheme is required to configure _jaro_ — the process is intuitive and straightforward.

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

Please see [Configuration](#configuration) down below.

General usage:

```bash
jaro [OPTIONS] <URI>
```

### Options

- `-t`, `--mime-type`: Print the MIME type of the URI.
- `-c`, `--cold-run`: Simulate the actions without executing them.
- `-f`, `--binding-file FILE`: Use a specific binding configuration file (default: `~/.config/associations`).
- `-m`, `--method METHOD`: Use a specific method to run the application.
- `-N`, `--no-stdin`: Do not read URI from standard input. This is helpful if you want to use jaro in a pipeline but you don't care about the results of the earlier steps.
- `-h`, `--help`: Display help information.

## Installation

To use _jaro_, ensure you have Guile (>= `1.8`) installed on your system. Place the `jaro` script in a directory that is in your system's PATH.

For enhanced mimetype detection, install `Perl MimeInfo`, otherwise jaro will fallback to standard `file` utility for mimetype detection, which is far more inferior. To install it:

``` shell
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

jaro, by default, looks for the file `~/.config/associations` and loads it. This file contains multiple `(bind ...)` definitions and arbitrary Guile Scheme code. jaro will try to match the given URI with each binding in order. I'll go trough some examples that shows you binding files/URIs with programs.

Here is a pretty basic rule for associating all image types with `sxiv`:

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

Let's go back to first example, and make a small addition.

```scheme
(bind
 #:pattern "image/.*"
 #:program '(sxiv %f)
 #:gallery '(nomacs %f))
```

When you run `jaro an-image.png`, this does exactly the same thing as the first binding. When you run `jaro --method=gallery an-image.png` however, instead of opening the image with `sxiv`, jaro uses `nomacs` now.

Some of the keywords (things that start with `#:`) have a reserved meaning in jaro. In addition to them, you can define arbitrary modes/methods like the `#:gallery` example from above. The following keywords are reserved:

- `#:name`: Assigns a unique identifier to a binding, which can be referenced by other bindings or methods.
- `#:program`: Specifies the command or application to be executed when the pattern matches. Can be a string, list, or Scheme procedure.
- `#:pattern`: Defines the regular expression(s) or list of expressions that determine which files or URIs the binding applies to. The match is done against the file/URI or the mimetype.
- `#:test`: An optional command or procedure to run before executing the main program. If this test passes, the main program is run; otherwise, it triggers the `#:on-fail` method.
- `#:on-fail`: Specifies an alternative command or procedure to execute if the `#:test` fails.
- `#:on-success`: Defines a command or procedure to run if the `#:program` executes successfully.
- `#:on-error`: Specifies a command or procedure to execute if the `#:program` fails. This can also be set to 'continue' to try alternative bindings.

This association opens given any URL that starts with http or https in `qutebrowser` if it's already open. If `qutebrowser` is not open, it'll simply open the URL in `firefox`.

``` scheme
(bind
 #:pattern "^https?://.*"
 #:program "qutebrowser %f"
 #:test "pgrep qutebrowser"
 #:on-fail "firefox %f")
```

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

;; If a compressed file is opened with jaro, then display a menu using
;; rofi (on Linux) or choose (on MacOS) to as user what to do with
;; this file. See beginning of this example file for menu program
;; configuration.
(bind
 #:pattern "^application/(x-)?(tar|gzip|bzip2|lzma|xz|compress|7z|rar|gtar|zip)(-compressed)?"
 ;; Instead of doing something directly, let user select one of the
 ;; methods (#:unpack, #:unpack-to-directory, #:view) of this binding.
 #:program (select-one-of #:methods)
 ;; Unpack the archive using atool
 #:unpack '(atool --extract %f)
 ;; Let user select a directory with `zenity` to extract the archive
 ;; into, using atool again
 #:unpack-to-directory "atool --extract-to=$(zenity  --file-selection --title='Choose a directory' --directory) %f"
 ;; Open the archive using `file-roller`.
 #:view '(file-roller %f))

;; Open a Zoom link. Extract the meeting number and password from the
;; link using regexp capture groups and feed it into the zoom app
;; using t %1 and %2
(bind
 #:pattern "https://.*zoom\\.us/j/(\\w+)\\?pwd=(\\w+)"
 #:program '(zoom zoommtg://zoom.us/join?confno=%1&pwd=%2))

;; A named binding, referenced above
(bind
 #:name 'nomacs
 #:pattern "^image/.*"
 #:program '(nomacs %f))
```

## Reference
### #:pattern
### #:program and other methods

See [Arbitrary options and different opening modes](#arbitrary-options-and-different-opening-modes)

### #:name

``` scheme
'name
'name.method
```

### #:test
### #:on-error
### #:on-success
### #:continue-on-error
### Arbitrary options and different opening modes

```sh
alias edit="jaro --method=edit"
alias view="jaro --method=view"
alias gallery="jaro --method=gallery"
# ...
```


### Environment detection, conditional runners
### `select-one-of`
### Other user level functions
### Running elisp
## Contributing

Contributions are welcome! Please make sure to include tests with any major changes.

While developing, I tend use the `run-tests.sh` script to continuously execute my tests upon file modifications; they run fairly quickly. You need the `entr` utility for this script to function. Alternatively you can simply use `guile tests.scm`.
