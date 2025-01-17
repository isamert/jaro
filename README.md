# jaro

Rewrite is still in progress. It changed quite a bit, the README is no longer relevant. I'll extensively document options when the rewrite is over but for now I'll leave you with the configuration I use:

```scheme
;; -*- mode: scheme; -*-

;;; Configuration

(set!
 dynamic-menu-program
 (oscase
  #:darwin "choose"
  #:gnu/linux "rofi -dmenu"))

(define-conditional-runner (kitty _)
  (getenv "KITTY_PID"))

;;; Bindings

(bind
 #:pattern '("(application|text)/(x-)?(pdf|postscript|ps|epub.*)" "image/(x-)?eps")
 #:program '(zathura %f))

(bind
 #:pattern '("^text/html" "^application/x?htm")
 #:program 'browser
 #:edit 'editor)

(bind
 #:name 'editor
 #:pattern '("^text/" "^application/(x-)?(shellscript|json|javascript|xml)")
 #:emacs (elisp
          (find-file "%F"))
 ;; If I simply give the file to emacsclient, it opens it in a split
 ;; for some reason, instead of making it the only window in the frame
 #:program '(emacsclient -c --eval "(find-file \"%F\")")
 #:term '(emacsclient -nw -c --eval "(find-file \"%F\")"))

(bind
 #:name 'empv
 #:pattern '("^video/" "^audio/")
 #:program (elisp (empv-play "%F"))
 #:on-error '(mpv %f))

(bind
 #:pattern "inode/directory"
 #:program '(thunar %f)
 #:term '(yazi %f)
 #:gallery 'nomacs)

(bind
 #:pattern "https://.*zoom\\.us/j/(\\w+)\\?pwd=(\\w+)"
 #:program '(zoom zoommtg://zoom.us/join?confno=%1&pwd=%2))

(bind
 #:pattern '("^https?://(www.)?youtube.com/"
             "^https?://(www.)?youtu.be/"
             "^https?://(www.)?v.redd.it/\\w+/DASH"
             "^https?://([a-zA-Z-]+)?streamable.com"
             "^https?://giant.gfycat.com/.+"
             "https?://v.redd.it/.+"
             "^https?://.+/.+\\.(gifv|mp4|webm)(\\?.+)?$")
 #:program 'empv
 #:on-error 'browser)

(bind
 #:pattern "^https?://.+/.+\\.(jpg|png|gif)(\\?.+)?$"
 #:program `(imv %f))

(bind
 #:pattern "^image/.*"
 ;; Start with given image, and open the directory of given file
 #:program '(imv -n %f %d)
 #:kitty '(kitty +kitten icat %f)
 #:gallery 'nomacs)

(bind
 #:pattern "^https?://(www.)?reddit.com/r/(\\w+)/comments/(.*?)/"
 #:program (elisp (reddigg-view-comments "https://www.reddit.com/r/%2/comments/%3"))
 #:on-error 'browser)

(bind
 #:pattern '("^magnet:" "\\.torrent$")
 #:program '(qbittorrent --skip-dialog=false %f))

(bind
 #:name 'browser
 #:pattern '("^https?://.*" "^.*\\.html?(#[\\w_-]+)?")
 #:emacs (elisp (eww "%f"))
 #:program (elisp (eww "%f"))
 ;; #:program '(qutebrowser %f)
 ;; #:test '(pgrep qutebrowser)
 #:on-fail '(firefox %f)
 #:edit 'editor)

(bind
 #:pattern "^application/(x-)?(tar|gzip|bzip2|lzma|xz|compress|7z|rar|gtar|zip)(-compressed)?"
 #:program (select-one-of #:methods)
 #:unpack '(atool --extract %f)
 #:unpack-to-directory "atool --extract-to=$(zenity  --file-selection --title='Choose a directory' --directory) %f"
 #:view '(file-roller %f))

(bind
 #:pattern "^application/(x-)?(vnd.)?(ms-|ms)?(excel|powerpoint|word)"
 #:program '(desktopeditors %F))

;;; Catch-all

(bind
 #:pattern ".*"
 #:program (select-one-of
            #:alternatives
            #:bindings
            #:binaries))

;;; References

(bind
 #:name 'bat
 #:pattern ".*"
 #:program '(bat --paging=always %f))


(bind
 #:name 'nomacs
 #:pattern "^image/.*"
 #:program '(nomacs %f))

;; vi:syntax=scheme
```
