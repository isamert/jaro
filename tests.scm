(eval-when (expand load eval)
  (load "jaro"))

(use-modules
 (srfi srfi-64))

;;; Preperation

(define-syntax with-cold-run
  (syntax-rules ()
    ((with-cold-run test-name body ...)
     (begin
       (format #t ">>>>> Running ~a <<<<<\n" test-name)
       (set! jaro-cold-run? #t)
       (set! jaro-bindings '())
       (set! jaro-named-bindings (make-hash-table))
       ;; FIXME(pDWQKhh): Following also removes the defaults
       ;; (set! jaro-conditional-runners (make-hash-table))
       (set! jaro-runner-method #f)
       (set! jaro-env #f)
       (set! dynamic-menu-program #f)
       body ...))))

(define-syntax with-warm-run
  (syntax-rules ()
    ((with-warm-run test-name body ...)
     (begin
       (format #t ">>>>> Running ~a <<<<<\n" test-name)
       (set! jaro-cold-run? #f)
       (set! jaro-bindings '())
       (set! jaro-named-bindings (make-hash-table))
       ;; FIXME(pDWQKhh): Following also removes the defaults
       ;; (set! jaro-conditional-runners (make-hash-table))
       (set! jaro-runner-method #f)
       (set! jaro-env #f)
       (set! dynamic-menu-program #f)
       body ...))))

;;; All tests

(test-begin "all-tests")

(with-cold-run
 "basic binding"
 (bind
  #:pattern ".json$"
  #:program "json-viewer %f")

 (test-equal (jaro-run "~/a.json") "json-viewer ~/a.json"))

(with-cold-run
 "binding with list of patterns."
 (bind
  #:pattern '(".js$" ".json$" ".yaml$")
  #:program "json-viewer %f")

 (test-equal (jaro-run "~/a.json") "json-viewer ~/a.json"))

(with-warm-run
 "#:program fails"
 (bind
  #:pattern "test-on-error$"
  #:program "false")

 (test-equal (jaro-run "test-on-error") 'jaro-non-zero-without-on-error))

(with-cold-run
 "#:program runs properly with string list"
 (bind
  #:pattern "^.*/.*"
  #:program '("mpv" "%f"))

 (test-equal (jaro-run "~/a.mp4") '("mpv" "~/a.mp4")))

(with-cold-run
 "#:program runs properly with symbol list"
 (bind
  #:pattern "^.*/.*"
  #:program '(mpv  %f))

 (test-equal (jaro-run "~/a.mp4") '("mpv" "~/a.mp4")))

(with-warm-run
 "run #:on-error in case of #:program fails"
 (bind
  #:pattern "test-on-error-2$"
  #:program "false"
  #:on-error (program 'on-error))

 (test-equal (jaro-run "test-on-error-2") 'on-error))

(with-warm-run
 "#:program fails and then #:on-error fails"
 (bind
  #:pattern "test-on-error-3$"
  #:program "false"
  #:on-error "false")

 (test-equal (jaro-run "test-on-error-3") #f))

(with-warm-run
 "#:program fails and #:on-error is 'continue"
 (bind
  #:pattern "test-on-error-4$"
  #:program "false"
  #:on-error 'continue)

 (bind
  #:pattern "test-on-error-4$"
  #:program "true")

 (test-equal (jaro-run "test-on-error-4") #t))

(with-warm-run
 "#:program fails and #:on-error is 'continue but there are no matches"
 (bind
  #:pattern "test-on-error-5$"
  #:program "false"
  #:on-error 'continue)

 (bind
  #:pattern "test-on-error-5-no-match-on-continue$"
  #:program "true")

 (test-equal (jaro-run "test-on-error-5") 'jaro-no-matches))

(with-cold-run
 "matched one redirects to a named binding"
 (bind
  #:pattern "matched-pattern$"
  #:program 'named-binding
  #:on-error 'continue)

 (bind
  #:name 'named-binding
  #:pattern "unmatched-pattern"
  #:program (program 'happy))

 (test-equal (jaro-run "matched-pattern") 'happy))

(with-warm-run
 "#:program fails and #:on-error calls a named binding"
 (bind
  #:pattern "test-on-error-6$"
  #:program "false"
  #:on-error 'on-error-handler-for-6)

 (bind
  #:name 'on-error-handler-for-6
  #:program "true")

 (test-equal (jaro-run "test-on-error-6") #t))

(with-cold-run
 "using capture group in a pattern in #:program"
 (bind
  #:pattern "capture-(\\w+)-test$"
  #:program "%0 %1 %2")

 (test-equal (jaro-run "capture-group-test") "capture-group-test group %2"))

(with-cold-run
 "replaces %F with full path in #:program"
 (bind
  #:pattern "full/path-test$"
  #:program "echo %F")

 (test-equal (jaro-run "full/path-test") (string-append "echo " (getcwd) "/full/path-test")))

(with-cold-run
 "replaces %F with full path #:program when given path is already a full path"
 (bind
  #:pattern "path-test$"
  #:program "echo %F")

 (test-equal (jaro-run "/full/path-test") "echo /full/path-test"))

(with-cold-run
 "replaces %U with URI of given path"
 (bind
  #:pattern "uri-test$"
  #:program "echo %U")

 (test-equal (jaro-run "/uri-test") "echo file:///uri-test")
 (test-equal (jaro-run "https://uri-test") "echo https://uri-test"))

(with-cold-run
 "is able to access matching groups in a procedural #:program"
 (bind
  #:pattern "fn-test.(\\w+)$"
  #:program (lambda* (input mimetype matches #:rest _)
              (format #f "~a - ~a - ~a" input mimetype matches)))

 (test-equal (jaro-run "fn-test.mp4") "fn-test.mp4 - #f - ((%0 . fn-test.mp4) (%1 . mp4))"))

(with-warm-run
 "conditionally fails based on given path"
 (bind
  #:pattern '("fn-(\\w+)-test$")
  #:program (lambda* (_1 _2 matches #:rest _)
              (if (string= (cdr (list-ref matches 1)) "sad")
                  #f 'happy))
  #:on-error (program 'sad))
 (test-equal (jaro-run "fn-success-test") 'happy)
 (test-equal (jaro-run "fn-sad-test") 'sad))

(with-warm-run
 "handles #:continue-on-error and #:on-fail properly"
 (bind
  #:program "dummy"
  #:pattern "test-test-rule$"
  #:test "false"
  #:continue-on-error #t)

 (bind
  #:pattern "test-test-rule$"
  #:program (program 'alternative))

 (bind
  #:program "dummy"
  #:pattern "test-on-fail-rule$"
  #:test "false"
  #:on-fail (program 'on-fail))

 (bind
  #:pattern "test-success-rule$"
  #:test "true"
  #:program (program 'test-success))

 (test-equal (jaro-run "test-test-rule") 'alternative)
 (test-equal (jaro-run "test-on-fail-rule") 'on-fail)
 (test-equal (jaro-run "test-success-rule") 'test-success))

(with-warm-run
 "skip #:test if something other than #:program is matched"
 (setenv "INSIDE_EMACS" "1")
 (bind
  #:program (program 'sad)
  #:emacs (program 'happy)
  #:pattern "skip-test-test$"
  #:test "false")

 (test-equal (jaro-run "skip-test-test") 'happy))

(with-warm-run
 "uses other alternative when #:continue-on-error is t"
 (bind
  #:pattern "test-on-error-continue$"
  #:program "false"
  #:on-error "false"
  #:continue-on-error #t)

 (bind
  #:pattern "test-on-error-continue$"
  #:program (program 'happy)
  #:on-error "false")


 (test-equal (jaro-run "test-on-error-continue") 'happy))

(with-warm-run
 "handles opening modes properly"
 (set! jaro-runner-method #:view)

 (bind
  #:pattern "test-opening-mode$"
  #:program "false"
  #:on-error (program 'failed)
  #:view (program 'happy)
  #:continue-on-error #t)

 (test-equal (jaro-run "test-opening-mode") 'happy))

(with-warm-run
 "uses default environment program when mode is not found"
 (set! jaro-runner-method #:view)

 (bind
  #:pattern "test-non-existent-opening-mode$"
  #:program (program 'happy)
  #:random-mode (program 'sad)
  #:continue-on-error #t)

 (test-equal (jaro-run "test-non-existent-opening-mode") 'happy))

(with-warm-run
 "uses a different binding referencing it with open-with"
 (bind
  #:pattern "test-open-with$"
  #:program (open-with 'another-binding))

 (bind
  #:name 'another-binding
  #:pattern "random-pattern"
  #:program (program 'happy))

 (test-equal (jaro-run "test-open-with") 'happy))

(with-warm-run
 "shows an error if named binding is not found"
 (bind
  #:pattern "test-open-with-missing$"
  #:program (open-with 'missing-binding))

 (test-equal (jaro-run "test-open-with-missing") 'jaro-non-zero-without-on-error))

(with-warm-run
 "runs #:on-success when program runs successfully"
 (bind
  #:pattern "test-on-success$"
  #:program "true"
  #:on-success (program 'happy))

 (test-equal (jaro-run "test-on-success") 'happy))

(with-warm-run
 "runs the program specified for a specific environment"
 (setenv "INSIDE_EMACS" "t")
 (bind
  #:pattern "test-env$"
  #:program (program 'sad)
  #:emacs (program 'happy))

 (test-equal (jaro-run "test-env") 'happy))

(with-warm-run
 "runs the program specified for a specific environment"
 (setenv "VIMRUNTIME" "t")
 (bind
  #:pattern "test-env$"
  #:program (program 'sad)
  #:env=VIMRUNTIME (program 'happy))

 (test-equal (jaro-run "test-env") 'happy))

(with-cold-run
 "(program) returns $input properly"
 (bind
  #:pattern "program-test-input"
  #:program (program
             (format #f "~a" $input)))

 (test-equal (jaro-run "program-test-input") "program-test-input"))

(with-cold-run
 "(program) returns submatches properly"
 (bind
  #:pattern "program-test (\\w+) (\\w+) (\\w+)"
  #:program (program
             (format #f "~a ~a ~a" $1 $2 $3)))

 (test-equal (jaro-run "program-test happy also happy") "happy also happy"))

(with-cold-run
 "should run first one if more than one conditionals are matching"

 (setenv "INSIDE_EMACS" "1")
 (setenv "VIMRUNTIME" "1")

 (bind
  #:pattern "test-prioritized-order-env-match$"
  #:emacs (program 'happy)
  #:vim (program 'sad))

 (bind
  #:pattern "test-prioritized-order-env-match2$"
  #:vim (program 'happy)
  #:emacs (program 'sad))

 (test-equal (jaro-run "test-prioritized-order-env-match") 'happy)
 (test-equal (jaro-run "test-prioritized-order-env-match2") 'happy))

(with-cold-run
 "selects supplied environment instead of running ones"
 (setenv "INSIDE_EMACS" "1")
 (setenv "VIMRUNTIME" "1")
 (set! jaro-env 'vim)

 (bind
  #:pattern "test-prioritized-env-match$"
  #:emacs (program 'sad)
  #:vim (program 'happy))

 (test-equal (jaro-run "test-prioritized-env-match") 'happy))

(with-cold-run
 "selects supplied environment instead of running ones"
 (setenv "INSIDE_EMACS" "1")
 (setenv "VIMRUNTIME" "1")
 (set! jaro-env 'vim)

 (bind
  #:pattern "test-prioritized-env-match$"
  #:emacs (program 'sad)
  #:vim (program 'happy))

 (test-equal (jaro-run "test-prioritized-env-match") 'happy))

(with-cold-run
 "correctly runs the matching conditional runner"

 (define-conditional-runner (runner1 _)
   #f)

 (define-conditional-runner (runner2 _)
   #t)

 (bind
  #:pattern "test$"
  #:runner1 (program 'sad)
  #:runner2 (program 'happy))

 (test-equal (jaro-run "test") 'happy))

(with-cold-run
 "runs #:program if no conditional runners are matching"

 (define-conditional-runner (runner1 _)
   #f)

 (define-conditional-runner (runner2 _)
   #f)

 (bind
  #:pattern "test$"
  #:program (program 'happy)
  #:runner1 (program 'sad)
  #:runner2 (program 'sad))

 (test-equal (jaro-run "test") 'happy))

(with-cold-run
 "runs elisp code"
 (bind
  #:pattern "test-elisp-macro$"
  #:program (elisp (message "hello")))

 (test-equal (jaro-run "test-elisp-macro")
   '("emacsclient" "--eval" "(progn (message \"hello\"))")))

(with-cold-run
 "runs elisp code and injects %f %F %1 %2 %3 ..."
 (bind
  #:pattern "(test)-(elisp)-(macro)-(params)$"
  #:program (elisp (message "%f is %1-%2-%3-%4")))

 (test-equal (jaro-run "test-elisp-macro-params")
   '("emacsclient" "--eval" "(progn (message \"test-elisp-macro-params is test-elisp-macro-params\"))")))

(with-cold-run
 "supports list of symbols"
 (bind
  #:pattern "list-of-symbols-as-program-test$"
  #:program '(echo happy %f))

 (test-equal (jaro-run "list-of-symbols-as-program-test")
   '("echo" "happy" "list-of-symbols-as-program-test")))

(with-cold-run
 "does not change URLs when absolute path is requested"
 (bind
  #:pattern ".*"
  #:program '(echo %F))

 (test-equal (jaro-run "https://isamert.net")
   '("echo" "https://isamert.net")))

;;; select-one-of

(with-cold-run
 "select-one-of #:methods returns the methods of the binding"

 (set! dynamic-menu-program "cat")

 (bind
  #:pattern ".*"
  #:program (select-one-of #:methods)
  #:continue-on-error #t
  #:program1 (program 'happy)
  #:program2 (program 'happy))

 (test-equal (jaro-run "something")
   '(select-one-of ("#:program1"
                    "#:program2"))))


(with-cold-run
 "select-one-of can access methods of another named binding, in given order"

 (set! dynamic-menu-program "cat")

 (bind
  #:pattern ".*"
  #:program (select-one-of 'named-binding.program1
                           'binding2.program2
                           'binding2.program1)
  #:continue-on-error #t)

 (bind
  #:name 'named-binding
  #:program (program 'dummy)
  #:program1 (program 'dummy)
  #:continue-on-error #t)

 (bind
  #:name 'binding2
  #:program (select-one-of #:methods)
  #:program (program 'dummy)
  #:program1 (program 'dummy)
  #:program2 (program 'dummy))

 (test-equal (jaro-run "something")
   '(select-one-of ("> binding2.program1"
                    "> binding2.program2"
                    "> named-binding.program1"))))

(with-warm-run
 "select-one-of successfully selects and runs the given method"

 ;; Select it by just printing what we want to select
 (set! dynamic-menu-program "printf '#:program1'")

 (bind
  #:pattern ".*"
  #:program (select-one-of #:methods)
  #:program1 (program 'happy)
  #:continue-on-error #t)

 (test-equal (jaro-run "something") 'happy))

(with-cold-run
 "select-one-of #:bindings lists all bindings and their methods correctly"

 (set! dynamic-menu-program "cat")

 (bind
  #:pattern ".*"
  #:program (select-one-of #:bindings)
  #:program1 (program 'happy)
  #:continue-on-error #t)

 (bind
  #:name 'binding1
  #:program (program 'dummy)
  #:program1 (program 'dummy)
  #:continue-on-error #t)

 ;; unnamed
 (bind
  #:program (program 'dummy)
  #:program5 (program 'dummy)
  #:continue-on-error #t)

 (bind
  #:name 'binding2
  #:program (program 'dummy)
  #:program1 (program 'dummy)
  #:program2 (program 'dummy)
  #:program3 (program 'dummy)
  #:continue-on-error #t)

 (test-equal (jaro-run "something")
   '(select-one-of ("> binding1"
                    "> binding1.program1"
                    "> binding2"
                    "> binding2.program1"
                    "> binding2.program2"
                    "> binding2.program3"))))

(with-warm-run
 "select-one-of runs the selected bindings method correctly"

 (set! dynamic-menu-program "printf '> binding2.program3'")

 (bind
  #:pattern ".*"
  #:program (select-one-of #:bindings #:methods)
  #:program1 (program 'dummy)
  #:continue-on-error #t)

 (bind
  #:name 'binding1
  #:program (program 'dummy)
  #:program1 (program 'dummy)
  #:continue-on-error #t)

 (bind
  #:name 'binding2
  #:program (program 'dummy)
  #:program1 (program 'dummy)
  #:program2 (program 'dummy)
  #:program3 (program 'happy)
  #:continue-on-error #t)

 (test-equal (jaro-run "something") 'happy))

(with-warm-run
 "select-one-of runs the selected bindings correctly"

 (set! dynamic-menu-program "printf '> binding2'")

 (bind
  #:pattern ".*"
  #:program (select-one-of #:bindings #:methods)
  #:program1 (program 'dummy)
  #:continue-on-error #t)

 (bind
  #:name 'binding1
  #:program (program 'dummy)
  #:program1 (program 'dummy)
  #:continue-on-error #t)

 (bind
  #:name 'binding2
  #:program (program 'happy)
  #:program1 (program 'dummy)
  #:program2 (program 'dummy)
  #:program3 (program 'dummy)
  #:continue-on-error #t)

 (test-equal (jaro-run "something") 'happy))

;;; sh, sh-out

(test-equal (sh "echo 'happy'") 0)
(test-equal (sh "false") 256)
(test-equal (sh-out "echo 'happy'") "happy\n")
(test-equal (sh-out "echo 'happy'; echo 'happy'") "happy\nhappy\n")
(test-equal (sh-out "false") #f)
(test-equal (sh-out '("echo" "happy")) "happy\n")
(test-equal (sh-out '("false")) #f)

(test-end "all-tests")
