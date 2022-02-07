(eval-when (expand load eval)
  (load "jaro"))

(use-modules
 (srfi srfi-64))

;;
;; Some macros
;;

(define-syntax with-cold-run
  (syntax-rules ()
    ((with-cold-run test-name body ...)
     (begin
       (format #t ">>>>> Running ~a <<<<<\n" test-name)
       (set! jaro/cold-run? #t)
       (set! jaro/assocs '())
       (set! jaro/named-assocs (make-hash-table))
       (set! jaro/runner-method #f)
       body ...))))

(define-syntax with-warm-run
  (syntax-rules ()
    ((with-warm-run test-name body ...)
     (begin
       (format #t ">>>>> Running ~a <<<<<\n" test-name)
       (set! jaro/cold-run? #f)
       (set! jaro/assocs '())
       (set! jaro/named-assocs (make-hash-table))
       (set! jaro/runner-method #f)
       body ...))))

;;
;; Tests
;;

(test-begin "all-tests")

(with-cold-run
 "basic assoc"
  (assoc
   #:pattern ".json$"
   #:program "json-viewer %f")

  (test-equal (jaro/run "~/a.json") "json-viewer ~/a.json"))

(with-cold-run
 "assoc with list of patterns."
 (assoc
  #:pattern '(".js$" ".json$" ".yaml$")
  #:program "json-viewer %f")

 (test-equal (jaro/run "~/a.json") "json-viewer ~/a.json"))

(with-warm-run
 "#:program fails"
  (assoc
   #:pattern "test-on-error$"
   #:program "false")

  (test-equal (jaro/run "test-on-error") 'jaro/non-zero-without-on-error))

(with-warm-run
 "run #:on-error in case of #:program fails"
  (assoc
   #:pattern "test-on-error-2$"
   #:program "false"
   #:on-error (program 'on-error))

  (test-equal (jaro/run "test-on-error-2") 'on-error))

(with-warm-run
 "#:program fails and then #:on-error fails"
  (assoc
   #:pattern "test-on-error-3$"
   #:program "false"
   #:on-error "false")

  (test-equal (jaro/run "test-on-error-3") #f))

(with-warm-run
 "#:program fails and #:on-error is 'continue"
  (assoc
   #:pattern "test-on-error-4$"
   #:program "false"
   #:on-error 'continue)

  (assoc
   #:pattern "test-on-error-4$"
   #:program "true")

  (test-equal (jaro/run "test-on-error-4") #t))

(with-warm-run
 "#:program fails and #:on-error is 'continue but there are no matches"
  (assoc
   #:pattern "test-on-error-5$"
   #:program "false"
   #:on-error 'continue)

  (assoc
   #:pattern "test-on-error-5-no-match-on-continue$"
   #:program "true")

  (test-equal (jaro/run "test-on-error-5") 'jaro/no-matches))

(with-warm-run
 "#:program fails and #:on-error calls a named assoc"
  (assoc
   #:pattern "test-on-error-6$"
   #:program "false"
   #:on-error 'on-error-handler-for-6)

  (assoc
   #:name 'on-error-handler-for-6
   #:program "true")

  (test-equal (jaro/run "test-on-error-6") #t))

(with-cold-run
 "using capture group in a pattern in #:program"
  (assoc
   #:pattern "capture-(\\w+)-test$"
   #:program "%0 %1 %2")

  (test-equal (jaro/run "capture-group-test") "capture-group-test group %2"))

(with-cold-run
 "replaces %F with full path in #:program"
  (assoc
   #:pattern "path-test$"
   #:program "echo %F")

  (test-equal (jaro/run "full/path-test") (string-append "echo " (getenv "PWD") "/full/path-test")))

(with-cold-run
 "replaces %F with full path #:program when given path is already a full path"
  (assoc
   #:pattern "path-test$"
   #:program "echo %F")

  (test-equal (jaro/run "/full/path-test") "echo /full/path-test"))

(with-cold-run
 "replaces %U with URI of given path"
  (assoc
   #:pattern "uri-test$"
   #:program "echo %U")

  (test-equal (jaro/run "/uri-test") "echo file:///uri-test")
  (test-equal (jaro/run "https://uri-test") "echo https://uri-test"))

(with-cold-run
 "is able to access matching groups in a procedural #:program"
  (assoc
   #:pattern "fn-test.(\\w+)$"
   #:program (lambda (input mimetype matches)
               (format #f "~a - ~a - ~a" input mimetype matches)))

  (test-equal (jaro/run "fn-test.mp4") "fn-test.mp4 - video/mp4 - ((%0 . fn-test.mp4) (%1 . mp4))"))

(with-warm-run
 "conditionally fails based on given path"
  (assoc
   #:pattern '("fn-(\\w+)-test$")
   #:program (lambda (_1 _2 matches)
               (if (string= (cdr (list-ref matches 1)) "sad")
                   #f 'happy))
   #:on-error (program 'sad))
  (test-equal (jaro/run "fn-success-test") 'happy)
  (test-equal (jaro/run "fn-sad-test") 'sad))

(with-warm-run
 "handles #:continue-on-error and #:on-fail properly"
  (assoc
   #:pattern "test-test-rule$"
   #:test "false"
   #:continue-on-error #t)

  (assoc
   #:pattern "test-test-rule$"
   #:program (program 'alternative))

  (assoc
   #:pattern "test-on-fail-rule$"
   #:test "false"
   #:on-fail (program 'on-fail))

  (assoc
   #:pattern "test-success-rule$"
   #:test "true"
   #:program (program 'test-success))

  (test-equal (jaro/run "test-test-rule") 'alternative)
  (test-equal (jaro/run "test-on-fail-rule") 'on-fail)
  (test-equal (jaro/run "test-success-rule") 'test-success))

(with-warm-run
 "uses other alternative when #:continue-on-error is t"
 (assoc
  #:pattern "test-on-error-continue$"
  #:program "false"
  #:on-error "false"
  #:continue-on-error #t)

 (assoc
  #:pattern "test-on-error-continue$"
  #:program (program 'happy)
  #:on-error "false")


 (test-equal (jaro/run "test-on-error-continue") 'happy))

(with-warm-run
 "handles opening modes properly"
 (set! jaro/runner-method #:view)

 (assoc
  #:pattern "test-opening-mode$"
  #:program "false"
  #:on-error (program 'failed)
  #:view (program 'happy)
  #:continue-on-error #t)

 (test-equal (jaro/run "test-opening-mode") 'happy))

(with-warm-run
 "uses default environment program when mode is not found"
 (set! jaro/runner-method #:view)

 (assoc
  #:pattern "test-non-existent-opening-mode$"
  #:program (program 'happy)
  #:random-mode (program 'sad)
  #:continue-on-error #t)

 (test-equal (jaro/run "test-non-existent-opening-mode") 'happy))

(with-warm-run
 "uses a different assoc referencing it with open-with"
 (assoc
  #:pattern "test-open-with$"
  #:program (open-with 'another-assoc))

 (assoc
  #:name 'another-assoc
  #:pattern "random-pattern"
  #:program (program 'happy))

 (test-equal (jaro/run "test-open-with") 'happy))

(with-warm-run
 "shows an error if named assoc is not found"
 (assoc
  #:pattern "test-open-with-missing$"
  #:program (open-with 'missing-assoc))

 (test-equal (jaro/run "test-open-with-missing") 'jaro/non-zero-without-on-error))

(with-warm-run
 "runs #:on-success when program runs successfully"
 (assoc
  #:pattern "test-on-success$"
  #:program "true"
  #:on-success (program 'happy))

 (test-equal (jaro/run "test-on-success") 'happy))

(with-warm-run
 "runs the program specified for a specific environment"
 (setenv "INSIDE_EMACS" "t")
 (assoc
  #:pattern "test-env$"
  #:program (program 'sad)
  #:emacs (program 'happy))

 (test-equal (jaro/run "test-env") 'happy))

(with-warm-run
 "runs the program specified for a specific environment"
 (setenv "VIMRUNTIME" "t")
 (assoc
  #:pattern "test-env$"
  #:program (program 'sad)
  #:env=VIMRUNTIME (program 'happy))

 (test-equal (jaro/run "test-env") 'happy))

(with-cold-run
 "(program) returns $input properly"
  (assoc
   #:pattern "program-test-input"
   #:program (program
              (format #f "~a" $input)))

  (test-equal (jaro/run "program-test-input") "program-test-input"))

(with-cold-run
 "(program) returns submatches properly"
  (assoc
   #:pattern "program-test (\\w+) (\\w+) (\\w+)"
   #:program (program
              (format #f "~a ~a ~a" $1 $2 $3)))

  (test-equal (jaro/run "program-test happy also happy") "happy also happy"))


;; sh, sh-out

(test-equal (sh "echo 'happy'") 0)
(test-equal (sh "false") 256)
(test-equal (sh-out "echo 'happy'") "happy\n")
(test-equal (sh-out "echo 'happy'; echo 'happy'") "happy\nhappy\n")
(test-equal (sh-out "false") #f)
(test-equal (sh-out '("echo" "happy")) "happy\n")
(test-equal (sh-out '("false")) #f)



(test-end "all-tests")
