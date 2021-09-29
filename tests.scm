(use-modules
 (srfi srfi-64))

(load "jaro")

;;
;; Some macros
;;

(define-syntax program
  (syntax-rules ()
    ((program body ...)
     (lambda (_1 _2 _3) body ...))))

(define-syntax with-cold-run
  (syntax-rules ()
    ((with-cold-run body ...)
     (begin
       (set! jaro/cold-run? #t)
       body ...))))

(define-syntax with-warm-run
  (syntax-rules ()
    ((with-warm-run body ...)
     (begin
       (set! jaro/cold-run? #f)
       body ...))))

(define-syntax with-clean-state
  (syntax-rules ()
    ((with-clean-state body ...)
     (begin
       (set! jaro/assocs '())
       (set! jaro/named-assocs (make-hash-table))
       body ...))))

;;
;; Tests
;;

(test-begin "all-tests")

(with-cold-run
  (assoc
   #:pattern ".json$"
   #:program "json-viewer %f")

  (test-equal (jaro/run "~/a.json") "json-viewer ~/a.json"))

(with-clean-state
 (assoc
  #:pattern '(".js$" ".json$" ".yaml$")
  #:program "json-viewer %f")

 (test-equal (jaro/run "~/a.json") "json-viewer ~/a.json"))

(with-warm-run
  (assoc
   #:pattern "test-on-error$"
   #:program "false")

  (test-equal (jaro/run "test-on-error") 'jaro/non-zero-without-on-error))

(with-warm-run
  (assoc
   #:pattern "test-on-error-2$"
   #:program "false"
   #:on-error (program 'on-error))

  (test-equal (jaro/run "test-on-error-2") 'on-error))

(with-warm-run
  (assoc
   #:pattern "test-on-error-3$"
   #:program "false"
   #:on-error "false")

  (test-equal (jaro/run "test-on-error-3") #f))

(with-warm-run
  (assoc
   #:pattern "test-on-error-4$"
   #:program "false"
   #:on-error 'continue)

  (assoc
   #:pattern "test-on-error-4$"
   #:program "true")

  (test-equal (jaro/run "test-on-error-4") #t))

(with-warm-run
  (assoc
   #:pattern "test-on-error-5$"
   #:program "false"
   #:on-error 'continue)

  (assoc
   #:pattern "test-on-error-5-no-match-on-continue$"
   #:program "true")

  (test-equal (jaro/run "test-on-error-5") 'jaro/no-matches))

(with-warm-run
  (assoc
   #:pattern "test-on-error-6$"
   #:program "false"
   #:on-error 'on-error-handler-for-6)

  (assoc
   #:name 'on-error-handler-for-6
   #:program "true")

  (test-equal (jaro/run "test-on-error-6") #t))

(with-cold-run
  (assoc
   #:pattern "capture-(\\w+)-test$"
   #:program "%0 %1 %2")

  (test-equal (jaro/run "capture-group-test") "capture-group-test group %2"))

(with-cold-run
  (assoc
   #:pattern "path-test$"
   #:program "echo %F")

  (test-equal (jaro/run "full/path-test") (string-append "echo " (getenv "PWD") "/full/path-test")))

(with-cold-run
  (assoc
   #:pattern "path-test$"
   #:program "echo %F")

  (test-equal (jaro/run "/full/path-test") "echo /full/path-test"))

(with-cold-run
  (assoc
   #:pattern "uri-test$"
   #:program "echo %U")

  (test-equal (jaro/run "/uri-test") "echo file:///uri-test")
  (test-equal (jaro/run "https://uri-test") "echo https://uri-test"))

(with-cold-run
  (assoc
   #:pattern "fn-test.(\\w+)$"
   #:program (lambda (input mimetype matches)
               (format "~a - ~a - ~a" input mimetype matches)))

  (test-equal (jaro/run "fn-test.mp4") "fn-test.mp4 - video/mp4 - ((%0 . fn-test.mp4) (%1 . mp4))"))

(with-warm-run
  (assoc
   #:pattern '("fn-(\\w+)-test$")
   #:program (lambda (_1 _2 matches) (if (string= (cdr (list-ref matches 1)) "sad")
                                    #f 'happy))
   #:on-error (program 'sad))

  (test-equal (jaro/run "fn-success-test") 'happy)
  (test-equal (jaro/run "fn-sad-test") 'sad))


(with-warm-run
  (assoc
   #:pattern "test-test-rule$"
   #:test "false")

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
 (assoc
  #:name 'editor
  #:pattern "text/markdown"
  #:program (program 'happy))

 (test-equal (mimetype "README.md") "text/markdown")
 (test-equal (jaro/run "README.md") 'happy))


(test-end "all-tests")
