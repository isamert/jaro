(use-modules
 (srfi srfi-64))

(load "jaro")

(define (jaro/clear-state)
  (set! jaro/assocs '())
  (set! jaro/named-assocs (make-hash-table))
  (set! jaro/cold-run? #f))

(test-begin "all-tests")

(begin
  (jaro/clear-state)
  (set! jaro/cold-run? #t)

  (assoc
   #:pattern ".json$"
   #:program "json-viewer %f")

  (test-equal (jaro/run "~/a.json") "json-viewer ~/a.json"))

(begin
  (jaro/clear-state)
  (set! jaro/cold-run? #t)

  (assoc
   #:pattern '(".js$" ".json$" ".yaml$")
   #:program "json-viewer %f")

  (test-equal (jaro/run "~/a.json") "json-viewer ~/a.json"))

(jaro/clear-state)

(begin
  (assoc
   #:pattern "test-on-error$"
   #:program "false")

  (test-equal (jaro/run "test-on-error") 'jaro/non-zero-without-on-error))

(begin
  (assoc
   #:pattern "test-on-error-2$"
   #:program "false"
   #:on-error "true")

  (test-equal (jaro/run "test-on-error-2") #t))

(begin
  (assoc
   #:pattern "test-on-error-3$"
   #:program "false"
   #:on-error "false")

  (test-equal (jaro/run "test-on-error-3") #f))

(begin
  (assoc
   #:pattern "test-on-error-4$"
   #:program "false"
   #:on-error 'continue)

  (assoc
   #:pattern "test-on-error-4$"
   #:program "true")

  (test-equal (jaro/run "test-on-error-4") #t))

(begin
  (assoc
   #:pattern "test-on-error-5$"
   #:program "false"
   #:on-error 'continue)

  (assoc
   #:pattern "test-on-error-5-no-match-on-continue$"
   #:program "true")

  (test-equal (jaro/run "test-on-error-5") 'jaro/no-matches))

(begin
  (assoc
   #:pattern "test-on-error-6$"
   #:program "false"
   #:on-error 'on-error-handler-for-6)

  (assoc
   #:name 'on-error-handler-for-6
   #:program "true")

  (test-equal (jaro/run "test-on-error-6") #t))

(begin
  (set! jaro/cold-run? #t)

  (assoc
   #:pattern "capture-(\\w+)-test$"
   #:program "%0 %1 %2")

  (test-equal (jaro/run "capture-group-test") "capture-group-test group %2"))

(begin
  (set! jaro/cold-run? #t)

  (assoc
   #:pattern "path-test$"
   #:program "echo %F")

  (test-equal (jaro/run "full/path-test") (string-append "echo " (getenv "PWD") "/full/path-test")))

(begin
  (set! jaro/cold-run? #t)

  (assoc
   #:pattern "path-test$"
   #:program "echo %F")

  (test-equal (jaro/run "/full/path-test") "echo /full/path-test"))

(begin
  (set! jaro/cold-run? #t)

  (assoc
   #:pattern "uri-test$"
   #:program "echo %U")

  (test-equal (jaro/run "/uri-test") "echo file:///uri-test")
  (test-equal (jaro/run "https://uri-test") "echo https://uri-test"))

(begin
  (set! jaro/cold-run? #f)

  (assoc
   #:pattern "fn-test.(\\w+)$"
   #:program (lambda (input mimetype matches)
               (format "~a - ~a - ~a" input mimetype matches)))

  (test-equal (jaro/run "fn-test.mp4") "fn-test.mp4 - video/mp4 - ((%0 . fn-test.mp4) (%1 . mp4))"))

(begin
  (set! jaro/cold-run? #f)

  (assoc
   #:pattern "fn-(\\w+)-test$"
   #:program (lambda (_1 _2 matches) (if (string= (cdr (list-ref matches 1)) "sad")
                                    #f "SUCCESS"))
   #:on-error (lambda (_1 _2 _3) "SAD"))

  (test-equal (jaro/run "fn-success-test") "SUCCESS")
  (test-equal (jaro/run "fn-sad-test") "SAD"))

(test-end "all-tests")
