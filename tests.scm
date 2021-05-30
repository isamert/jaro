(use-modules
 (srfi srfi-64))

(load "jaro")

(define (jaro/clear-state)
  (set! jaro/assocs '())
  (set! jaro/named-assocs (make-hash-table))
  (set! jaro/cold-run? #t))

(test-begin "all-tests")

(begin
  (jaro/clear-state)

  (assoc
   #:pattern ".json$"
   #:program "json-viewer %f")

  (test-equal (jaro/run "~/a.json") "json-viewer ~/a.json"))

(begin
  (jaro/clear-state)

  (assoc
   #:pattern '(".js$" ".json$" ".yaml$")
   #:program "json-viewer %f")

  (test-equal (jaro/run "~/a.json") "json-viewer ~/a.json"))

(set! jaro/cold-run? #f)

(begin
  (assoc
   #:pattern "test-on-fail$"
   #:program "false")

  (test-equal (jaro/run "test-on-fail") 'jaro/non-zero-without-on-error))

(begin
  (assoc
   #:pattern "test-on-fail-2$"
   #:program "false"
   #:on-error "true")

  (test-equal (jaro/run "test-on-fail-2") #t))

(begin
  (assoc
   #:pattern "test-on-fail-3$"
   #:program "false"
   #:on-error "false")

  (test-equal (jaro/run "test-on-fail-3") #f))

(begin
  (assoc
   #:pattern "test-on-fail-4$"
   #:program "false"
   #:on-error 'continue)

  (assoc
   #:pattern "test-on-fail-4$"
   #:program "true")

  (test-equal (jaro/run "test-on-fail-4") #t))

(begin
  (assoc
   #:pattern "test-on-fail-5$"
   #:program "false"
   #:on-error 'continue)

  (assoc
   #:pattern "test-on-fail-5-no-match-on-continue$"
   #:program "true")

  (test-equal (jaro/run "test-on-fail-5") 'jaro/no-matches))

(test-end "all-tests")
