(defmodule http.status
  (export all)) ; Keep all existing exports

;;; ---------------------------------------------------------------------------
;;; Status Code Macros (Compile-Time Constants)
;;; ---------------------------------------------------------------------------

;; 2xx Success
(defmacro OK () 200)
(defmacro CREATED () 201)
(defmacro ACCEPTED () 202)
(defmacro NO-CONTENT () 204)

;; 3xx Redirection
(defmacro MOVED-PERMANENTLY () 301)
(defmacro FOUND () 302)
(defmacro SEE-OTHER () 303)
(defmacro NOT-MODIFIED () 304)
(defmacro TEMPORARY-REDIRECT () 307)
(defmacro PERMANENT-REDIRECT () 308)

;; 4xx Client Error
(defmacro BAD-REQUEST () 400)
(defmacro UNAUTHORIZED () 401)
(defmacro FORBIDDEN () 403)
(defmacro NOT-FOUND () 404)
(defmacro METHOD-NOT-ALLOWED () 405)
(defmacro CONFLICT () 409)
(defmacro GONE () 410)
(defmacro TOO-MANY-REQUESTS () 429)

;; 5xx Server Error
(defmacro INTERNAL-SERVER-ERROR () 500)
(defmacro NOT-IMPLEMENTED () 501)
(defmacro BAD-GATEWAY () 502)
(defmacro SERVICE-UNAVAILABLE () 503)
(defmacro GATEWAY-TIMEOUT () 504)

;;; ---------------------------------------------------------------------------
;;; Validation and Lookup Functions
;;; ---------------------------------------------------------------------------

(defun valid?
  "Check if status code is valid (100-599).

  Args:
    code: Integer status code

  Returns:
    Boolean true/false"
  ((code) (when (is_integer code))
   (and (>= code 100) (=< code 599)))
  ((_) 'false))

(defun reason-phrase
  "Get reason phrase for status code.

  Args:
    code: Integer status code

  Returns:
    Binary reason phrase or #\"Unknown\""
  ;; 1xx Informational
  ((100) #"Continue")
  ((101) #"Switching Protocols")
  ((102) #"Processing")
  ;; 2xx Success
  ((200) #"OK")
  ((201) #"Created")
  ((202) #"Accepted")
  ((203) #"Non-Authoritative Information")
  ((204) #"No Content")
  ((205) #"Reset Content")
  ((206) #"Partial Content")
  ((207) #"Multi-Status")
  ((208) #"Already Reported")
  ((226) #"IM Used")
  ;; 3xx Redirection
  ((300) #"Multiple Choices")
  ((301) #"Moved Permanently")
  ((302) #"Found")
  ((303) #"See Other")
  ((304) #"Not Modified")
  ((305) #"Use Proxy")
  ((307) #"Temporary Redirect")
  ((308) #"Permanent Redirect")
  ;; 4xx Client Error
  ((400) #"Bad Request")
  ((401) #"Unauthorized")
  ((402) #"Payment Required")
  ((403) #"Forbidden")
  ((404) #"Not Found")
  ((405) #"Method Not Allowed")
  ((406) #"Not Acceptable")
  ((407) #"Proxy Authentication Required")
  ((408) #"Request Timeout")
  ((409) #"Conflict")
  ((410) #"Gone")
  ((411) #"Length Required")
  ((412) #"Precondition Failed")
  ((413) #"Payload Too Large")
  ((414) #"URI Too Long")
  ((415) #"Unsupported Media Type")
  ((416) #"Range Not Satisfiable")
  ((417) #"Expectation Failed")
  ((418) #"I'm a teapot")
  ((422) #"Unprocessable Entity")
  ((423) #"Locked")
  ((424) #"Failed Dependency")
  ((426) #"Upgrade Required")
  ((428) #"Precondition Required")
  ((429) #"Too Many Requests")
  ((431) #"Request Header Fields Too Large")
  ((451) #"Unavailable For Legal Reasons")
  ;; 5xx Server Error
  ((500) #"Internal Server Error")
  ((501) #"Not Implemented")
  ((502) #"Bad Gateway")
  ((503) #"Service Unavailable")
  ((504) #"Gateway Timeout")
  ((505) #"HTTP Version Not Supported")
  ((506) #"Variant Also Negotiates")
  ((507) #"Insufficient Storage")
  ((508) #"Loop Detected")
  ((510) #"Not Extended")
  ((511) #"Network Authentication Required")
  ;; Default
  ((_) #"Unknown"))

;;; ---------------------------------------------------------------------------
;;; Keep ALL existing function-based status codes for compatibility
;;; ---------------------------------------------------------------------------

; HTTP 1xx
(defun continue () 100)
(defun switching-protocols () 101)
(defun processing () 102)

; HTTP 2xx
(defun ok () 200)
(defun created () 201)
(defun accepted () 202)
(defun non-authoritative-information () 203)
(defun no-content () 204)
(defun reset-content () 205)
(defun partial-content () 206)
(defun multi-status () 207)
(defun already-reported () 208)
(defun im-used () 226)

; HTTP 3xx
(defun multiple-choices () 300)
(defun moved-permanently () 301)
(defun found () 302)
(defun see-other () 303)
(defun not-modified () 304)
(defun use-proxy () 305)
(defun switch-proxy () 306)
(defun temporary-redirect () 307)
(defun permanent-redirect () 308)

; HTTP 4xx
(defun bad-request () 400)
(defun unauthorized () 401)
(defun payment-required () 402)
(defun forbidden () 403)
(defun not-found () 404)
(defun method-not-allowed () 405)
(defun not-acceptable () 406)
(defun proxy-authentication-required () 407)
(defun request-timeout () 408)
(defun conflict () 409)
(defun gone () 410)
(defun length-required () 411)
(defun precondition-failed () 412)
(defun request-entity-too-large () 413)
(defun request-uri-too-long () 414)
(defun unsupported-media-type () 415)
(defun requested-range-not-satisfiable () 416)
(defun expectation-failed () 417)
(defun im-a-teapot () 418)
(defun authentication-timeout () 419)
(defun method-failure () 420)
(defun unprocessable-entity () 422)
(defun locked () 423)
(defun unordered-collection () 425)
(defun upgrade-required () 426)
(defun precondition-required () 428)
(defun too-many-requests () 429)
(defun request-header-fields-too-large () 431)
(defun no-response () 444)
(defun retry-with () 449)
(defun blocked-by-windows-parental-controls () 450)
(defun unavailable-for-legal-reasons () 451)
(defun request-header-too-large () 494)
(defun cert-error () 495)
(defun no-cert () 496)
(defun http-to-https () 497)
(defun client-closed-request () 499)

; HTTP 5xx
(defun internal-server-error () 500)
(defun not-implemented () 501)
(defun bad-gateway () 502)
(defun service-unavailable () 503)
(defun gateway-timeout () 504)
(defun http-version-not-supported () 505)
(defun variant-also-negotiates () 506)
(defun insufficient-storage () 507)
(defun loop-detected () 508)
(defun bandwidth-limit-exceeded () 509)
(defun not-extended () 510)
(defun network-authentication-required () 511)
(defun connection-timed-out () 522)
(defun network-read-timeout-error () 598)
(defun network-connect-timeout-error () 599)

; HTTP 70x - Inexcusable
(defun meh () 701)
(defun emacs () 702)
(defun explosion () 703)
(defun goto-fail () 704)
(defun i-wrote-the-code-and-missed-the-necessary-validation-by-an-oversight () 705)

; 71X - Novelty Implementations
(defun php () 710)
(defun convenience-store () 711)
(defun nosql () 712)
(defun i-am-not-a-teapot () 719)

; 72X - Edge Cases
(defun unpossible () 720)
(defun known-unknowns () 721)
(defun unknown-unknowns () 722)
(defun tricky () 723)
(defun this-line-should-be-unreachable () 724)
(defun it-works-on-my-machine () 725)
(defun its-a-feature-not-a-bug () 726)
(defun 32-bits-is-plenty () 727)

; 73X - Fucking
(defun fucking-rubygems () 731)
(defun fucking-unicode () 732)
(defun fucking-deadlocks () 733)
(defun fucking-deferreds () 734)
(defun fucking-ie () 735)
(defun fucking-race-conditions () 736)
(defun fuckthreadsing () 737)
(defun fucking-bundler () 738)
(defun fucking-windows () 739)

; 74X - Meme Driven
(defun computer-says-no () 740)
(defun compiling () 741)
(defun a-kitten-dies () 742)
(defun i-thought-i-knew-regular-expressions () 743)
(defun y-u-no-write-integration-tests () 744)
(defun i-dont-always-test-my-code-but-when-i-do-i-do-it-in-production () 745)
(defun missed-ballmer-peak () 746)
(defun motherfucking-snakes-on-the-motherfucking-plane () 747)
(defun confounded-by-ponies () 748)
(defun reserved-for-chuck-norris () 749)

; 75X - Syntax Errors
(defun didnt-bother-to-compile-it () 750)
(defun not-enough-parens () 751)
(defun too-many-parens () 752)
(defun syntax-error () 753)
(defun too-many-semi-colons () 754)
(defun not-enough-semi-colons () 755)
(defun insufficiently-polite () 756)
(defun excessively-polite () 757)
(defun unexpected-t-paamayim-nekudotayim () 759)

; 76X - Substance-Affected Developer
(defun hungover () 761)
(defun stoned () 762)
(defun under-caffeinated () 763)
(defun over-caffeinated () 764)
(defun railscamp () 765)
(defun sober () 766)
(defun drunk () 767)
(defun accidentally-took-sleeping-pills-instead-of-migraine-pills-during-crunch-week () 768)
(defun questionable-maturity-level () 769)

; 77X - Predictable Problems
(defun cached-for-too-long () 771)
(defun not-cached-long-enough () 772)
(defun not-cached-at-all () 773)
(defun why-was-this-cached () 774)
(defun error-on-the-exception () 776)
(defun coincidence () 777)
(defun off-by-one-error () 778)
(defun off-by-too-many-to-count-error () 779)

; 78X - Somebody Else's Problem
(defun project-owner-not-responding () 780)
(defun operations () 781)
(defun qa () 782)
(defun it-was-a-customer-request-honestly () 783)
(defun management-obviously () 784)
(defun tps-cover-sheet-not-attached () 785)
(defun try-it-now () 786)
(defun further-funding-required () 787)

; 79X - Internet crashed
(defun the-internet-shut-down-due-to-copyright-restrictions () 791)
(defun climate-change-driven-catastrophic-weather-event () 792)
(defun zombie-apocalypse () 793)
(defun someone-let-pg-near-a-repl () 794)
(defun heartbleed () 795)
(defun this-is-the-last-page-of-the-internet-go-back () 797)
(defun end-of-the-world () 799)
