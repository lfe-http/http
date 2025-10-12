(defmodule http.mimetype-tests
  (behaviour ltest-unit))

(include-lib "ltest/include/ltest-macros.lfe")

;;; ---------------------------------------------------------------------------
;;; MIME type function tests
;;; ---------------------------------------------------------------------------

(deftest text-plain
  (is-equal #"text/plain" (http.mimetype:text/plain)))

(deftest text-html
  (is-equal #"text/html" (http.mimetype:text/html)))

(deftest application-json
  (is-equal #"application/json" (http.mimetype:application/json)))

(deftest multipart-form-data
  (is-equal #"multipart/form-data" (http.mimetype:multipart/form-data)))

;;; ---------------------------------------------------------------------------
;;; from-extension tests
;;; ---------------------------------------------------------------------------

(deftest from-extension-html
  (is-equal #"text/html; charset=utf-8"
            (http.mimetype:from-extension #"html")))

(deftest from-extension-htm
  (is-equal #"text/html; charset=utf-8"
            (http.mimetype:from-extension #"htm")))

(deftest from-extension-txt
  (is-equal #"text/plain; charset=utf-8"
            (http.mimetype:from-extension #"txt")))

(deftest from-extension-json
  (is-equal #"application/json; charset=utf-8"
            (http.mimetype:from-extension #"json")))

(deftest from-extension-xml
  (is-equal #"application/xml; charset=utf-8"
            (http.mimetype:from-extension #"xml")))

(deftest from-extension-png
  (is-equal #"image/png"
            (http.mimetype:from-extension #"png")))

(deftest from-extension-jpg
  (is-equal #"image/jpeg"
            (http.mimetype:from-extension #"jpg")))

(deftest from-extension-jpeg
  (is-equal #"image/jpeg"
            (http.mimetype:from-extension #"jpeg")))

(deftest from-extension-unknown
  (is-equal #"application/octet-stream"
            (http.mimetype:from-extension #"unknown")))

(deftest from-extension-case-insensitive
  (is-equal #"text/html; charset=utf-8"
            (http.mimetype:from-extension #"HTML"))
  (is-equal #"application/json; charset=utf-8"
            (http.mimetype:from-extension #"JSON")))

(deftest from-extension-string
  (is-equal #"text/html; charset=utf-8"
            (http.mimetype:from-extension "html")))

(deftest from-extension-atom
  (is-equal #"text/html; charset=utf-8"
            (http.mimetype:from-extension 'html)))

;;; ---------------------------------------------------------------------------
;;; from-path tests
;;; ---------------------------------------------------------------------------

(deftest from-path-html
  (is-equal #"text/html; charset=utf-8"
            (http.mimetype:from-path #"/path/to/file.html")))

(deftest from-path-json
  (is-equal #"application/json; charset=utf-8"
            (http.mimetype:from-path #"/api/data.json")))

(deftest from-path-png
  (is-equal #"image/png"
            (http.mimetype:from-path #"/images/logo.png")))

(deftest from-path-multiple-dots
  (is-equal #"application/gzip"
            (http.mimetype:from-path #"/path/to/archive.tar.gz")))

(deftest from-path-no-extension
  (is-equal #"application/octet-stream"
            (http.mimetype:from-path #"/no/extension")))

(deftest from-path-string
  (is-equal #"text/html; charset=utf-8"
            (http.mimetype:from-path "/path/to/file.html")))

;;; ---------------------------------------------------------------------------
;;; mime-map tests
;;; ---------------------------------------------------------------------------

(deftest mime-map-returns-map
  (is-equal 'true (is_map (http.mimetype:mime-map))))

(deftest mime-map-has-common-types
  (let ((map (http.mimetype:mime-map)))
    (is-equal 'true (maps:is_key #"html" map))
    (is-equal 'true (maps:is_key #"json" map))
    (is-equal 'true (maps:is_key #"png" map))))
