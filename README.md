# Rummage

Rummage is a small web-scraping library for Common Lisp.

## Quickstart

```lisp
(ql:quickload '(:rummage
                :generator ; for yielding multiple results
                :lquery))  ; for parsing html


;; Prepare a parser.
; Use of defun* gives access to yield (see :generator).
(defun* quickstart-parser (ctx response)
  ; We expect an HTML response, so we parse it with lquery.
  (let ((doc (lquery:$ (initialize response))))
    ; Get the required information out of matching elements.
    (loop for author across (lquery:$ doc "small.author" (text))
      ; Yield the extracted information as an 'item' (e.g. a plist).
      do (yield (list :author author)))
    ; If there is a link to the next page,
    (let ((next-page (lquery:$ doc "li.next a" (attr :href))))
      (when (not (emptyp next-page))
        ; we follow the link and parse the response.
        ; We re-yield any newly extracted items.
        (yield* (rummage:follow ctx (elt next-page 0) #'quickstart-parser))))))

;; Extracted items are passed through a pipeline of functions.
;; Prepare a function that prints the author in each item.
(defun print-author (item)
  ; Do something with the passed item.
  (format t "~a~%" (getf item :author))
  ; Pass it on to the next function in the pipeline (or not).
  item)

;; Scrape a URL with our parser and pipeline.
(rummage:scrape "http://quotes.toscrape.com"
        #'quickstart-parser
        :pipeline (list #'print-author))
```

Output:

```
 <INFO> [14:45:58] rummage main.lisp (request) -
  Requesting <http://quotes.toscrape.com>
Albert Einstein
J.K. Rowling
Albert Einstein
Jane Austen
Marilyn Monroe
Albert Einstein
André Gide
Thomas A. Edison
Eleanor Roosevelt
Steve Martin
 <INFO> [14:46:00] rummage main.lisp (request) -
  Requesting <http://quotes.toscrape.com/page/2/>
Marilyn Monroe
J.K. Rowling
...and so on...
```

See `tests/main.lisp` for more examples.

## Installation

```
$ cd ~/.roswell/local-projects/
$ git clone https://github.com/armontoubman/rummage.git
```

## Testing

```lisp
* (ql:quickload :rummage)
* (asdf:test-system :rummage)
```

Rummage was tested on SBCL 2.0.10 and CCL 1.12 (x64, Linux and macOS).
