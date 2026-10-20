(ns nicheware.platform.utilities.common.string
"
Functions complementing those in clojure.string.

There are groups of functions within common.string that deal with:

|Function group|Functions|
|---|---|
|markup escaping| [[escape-html]], [[escape-attr]]|

## Markup escaping

Both escapers replace each special character in a single pass over the input, so an `&` introduced by
one replacement is never escaped again. When the input contains nothing that needs escaping it is
returned unchanged (the identical string), so escaping already-safe text allocates nothing.

```clojure
(escape-html \"Fish & Chips <b>\")        => \"Fish &amp; Chips &lt;b&gt;\"
(escape-attr \"say \\\"hi\\\" & 'bye'\")    => \"say &quot;hi&quot; &amp; &#39;bye&#39;\"
```
"
  (:require [clojure.string :as str]
            [nicheware.platform.utilities.common.core :as core]))

;; ============================== Markup escaping ===============================

(def ^:no-doc html-escapes
  "Replacements for characters that are special in HTML text content."
  {\& "&amp;" \< "&lt;" \> "&gt;"})

(def ^:no-doc attr-escapes
  "Replacements for characters that are special in a quoted HTML attribute value (either quote style)."
  (assoc html-escapes \" "&quot;" \' "&#39;"))

(defn ^:no-doc escape-with
  "Escapes s using the char => replacement map, returning s itself when no character matches.

  - fn-name: name of the calling public function, used in the exception message.
  - escapes: map of character to replacement string.
  - special-re: regex matching any character in escapes.
  - s: string to escape, or nil.
  - returns: escaped string, s if nothing needed escaping, or nil for nil.
  - throws: IllegalArgumentException for a non-string s."
  [fn-name escapes special-re s]
  (cond
    (nil? s) nil
    (not (string? s)) (core/throw-illegal-arg (str fn-name ": s must be a string or nil"))
    (re-find special-re s) (str/escape s escapes)
    :else s))

(defn escape-html
  "Escapes a string for use as HTML (or XML/SVG) text content: `&`, `<` and `>` are replaced by
   `&amp;`, `&lt;` and `&gt;`. Quotes are left alone; use [[escape-attr]] for attribute values.

  - s: string to escape. nil returns nil.
  - returns: the escaped string, or s itself (identical) when it contains no special characters.
  - throws: IllegalArgumentException for a non-string s.

eg:
```clojure
(escape-html \"Fish & Chips <b>\") => \"Fish &amp; Chips &lt;b&gt;\"
(escape-html \"&amp;\")            => \"&amp;amp;\"
(escape-html \"plain\")            => \"plain\"
```
"
  [s]
  (escape-with "escape-html" html-escapes #"[&<>]" s))

(defn escape-attr
  "Escapes a string for use inside a quoted HTML (or XML/SVG) attribute value: as [[escape-html]], plus
   `\"` => `&quot;` and `'` => `&#39;`, so the result is safe within either quote style.

  - s: string to escape. nil returns nil.
  - returns: the escaped string, or s itself (identical) when it contains no special characters.
  - throws: IllegalArgumentException for a non-string s.

eg:
```clojure
(escape-attr \"say \\\"hi\\\" & 'bye'\") => \"say &quot;hi&quot; &amp; &#39;bye&#39;\"
(escape-attr \"a<b\")                 => \"a&lt;b\"
```
"
  [s]
  (escape-with "escape-attr" attr-escapes #"[&<>\"']" s))
