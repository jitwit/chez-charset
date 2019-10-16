
(library (charset)
  (export charset-build
          list->charset
          integers->charset
          string->charset
          empty-set
          charset-empty?
          charset?
          charset=?
          
          charset-member?
          charset-not-member?
          charset-insert
          charset-delete
          charset-complement
          charset-union
          charset-intersection
          charset-difference
          charset-symmetric-difference
          charset-filter

          charset:unicode
          charset:upper-case
          charset:lower-case
          )
  (import (chezscheme)
          (prefix (patricia-set) set:))
  (include "charset.scm"))
