#! /usr/bin/env scheme-script

(import (chezscheme))

(for-each compile-library
          '("charset.sls"))
