#!/bin/bash
#
# Copyright (C) Anton Vodonosov (avodonosov@yandex.ru)
#
# See LICENSE for details.

# safe mode
set -euo pipefail

# verbose
set -v

cd "`dirname $0`"

MAIN='(handler-case (load "test.lisp") (serious-condition (c) (format t "~A: ~A~%" (type-of c) c) (asdf/driver:print-backtrace :condition c :count 9999) (uiop:quit 1)))'

case $LISP in
    clisp)
        $LISP -i ~/quicklisp/setup.lisp -x "$MAIN";;
    *)
        $LISP --eval "$MAIN";;
esac
