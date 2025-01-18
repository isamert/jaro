#!/bin/sh

printf "tests.scm\njaro" | entr guile --no-auto-compile tests.scm
