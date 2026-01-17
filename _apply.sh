#!/usr/bin/env bash

echo "Running LanguageTool ERT test suite..."
emacs -Q -batch -l test.el -f ert-run-tests-batch-and-exit
