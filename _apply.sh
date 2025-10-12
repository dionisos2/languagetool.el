#!/usr/bin/env bash

echo "Running LanguageTool ERT test suite..."
emacs -batch -l test.el -f ert-run-tests-batch-and-exit
