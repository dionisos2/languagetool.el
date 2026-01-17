#!/usr/bin/env bash


emacs -Q --batch --eval "(dolist (file (directory-files \".\" t \"\\\\.el\\$\")) (message file))"

emacs -Q --batch \
    --eval "(dolist (file (directory-files \".\" t \"\\\\.el\\$\"))
                (find-file file)
                (tabify (point-min) (point-max))
                (save-buffer)
                (kill-buffer))"
