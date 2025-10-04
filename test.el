;;; test.el --- tests  -*- lexical-binding: t -*-
;;; Commentary:
;;; Nothing special.

;;; Code:

(require 'ert)

(use-package languagetool
	:demand
	:load-path "~/projets/programmation/emacs/languagetool.el"
  :commands (
						 languagetool-clear-suggestions
             languagetool-correct-at-point
             languagetool-correct-buffer
						 languagetool-correct-buffer-forward
             languagetool-set-language
             languagetool-server-mode
						 )
	:hook
  (text-mode-hook . languagetool-server-mode)
  (org-mode-hook . languagetool-server-mode)
  (markdown-mode-hook . languagetool-server-mode)
	:bind (
				 ("C-c s" . languagetool-correct-at-point)
				 ("C-c C-s" . languagetool-correct-buffer-forward)
				 ("C-<f10>" . languagetool-server-mode)
				 ("<f10>" . languagetool-server-check-region-around-point)
				 )
  :custom
	(languagetool-correction-language "fr")
  (languagetool-java-arguments '("-dfile.encoding=utf-8"))
	(languagetool-server-url "http://localhost")
	(languagetool-server-port 8081)
	(languagetool-hint-idle-delay 0.5)
	(languagetool-server-check-delay 1.5)
	(languagetool-server-lines-before 10)
	(languagetool-server-lines-after 10)
	(languagetool-correction-keys (string-to-vector "auienrstdoygov123456789"))
	)

(ert-deftest languagetool-test-region-around-point-middle ()
  "Test extraction arround the points in the middle of the buffer."
  (with-temp-buffer
    (insert "Line 1\nLine 2\nLine 3\nLine 4\nLine 5\nLine 6\nLine 7\n")
    (goto-char (point-min))
    (forward-line 3) ;; Cursor sur Line 4
    (let* ((region (languagetool-server-region-around-point 2 2))
           (text (buffer-substring-no-properties (car region) (cdr region))))
      (should (string= text "Line 2\nLine 3\nLine 4\nLine 5\nLine 6"))
			)
		)
	)

(ert-deftest languagetool-test-region-around-point-beginning ()
  "Test extraction around the points at the beginning of the buffer."
  (with-temp-buffer
    (insert "Line 1\nLine 2\nLine 3\nLine 4\nLine 5\n")
    (goto-char (point-min)) ;; Cursor sur Line 1
    (let* ((region (languagetool-server-region-around-point 2 2))
           (text (buffer-substring-no-properties (car region) (cdr region))))
      (should (string= text "Line 1\nLine 2\nLine 3"))
			)
		)
	)

(ert-deftest languagetool-test-region-around-point-end ()
  "Test extraction around the points at the end of the buffer."
  (with-temp-buffer
    (insert "Line 1\nLine 2\nLine 3\nLine 4\nLine 5\n")
    (goto-char (point-max))
    (forward-line -1) ;; Cursor sur Line 5
    (let* ((region (languagetool-server-region-around-point 2 2))
           (text (buffer-substring-no-properties (car region) (cdr region))))
      (should (string= text "Line 3\nLine 4\nLine 5\n"))
			)
		)
	)

(ert-deftest languagetool-server-parse-request-test ()
  "Test that languagetool-server-parse-request returns correct alist for region."
  (with-temp-buffer
    (insert "Ceci est un test.\nDeuxième ligne.")
    (let ((languagetool-correction-language "fr")
          (languagetool-mother-tongue nil)
          (languagetool-api-key nil)
          (languagetool-username nil)
          (languagetool-suggestion-level nil)
          (languagetool-disabled-rules nil))
      (let ((alist (languagetool-server-parse-request 1 10)))
				(should (member '("language" "fr") alist))
				(should (member `("text" ,(buffer-substring-no-properties 1 10)) alist))
				)
			)
		)
	)

(defun languagetool-test-callback (_status orig-buffer region-start)
  "Callback de test pour url-retrieve. Affiche la réponse JSON brute."
  (goto-char (point-min))
  (re-search-forward "\n\n" nil 'move) sauter les headers
  (let ((json (buffer-substring-no-properties (point) (point-max))))
    (message "Réponse JSON: %s" json)
    (message "region-start: %d" region-start)
    ;; Optionnel : parser le JSON
    (let ((parsed (json-read-from-string json)))
      (message "JSON parsé: %S" parsed))
    ))

(defun languagetool-test-send-request ()
  "Test d'envoi de requête à LanguageTool Server sur une région."
  (interactive)
  (let ((start (+ (point-min) 5))
        (end (min (+ (point-min) 40) (point-max))))
		(message (number-to-string start))
		(message (number-to-string end))
    (let ((url-request-method "POST")
          (url-request-data (url-build-query-string
                             (languagetool-server-parse-request start end))))
      (url-retrieve
       (url-encode-url (format "%s:%d/v2/check" languagetool-server-url languagetool-server-port))
       #'languagetool-test-callback
       (list (current-buffer) start)
       t))))

;; Example predicate: ignore words longer than 20 characters
(defun my-ignore-long-words-p (word)
  "Ignore words longer than 20 characters."
  (> (length word) 20))

;; Add the predicate
(add-to-list 'languagetool-core-correct-predicates #'my-ignore-long-words-p)

;; Remove the predicate
;; (setq languagetool-core-correct-predicates
;;       (remove #'my-ignore-long-words-p languagetool-core-correct-predicates))


(ert-deftest languagetool-test-dict-file-path ()
  "Test that languagetool-core--dict-file returns the correct path."
  (let ((languagetool-correction-language "fr")
        (languagetool-dict-directory "/tmp/languagetool-test-dir"))
    (should (string= (languagetool-core--dict-file)
                     "/tmp/languagetool-test-dir/languagetool-dict-fr.txt"))))

(ert-deftest languagetool-test-word-in-dict-file-p ()
  "Test that languagetool-core--word-in-dict-file-p finds a word in the dictionary file."
  (let* ((languagetool-correction-language "fr")
         (languagetool-dict-directory (make-temp-file "lt-dict-dir" t))
         (dict-file (languagetool-core--dict-file)))
    (with-temp-file dict-file
      (insert "bonjour\nsalut\n"))
		(languagetool-core-load-dict-file)
    (should (languagetool-core--word-in-dict-file-p "bonjour"))
    (should-not (languagetool-core--word-in-dict-file-p "hello"))))

(ert-deftest languagetool-test-core-correct-p-predicates ()
  "Test languagetool-core-correct-p with multiple predicates."
  (let ((languagetool-correction-language "fr")
        (languagetool-dict-directory (make-temp-file "lt-dict-dir" t))
        (languagetool-core-correct-predicates nil))
    ;; Add a word to the dictionary file
    (with-temp-file (languagetool-core--dict-file)
      (insert "bonjour\n"))
		(languagetool-core-load-dict-file)
    ;; Add the dictionary predicate
    (add-to-list 'languagetool-core-correct-predicates #'languagetool-core--word-in-dict-file-p)
    ;; Add a custom predicate: ignore words longer than 10 characters
    (defun my-ignore-long-words-p (word)
      (> (length word) 10))
    (add-to-list 'languagetool-core-correct-predicates #'my-ignore-long-words-p)
    ;; Should ignore "bonjour" (in dict)
    (should (languagetool-core-correct-p "bonjour"))
    ;; Should ignore "supercalifragilistic" (long word)
    (should (languagetool-core-correct-p "supercalifragilistic"))
    ;; Should not ignore "chat"
    (should-not (languagetool-core-correct-p "chat"))))



(ert-deftest languagetool-test-rules-json-load-save ()
  "Test loading and saving the LanguageTool rules JSON."
  (let* ((temp-dir (make-temp-file "lt-rules-dir" t))
         (languagetool-dict-directory temp-dir)
         (languagetool-rules-json-path (expand-file-name "languagetool-rules.json" temp-dir)))
    (unwind-protect
        (let ((rules (make-hash-table :test 'equal)))
          (puthash "/tmp/test1.txt" '("RULE_A" "RULE_B") rules)
          (puthash "/tmp/test2.txt" '("RULE_C") rules)
          (languagetool-save-rules-json rules)
          (let ((loaded (languagetool-load-rules-json)))
            (should (equal (gethash "/tmp/test1.txt" loaded) '("RULE_A" "RULE_B")))
            (should (equal (gethash "/tmp/test2.txt" loaded) '("RULE_C")))))
      ;; Cleanup
      (when (file-exists-p languagetool-rules-json-path)
        (delete-file languagetool-rules-json-path))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))



(ert-deftest languagetool-test-get-rules-for-file ()
  "Test getting disabled rules for a specific file."
  (let* ((temp-dir (make-temp-file "lt-rules-dir" t))
         (languagetool-dict-directory temp-dir)
         (languagetool-rules-json-path (expand-file-name "languagetool-rules.json" temp-dir)))
    (unwind-protect
        (let ((rules (make-hash-table :test 'equal)))
          (puthash "/tmp/test3.txt" '("RULE_X" "RULE_Y") rules)
          (languagetool-save-rules-json rules)
          (should (equal (languagetool-get-rules-for-file "/tmp/test3.txt")
                         '("RULE_X" "RULE_Y")))
          (should (equal (languagetool-get-rules-for-file "/tmp/unknown.txt")
                         '())))
      ;; Cleanup
      (when (file-exists-p languagetool-rules-json-path)
        (delete-file languagetool-rules-json-path))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))



(ert-deftest languagetool-test-update-rule-for-file ()
  "Test adding and removing rules for a file."
  (let* ((temp-dir (make-temp-file "lt-rules-dir" t))
         (languagetool-dict-directory temp-dir)
         (languagetool-rules-json-path (expand-file-name "languagetool-rules.json" temp-dir))
         (file "/tmp/test4.txt"))
    (unwind-protect
        (progn
          ;; Add a rule
          (languagetool-update-rule-for-file file "RULE_ADD")
          (should (member "RULE_ADD" (languagetool-get-rules-for-file file)))
          ;; Add another rule
          (languagetool-update-rule-for-file file "RULE_OTHER")
          (should (member "RULE_OTHER" (languagetool-get-rules-for-file file)))
          ;; Remove a rule
          (languagetool-update-rule-for-file file "RULE_ADD" t)
          (should-not (member "RULE_ADD" (languagetool-get-rules-for-file file)))
          ;; Remove a non-existing rule (should not error)
          (languagetool-update-rule-for-file file "RULE_UNKNOWN" t)
          (should-not (member "RULE_UNKNOWN" (languagetool-get-rules-for-file file))))
      ;; Cleanup
      (when (file-exists-p languagetool-rules-json-path)
        (delete-file languagetool-rules-json-path))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))



(ert-deftest languagetool-test-update-and-get-rules-for-current-buffer ()
  "Test updating and getting rules for the current buffer's file."
  (let* ((temp-dir (make-temp-file "lt-rules-dir" t))
         (languagetool-dict-directory temp-dir)
         (languagetool-rules-json-path (expand-file-name "languagetool-rules.json" temp-dir)))
    (unwind-protect
        (with-temp-buffer
          (let ((buffer-file-name "/tmp/test5.txt"))
            (languagetool-update-rule-for-current-buffer "RULE_CUR")
            (should (member "RULE_CUR" (languagetool-get-rules-for-current-buffer)))
            (languagetool-update-rule-for-current-buffer "RULE_CUR" t)
            (should-not (member "RULE_CUR" (languagetool-get-rules-for-current-buffer)))))
      ;; Cleanup
      (when (file-exists-p languagetool-rules-json-path)
        (delete-file languagetool-rules-json-path))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))


;; test.el ends here
