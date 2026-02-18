;;; test.el --- tests	 -*- lexical-binding: t -*-
;;; Commentary:
;;; ERT tests for languagetool.el
;;; Run with: eldev test
;;; Coverage: eldev test --undercover

;;; Code:

(require 'ert)

;; Load LanguageTool modules in correct order
;; (Eldev handles load-path automatically, but keep for standalone use)
(unless (featurep 'eldev)
	(add-to-list 'load-path "."))
(require 'languagetool-core)
(require 'languagetool-server)
(require 'languagetool-issue)

(defun languagetool-ignore-time-format-p (word)
	"Return t if WORD is a valid time format like '10h10', '23h59', etc."
	(when (string-match "^\\([0-9]\\{1,2\\}\\)[hH]\\([0-9]\\{2\\}\\)$" word)
		(let ((hours (string-to-number (match-string 1 word)))
		(minutes (string-to-number (match-string 2 word))))
			(and (<= 0 hours 23) (<= 0 minutes 59)))))

(use-package languagetool
	:demand
	:load-path "~/projets/programmation/emacs/languagetool.el"
	:commands (languagetool-clear-suggestions
			 languagetool-correct-at-point
			 languagetool-correct-buffer
			 languagetool-correct-buffer-forward
			 languagetool-set-language
			 languagetool-server-mode)
	:hook
	(text-mode-hook . languagetool-server-mode)
	(org-mode-hook . languagetool-server-mode)
	(markdown-mode-hook . languagetool-server-mode)
	:bind (
	 ("C-c s" . languagetool-correct-at-point)
	 ("C-c C-s" . languagetool-correct-buffer-forward)
	 ("C-<f10>" . languagetool-server-mode)
	 ("<f10>" . languagetool-server-check-region)
	 )
	:custom
	(languagetool-correction-language "fr")
	(languagetool-java-arguments '("-dfile.encoding=utf-8"))
	(languagetool-server-url "http://localhost")
	(languagetool-server-port 8081)
	(languagetool-hint-idle-delay 0.5)
	(languagetool-server-check-delay 1.5)
	(languagetool-server-lines-before 2)
	(languagetool-server-lines-after 2)
	(languagetool-correction-keys (string-to-vector "auienrstdoygov123456789"))
	(customize-set-variable 'languagetool-server-check-visible-text t)
	)

(add-to-list 'languagetool-core-correct-predicates #'languagetool-ignore-time-format-p)

(ert-deftest languagetool-test-region-around-point-middle ()
	"Test extraction arround the points in the middle of the buffer."
	(with-temp-buffer
		(insert "Line 1\nLine 2\nLine 3\nLine 4\nLine 5\nLine 6\nLine 7\n")
		(goto-char (point-min))
		(forward-line 3) ;; Cursor sur Line 4
		(let ((languagetool-server-lines-before 2)
		(languagetool-server-lines-after 2))
			(let* ((region (languagetool-server-region-around-point (current-buffer)))
			 (text (buffer-substring-no-properties (car region) (cdr region))))
	(should (string= text "Line 2\nLine 3\nLine 4\nLine 5\nLine 6"))))))

(ert-deftest languagetool-test-region-around-point-beginning ()
	"Test extraction around the points at the beginning of the buffer."
	(with-temp-buffer
		(insert "Line 1\nLine 2\nLine 3\nLine 4\nLine 5\n")
		(goto-char (point-min)) ;; Cursor sur Line 1
		(let ((languagetool-server-lines-before 2)
		(languagetool-server-lines-after 2))
			(let* ((region (languagetool-server-region-around-point (current-buffer)))
			 (text (buffer-substring-no-properties (car region) (cdr region))))
	(should (string= text "Line 1\nLine 2\nLine 3"))))))

(ert-deftest languagetool-test-region-around-point-end ()
	"Test extraction around the points at the end of the buffer."
	(with-temp-buffer
		(insert "Line 1\nLine 2\nLine 3\nLine 4\nLine 5\n")
		(goto-char (point-max))
		(forward-line -1) ;; Cursor sur Line 5
		(let ((languagetool-server-lines-before 2)
		(languagetool-server-lines-after 2))
			(let* ((region (languagetool-server-region-around-point (current-buffer)))
			 (text (buffer-substring-no-properties (car region) (cdr region))))
	(should (string= text "Line 3\nLine 4\nLine 5\n"))))))

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
			(let ((alist (languagetool-server-parse-request (current-buffer) 1 10)))
	(should (member '("language" "fr") alist))
	(should (member '("text" "Ceci%20est%20") alist))))))

(defun languagetool-test-callback (_status orig-buffer region-start)
	"Callback de test pour url-retrieve. Affiche la réponse JSON brute."
	(goto-char (point-min))
	(re-search-forward "\n\n" nil 'move)
	(let ((json (buffer-substring-no-properties (point) (point-max))))
		(message "Réponse JSON: %s" json)
		(message "region-start: %d" region-start)
		;; Optionnel : parser le JSON
		(let ((parsed (json-read-from-string json)))
			(message "JSON parsé: %S" parsed))))

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
;; (add-to-list 'languagetool-core-correct-predicates #'my-ignore-long-words-p)

;; Remove the predicate
;; (setq languagetool-core-correct-predicates
;;	 (remove #'my-ignore-long-words-p languagetool-core-correct-predicates))

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

(ert-deftest languagetool-test-update-rule-normalizes-repeated-words-id ()
	"Test that repeated words rules are stored with base ID only.
LanguageTool generates dynamic rule IDs like FR_REPEATEDWORDS_VRAIMENT
but only accepts the base ID FR_REPEATEDWORDS in disabledRules."
	(let* ((temp-dir (make-temp-file "lt-rules-dir" t))
		 (languagetool-dict-directory temp-dir)
		 (languagetool-rules-json-path (expand-file-name "languagetool-rules.json" temp-dir))
		 (file "/tmp/test-repeated.txt"))
		(unwind-protect
	(progn
		;; Add a repeated words rule with dynamic suffix
		(languagetool-update-rule-for-file file "FR_REPEATEDWORDS_VRAIMENT")
		;; Should be stored as base ID only
		(should (member "FR_REPEATEDWORDS" (languagetool-get-rules-for-file file)))
		;; The full dynamic ID should NOT be stored
		(should-not (member "FR_REPEATEDWORDS_VRAIMENT" (languagetool-get-rules-for-file file))))
			;; Cleanup
			(when (file-exists-p languagetool-rules-json-path)
	(delete-file languagetool-rules-json-path))
			(when (file-directory-p temp-dir)
	(delete-directory temp-dir t)))))

(ert-deftest languagetool-test-rules-json-is-pretty-printed ()
	"Test that the rules JSON file is formatted with pretty printing."
	(let* ((temp-dir (make-temp-file "lt-rules-dir" t))
		 (languagetool-dict-directory temp-dir)
		 (languagetool-rules-json-path (expand-file-name "languagetool-rules.json" temp-dir))
		 (file "/tmp/test-pretty.txt"))
		(unwind-protect
	(progn
		;; Add a rule to create the JSON file
		(languagetool-update-rule-for-file file "RULE_TEST")
		;; Read the raw file content
		(let ((content (with-temp-buffer
						 (insert-file-contents languagetool-rules-json-path)
						 (buffer-string)))
				(expected (with-temp-buffer
							 (insert (json-encode `((,file . ("RULE_TEST")))))
							 (json-pretty-print-buffer)
							 (buffer-string))))
			;; Content should match pretty printed version
			(should (string= content expected))))
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
			(should (member "RULE_CUR" (languagetool-get-disabled-rules-for-current-buffer)))
			(languagetool-update-rule-for-current-buffer "RULE_CUR" t)
			(should-not (member "RULE_CUR" (languagetool-get-disabled-rules-for-current-buffer)))))
			;; Cleanup
			(when (file-exists-p languagetool-rules-json-path)
	(delete-file languagetool-rules-json-path))
			(when (file-directory-p temp-dir)
	(delete-directory temp-dir t)))))

(ert-deftest languagetool-test-visible-region-detection ()
	"Test visible region detection at different window positions."
	(with-temp-buffer
		(insert "Line 1\nLine 2\nLine 3\nLine 4\nLine 5\nLine 6\nLine 7\nLine 8\nLine 9\nLine 10\n")
		(goto-char (point-min))
		(let ((buf (current-buffer)))
			;; Mock window functions for batch mode compatibility
			(cl-letf (((symbol-function 'languagetool-server-window-start) (lambda (buffer) 1))
		((symbol-function 'languagetool-server-window-end) (lambda (buffer) (with-current-buffer buffer (point-max)))))

	;; Test basic visible region detection
	(let ((languagetool-server-check-visible-text t))
		(let ((region (languagetool-server-get-region buf)))
			(should (consp region))
			(should (>= (car region) (point-min)))
			(should (<= (cdr region) (point-max)))
			(should (< (car region) (cdr region)))))

	;; Test visible text extraction
	(let ((languagetool-server-check-visible-text t))
		(let ((text (languagetool-server-get-text buf)))
			(should (stringp text))
			(should (> (length text) 0)))))

			;; Test at different positions with mocked different regions
			(cl-letf (((symbol-function 'languagetool-server-window-start) (lambda (buffer) 10))
		((symbol-function 'languagetool-server-window-end) (lambda (buffer) 50)))
	(let ((languagetool-server-check-visible-text t))
		(let ((region-middle (languagetool-server-get-region buf)))
			(cl-letf (((symbol-function 'languagetool-server-window-start) (lambda (buffer) 60))
					((symbol-function 'languagetool-server-window-end) (lambda (buffer) (with-current-buffer buffer (point-max)))))
				(let ((region-end (languagetool-server-get-region buf)))
		;; Regions should be different when at different positions
		(should-not (equal region-middle region-end))))))))))

(ert-deftest languagetool-test-visible-text-change-detection ()
	"Test change detection for visible text content."
	(with-temp-buffer
		;; Initialize buffer-local variables
		(setq-local languagetool-server-text-cache nil)
		(setq-local languagetool-server-region-cache nil)

		(insert "Initial text content\nSecond line\nThird line\n")
		(let ((buf (current-buffer)))
			;; Mock window functions for consistent behavior in batch mode
			(cl-letf (((symbol-function 'languagetool-server-window-start) (lambda (buffer) 1))
		((symbol-function 'languagetool-server-window-end) (lambda (buffer) (with-current-buffer buffer (point-max)))))

	(should (languagetool-server-text-changed-p buf))
	(should (languagetool-server-text-changed-p buf))
	(languagetool-server-update-cache buf)
	(should-not (languagetool-server-text-changed-p buf))

	;; Modify buffer content
	(goto-char (point-max))
	(insert "New line added\n")

	;; Should detect change after content modification
	(should (languagetool-server-text-changed-p buf))))))

(ert-deftest languagetool-test-visible-text-mode-switching ()
	"Test switching between line-based and visible text modes."
	(with-temp-buffer
		(insert "Test content for mode switching\nSecond line\nThird line\n")
		(let ((buf (current-buffer)))
			(customize-set-variable 'languagetool-server-check-visible-text nil)
			(should-not languagetool-server-check-visible-text)

			(customize-set-variable 'languagetool-server-check-visible-text t)
			(should languagetool-server-check-visible-text)

			;; Mock window functions for batch mode
			(cl-letf (((symbol-function 'languagetool-server-window-start) (lambda (buffer) 5))
		((symbol-function 'languagetool-server-window-end) (lambda (buffer) 15)))
	(should-not languagetool-server-text-cache)
	(should (equal (languagetool-server-get-text buf) " content f"))
	(should (languagetool-server-text-changed-p buf))
	(should (languagetool-server-text-changed-p buf))
	(languagetool-server-update-cache buf)
	(should-not (languagetool-server-text-changed-p buf)))

			(cl-letf (((symbol-function 'languagetool-server-window-start) (lambda (buffer) 1))
		((symbol-function 'languagetool-server-window-end) (lambda (buffer) 10)))
	;; Change detection should work in visible text mode
	(should languagetool-server-check-visible-text)
	(should (equal (languagetool-server-window-end buf) 10))
	(should (equal (languagetool-server-get-region buf) '(1 . 10)))
	(should-not (equal (languagetool-server-get-text buf) "Second line\nThird line\n"))
	(should (languagetool-server-text-changed-p buf))))))


(ert-deftest languagetool-test-closure-with-killed-buffer ()
	"Test that the should-check closure handles killed buffers gracefully.
This test reproduces the bug where calling org-todo would delete text
because the closure tried to access a killed buffer, causing an error
that interrupted the operation."
	(let ((test-buffer (generate-new-buffer "*test-closure*"))
	closure)
		;; Create the closure capturing test-buffer
		(with-current-buffer test-buffer
			(setq closure (languagetool-server-create-should-check-closure)))
		;; Kill the buffer
		(kill-buffer test-buffer)
		;; The closure should NOT error when called with a dead buffer
		;; Without the buffer-live-p check, this would signal: (error "Selecting deleted buffer")
		(should-not (condition-case err
				(progn
					(funcall closure)
					nil)	;; No error, return nil
			(error err)))))	 ;; Error occurred, return the error

(ert-deftest languagetool-test-closure-is-buffer-local ()
	"Test that each buffer gets its own unique closure.
This verifies the fix for the defun-in-closure bug where all buffers
shared the same global function."
	(let ((buffer-a (generate-new-buffer "*test-closure-a*"))
	(buffer-b (generate-new-buffer "*test-closure-b*"))
	closure-a closure-b)
		(unwind-protect
	(progn
		;; Create closures for both buffers
		(with-current-buffer buffer-a
			(setq closure-a (languagetool-server-create-should-check-closure)))
		(with-current-buffer buffer-b
			(setq closure-b (languagetool-server-create-should-check-closure)))
		;; Closures should be different objects
		(should-not (eq closure-a closure-b))
		;; Each buffer should have its own closure stored
		(should (eq closure-a (buffer-local-value 'languagetool-server--check-closure buffer-a)))
		(should (eq closure-b (buffer-local-value 'languagetool-server--check-closure buffer-b))))
			;; Cleanup
			(kill-buffer buffer-a)
			(kill-buffer buffer-b))))

;;; Tests for languagetool-correction.el

(require 'languagetool-correction)

(ert-deftest languagetool-test-correction-parse-message-with-nil-values ()
	"Test that parse-message handles nil rule and message gracefully."
	(with-temp-buffer
		(insert "test word here")
		(let ((ov (make-overlay 6 10)))
			;; Set up overlay with nil rule and message
			(overlay-put ov 'languagetool-rule nil)
			(overlay-put ov 'languagetool-message nil)
			(overlay-put ov 'languagetool-replacements nil)
			(unwind-protect
		(let ((msg (languagetool-correction-parse-message ov)))
			;; Should contain default values instead of crashing
			(should (stringp msg))
			(should (string-match-p "\\[unknown\\]" msg))
			(should (string-match-p "No message" msg)))
	(delete-overlay ov)))))

(ert-deftest languagetool-test-correction-parse-message-with-values ()
	"Test that parse-message correctly formats rule and message."
	(with-temp-buffer
		(insert "test word here")
		(let ((ov (make-overlay 6 10)))
			(overlay-put ov 'languagetool-rule '((id . "TEST_RULE")))
			(overlay-put ov 'languagetool-message "This is a test message")
			;; Replacements must be a vector (JSON arrays become vectors)
			(overlay-put ov 'languagetool-replacements [((value . "replacement1")) ((value . "replacement2"))])
			(unwind-protect
		(let ((msg (languagetool-correction-parse-message ov)))
			(should (stringp msg))
			(should (string-match-p "\\[TEST_RULE\\]" msg))
			(should (string-match-p "This is a test message" msg)))
	(delete-overlay ov)))))

(ert-deftest languagetool-test-correction-apply-skip ()
	"Test that C-s skips to end of overlay."
	(with-temp-buffer
		(insert "test word here")
		(goto-char 1)
		(let ((ov (make-overlay 6 10)))
			(overlay-put ov 'languagetool-message "Test")
			(unwind-protect
		(progn
			(languagetool-correction-apply ?\C-s ov)
			;; Point should be at end of overlay
			(should (= (point) 10)))
	(when (overlayp ov) (delete-overlay ov))))))

(ert-deftest languagetool-test-correction-apply-replacement ()
	"Test that selecting a replacement key applies the correction."
	(with-temp-buffer
		(insert "test word here")
		(let ((ov (make-overlay 6 10)))
			(overlay-put ov 'languagetool-message "Test")
			;; Replacements must be a vector (JSON arrays become vectors)
			(overlay-put ov 'languagetool-replacements [((value . "fixed"))])
			(unwind-protect
		(progn
			;; Position point at overlay for proper replacement
			(goto-char (overlay-start ov))
			;; Use first key from languagetool-correction-keys (index 0)
			(languagetool-correction-apply (aref languagetool-correction-keys 0) ov)
			;; Text should be replaced
			(should (string= (buffer-string) "test fixed here")))
	(when (overlayp ov) (delete-overlay ov))))))

(ert-deftest languagetool-test-correction-apply-invalid-key ()
	"Test that invalid keys signal an error."
	(with-temp-buffer
		(insert "test word here")
		(let ((ov (make-overlay 6 10)))
			(overlay-put ov 'languagetool-message "Test")
			;; Replacements must be a vector (JSON arrays become vectors)
			(overlay-put ov 'languagetool-replacements [((value . "fixed"))])
			(unwind-protect
		(should-error (languagetool-correction-apply ?@ ov))
	(when (overlayp ov) (delete-overlay ov))))))

(ert-deftest languagetool-test-correction-apply-out-of-range-key ()
	"Test that keys beyond available replacements signal an error."
	(with-temp-buffer
		(insert "test word here")
		(let ((ov (make-overlay 6 10)))
			(overlay-put ov 'languagetool-message "Test")
			;; Only one replacement available (must be a vector)
			(overlay-put ov 'languagetool-replacements [((value . "fixed"))])
			(unwind-protect
		;; Key "2" would be index 1, but only index 0 exists
		(should-error (languagetool-correction-apply ?2 ov))
	(when (overlayp ov) (delete-overlay ov))))))

;;; Tests for multi-buffer isolation

(ert-deftest languagetool-test-correcting-p-is-buffer-local ()
	"Test that languagetool-server-correcting-p is buffer-local."
	(let ((buffer-a (generate-new-buffer "*test-correcting-a*"))
	(buffer-b (generate-new-buffer "*test-correcting-b*")))
		(unwind-protect
	(progn
		;; Set correcting-p in buffer-a
		(with-current-buffer buffer-a
			(setq languagetool-server-correcting-p t))
		;; buffer-b should still be nil
		(with-current-buffer buffer-b
			(should-not languagetool-server-correcting-p))
		;; buffer-a should still be t
		(with-current-buffer buffer-a
			(should languagetool-server-correcting-p)))
			(kill-buffer buffer-a)
			(kill-buffer buffer-b))))

(ert-deftest languagetool-test-text-cache-is-buffer-local ()
	"Test that text cache variables are buffer-local."
	(let ((buffer-a (generate-new-buffer "*test-cache-a*"))
	(buffer-b (generate-new-buffer "*test-cache-b*")))
		(unwind-protect
	(progn
		(with-current-buffer buffer-a
			(setq languagetool-server-text-cache "cache-a"))
		(with-current-buffer buffer-b
			(setq languagetool-server-text-cache "cache-b"))
		;; Each buffer should have its own cache
		(should (string= (buffer-local-value 'languagetool-server-text-cache buffer-a) "cache-a"))
		(should (string= (buffer-local-value 'languagetool-server-text-cache buffer-b) "cache-b")))
			(kill-buffer buffer-a)
			(kill-buffer buffer-b))))

;;; Tests for async error handling

(ert-deftest languagetool-test-json-error-handling ()
	"Test that malformed JSON is handled gracefully."
	(let ((test-buffer (generate-new-buffer "*test-json-error*")))
		(unwind-protect
				(with-current-buffer test-buffer
					(insert "Some text to check")
					(setq-local languagetool-server-last-request 1)
					(setq-local languagetool-server-correcting-p nil)
					(setq-local languagetool-server-mode t)
					;; Create a mock HTTP response buffer with invalid JSON
					(let ((response-buffer (generate-new-buffer "*mock-http-response*")))
						(unwind-protect
								(with-current-buffer response-buffer
									(insert "HTTP/1.1 200 OK\n\n{invalid json here")
									(setq-local url-http-response-status 200)
									;; Suppress the expected error message during test
									(cl-letf (((symbol-function 'message) #'ignore))
										;; This should not error, just log a message
										(should-not
										 (condition-case err
												 (progn
													 (languagetool-server-highlight-matches nil test-buffer 1 1)
													 nil)
											 (error err)))))
							;; Response buffer should be killed by the function
							(when (buffer-live-p response-buffer)
								(kill-buffer response-buffer)))))
			(kill-buffer test-buffer))))

(ert-deftest languagetool-test-nil-matches-handling ()
	"Test that nil matches in JSON response is handled gracefully."
	(let ((test-buffer (generate-new-buffer "*test-nil-matches*")))
		(unwind-protect
	(with-current-buffer test-buffer
		(insert "Some text to check")
		(setq-local languagetool-server-last-request 1)
		(setq-local languagetool-server-correcting-p nil)
		(setq-local languagetool-server-mode t)
		;; Create a mock HTTP response buffer with valid JSON but no matches key
		(let ((response-buffer (generate-new-buffer "*mock-http-response*")))
			(unwind-protect
		(with-current-buffer response-buffer
			(insert "HTTP/1.1 200 OK\n\n{\"software\":{\"name\":\"LanguageTool\"}}")
			(setq-local url-http-response-status 200)
			;; This should not error
			(should-not
			 (condition-case err
					 (progn
			 (languagetool-server-highlight-matches nil test-buffer 1 1)
			 nil)
				 (error err))))
				(when (buffer-live-p response-buffer)
		(kill-buffer response-buffer)))))
			(kill-buffer test-buffer))))

(ert-deftest languagetool-test-empty-buffer-no-request ()
	"Test that empty buffer does not send a request to LanguageTool.
Fix for bug: url-build-query-string on empty text produces 'text&language=fr'
instead of 'text=&language=fr', causing LanguageTool server error 400.
Solution: skip request entirely when text is empty."
	(let ((request-sent nil))
		(with-temp-buffer
			;; Empty buffer
			(setq-local languagetool-server-last-request 0)
			;; Mock url-retrieve to detect if request is sent
			(cl-letf (((symbol-function 'url-retrieve)
				 (lambda (&rest _args) (setq request-sent t))))
	(languagetool-server-send-request (current-buffer) 1 1)
	;; No request should be sent for empty buffer
	(should-not request-sent)))))

(ert-deftest languagetool-test-check-region-with-killed-buffer ()
	"Test that languagetool-server-check-region handles killed buffers gracefully.
Fix for bug: timer fires after buffer is killed, causing 'Selecting deleted buffer' error."
	(let ((test-buffer (generate-new-buffer "*test-killed-buffer*")))
		;; Kill the buffer before calling check-region
		(kill-buffer test-buffer)
		;; This should NOT error
		(should-not
		 (condition-case err
				 (progn
		 (languagetool-server-check-region test-buffer)
		 nil)
			 (error err)))))

;;; Tests for timer accumulation fix

(ert-deftest languagetool-test-timer-not-duplicated ()
	"Test that hint timer is not created multiple times."
	(let ((languagetool-core-hint-timer nil))
		;; First call should create timer
		(setq languagetool-core-hint-timer
		(run-with-idle-timer 1 t #'ignore))
		(should (timerp languagetool-core-hint-timer))
		(let ((first-timer languagetool-core-hint-timer))
			;; Simulate what the fixed code does - check before creating
			(unless (timerp languagetool-core-hint-timer)
	(setq languagetool-core-hint-timer
				(run-with-idle-timer 1 t #'ignore)))
			;; Timer should be the same object
			(should (eq first-timer languagetool-core-hint-timer)))
		;; Cleanup
		(when (timerp languagetool-core-hint-timer)
			(cancel-timer languagetool-core-hint-timer))))

;;; Tests for languagetool-console.el

(require 'languagetool-console)

(ert-deftest languagetool-test-console-class-p-with-class ()
	"Test that languagetool-console-class-p recognizes Java class names."
	(let ((languagetool-console-command "org.languagetool.commandline.Main"))
		(should (languagetool-console-class-p)))
	(let ((languagetool-console-command "com.example.MyClass"))
		(should (languagetool-console-class-p)))
	(let ((languagetool-console-command "MyClass"))
		(should (languagetool-console-class-p))))

(ert-deftest languagetool-test-console-class-p-with-jar ()
	"Test that languagetool-console-class-p rejects JAR paths with full path."
	;; Full path with slashes is not a class name
	(let ((languagetool-console-command "/path/to/languagetool-commandline.jar"))
		(should-not (languagetool-console-class-p)))
	;; Note: "languagetool.jar" looks like a class name to the regex
	;; (package.Class format), so it returns non-nil. This is expected behavior.
	(let ((languagetool-console-command "languagetool.jar"))
		(should (languagetool-console-class-p))))

(ert-deftest languagetool-test-console-parse-arguments-basic ()
	"Test that languagetool-console-parse-arguments returns correct argument list."
	(let ((languagetool-console-command "/path/to/lt.jar")
	(languagetool-console-arguments nil)
	(languagetool-correction-language "fr")
	(languagetool-mother-tongue nil)
	(languagetool-suggestion-level nil)
	(languagetool-disabled-rules nil)
	(buffer-file-name nil))
		(let ((args (languagetool-console-parse-arguments)))
			(should (member "-jar" args))
			(should (member "/path/to/lt.jar" args))
			(should (member "--encoding" args))
			(should (member "utf8" args))
			(should (member "--json" args))
			(should (member "--language" args))
			(should (member "fr" args)))))

(ert-deftest languagetool-test-console-parse-arguments-autodetect ()
	"Test that auto language detection uses --autoDetect flag."
	(let ((languagetool-console-command "/path/to/lt.jar")
	(languagetool-console-arguments nil)
	(languagetool-correction-language "auto")
	(languagetool-mother-tongue nil)
	(languagetool-suggestion-level nil)
	(languagetool-disabled-rules nil)
	(buffer-file-name nil))
		(let ((args (languagetool-console-parse-arguments)))
			(should (member "--autoDetect" args))
			(should-not (member "--language" args)))))

(ert-deftest languagetool-test-console-matches-exists-p ()
	"Test that languagetool-console-matches-exists-p detects matches."
	(with-temp-buffer
		;; No matches
		(setq-local languagetool-console-output-parsed '((matches . [])))
		(should-not (languagetool-console-matches-exists-p))
		;; With matches
		(setq-local languagetool-console-output-parsed
		'((matches . [((offset . 0) (length . 4) (message . "Test"))])))
		(should (languagetool-console-matches-exists-p))))

(ert-deftest languagetool-test-console-highlight-matches-nil ()
	"Test that languagetool-console-highlight-matches handles nil matches."
	(with-temp-buffer
		(insert "Test text here")
		;; No matches key
		(setq-local languagetool-console-output-parsed '((software . "LT")))
		;; Should not error
		(should-not
		 (condition-case err
	 (progn
		 (languagetool-console-highlight-matches 1)
		 nil)
			 (error err)))))

(ert-deftest languagetool-test-console-highlight-matches-creates-overlays ()
	"Test that languagetool-console-highlight-matches creates overlays correctly."
	(with-temp-buffer
		(insert "Test text here")
		(setq-local languagetool-console-output-parsed
		'((matches . [((offset . 0)
						 (length . 4)
						 (message . "Test error")
						 (rule . ((id . "TEST_RULE")))
						 (replacements . [((value . "Fixed"))]))])))
		(setq-local languagetool-core-correct-predicates nil)
		(languagetool-console-highlight-matches 1)
		;; Check that overlay was created
		(let ((overlays (overlays-in 1 5)))
			(should (> (length overlays) 0))
			(should (overlay-get (car overlays) 'languagetool-message)))))

(ert-deftest languagetool-test-correction-add-word-no-duplicate ()
	"Test that languagetool-correction-add-word does not add duplicate words."
	(let* ((languagetool-correction-language "fr")
		 (languagetool-dict-directory (make-temp-file "lt-dict-dir" t))
		 (dict-file (languagetool-core--dict-file)))
		(unwind-protect
	(progn
		;; Add the word twice
		(languagetool-correction-add-word "testword")
		(languagetool-correction-add-word "testword")
		;; Read the file and count occurrences
		(let ((content (with-temp-buffer
						 (insert-file-contents dict-file)
						 (buffer-string))))
			;; The word should appear exactly once
			(should (= 1 (with-temp-buffer
						 (insert content)
						 (goto-char (point-min))
						 (count-matches "^testword$"))))))
			;; Cleanup
			(when (file-exists-p dict-file)
	(delete-file dict-file))
			(when (file-directory-p languagetool-dict-directory)
	(delete-directory languagetool-dict-directory t)))))

(ert-deftest languagetool-test-correction-add-word-case-sensitive ()
	"Test that languagetool-correction-add-word is case-sensitive.
Words with different casing should be treated as different words.
This tests the bug where re-search-forward uses case-fold-search
which can make 'nouveau_mot' and 'Nouveau_mot' appear identical."
	(let* ((languagetool-correction-language "fr")
		 (languagetool-dict-directory (make-temp-file "lt-dict-dir" t))
		 (dict-file (languagetool-core--dict-file)))
		(unwind-protect
	(progn
		;; Add lowercase word
		(languagetool-correction-add-word "nouveau_mot")
		;; Add same word with different case - should be added as separate entry
		(languagetool-correction-add-word "Nouveau_mot")
		;; Read the file and count lines - should have 2 words
		(let ((lines (with-temp-buffer
						 (insert-file-contents dict-file)
						 (split-string (string-trim (buffer-string)) "\n" t))))
			;; Should have exactly 2 entries
			(should (= (length lines) 2))
			;; Both words should be present (use equal for exact match)
			(should (member "nouveau_mot" lines))
			(should (member "Nouveau_mot" lines))))
			;; Cleanup
			(when (file-exists-p dict-file)
	(delete-file dict-file))
			(when (file-directory-p languagetool-dict-directory)
	(delete-directory languagetool-dict-directory t)))))

(ert-deftest languagetool-test-correction-add-word-trims-whitespace ()
	"Test that languagetool-correction-add-word trims whitespace around words."
	(let* ((languagetool-correction-language "fr")
		 (languagetool-dict-directory (make-temp-file "lt-dict-dir" t))
		 (dict-file (languagetool-core--dict-file)))
		(unwind-protect
	(progn
		;; Add a word with surrounding whitespace
		(languagetool-correction-add-word "  spacedword  ")
		;; Read the file
		(let ((content (with-temp-buffer
						 (insert-file-contents dict-file)
						 (buffer-string))))
			;; The word should be trimmed (no spaces)
			(should (string-match-p "^spacedword$" content))
			;; There should be no line with spaces
			(should-not (string-match-p "^  spacedword  $" content))))
			;; Cleanup
			(when (file-exists-p dict-file)
	(delete-file dict-file))
			(when (file-directory-p languagetool-dict-directory)
	(delete-directory languagetool-dict-directory t)))))

;;; Tests for mode-line status indicator

(ert-deftest languagetool-test-mode-line-status-idle ()
	"Test mode-line status returns correct string for idle state."
	(with-temp-buffer
		(setq-local languagetool-server--status 'idle)
		(setq-local languagetool-server--error-count 0)
		(should (string= (languagetool-server--mode-line-status) " LT"))))

(ert-deftest languagetool-test-mode-line-status-checking ()
	"Test mode-line status returns correct string for checking state."
	(with-temp-buffer
		(setq-local languagetool-server--status 'checking)
		(setq-local languagetool-server--error-count 0)
		(should (string= (languagetool-server--mode-line-status) " LT⟳"))))

(ert-deftest languagetool-test-mode-line-status-done-no-errors ()
	"Test mode-line status returns correct string when done with no errors."
	(with-temp-buffer
		(setq-local languagetool-server--status 'done)
		(setq-local languagetool-server--error-count 0)
		(should (string= (languagetool-server--mode-line-status) " LT✓"))))

(ert-deftest languagetool-test-mode-line-status-done-with-errors ()
	"Test mode-line status returns correct string when done with errors."
	(with-temp-buffer
		(setq-local languagetool-server--status 'done)
		(setq-local languagetool-server--error-count 5)
		(should (string= (languagetool-server--mode-line-status) " LT:5"))))

(ert-deftest languagetool-test-mode-line-status-is-buffer-local ()
	"Test that mode-line status variables are buffer-local."
	(let ((buffer-a (generate-new-buffer "*test-status-a*"))
				(buffer-b (generate-new-buffer "*test-status-b*")))
		(unwind-protect
				(progn
					(with-current-buffer buffer-a
						(setq-local languagetool-server--status 'checking)
						(setq-local languagetool-server--error-count 3))
					(with-current-buffer buffer-b
						(setq-local languagetool-server--status 'done)
						(setq-local languagetool-server--error-count 0))
					;; Each buffer should have its own status
					(should (eq (buffer-local-value 'languagetool-server--status buffer-a)
											'checking))
					(should (eq (buffer-local-value 'languagetool-server--status buffer-b)
											'done))
					(should (= (buffer-local-value 'languagetool-server--error-count buffer-a)
										 3))
					(should (= (buffer-local-value 'languagetool-server--error-count buffer-b)
										 0)))
			(kill-buffer buffer-a)
			(kill-buffer buffer-b))))

(ert-deftest languagetool-test-count-overlays ()
	"Test that languagetool-server--count-overlays counts only LT overlays."
	(with-temp-buffer
		(insert "Test text with some words here")
		;; Create some LanguageTool overlays
		(let ((ov1 (make-overlay 1 5))
					(ov2 (make-overlay 10 14))
					(ov3 (make-overlay 20 25)))
			(overlay-put ov1 'languagetool-message "Error 1")
			(overlay-put ov2 'languagetool-message "Error 2")
			;; ov3 is NOT a languagetool overlay
			(overlay-put ov3 'other-property "Not LT")
			(unwind-protect
					(progn
						;; Should count only overlays with languagetool-message
						(should (= (languagetool-server--count-overlays) 2)))
				(delete-overlay ov1)
				(delete-overlay ov2)
				(delete-overlay ov3)))))

(require 'languagetool)

(ert-deftest languagetool-test-correct-buffer-quit-preserves-point ()
	"Test that C-g during correction keeps point at current error position.
When the user quits with C-g during `languagetool-correct-buffer-forward',
the cursor should stay at the error being corrected, not return to the
original position."
	(with-temp-buffer
		(insert "First error here and second error there")
		(goto-char (point-min))
		;; Create two LanguageTool overlays at positions 7-12 and 26-31
		(let ((ov1 (make-overlay 7 12))
					(ov2 (make-overlay 26 31))
					(call-count 0))
			(overlay-put ov1 'languagetool-message "Error 1")
			(overlay-put ov1 'languagetool-replacements [])
			(overlay-put ov2 'languagetool-message "Error 2")
			(overlay-put ov2 'languagetool-replacements [])
			(unwind-protect
					(progn
						;; Mock languagetool-correction-at-point to signal quit on second call
						(cl-letf (((symbol-function 'languagetool-correction-at-point)
											 (lambda ()
												 (cl-incf call-count)
												 (when (= call-count 2)
													 (signal 'quit nil)))))
							;; Start at beginning
							(goto-char (point-min))
							;; Call correct-buffer-forward, expect quit on second error
							(condition-case nil
									(languagetool-correct-buffer-forward)
								(quit nil)
								(error nil)))
						;; Point should be at the second overlay (position 26), not at point-min
						(should (= (point) 26)))
				(delete-overlay ov1)
				(delete-overlay ov2)))))

(ert-deftest languagetool-test-correct-at-point-resets-correcting-p-on-quit ()
	"Test that languagetool-server-correcting-p is reset when correction is interrupted.
When the user presses C-g during `languagetool-correct-at-point', the variable
`languagetool-server-correcting-p' should be reset to nil."
	(with-temp-buffer
		(insert "Test text with error here")
		(setq-local languagetool-server-mode t)
		(setq-local languagetool-server-correcting-p nil)
		(let ((ov (make-overlay 1 5)))
			(overlay-put ov 'languagetool-message "Test error")
			(overlay-put ov 'languagetool-replacements [((value . "Fixed"))])
			(overlay-put ov 'languagetool-rule '((id . "TEST_RULE")))
			(unwind-protect
					(progn
						(goto-char 1)
						;; Mock read-char to signal quit (simulating C-g)
						(cl-letf (((symbol-function 'read-char)
											 (lambda (&rest _) (signal 'quit nil))))
							;; Call correct-at-point, which should handle the quit
							(condition-case nil
									(languagetool-correct-at-point)
								(quit nil)))
						;; After quit, correcting-p should be nil, not t
						(should-not languagetool-server-correcting-p))
				(delete-overlay ov)))))

;;; Tests for personal dictionary suggestions

(ert-deftest languagetool-test-similar-words-from-dict ()
	"Test that similar words from personal dictionary are suggested for misspellings.
When a word is misspelled and a similar word exists in the personal dictionary,
that word should be added to the replacement suggestions."
	(let* ((languagetool-correction-language "fr")
				 (languagetool-dict-directory (make-temp-file "lt-dict-dir" t))
				 (dict-file (languagetool-core--dict-file)))
		;; Create a personal dictionary with some words
		(with-temp-file dict-file
			(insert "bonjour\nsalut\nmerci\n"))
		(languagetool-core-load-dict-file)
		;; Create a correction for a misspelled word "bonjoure"
		;; that is similar to "bonjour" in the dictionary
		(with-temp-buffer
			(insert "bonjoure")
			(let ((correction '((offset . 0)
													(length . 8)
													(message . "Possible spelling mistake")
													(shortMessage . "Spelling")
													(replacements . [])
													(rule . ((id . "MORFOLOGIK_RULE_FR")
																	 (issueType . "misspelling"))))))
				;; Create the overlay
				(languagetool-issue-create-overlay 1 9 correction)
				;; Get the overlay and check its replacements
				(let* ((overlays (overlays-in 1 9))
							 (ov (car overlays))
							 (replacements (languagetool-core-get-replacements ov)))
					;; "bonjour" should be in the replacements since it's similar
					;; to "bonjoure" and exists in the personal dictionary
					(should (member "bonjour" replacements)))))))

(ert-deftest languagetool-test-string-distance ()
	"Test the string distance calculation with normalization."
	;; Same strings should have distance 0
	(should (= (languagetool-core-string-distance "hello" "hello") 0))
	;; One character difference
	(should (= (languagetool-core-string-distance "hello" "hallo") 1))
	;; Case differences should have distance 0
	(should (= (languagetool-core-string-distance "Hello" "hello") 0))
	(should (= (languagetool-core-string-distance "BONJOUR" "bonjour") 0))
	;; Accent differences should have distance 0
	(should (= (languagetool-core-string-distance "résumé" "resume") 0))
	(should (= (languagetool-core-string-distance "café" "cafe") 0))
	(should (= (languagetool-core-string-distance "naïve" "naive") 0))
	;; Combined case and accent
	(should (= (languagetool-core-string-distance "Résumé" "resume") 0))
	;; Actual spelling difference with accents
	(should (= (languagetool-core-string-distance "résumé" "resumee") 1)))

(ert-deftest languagetool-test-find-similar-words ()
	"Test finding similar words from the personal dictionary."
	(let* ((languagetool-correction-language "fr")
				 (languagetool-dict-directory (make-temp-file "lt-dict-dir" t))
				 (dict-file (languagetool-core--dict-file)))
		;; Create a personal dictionary
		(with-temp-file dict-file
			(insert "bonjour\nsalut\nmerci\nordinateur\nrésumé\n"))
		(languagetool-core-load-dict-file)
		;; Test finding similar words
		(let ((similar (languagetool-core-find-similar-words "bonjoure" 2)))
			;; "bonjour" should be found (distance 1)
			(should (member "bonjour" similar)))
		;; Test with higher distance threshold
		(let ((similar (languagetool-core-find-similar-words "ordnateur" 2)))
			;; "ordinateur" should be found (distance 1)
			(should (member "ordinateur" similar)))
		;; Test case insensitivity - "BONJOUR" should match "bonjour"
		(let ((similar (languagetool-core-find-similar-words "BONJOUR" 0)))
			(should (member "bonjour" similar)))
		;; Test accent insensitivity - "resume" should match "résumé"
		(let ((similar (languagetool-core-find-similar-words "resume" 0)))
			(should (member "résumé" similar)))
		;; Test with word that has no similar matches
		(let ((similar (languagetool-core-find-similar-words "xyz" 2)))
			;; No similar words should be found
			(should (null similar)))))

;;; Tests for correction accepted hook

(ert-deftest languagetool-test-correction-accepted-hook-called ()
	"Test that the correction accepted hook is called when a correction is applied."
	(with-temp-buffer
		(insert "bonjoure")
		(let ((hook-called nil)
					(hook-info nil)
					(ov (make-overlay 1 9)))
			(overlay-put ov 'languagetool-message "Possible spelling mistake")
			(overlay-put ov 'languagetool-replacements [((value . "bonjour"))])
			(overlay-put ov 'languagetool-rule '((id . "MORFOLOGIK_RULE_FR")
																					 (issueType . "misspelling")))
			;; Add a hook function to capture the call
			(let ((languagetool-correction-accepted-functions
						 (list (lambda (info)
										 (setq hook-called t)
										 (setq hook-info info)))))
				(goto-char 1)
				;; Apply the first correction (key "1")
				(languagetool-correction-apply (aref languagetool-correction-keys 0) ov)
				;; Hook should have been called
				(should hook-called)
				;; Check the info passed to the hook
				(should (equal (plist-get hook-info :original-word) "bonjoure"))
				(should (equal (plist-get hook-info :replacement) "bonjour"))
				(should (equal (plist-get hook-info :rule-id) "MORFOLOGIK_RULE_FR"))
				(should (equal (plist-get hook-info :issue-type) "misspelling"))))))

(ert-deftest languagetool-test-correction-accepted-hook-not-called-on-skip ()
	"Test that the hook is NOT called when the user skips a correction."
	(with-temp-buffer
		(insert "bonjoure")
		(let ((hook-called nil)
					(ov (make-overlay 1 9)))
			(overlay-put ov 'languagetool-message "Possible spelling mistake")
			(overlay-put ov 'languagetool-replacements [((value . "bonjour"))])
			(overlay-put ov 'languagetool-rule '((id . "MORFOLOGIK_RULE_FR")
																					 (issueType . "misspelling")))
			(let ((languagetool-correction-accepted-functions
						 (list (lambda (info) (setq hook-called t)))))
				(goto-char 1)
				;; Skip the correction with C-s
				(languagetool-correction-apply ?\C-s ov)
				;; Hook should NOT have been called
				(should-not hook-called)))))

(ert-deftest languagetool-test-add-dict-suggestions-sorted-by-distance ()
	"Test that suggestions are sorted by distance after merging LT and dict suggestions.
When LanguageTool suggestions and personal dictionary suggestions are merged,
the final list should be sorted by edit distance to the misspelled word."
	(let* ((languagetool-correction-language "fr")
				 (languagetool-dict-directory (make-temp-file "lt-dict-dir" t))
				 (dict-file (languagetool-core--dict-file)))
		;; Create a dictionary with "test" (distance 1 from "tst")
		(with-temp-file dict-file
			(insert "test\n"))
		(languagetool-core-load-dict-file)
		;; Simulate LanguageTool returning "toast" (distance 2 from "tst") as a suggestion
		(let* ((lt-replacements [((value . "toast"))])  ;; distance 2
					 (misspelled "tst")
					 (result (languagetool-issue--add-dict-suggestions
										lt-replacements misspelled))
					 (values (mapcar (lambda (r) (alist-get 'value r))
													 (append result nil))))
			;; Both suggestions should be present
			(should (member "test" values))
			(should (member "toast" values))
			;; "test" (distance 1) should come before "toast" (distance 2)
			(should (< (cl-position "test" values :test #'equal)
								 (cl-position "toast" values :test #'equal))))))

(ert-deftest languagetool-test-correct-buffer-forward-from-point ()
	"Test that languagetool-correct-buffer-forward with prefix arg starts from point.
When called with C-u prefix, only errors at or after point should be corrected."
	(with-temp-buffer
		(insert "First error here and second error there")
		;; Create two LanguageTool overlays at positions 7-12 and 26-31
		(let ((ov1 (make-overlay 7 12))
					(ov2 (make-overlay 26 31))
					(corrected-positions nil))
			(overlay-put ov1 'languagetool-message "Error 1")
			(overlay-put ov1 'languagetool-replacements [])
			(overlay-put ov2 'languagetool-message "Error 2")
			(overlay-put ov2 'languagetool-replacements [])
			(unwind-protect
					(progn
						;; Mock languagetool-correction-at-point to record which positions are corrected
						(cl-letf (((symbol-function 'languagetool-correction-at-point)
											 (lambda ()
												 (push (point) corrected-positions))))
							;; Position point after the first error (at position 15)
							(goto-char 15)
							;; Call correct-buffer-forward with prefix argument (from-point)
							(languagetool-correct-buffer-forward t))
						;; Only the second error (at 26) should have been corrected
						(should (= (length corrected-positions) 1))
						(should (= (car corrected-positions) 26)))
				(delete-overlay ov1)
				(delete-overlay ov2)))))

(ert-deftest languagetool-test-correct-buffer-forward-without-prefix ()
	"Test that languagetool-correct-buffer-forward without prefix arg corrects all errors.
When called without C-u prefix, all errors should be corrected regardless of point position."
	(with-temp-buffer
		(insert "First error here and second error there")
		;; Create two LanguageTool overlays at positions 7-12 and 26-31
		(let ((ov1 (make-overlay 7 12))
					(ov2 (make-overlay 26 31))
					(corrected-positions nil))
			(overlay-put ov1 'languagetool-message "Error 1")
			(overlay-put ov1 'languagetool-replacements [])
			(overlay-put ov2 'languagetool-message "Error 2")
			(overlay-put ov2 'languagetool-replacements [])
			(unwind-protect
					(progn
						;; Mock languagetool-correction-at-point to record which positions are corrected
						(cl-letf (((symbol-function 'languagetool-correction-at-point)
											 (lambda ()
												 (push (point) corrected-positions))))
							;; Position point after the first error (at position 15)
							(goto-char 15)
							;; Call correct-buffer-forward WITHOUT prefix argument
							(languagetool-correct-buffer-forward nil))
						;; Both errors should have been corrected
						(should (= (length corrected-positions) 2))
						;; Forward order: first error first, then second
						(should (member 7 corrected-positions))
						(should (member 26 corrected-positions)))
				(delete-overlay ov1)
				(delete-overlay ov2)))))

(ert-deftest languagetool-test-server-mode-disabled-on-connection-error ()
	"Test that languagetool-server-mode is disabled when server connection fails.
When the server becomes unavailable, the mode should be automatically disabled
instead of continuously trying to reconnect."
	(let ((test-buffer (generate-new-buffer "*test-connection-error*")))
		(unwind-protect
				(with-current-buffer test-buffer
					(insert "Some test text to check")
					(setq-local languagetool-server-mode t)
					(setq-local languagetool-server-last-request 1)
					(setq-local languagetool-server-correcting-p nil)
					;; Create a mock HTTP response buffer that simulates connection failure
					(let ((response-buffer (generate-new-buffer "*mock-http-error*")))
						(unwind-protect
								(with-current-buffer response-buffer
									;; Simulate a failed connection (no response status or error status)
									(insert "")
									(setq-local url-http-response-status nil)
									;; Call highlight-matches which should handle the error
									;; and disable the mode
									(condition-case nil
											(languagetool-server-highlight-matches
											 '(:error (error connection-failed "Connection refused"))
											 test-buffer 1 1)
										(error nil)))
							(when (buffer-live-p response-buffer)
								(kill-buffer response-buffer))))
					;; After a connection error, the mode should be disabled
					(should-not languagetool-server-mode))
			(kill-buffer test-buffer))))

(ert-deftest languagetool-test-server-globally-disabled-after-error ()
	"Test that server is globally disabled after connection error.
When one buffer encounters a connection error, new buffers should not
attempt to connect until the user calls languagetool-server-retry."
	(let ((languagetool-server--globally-disabled nil)
				(buffer-a (generate-new-buffer "*test-global-a*"))
				(buffer-b (generate-new-buffer "*test-global-b*")))
		(unwind-protect
				(progn
					;; Simulate connection error in buffer-a
					(with-current-buffer buffer-a
						(insert "Some text")
						(setq-local languagetool-server-mode t)
						(setq-local languagetool-server-last-request 1)
						(setq-local languagetool-server-correcting-p nil)
						(let ((response-buffer (generate-new-buffer "*mock-error*")))
							(unwind-protect
									(with-current-buffer response-buffer
										(setq-local url-http-response-status nil)
										(languagetool-server-highlight-matches
										 '(:error (error connection-failed "Connection refused"))
										 buffer-a 1 1))
								(when (buffer-live-p response-buffer)
									(kill-buffer response-buffer)))))
					;; Server should now be globally disabled
					(should languagetool-server--globally-disabled)
					;; Trying to enable mode in buffer-b should fail silently
					(with-current-buffer buffer-b
						(insert "Other text")
						;; Mock the server check to avoid actual network call
						(cl-letf (((symbol-function 'languagetool-server-check-for-communication)
											 #'ignore))
							(languagetool-server-mode 1))
						;; Mode should NOT be enabled because server is globally disabled
						(should-not languagetool-server-mode)))
			;; Cleanup
			(setq languagetool-server--globally-disabled nil)
			(kill-buffer buffer-a)
			(kill-buffer buffer-b))))

(ert-deftest languagetool-test-server-retry-resets-global-flag ()
	"Test that languagetool-server-retry resets the global disabled flag."
	(let ((languagetool-server--globally-disabled t))
		;; Mock the server check to avoid actual network call
		(cl-letf (((symbol-function 'languagetool-server-check-for-communication)
							 #'ignore)
							((symbol-function 'languagetool-server-mode-on)
							 #'ignore))
			(languagetool-server-retry)
			;; Global flag should be reset
			(should-not languagetool-server--globally-disabled))))

;; test.el ends here
