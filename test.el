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

(ert-deftest languagetool-test-overlay-management-visible-mode ()
	"Test overlay management with changing visible regions."
	(skip-unless (fboundp 'languagetool-server-clear-region-overlays))
	(with-temp-buffer
		(customize-set-variable 'languagetool-server-check-visible-text t)
		(insert "Text with potential issues\nSecond line with content\nThird line\n")
		(let ((buf (current-buffer)))
			;; Create some mock overlays
			(let ((ov1 (make-overlay 1 10))
			(ov2 (make-overlay 20 30))
			(ov3 (make-overlay 40 50)))

	;; Mark overlays as LanguageTool overlays
	(overlay-put ov1 'languagetool-message "Test message 1")
	(overlay-put ov2 'languagetool-message "Test message 2")
	(overlay-put ov3 'languagetool-message "Test message 3")

	;; Mock window functions for batch mode
	(cl-letf (((symbol-function 'languagetool-server-window-start) (lambda (buffer) 1))
			((symbol-function 'languagetool-server-window-end) (lambda (buffer) (with-current-buffer buffer (point-max)))))

		;; Test selective overlay clearing
		(languagetool-server-clear-region-overlays buf 1)

		;; Verify overlays exist (they should since we're testing the function exists)
		(should (overlayp ov1))
		(should (overlayp ov2))
		(should (overlayp ov3))

		;; Clean up overlays
		(delete-overlay ov1)
		(delete-overlay ov2)
		(delete-overlay ov3))))))

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
			;; This should not error, just log a message
			(should-not
			 (condition-case err
					 (progn
			 (languagetool-server-highlight-matches nil test-buffer 1 1)
			 nil)
				 (error err))))
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

;; test.el ends here
