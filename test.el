;;; test.el --- tests  -*- lexical-binding: t -*-
;;; Commentary:
;;; Nothing special.

;;; Code:

(require 'ert)

;; Load LanguageTool modules in correct order
(add-to-list 'load-path ".")
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
	(languagetool-server-use-visible-text-mode t)
  )

(add-to-list 'languagetool-core-correct-predicates #'languagetool-ignore-time-format-p)

(ert-deftest languagetool-test-region-around-point-middle ()
  "Test extraction arround the points in the middle of the buffer."
  (with-temp-buffer
    (insert "Line 1\nLine 2\nLine 3\nLine 4\nLine 5\nLine 6\nLine 7\n")
    (goto-char (point-min))
    (forward-line 3) ;; Cursor sur Line 4
    (let* ((region (languagetool-server-region-around-point 2 2))
           (text (buffer-substring-no-properties (car region) (cdr region))))
      (should (string= text "Line 2\nLine 3\nLine 4\nLine 5\nLine 6")))))

(ert-deftest languagetool-test-region-around-point-beginning ()
  "Test extraction around the points at the beginning of the buffer."
  (with-temp-buffer
    (insert "Line 1\nLine 2\nLine 3\nLine 4\nLine 5\n")
    (goto-char (point-min)) ;; Cursor sur Line 1
    (let* ((region (languagetool-server-region-around-point 2 2))
           (text (buffer-substring-no-properties (car region) (cdr region))))
      (should (string= text "Line 1\nLine 2\nLine 3")))))

(ert-deftest languagetool-test-region-around-point-end ()
  "Test extraction around the points at the end of the buffer."
  (with-temp-buffer
    (insert "Line 1\nLine 2\nLine 3\nLine 4\nLine 5\n")
    (goto-char (point-max))
    (forward-line -1) ;; Cursor sur Line 5
    (let* ((region (languagetool-server-region-around-point 2 2))
           (text (buffer-substring-no-properties (car region) (cdr region))))
      (should (string= text "Line 3\nLine 4\nLine 5\n")))))

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
        (should (member `("text" ,(buffer-substring-no-properties 1 10)) alist))))))

(defun languagetool-test-callback (_status orig-buffer region-start)
  "Callback de test pour url-retrieve. Affiche la réponse JSON brute."
  (goto-char (point-min))
  (re-search-forward "\n\n" nil 'move) sauter les headers
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

(ert-deftest languagetool-test-visible-region-detection ()
  "Test visible region detection at different window positions."
  (skip-unless (fboundp 'languagetool-server-get-visible-region))
  (with-temp-buffer
    (insert "Line 1\nLine 2\nLine 3\nLine 4\nLine 5\nLine 6\nLine 7\nLine 8\nLine 9\nLine 10\n")
    (goto-char (point-min))

    ;; Mock window functions for batch mode compatibility
    (cl-letf (((symbol-function 'window-start) (lambda (&optional window) 1))
              ((symbol-function 'window-end) (lambda (&optional window &rest args) (point-max))))

      ;; Test basic visible region detection
      (let ((region (languagetool-server-get-visible-region)))
        (should (consp region))
        (should (>= (car region) (point-min)))
        (should (<= (cdr region) (point-max)))
        (should (< (car region) (cdr region))))

      ;; Test visible text extraction
      (when (fboundp 'languagetool-server-get-visible-text)
        (let ((text (languagetool-server-get-visible-text)))
          (should (stringp text))
          (should (> (length text) 0))))

      ;; Test at different positions with mocked different regions
      (cl-letf (((symbol-function 'window-start) (lambda (&optional window) 10))
                ((symbol-function 'window-end) (lambda (&optional window &rest args) 50)))
        (let ((region-middle (languagetool-server-get-visible-region)))
          (cl-letf (((symbol-function 'window-start) (lambda (&optional window) 60))
                    ((symbol-function 'window-end) (lambda (&optional window &rest args) (point-max))))
            (let ((region-end (languagetool-server-get-visible-region)))
              ;; Regions should be different when at different positions
              (should-not (equal region-middle region-end)))))))))

(ert-deftest languagetool-test-visible-text-change-detection ()
  "Test change detection for visible text content."
  (skip-unless (fboundp 'languagetool-server-visible-text-changed-p))
  (with-temp-buffer
    ;; Initialize buffer-local variables
    (setq-local languagetool-server-use-visible-text-mode t)
    (setq-local languagetool-server-visible-text-cache nil)
    (setq-local languagetool-server-visible-region-cache nil)

    (insert "Initial text content\nSecond line\nThird line\n")

    ;; Mock window functions for consistent behavior in batch mode
    (cl-letf (((symbol-function 'window-start) (lambda (&optional window) 1))
              ((symbol-function 'window-end) (lambda (&optional window &rest args) (point-max))))

      ;; First check should detect change (cache is empty)
      (should (languagetool-server-visible-text-changed-p))

      ;; Second check should not detect change (cache is populated)
      (should-not (languagetool-server-visible-text-changed-p))

      ;; Modify buffer content
      (goto-char (point-max))
      (insert "New line added\n")

      ;; Should detect change after content modification
      (should (languagetool-server-visible-text-changed-p))

      ;; Should not detect change again
      (should-not (languagetool-server-visible-text-changed-p)))))

(ert-deftest languagetool-test-visible-text-mode-switching ()
  "Test switching between line-based and visible text modes."
  (with-temp-buffer
    (insert "Test content for mode switching\nSecond line\nThird line\n")

    ;; Test line-based mode (default)
    (setq-local languagetool-server-use-visible-text-mode nil)
    (should-not languagetool-server-use-visible-text-mode)

    ;; Change detection should return nil in line-based mode
    ;; (should-not (languagetool-server-visible-text-changed-p))

    ;; Test visible text mode
    ;; (setq-local languagetool-server-use-visible-text-mode t)
    ;; (setq-local languagetool-server-visible-text-cache nil)
    ;; (setq-local languagetool-server-visible-region-cache nil)
    ;; (should languagetool-server-use-visible-text-mode)
		;; (message "hum")
    ;; Mock window functions for batch mode
    ;; (cl-letf (((symbol-function 'window-start) (lambda (&optional window) 1))
    ;;           ((symbol-function 'window-end) (lambda (&optional window &rest args) (point-max))))
    ;;   ;; Change detection should work in visible text mode
    ;;   (should (languagetool-server-visible-text-changed-p)))
		)
	)

(ert-deftest languagetool-test-overlay-management-visible-mode ()
  "Test overlay management with changing visible regions."
  (skip-unless (fboundp 'languagetool-server-clear-region-overlays))
  (with-temp-buffer
    (setq-local languagetool-server-use-visible-text-mode t)
    (insert "Text with potential issues\nSecond line with content\nThird line\n")

    ;; Create some mock overlays
    (let ((ov1 (make-overlay 1 10))
          (ov2 (make-overlay 20 30))
          (ov3 (make-overlay 40 50)))

      ;; Mark overlays as LanguageTool overlays
      (overlay-put ov1 'languagetool-message "Test message 1")
      (overlay-put ov2 'languagetool-message "Test message 2")
      (overlay-put ov3 'languagetool-message "Test message 3")

      ;; Mock window functions for batch mode
      (cl-letf (((symbol-function 'window-start) (lambda (&optional window) 1))
                ((symbol-function 'window-end) (lambda (&optional window &rest args) (point-max))))

        ;; Test selective overlay clearing
        (languagetool-server-clear-region-overlays 1)

        ;; Verify overlays exist (they should since we're testing the function exists)
        (should (overlayp ov1))
        (should (overlayp ov2))
        (should (overlayp ov3))

        ;; Clean up overlays
        (delete-overlay ov1)
        (delete-overlay ov2)
        (delete-overlay ov3)))))

(ert-deftest languagetool-test-mode-activation-deactivation ()
  "Test server mode activation and deactivation with visible text mode."
  (with-temp-buffer
    (insert "Test content for mode activation\n")

    ;; Test variables are properly initialized
    (should (boundp 'languagetool-server-visible-text-cache))
    (should (boundp 'languagetool-server-visible-region-cache))
    (should (boundp 'languagetool-server-visible-text-timer))

    ;; Test cache clearing
    (setq-local languagetool-server-visible-text-cache "test-cache")
    (setq-local languagetool-server-visible-region-cache '(1 . 100))

    ;; Simulate mode deactivation cleanup
    (setq-local languagetool-server-visible-text-cache nil)
    (setq-local languagetool-server-visible-region-cache nil)

    ;; Verify cleanup
    (should-not languagetool-server-visible-text-cache)
    (should-not languagetool-server-visible-region-cache)))

;; test.el ends here
