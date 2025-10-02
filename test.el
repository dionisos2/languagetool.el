;;; test.el --- tests  -*- lexical-binding: t -*-
;;; Commentary:
;;; Nothing special.

;;; Code:

(require 'ert)

(use-package languagetool
	:ensure nil
	:load-path "~/projets/programmation/emacs/languagetool.el"
  :commands (languagetool-clear-suggestions
             languagetool-correct-at-point
             languagetool-correct-buffer
						 languagetool-correct-buffer-forward
             languagetool-set-language
             languagetool-server-mode
             languagetool-server-start
             languagetool-server-stop)
  :custom
	(languagetool-correction-language "fr")
  (languagetool-java-arguments '("-Dfile.encoding=UTF-8"))
	(languagetool-server-url "http://localhost")
	(languagetool-server-port 8081)
	(languagetool-hint-idle-delay 2)
	(languagetool-correction-keys (string-to-vector "auienrstdoygov123456789"))
)

(ert-deftest languagetool-test-region-around-point-middle ()
  "Test extraction arround the points in the middle of the buffer."
  (with-temp-buffer
    (insert "Line 1\nLine 2\nLine 3\nLine 4\nLine 5\nLine 6\nLine 7\n")
    (goto-char (point-min))
    (forward-line 3) ; Cursor sur Line 4
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
    (goto-char (point-min)) ; Cursor sur Line 1
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
    (forward-line -1) ; Curseur sur Line 5
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
          (languagetool-disabled-rules nil)
          (languagetool-local-disabled-rules nil))
      (let ((alist (languagetool-server-parse-request 1 10)))
				(message "alist: %s" alist)
				(should (member '("language" "fr") alist))
				(should (member `("text" ,(buffer-substring-no-properties 1 10)) alist))
				)
			)
		)
	)

(defun languagetool-test-callback (_status orig-buffer region-start)
  "Callback de test pour url-retrieve. Affiche la réponse JSON brute."
  (goto-char (point-min))
  (re-search-forward "\n\n" nil 'move) ; sauter les headers
  (let ((json (buffer-substring-no-properties (point) (point-max))))
    (message "Réponse JSON: %s" json)
    (message "region-start: %d" region-start)
    ;; Optionnel : parser le JSON
    ;; (let ((parsed (json-read-from-string json)))
    ;;   (message "JSON parsé: %S" parsed))
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


;;; test.el ends here
