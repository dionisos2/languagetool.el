;;; languagetool-server.el --- LanguageTool Server commands -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2022	Joar Buitrago

;; Author: Joar Buitrago <jebuitragoc@unal.edu.co>
;; Keywords: grammar text docs tools convenience checker
;; URL: https://github.com/PillFall/Emacs-LanguageTool.el
;; Version: 1.3.0
;; Package-Requires: ((emacs "27.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.	 If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; LanguageTool commands and variables to use LanguageTool via
;; languagetool-server.jar or org.languagetool.server.HTTPServer.

;;; Code:

(require 'json)
(require 'url)
(require 'languagetool-java)
(require 'languagetool-core)
(require 'languagetool-issue)

;; Group definition:

(defcustom languagetool-server-lines-before 2
	"Number of lines before point to include in the region."
	:group 'languagetool-server
	:type 'integer)

(defcustom languagetool-server-lines-after 2
	"Number of lines after point to include in the region."
	:group 'languagetool-server
	:type 'integer)

(defgroup languagetool-server nil
	"Real time LanguageTool Server."
	:tag "Server"
	:prefix "languagetool-server-"
	:group 'languagetool)

;; Variable definitions:

(defcustom languagetool-server-command nil
	"LanguageTool Server path or class.

When using LanguageTool you should set this variable to either
the path or the class to call LanguageTool."
	:group 'languagetool-server
	:type '(choice
					file
					string))

(defcustom languagetool-server-arguments nil
	"LanguageTool Server extra arguments.

More info at http://wiki.languagetool.org/command-line-options."
	:group 'languagetool-server
	:type '(choice
					(const nil)
					(repeat string)))

(defcustom languagetool-server-url "http://localhost"
	"LanguageTool Server host URL."
	:group 'languagetool-server
	:type 'string)

(defcustom languagetool-server-port 8081
	"LanguageTool Server host port."
	:group 'languagetool-server
	:type 'integer)

(defcustom languagetool-server-max-timeout 5.0
	"LanguageTool Server maximum number of seconds to wait before giving up."
	:group 'languagetool-server
	:type 'number)

(defcustom languagetool-server-check-delay 3.0
	"LanguageTool Server delay time before checking again for issues."
	:group 'languagetool-server
	:type 'number)

(defvar-local languagetool-server-last-request 0
	"Count request so only the last one will highlight text."
	)

(defvar-local languagetool-server-check-timer nil
	"Hold idle time that send request to LanguageTool server.")

(defvar-local languagetool-server-text-cache nil
	"Cache of the last visible text content that was checked.

Used in visible text mode to avoid redundant checks when the visible
content hasn't actually changed.")

(defvar-local languagetool-server-region-cache nil
	"Cache of the last visible region boundaries (start . end).

Used in visible text mode to track when the visible region has changed
due to scrolling or window resizing.")

(defvar-local languagetool-server--check-closure nil
	"Buffer-local closure for the should-check hooks.

Each buffer gets its own closure that captures its buffer reference,
allowing proper cleanup when the mode is disabled.")

(defun languagetool-server-window-start(buffer)
	"Alias to mock of `window-start`."
	(with-current-buffer buffer
		(if (get-buffer-window buffer)
				(max (point-min) (window-start (get-buffer-window buffer)))
			nil
			)
		)
	)

(defun languagetool-server-window-end(buffer)
	"Alias to mock of `window-end`."
	(with-current-buffer buffer
		(if (get-buffer-window buffer)
				(min (point-max) (window-end (get-buffer-window buffer)))
			nil
			)
		)
	)

(defun languagetool-server-create-should-check-closure ()
	"Create and return a closure to run languagetool-server-should-check.

The closure is stored in the buffer-local variable
`languagetool-server--check-closure' so it can be properly removed
from hooks later. Each buffer gets its own unique closure."
	(let ((buffer (current-buffer)))
		(setq languagetool-server--check-closure
					(lambda (&rest _args)
						(when (buffer-live-p buffer)
							(with-current-buffer buffer
								(languagetool-server-should-check)))))
		languagetool-server--check-closure))

(defun languagetool-server-clear ()
	"Clean hooks, timers, caches."
	;; Clean up hooks using the stored closure (not a new one)
	(when languagetool-server--check-closure
		(remove-hook 'after-change-functions languagetool-server--check-closure t)
		(remove-hook 'window-scroll-functions languagetool-server--check-closure t)
		(remove-hook 'window-size-change-functions languagetool-server--check-closure t)
		(setq languagetool-server--check-closure nil))

	;; Cancel any existing timers
	(when (timerp languagetool-server-check-timer)
		(cancel-timer languagetool-server-check-timer)
		(setq languagetool-server-check-timer nil))

	(when (timerp languagetool-core-hint-timer)
		(cancel-timer languagetool-core-hint-timer)
		(setq languagetool-core-hint-timer nil))

	;; Clear caches
	(setq languagetool-server-text-cache nil)
	(setq languagetool-server-region-cache nil)
	)

(defun languagetool-server-activate-check-visible-text ()
	"Activate LanguageTool highlighting for all visible text."
	(interactive)
	(set-default 'languagetool-server-check-visible-text t)
	(languagetool-server-clear)
	;; Create closure once and reuse for all hooks to avoid memory leaks
	(let ((closure (languagetool-server-create-should-check-closure)))
		(add-hook 'after-change-functions closure nil t)
		(add-hook 'window-scroll-functions closure nil t)
		(add-hook 'window-size-change-functions closure nil t))
	)

(defun languagetool-server-desactivate-check-visible-text ()
	"Desactivate LanguageTool highlighting for all visible text."
	(interactive)
	(set-default 'languagetool-server-check-visible-text nil)
	(languagetool-server-clear)
	(add-hook 'after-change-functions (languagetool-server-create-should-check-closure) nil t)
	;; (add-hook 'post-command-hook (languagetool-server-create-should-check-closure) nil t)
	)

;;;###autoload
(define-minor-mode languagetool-server-mode
	"Toggle LanguageTool issue highlighting."
	:group 'languagetool-server
	:lighter " LT"
	(if languagetool-server-mode
			(languagetool-server-mode-on)
		(languagetool-server-mode-off)))

(defun languagetool-server-check-visible-text--set (symbol value)
	"Set languagetool-server-check-visible-text."
	(set-default symbol value)

	(if languagetool-server-mode
			(if value
					(languagetool-server-activate-check-visible-text)
				(languagetool-server-desactivate-check-visible-text)
				)
		)
	)

(defcustom languagetool-server-check-visible-text nil
	"If non-nil, activate LanguageTool highlighting for all visible text."
	:group 'languagetool-server
	:type 'boolean
	:set #'languagetool-server-check-visible-text--set
	)

(defvar languagetool-server-output-buffer-name "*LanguageTool Server Output*"
	"LanguageTool Server output buffer for debugging.")

(defvar languagetool-server-process nil
	"LanguageTool Server inferior process reference if any.")

(defvar-local languagetool-server-open-communication-p nil
	"Set to non-nil if server communication is open, nil otherwise.")

(defvar-local languagetool-server-correcting-p nil
	"Set to non-nil if correcting errors, nil otherwise.
Buffer-local to allow independent correction in multiple buffers.")

;; Function definitions:

(defvar-local languagetool-server--last-line nil
	"Stores the last line number for LanguageTool server checks.")

(defun languagetool-server-mode-on ()
	"Turn on LanguageTool Server mode.

Don't use this function, use `languagetool-server-mode' instead."
	;; Start checking for LanguageTool server is able to handle requests
	(languagetool-server-check-for-communication)
	(languagetool-core-load-dict-file)

	(if languagetool-server-check-visible-text
			(languagetool-server-activate-check-visible-text)
		(languagetool-server-desactivate-check-visible-text)
		)
	;; Initial check of visible text
	(languagetool-server-should-check)

	;; Init hint timer if not already running
	(unless (timerp languagetool-core-hint-timer)
		(setq languagetool-core-hint-timer
		(run-with-idle-timer languagetool-hint-idle-delay t
						 languagetool-hint-function))))

(defun languagetool-server-mode-off ()
	"Turn off LanguageTool Server mode.

Don't use this function, use `languagetool-server-mode' instead."
	;; Turn off buffer local flag for server communication.
	(setq languagetool-server-open-communication-p nil)

	(languagetool-server-clear)
	;; Delete all LanguageTool overlays
	(languagetool-core-clear-buffer)
	)



(defun languagetool-server-class-p ()
	"Return non-nil if `languagetool-server-command' is a Java class."
	(let ((regex (rx
								line-start
								(zero-or-more
								 (group
									(in alpha ?_ ?$)
									(zero-or-more
									 (in alnum ?_ ?$))
									?.))
								(in alpha ?_ ?$)
								(zero-or-more
								 (in alnum ?_ ?$))
								line-end)))
		(string-match-p regex languagetool-server-command)))

(defun languagetool-server-command-exists-p ()
	"Return non-nil is `languagetool-console-command' can be used or exists.

Also sets `languagetool-console-command' to a full path if needed
for this package to work."
	(or (languagetool-server-class-p)
			(when (file-readable-p (file-truename languagetool-server-command))
				(setq languagetool-server-command (file-truename languagetool-server-command))
				t)))

;;;###autoload
(defun languagetool-server-start ()
	"Start the LanguageTool Server.

It's not recommended to run this function more than once."
	(interactive)
	(unless (process-live-p languagetool-server-process)
		(unless (executable-find languagetool-java-bin)
			(error "Java could not be found"))
		(unless languagetool-server-command
			(error "LanguageTool Server Command is not set"))
		(unless (languagetool-server-command-exists-p)
			(error "LanguageTool Server Command could not be found"))

		(let ((buffer (get-buffer-create languagetool-server-output-buffer-name)))
			;; Clean the buffer before printing the LanguageTool Server Log
			(with-current-buffer buffer
				(erase-buffer))

			;; Start LanguageTool Server
			(setq languagetool-server-process
						(apply #'start-process
									 "*LanguageTool Server*"
									 buffer
									 languagetool-java-bin
									 (append
										(languagetool-java-parse-arguments)
										(languagetool-server-parse-arguments))))

			;; Does not block Emacs when close and do not shutdown the server
			(set-process-query-on-exit-flag languagetool-server-process nil))

		;; Start running the hint idle timer if not already running
		(unless (timerp languagetool-core-hint-timer)
			(setq languagetool-core-hint-timer
						(run-with-idle-timer languagetool-hint-idle-delay t
																 languagetool-hint-function)))))

(defun languagetool-server-parse-arguments ()
	"Parse the arguments needed to start HTTP server."
	(unless (listp languagetool-server-arguments)
		(error "LanguageTool Server Arguments must be a list of strings"))

	(let (arguments)

		;; Appends the LanguageTool Server Command
		(unless (languagetool-server-class-p)
			(push "-cp" arguments))
		(push languagetool-server-command arguments)
		(unless (languagetool-server-class-p)
			(push "org.languagetool.server.HTTPServer" arguments))

		(push languagetool-server-arguments arguments)

		;; Appends the port information
		(push (list "--port" (format "%d" languagetool-server-port)) arguments)

		(flatten-tree (reverse arguments))))

;;;###autoload
(defun languagetool-server-stop ()
	"Stops the LanguageTool Server."
	(interactive)
	(delete-process languagetool-server-process))

(defun languagetool-server-check-for-communication ()
	"Check if the LanguageTool Server is able to handle requests.

This methods will only check if the server is up for the number
of seconds specified in `languagetool-server-max-timeout'."
	(unless languagetool-server-open-communication-p
		(condition-case nil
				(let ((url-request-method "GET"))
					(with-current-buffer (url-retrieve-synchronously
																(url-encode-url (format "%s:%d/v2/languages" languagetool-server-url languagetool-server-port))
																nil
																nil
																languagetool-server-max-timeout)
						(when (/= (symbol-value 'url-http-response-status) 200)
							(error "Not successful response"))
						(setq languagetool-server-open-communication-p t)
						(message "LanguageTool Server communication is up...")))
			(error
			 (languagetool-server-mode -1)
			 (error "LanguageTool Server cannot communicate with server")))
		(languagetool-server-should-check)))

(defun languagetool-server-parse-request (buffer start end)
	"Return a assoc-list with LanguageTool Server request arguments parsed.

Return the arguments as an assoc list of string which will be
used in the POST request made to the LanguageTool server."
	(with-current-buffer buffer
		(let ((region-start (or start (point-min)))
					(region-end (or end (point-max)))
					arguments)

			;; Appends the correction language information
			(push (list "language" languagetool-correction-language) arguments)

			;; Appends the mother tongue information
			(when (stringp languagetool-mother-tongue)
				(push (list "motherTongue" languagetool-mother-tongue) arguments))

			;; Add LanguageTool Preamium features
			(when (stringp languagetool-api-key)
				(push (list "apiKey" languagetool-api-key) arguments))

			(when (stringp languagetool-username)
				(push (list "username" languagetool-username) arguments))

			;; Appends LanguageTool suggestion level information
			(when (stringp languagetool-suggestion-level)
				(push (list "level" languagetool-suggestion-level) arguments))

			;; Appends the disabled rules
			(let ((rules))
				;; Global disabled rules
				(setq rules (string-join (append languagetool-disabled-rules (languagetool-get-rules-for-current-buffer)) ","))
				(unless (string= rules "")
					(push (list "disabledRules" rules) arguments)))
			(push (list "text" (url-hexify-string (buffer-substring-no-properties region-start region-end))) arguments)
			)
		)
	)

(defun languagetool-server-region-around-point (buffer)
	"Return cons cell (start . end) for region around point, using line offsets."
	(with-current-buffer buffer
		(let* (
					 (start (save-excursion
										(forward-line (- languagetool-server-lines-before))
									(line-beginning-position)))
					 (end (save-excursion
									(forward-line languagetool-server-lines-after)
									(line-end-position)))
					 )
			(cons start end)
			)
		)
	)

(defun languagetool-server-get-region (buffer)
	"Return cons cell (start . end) for the visible text region in WINDOW.
Or for region around point.

WINDOW defaults to the selected window. Uses `languagetool-server-window-start' and `languagetool-server-window-end'
to determine the boundaries of text currently visible in the window."
	(with-current-buffer buffer
		(if languagetool-server-check-visible-text
				(cons
				 (funcall #'languagetool-server-window-start buffer)
				 (funcall #'languagetool-server-window-end buffer)
				 )
			(languagetool-server-region-around-point buffer)
			)
		)
	)

(defun languagetool-server-get-text (buffer)
	"Return the text content currently visible in WINDOW.
Or for region around point
WINDOW defaults to the selected window. Returns the text between
`languagetool-server-window-start' and `languagetool-server-window-end' as a string with properties removed."
	(with-current-buffer buffer
		(let* ((region (languagetool-server-get-region buffer))
					 (start (car region))
					 (end (cdr region)))
			(if (and start end)
					(buffer-substring-no-properties start end)
				nil
				)
			)
		)
	)


(defun languagetool-server-text-changed-p (buffer)
	"Return non-nil if the visible text content has changed since last check.

WINDOW defaults to the selected window. Compares the current visible
region and text content against the cached values in buffer-local
variables. Updates the cache if content has changed."
	(interactive)
	(with-current-buffer buffer
		(let ((current-region (languagetool-server-get-region buffer))
					(current-text (languagetool-server-get-text buffer)))
			(or
			 (not (equal current-text languagetool-server-text-cache))
			 (not (equal current-region languagetool-server-region-cache))
			 ;; Always detect change if cache is empty
			 (null languagetool-server-text-cache)
			 (null languagetool-server-region-cache)
			 )
			)
		)
	)

(defun languagetool-server-update-cache (buffer)
	"Update cache with the current region of text."
	;; (interactive)
	(with-current-buffer buffer
		(let ((current-region (languagetool-server-get-region buffer))
					(current-text (languagetool-server-get-text buffer)))
			(setq languagetool-server-region-cache current-region)
			(setq languagetool-server-text-cache current-text)
			)
		)
	)

(defun languagetool-server-check-region (&optional buffer)
	"Check the currently visible text region for grammar issues."
	(interactive)
	(with-current-buffer (or buffer (current-buffer))
		(when languagetool-server-mode
			(let* ((region (languagetool-server-get-region (current-buffer)))
						 (start (car region))
						 (end (cdr region)))
				(when (and start end)
					(languagetool-server-send-request (current-buffer) start end)
					)
				)
			(languagetool-server-update-cache (current-buffer))
			)
		)
	)

(defun languagetool-server-should-check (&rest _args)
	"Schedule a LanguageTool check if the buffer content or visible region has changed.

This function unifies debouncing and cache-based change detection for both
visible text mode and line-based mode. It cancels any existing timer, checks
if a correction is already in progress, and only schedules a new check if
the relevant region or text has changed."
	(let ((buffer (current-buffer)))
		(when (timerp languagetool-server-check-timer)
			(cancel-timer languagetool-server-check-timer)
			(setq languagetool-server-check-timer nil)
			)
		(when (and
					 (not languagetool-server-correcting-p)
					 (languagetool-server-text-changed-p buffer)
					 (get-buffer-window buffer)
					 )
			(cl-incf languagetool-server-last-request)
			(setq languagetool-server-check-timer
						(run-with-timer languagetool-server-check-delay nil #'languagetool-server-check-region buffer))
			)
		)
	)

(defun languagetool-server-send-request (buffer start end)
	"Send a request to the server and parse the output given."
	(with-current-buffer buffer
		(message (concat "Send request to languagetool: " (buffer-name (current-buffer))))
		(let* (
					 (region-start (or start (point-min)))
					 (region-end (or end (point-max)))
					 (url-request-method "POST")
					 (url-request-data (url-build-query-string (languagetool-server-parse-request buffer region-start region-end)))
					 (url-request-extra-headers '(("Content-Type" . "application/x-www-form-urlencoded")))
					 )
			(url-retrieve
			 (url-encode-url(format "%s:%d/v2/check" languagetool-server-url languagetool-server-port))
			 #'languagetool-server-highlight-matches
			 (list buffer region-start languagetool-server-last-request)
			 t))
		)
	)

(defun languagetool-server-highlight-matches (_status checking-buffer region-start last-request)
	"Highlight LanguageTool Server issues in CHECKING-BUFFER for region starting at REGION-START."
	;; Save the HTTP response buffer so we can clean it up at the end
	(let ((response-buffer (current-buffer)))
		(unwind-protect
	(when (equal last-request (buffer-local-value 'languagetool-server-last-request checking-buffer))
		(message (concat "languagetool-server-highlight-matches: " (buffer-name checking-buffer)))
		(when (/= (symbol-value 'url-http-response-status) 200)
			(error "LanguageTool Server closed"))
		(unless (buffer-local-value 'languagetool-server-correcting-p checking-buffer)
			(set-buffer-multibyte t)
			(goto-char (point-max))
			(backward-sexp)
			;; Wrap JSON parsing in condition-case to handle malformed responses
			(condition-case err
		(let ((json-parsed (json-read)))
			(with-current-buffer checking-buffer
				(save-excursion
					;; Safety check: verify buffer is still valid
					(when (buffer-live-p checking-buffer)
			;; Clear existing overlays
			(languagetool-core-clear-buffer)
			(when languagetool-server-mode
				;; Use when-let to guard against nil matches
				(when-let ((corrections (alist-get 'matches json-parsed)))
					(let ((buffer-max (point-max)))
						(dotimes (index (length corrections))
				(let* ((correction (aref corrections index))
							 (offset (alist-get 'offset correction))
							 (size	 (alist-get 'length correction))
							 (start	 (+ region-start offset))
							 (end		 (+ region-start offset size)))
					;; Safety check: verify positions are valid
					(when (and (>= start (point-min))
							 (<= end buffer-max)
							 (< start end))
						(let ((word (buffer-substring-no-properties start end)))
							(unless (languagetool-core-correct-p word)
					(languagetool-issue-create-overlay start end correction)))))))))))))
				(json-error
				 (message "LanguageTool: Failed to parse server response: %s" (error-message-string err))))))
			;; Always kill the HTTP response buffer to prevent memory leak
			(when (buffer-live-p response-buffer)
	(kill-buffer response-buffer)))))

(provide 'languagetool-server)

;;; languagetool-server.el ends here
