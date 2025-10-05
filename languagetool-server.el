;;; languagetool-server.el --- LanguageTool Server commands -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2022  Joar Buitrago

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
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

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

(defcustom languagetool-server-use-visible-text-mode nil
	"When non-nil, check visible text in window instead of lines around point.

When enabled, LanguageTool will check all text currently visible in the
window and trigger checks when the visible content changes due to scrolling,
window resizing, or editing. When disabled, uses the traditional line-based
checking around the cursor position."
	:group 'languagetool-server
	:type 'boolean)

(defcustom languagetool-server-visible-text-debounce-delay 0.5
	"Delay in seconds before checking visible text after changes.

This applies to visible text mode when the visible content changes due to
scrolling or window resizing. A shorter delay provides more responsive
checking but may impact performance with rapid scrolling."
	:group 'languagetool-server
	:type 'number)

(defvar languagetool-server-output-buffer-name "*LanguageTool Server Output*"
	"LanguageTool Server output buffer for debugging.")

(defvar languagetool-server-process nil
	"LanguageTool Server inferior process reference if any.")

(defvar-local languagetool-server-check-timer nil
	"Hold idle time that send request to LanguageTool server.")

(defvar-local languagetool-server-open-communication-p nil
	"Set to non-nil if server communication is open, nil otherwise.")

(defvar-local languagetool-server-visible-text-cache nil
	"Cache of the last visible text content that was checked.

Used in visible text mode to avoid redundant checks when the visible
content hasn't actually changed.")

(defvar-local languagetool-server-visible-region-cache nil
	"Cache of the last visible region boundaries (start . end).

Used in visible text mode to track when the visible region has changed
due to scrolling or window resizing.")

(defvar-local languagetool-server-visible-text-timer nil
	"Timer for debouncing visible text changes.

Used in visible text mode to delay checking after rapid scrolling or
window resize events.")

(defvar languagetool-server-correcting-p nil
	"Set to non-nil if correcting errors, nil otherwise.")

;; Function definitions:

;;;###autoload
(define-minor-mode languagetool-server-mode
	"Toggle LanguageTool issue highlighting."
	:group 'languagetool-server
	:lighter " LT"
	(if languagetool-server-mode
			(languagetool-server-mode-on)
		(languagetool-server-mode-off)))

(defvar-local languagetool-server--last-line nil
  "Stores the last line number for LanguageTool server checks.")

(defun languagetool-server-check-line-change ()
  "Check grammar only if the cursor line has changed."
  (let ((current-line (line-number-at-pos)))
    (unless (eq current-line languagetool-server--last-line)
      (setq languagetool-server--last-line current-line)
      (languagetool-server-should-check))))


(defun languagetool-server-mode-on ()
  "Turn on LanguageTool Server mode.

Don't use this function, use `languagetool-server-mode' instead."
  ;; Start checking for LanguageTool server is able to handle requests
  (languagetool-server-check-for-communication)
  (languagetool-core-load-dict-file)

  ;; Clean up ALL hooks and timers first to prevent conflicts
  (remove-hook 'after-change-functions #'languagetool-server-should-check t)
  (remove-hook 'post-command-hook #'languagetool-server-check-line-change t)
  (remove-hook 'after-change-functions #'languagetool-server-handle-visible-text-change t)
  (remove-hook 'window-scroll-functions #'languagetool-server-handle-window-scroll t)
  (remove-hook 'window-size-change-functions #'languagetool-server-handle-window-size-change t)

  ;; Cancel any existing timers
  (when (timerp languagetool-server-check-timer)
    (cancel-timer languagetool-server-check-timer)
    (setq languagetool-server-check-timer nil))
  (when (timerp languagetool-server-visible-text-timer)
    (cancel-timer languagetool-server-visible-text-timer)
    (setq languagetool-server-visible-text-timer nil))

  ;; Clear caches
  (setq languagetool-server-visible-text-cache nil)
  (setq languagetool-server-visible-region-cache nil)

  ;; Add checking system based on mode
  (if languagetool-server-use-visible-text-mode
      (progn
        ;; Visible text mode: check on text changes and window events
        (add-hook 'after-change-functions #'languagetool-server-handle-visible-text-change nil t)
        (add-hook 'window-scroll-functions #'languagetool-server-handle-window-scroll nil t)
        (add-hook 'window-size-change-functions #'languagetool-server-handle-window-size-change nil t)
        ;; Initial check of visible text
        (languagetool-server-handle-visible-text-change))
    ;; Line-based mode: check on line changes
    (add-hook 'after-change-functions #'languagetool-server-should-check nil t)
    (add-hook 'post-command-hook #'languagetool-server-check-line-change nil t))

  ;; Init hint timer in the current buffer if not already
  (setq languagetool-core-hint-timer
        (run-with-idle-timer languagetool-hint-idle-delay t
                             languagetool-hint-function)))
    


(defun languagetool-server-mode-off ()
  "Turn off LanguageTool Server mode.

Don't use this function, use `languagetool-server-mode' instead."
  ;; Turn off buffer local flag for server communication.
  (setq languagetool-server-open-communication-p nil)

  ;; Remove ALL hooks completely (both modes)
  (remove-hook 'after-change-functions #'languagetool-server-should-check t)
  (remove-hook 'post-command-hook #'languagetool-server-check-line-change t)
  (remove-hook 'after-change-functions #'languagetool-server-handle-visible-text-change t)
  (remove-hook 'window-scroll-functions #'languagetool-server-handle-window-scroll t)
  (remove-hook 'window-size-change-functions #'languagetool-server-handle-window-size-change t)

  ;; Cancel ALL timers
  (when (timerp languagetool-server-check-timer)
    (cancel-timer languagetool-server-check-timer)
    (setq languagetool-server-check-timer nil))
  (when (timerp languagetool-server-visible-text-timer)
    (cancel-timer languagetool-server-visible-text-timer)
    (setq languagetool-server-visible-text-timer nil))

  ;; Clear ALL caches
  (setq languagetool-server-visible-text-cache nil)
  (setq languagetool-server-visible-region-cache nil)

  ;; Delete all LanguageTool overlays
  (languagetool-core-clear-buffer))
    


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

		;; Start running the hint idle timer
		(setq languagetool-core-hint-timer
					(run-with-idle-timer languagetool-hint-idle-delay t
															 languagetool-hint-function))))

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

(defun languagetool-server-parse-request (&optional start end)
	"Return a assoc-list with LanguageTool Server request arguments parsed.

Return the arguments as an assoc list of string which will be
used in the POST request made to the LanguageTool server."
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

		;; Add the buffer contents
		(push (list "text" (buffer-substring-no-properties region-start region-end)) arguments)
		)
	)

(defun languagetool-server-region-around-point (lines-before lines-after)
	"Return cons cell (start . end) for region around point, using line offsets."
	(let ((start (save-excursion
								 (forward-line (- lines-before))
								 (line-beginning-position)))
				(end (save-excursion
							 (forward-line lines-after)
							 (line-end-position))))
		(cons start end)))

(defun languagetool-server-get-visible-region (&optional window)
	"Return cons cell (start . end) for the visible text region in WINDOW.

WINDOW defaults to the selected window. Uses `window-start' and `window-end'
to determine the boundaries of text currently visible in the window."
	(let ((win (or window (selected-window))))
		(cons (window-start win) (window-end win))))

(defun languagetool-server-get-visible-text (&optional window)
	"Return the text content currently visible in WINDOW.

WINDOW defaults to the selected window. Returns the text between
`window-start' and `window-end' as a string with properties removed."
	(let* ((region (languagetool-server-get-visible-region window))
				 (start (car region))
				 (end (cdr region)))
		(buffer-substring-no-properties start end)))


(defun languagetool-server-visible-text-changed-p (&optional window)
  "Return non-nil if the visible text content has changed since last check.

WINDOW defaults to the selected window. Compares the current visible
region and text content against the cached values in buffer-local
variables. Updates the cache if content has changed."
  (when languagetool-server-use-visible-text-mode
    (let* ((current-region (languagetool-server-get-visible-region window))
           (current-text (languagetool-server-get-visible-text window))
           (region-changed (not (equal current-region languagetool-server-visible-region-cache)))
           (text-changed (not (equal current-text languagetool-server-visible-text-cache))))
      (when (or region-changed text-changed
                ;; Always detect change if cache is empty
                (null languagetool-server-visible-text-cache)
                (null languagetool-server-visible-region-cache))
        ;; Update cache with new values
        (setq languagetool-server-visible-region-cache current-region)
        (setq languagetool-server-visible-text-cache current-text)
        t))))
    

(defun languagetool-server-handle-window-scroll (window display-start)
	"Handle window scroll events for visible text mode.

WINDOW is the window that scrolled and DISPLAY-START is the new start position.
This function is designed to be used with `window-scroll-functions'."
	(when (and languagetool-server-use-visible-text-mode
						 languagetool-server-mode
						 (eq window (selected-window))
						 (eq (window-buffer window) (current-buffer)))
		(languagetool-server-handle-visible-text-change)))

(defun languagetool-server-handle-window-size-change (frame)
	"Handle window size change events for visible text mode.

FRAME is the frame whose window configuration changed.
This function is designed to be used with `window-size-change-functions'."
	(when (and languagetool-server-use-visible-text-mode
						 languagetool-server-mode
						 (eq frame (selected-frame)))
		(languagetool-server-handle-visible-text-change)))


(defun languagetool-server-handle-visible-text-change (&optional beg end len)
  "Handle changes to visible text content with debouncing.

BEG, END, and LEN are the standard arguments from `after-change-functions'.
This function checks if the visible text has actually changed and schedules
a grammar check after the debounce delay. It cancels any existing timer to
avoid redundant checks during rapid changes."
  (when (and languagetool-server-use-visible-text-mode
             languagetool-server-mode)
    ;; Invalidate cache when text changes
    (when (and beg end)
      (setq languagetool-server-visible-text-cache nil)
      (setq languagetool-server-visible-region-cache nil))

    ;; Check if visible content has changed
    (when (languagetool-server-visible-text-changed-p)
      ;; Cancel existing timer if any
      (when (timerp languagetool-server-visible-text-timer)
        (cancel-timer languagetool-server-visible-text-timer))

      ;; Schedule new check after debounce delay
      (setq languagetool-server-visible-text-timer
            (run-with-timer languagetool-server-visible-text-debounce-delay
                            nil
                            #'languagetool-server-check-visible-text)))))
    

(defun languagetool-server-check-visible-text ()
	"Check the currently visible text region for grammar issues."
	(when (and languagetool-server-mode
						 languagetool-server-use-visible-text-mode)
		(let* ((region (languagetool-server-get-visible-region))
					 (start (car region))
					 (end (cdr region)))
			(languagetool-server-send-request start end))))

(defun languagetool-server-check-region-around-point ()
	"Check the region around point using customizable line settings."
	(interactive)
	(when languagetool-server-mode
		(let* ((region (languagetool-server-region-around-point languagetool-server-lines-before languagetool-server-lines-after))
					 (start (car region))
					 (end (cdr region)))
			(languagetool-server-send-request start end))))

(defun languagetool-server-should-check (&rest _args)
	"Tell the package to send a request if there are no more edit commands in a time.

When attached to `after-change-functions', Emacs sends the begin,
end and length into the ARGS argument."
	(when (timerp languagetool-server-check-timer)
		(cancel-timer languagetool-server-check-timer))

	(unless languagetool-server-correcting-p
		(setq languagetool-server-check-timer (run-with-timer languagetool-server-check-delay nil #'languagetool-server-check-region-around-point))))

(defun languagetool-server-send-request (&optional start end)
	"Send a request to the server and parse the output given."
	(message "Send request to languagetool")
	(let* ((region-start (or start (point-min)))
				 (region-end (or end (point-max)))
				 (url-request-method "POST")
				 (url-request-data (url-build-query-string (languagetool-server-parse-request region-start region-end))))
		(url-retrieve
		 (url-encode-url(format "%s:%d/v2/check" languagetool-server-url languagetool-server-port))
		 #'languagetool-server-highlight-matches
		 (list (current-buffer) region-start)
		 t)))


(defun languagetool-server-highlight-matches (_status checking-buffer region-start)
  "Highlight LanguageTool Server issues in CHECKING-BUFFER for region starting at REGION-START."
  (message "Request received from languagetool")
  (when (/= (symbol-value 'url-http-response-status) 200)
    (error "LanguageTool Server closed"))
  (unless languagetool-server-correcting-p
    (set-buffer-multibyte t)
    (goto-char (point-max))
    (backward-sexp)
    (let ((json-parsed (json-read)))
      (with-current-buffer checking-buffer
        (save-excursion
          ;; Smart overlay clearing for visible text mode
          (if languagetool-server-use-visible-text-mode
              (languagetool-server-clear-region-overlays region-start)
            (languagetool-core-clear-buffer))
          (when languagetool-server-mode
            (let ((corrections (alist-get 'matches json-parsed)))
              (dotimes (index (length corrections))
                (let* ((correction (aref corrections index))
                       (offset (alist-get 'offset correction))
                       (size   (alist-get 'length correction))
                       (start  (+ region-start offset))
                       (end    (+ region-start offset size))
                       (word   (buffer-substring-no-properties start end)))
                  (unless (languagetool-core-correct-p word)
                    (languagetool-issue-create-overlay start end correction)))))))))))

(defun languagetool-server-clear-region-overlays (region-start)
  "Clear LanguageTool overlays only in the region being checked.

REGION-START is the start position of the region being checked.
This function preserves overlays outside the checked region to avoid
unnecessary clearing and redrawing when only part of the visible text changes."
  (when languagetool-server-use-visible-text-mode
    (let* ((region     (languagetool-server-get-visible-region))
           (region-end (cdr region)))
      ;; Only clear overlays within the current visible region
      (dolist (ov (overlays-in region-start region-end))
        (when (overlay-get ov 'languagetool-message)
          (delete-overlay ov))))))
    

(provide 'languagetool-server)

;;; languagetool-server.el ends here
