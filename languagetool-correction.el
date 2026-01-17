;;; languagetool-correction.el --- LanguageTool Correction module -*- lexical-binding: t; -*-

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

;; LanguageTool Correction routines and variables.

;;; Code:

(require 'cl-lib)
(require 'languagetool-core)
(require 'ispell)

;; Variable definitions:

(defcustom languagetool-correction-keys
	 (string-to-vector "1234567890abcdefghijklmnopqrstuvwxyz")
	"Vector of keys used for LanguageTool suggestion selection.

Each element should be a character (integer) used to select suggestions."
	:type '(vector character)
	:group 'languagetool)

;; Function definitions:

(defun languagetool-correction-parse-message (overlay)
	"Parse and style minibuffer correction.

Get the information about corrections from OVERLAY."
	(let* ((msg nil)
				 (rule (or (alist-get 'id (overlay-get overlay 'languagetool-rule)) "unknown"))
	 (message (or (overlay-get overlay 'languagetool-message) "No message"))
	 (replacements (languagetool-core-get-replacements overlay))
	 (num-choices (length replacements)))
		;; Add LanguageTool rule to the message
		(setq msg (concat msg "[" rule "] "))

		;; Add LanguageTool correction suggestion
		(setq msg (concat msg (propertize (format "%s" message) 'face 'font-lock-warning-face) "\n"))

		;; Format all the possible replacements for the correction suggestion
		;; If can't assoc each replacement with each hotkey truncate the replacements
		(when (> (length replacements) (length languagetool-correction-keys))
			(setq num-choices (length languagetool-correction-keys))
			(setq msg (concat msg "Not all choices shown.\n")))
		(setq msg (concat msg "\n"))
		;; Format all choices
		(dotimes (index num-choices)
			(setq msg (concat msg
			"["
			(propertize
			 (format "%c" (aref languagetool-correction-keys index))
			 'face 'font-lock-keyword-face)
			"]: "))
			(setq msg (concat msg (nth index replacements) "	")))
		;; Add default Ignore, Add and Skip options
		(setq msg (concat msg "\n["
					(propertize "C-i" 'face 'font-lock-keyword-face)
					"]: Ignore rule	 "))
		(setq msg (concat msg "["
					(propertize "C-a" 'face 'font-lock-keyword-face)
					"]: Add to LocalWords	 "))
		(setq msg (concat msg "["
					(propertize "C-s" 'face 'font-lock-keyword-face)
					"]: Skip match	"))
		;; Some people do not know C-g is the global exit key
		(setq msg (concat msg "["
					(propertize "C-g" 'face 'font-lock-keyword-face)
					"]: Quit\n"))))

(defun languagetool-correction-add-word(word)
	;; Append word to dictionary file
	(let ((dict-file (languagetool-core--dict-file)))
		(with-temp-buffer
			(when (file-exists-p dict-file)
				(insert-file-contents dict-file))
			(goto-char (point-max))
			(unless (or (bobp) (looking-back "\n" 1))
				(insert "\n"))
			(insert word "\n")
			(write-region (point-min) (point-max) dict-file))
		;; Reload dictionary list
		(languagetool-core-load-dict-file)
		)
	)

(defun languagetool-correction-apply (pressed-key overlay)
	"Apply LanguageTool replacement suggestion in OVERLAY.

PRESSED-KEY is the index of the suggestion in the array contained
on OVERLAY."
	(cond
	 ((char-equal ?\C-i pressed-key)
		(save-excursion
			(let ((rule-id (alist-get 'id (overlay-get overlay 'languagetool-rule))))
	(languagetool-update-rule-for-current-buffer rule-id)
	(delete-overlay overlay))))
	 ((char-equal ?\C-a pressed-key)
		(progn
			(let ((word (buffer-substring-no-properties (overlay-start overlay) (overlay-end overlay))))
				(languagetool-correction-add-word word)
			)
			(goto-char (overlay-end overlay))
			(delete-overlay overlay)))
	 ((char-equal ?\C-s pressed-key)
		(goto-char (overlay-end overlay)))
	 ((not (cl-position pressed-key languagetool-correction-keys))
		(error "Key `%c' cannot be used" pressed-key))
	 (t
		(let ((size (length (languagetool-core-get-replacements overlay)))
		(pos (cl-position pressed-key languagetool-correction-keys)))
			(when (> (1+ pos) size)
	(error "Correction key `%c' cannot be used" pressed-key))
			(delete-region (overlay-start overlay) (overlay-end overlay))
			(insert (nth pos (languagetool-core-get-replacements overlay)))
			(delete-overlay overlay)))))

(defun languagetool-correction-at-point ()
	"Show issue at point and try to apply suggestion."
	(dolist (ov (overlays-at (point)))
		(when (overlay-get ov 'languagetool-message)
			;; Cancel any previous message
			(message nil)
			(languagetool-correction-apply
			 (read-char (languagetool-correction-parse-message ov))
			 ov))))

(provide 'languagetool-correction)

;;; languagetool-correction.el ends here

; LocalWords:	 languagetool
