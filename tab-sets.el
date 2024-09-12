;;; tab-sets.el --- Save and restore tab-bar tabs   -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Grant Rosson

;; Author: Grant Rosson <https://github.com/localauthor>
;; Created: July 6, 2024
;; License: GPL-3.0-or-later
;; Version: 0.1
;; Homepage: https://github.com/localauthor/tab-sets
;; Package-Requires: ((emacs "27.1"))

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
;; for more details.

;; You should have received a copy of the GNU General Public License along
;; with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This package allows you to save the configuration of `tab-bar-mode’ tabs
;; open in the current frame and restore that set of tabs later. Saved tab
;; sets persist between sessions, saved in `tab-sets-data-file’. (Set this
;; variable to a path before trying to save.)
;;
;; To use with `embark’, add `(tab-sets-setup-embark)’ to your init file.

;;; Code:

(require 'frameset)
(require 'tab-bar)
(require 'bookmark)

;;; Variables

(defvar embark-keymap-alist)

(defvar tab-sets--alist nil)

(defgroup tab-sets nil
  "Save and restore tab-bar tabs."
  :group 'convenience
  :prefix "tab-sets-")

(defcustom tab-sets-data-file nil
  "File for saving tab-sets."
  :type 'string)

(defcustom tab-sets-same-frame nil
  "If non-nil, open tab-set in current frame.
Default is to open tab-set in new frame.
Prefix arg toggles behavior, either way."
  :type 'boolean)

(defcustom tab-sets-confirm-delete t
  "If non-nil, confirm before deleting a tab-set."
  :type 'boolean)

(defcustom tab-sets-bookmark-store t
  "Store tab-sets as bookmark.
This allows for opening tab-sets with `bookmark-jump’."
  :type 'boolean)

(defcustom tab-sets-bookmark-prefix "Tab-Set: "
  "Prefix for tab-set bookmark names."
  :type 'string)

;;; Low-level functions

(defun tab-sets--load-from-file ()
  "Load saved tab-set from file."
  (let ((file tab-sets-data-file))
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (let ((tabs-list (when (search-forward "(" nil t)
                           (forward-char -1)
                           (read (current-buffer)))))
          (if (not (listp tabs-list))
              (error "No tabs found in `%s'\n" file)
            (setq tab-sets--alist tabs-list)))))))

(defun tab-sets--alist ()
  "Return variable `tab-sets--alist’, loading if needed."
  (unless tab-sets--alist
    (tab-sets--load-from-file))
  tab-sets--alist)

(defun tab-sets--all-names ()
  "Return list of saved set names."
  (unless tab-sets--alist
    (tab-sets--load-from-file))
  (mapcar #'car tab-sets--alist))

(defun tab-sets--check-name (name)
  "Check NAME for tab-set to avoid duplicates."
  (while (member name (tab-sets--all-names))
    (if (y-or-n-p
         (format "Tab-set ‘%s’ already exists. Overwrite?" name))
        (tab-sets-delete name)
      (setq name (read-string "Duplicate name. Choose another: "))))
  name)

(defun tab-sets--tab-files ()
  "Return list of files from windows in current tab."
  (let ((bufs-in-tab))
    (walk-windows (lambda (win)
                    (push
                     (buffer-file-name
                      (window-buffer win))
                     bufs-in-tab)))
    (remq nil bufs-in-tab)))

(defun tab-sets--frame-files ()
  "Return list of files from current frame."
  (let ((tab-nums (length (funcall tab-bar-tabs-function)))
        (current (tab-bar--current-tab-index))
        files)
    (dotimes (num tab-nums)
      (tab-select (1+ num))
      (push (tab-sets--tab-files) files))
    (tab-select (1+ current))
    (flatten-list files)))

(defun tab-sets--select (&optional prompt)
  "Completing read function for selecting a tab-set.
With optional PROMPT."
  (let ((sets (tab-sets--alist)))
    (completing-read
     (or prompt "Select: ")
     (lambda (string predicate action)
       (if (eq action 'metadata)
           `(metadata
             (category . tab-set))
         (complete-with-action action sets string predicate)))
     nil (not (or (eq this-command 'tab-sets-save)
                  (eq this-command 'tab-sets-bookmark-store))))))

;;; User-facing functions

(defun tab-sets-save-to-file ()
  "Write variable `tab-sets--alist' to `tab-sets-data-file’."
  (interactive)
  (let ((file tab-sets-data-file))
    (if (file-writable-p file)
        (with-temp-buffer
          (let ((print-length nil)
                (print-level nil)
                (print-circle nil))
            (insert ";; -*- lisp-data -*-\n\n")
  (insert (format ";; tab-sets file\n;; Saved on %s\n\n"
                  (format-time-string "%Y.%m.%d %R")))
  (pp tab-sets--alist (current-buffer))
  (write-region (point-min) (point-max) file)))
      (error "Could not write to filters file `%s'" file))))

;;;###autoload
(defun tab-sets-save (name)
  "Save tab-set of current frame as NAME."
  (interactive
   (list (tab-sets--select "Save as: ")))
  (let* ((tab-sets-confirm-delete nil)
         (name (tab-sets--check-name name))
         (frame-set (frameset-save (list (window-frame))
                                   :name name)))
    (push (list name (tab-sets--frame-files) frame-set) tab-sets--alist)
    (tab-sets-save-to-file)
    (when tab-sets-bookmark-store
      (tab-sets-bookmark-store name))
    (message "Tab-set ‘%s’ saved to file." name)))

;;;###autoload
(defun tab-sets-open (name)
  "Open tab-set NAME in new frame.
With prefix arg, open in current frame."
  (interactive
   (list (tab-sets--select "Open: ")))
  (unless tab-sets--alist
    (user-error "No saved tab-sets"))
  (let* ((frame-set
          (alist-get name tab-sets--alist nil nil 'equal)))
    (dolist (file (car frame-set))
      (when (file-exists-p file)
        (find-file-noselect file)))
    (when (or (and tab-sets-same-frame
                   (not current-prefix-arg))
              (and current-prefix-arg
                   (not tab-sets-same-frame)))
      (delete-frame))
    (frameset-restore (cadr frame-set))))

;;;###autoload
(defun tab-sets-delete (name)
  "Delete tab-set NAME."
  (interactive
   (list (tab-sets--select "Delete: ")))
  (when (or (not tab-sets-confirm-delete)
            (y-or-n-p (format "Really delete tab-set ‘%s’?" name)))
    (let* ((tab-set (cons name
                          (alist-get name tab-sets--alist
                                     nil nil 'equal))))
      (setq tab-sets--alist
            (delete tab-set tab-sets--alist))
      (when tab-sets-bookmark-store
        (bookmark-delete (concat tab-sets-bookmark-prefix name)))
      (tab-sets-save-to-file))))

;;;###autoload
(defun tab-sets-rename (name)
  "Rename tab-set NAME."
  (interactive
   (list (tab-sets--select "Rename: ")))
  (let* ((new-name
          (tab-sets--check-name (read-string
                                 (format "Rename \"%s\" to: " name)))))
    (setf (car (assoc name tab-sets--alist)) new-name)
    (when tab-sets-bookmark-store
      (bookmark-prop-set name 'tab-set-name new-name)
      (bookmark-rename name new-name))
    (message "Tab-set ‘%s’ renamed ‘%s’." name new-name)))

;;; Bookmark Integration

(defun tab-sets-bookmark-store (tab-set-name)
  "Store a `bookmark’ record for TAB-SET-NAME."
  (bookmark-maybe-load-default-file)
  (let* ((bookmark-name (concat tab-sets-bookmark-prefix tab-set-name))
         (props `((tab-set-name . ,tab-set-name)
                  (handler . tab-sets-bookmark-handler))))
    (bookmark-store bookmark-name props nil)))

(defun tab-sets-bookmark-handler (bookmark)
  "Open BOOKMARK’s tab-set."
  (let ((bookmark-fringe-mark nil))
    (tab-sets-open (bookmark-prop-get bookmark 'tab-set-name))))

(put 'tab-sets-bookmark-handler 'bookmark-handler-type "Tab-Set")

(defun tab-sets-ensure-bookmarks ()
  "Ensure tab-sets in `tab-sets-data-file’ saved as bookmarks.
Delete stale tab-set bookmarks."
  (interactive)
  (let ((tab-set-names (tab-sets--all-names)))
    (dolist (x tab-set-names)
      (unless
          (bookmark-get-bookmark (concat tab-sets-bookmark-prefix x) t)
        (tab-sets-bookmark-store x)))))

(defun tab-sets-prune-stale-bookmarks ()
  "Delete stale bookmarks, with no corresponding tab-set."
  (interactive)
  (let ((tab-set-names (tab-sets--all-names)))
    (dolist (x (bookmark-all-names))
      (when (and (bookmark-prop-get x 'tab-set-name)
                 (not (member (bookmark-prop-get x 'tab-set-name) tab-set-names)))
        (bookmark-delete x)))))

(defun tab-sets-reconcile-bookmarks ()
  "Reconcile tab-sets and bookmarks."
  (interactive)
  (tab-sets-ensure-bookmarks)
  (tab-sets-prune-stale-bookmarks))

;;; Embark Integration

(defvar tab-sets-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "d") #'tab-sets-delete)
    (define-key map (kbd "r") #'tab-sets-rename)
    (define-key map (kbd "o") #'tab-sets-open)
    map)
  "Keymap for Embark tab-sets actions.")

;;;###autoload
(defun tab-sets-setup-embark ()
  "Setup Embark integration for `tab-sets’.
Adds tab-set as an Embark target, and adds `tab-sets-map'
to `embark-keymap-alist'."
  (with-eval-after-load 'embark
    (add-to-list 'embark-keymap-alist '(tab-set . tab-sets-map))))

(provide 'tab-sets)

;;; tab-sets.el ends here
