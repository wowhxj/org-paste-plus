;;; org-image-paste.el --- Paste and resize images in Org mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Randolph

;; Author: Randolph <xiaojianghuang@yahoo.com>
;; Maintainer: Randolph <xiaojianghuang@yahoo.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: multimedia, org, convenience
;; URL: https://github.com/randolph/org-image-paste

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This package makes it easy to paste images from the system clipboard
;; into Org buffers and to adjust how those inline images are displayed.
;;
;; Pasting:
;;   `org-image-paste-from-clipboard' grabs the current clipboard image,
;;   writes it to a `<basename>.assets/' folder next to the Org file, and
;;   inserts a `#+DOWNLOADED' / `#+CAPTION' / `#+ATTR_ORG' /
;;   `#+ATTR_LATEX' / `#+ATTR_HTML' block plus a `file:' link.
;;
;;   The clipboard tool is selected automatically from `system-type':
;;     - macOS:      `pngpaste'
;;     - GNU/Linux:  `xclip -selection clipboard -t image/png -o'
;;     - Windows:    PowerShell `(Get-Clipboard -Format Image).Save'
;;   Override with `org-image-paste-clipboard-command' if needed.
;;
;; Resizing:
;;   When point is on an `#+ATTR_*' / `#+CAPTION' line, `+' / `-' bump
;;   every `:width N' value on that block by `org-image-paste-resize-step'
;;   and refresh the inline display of the current subtree.
;;   When point is on an inline image, `+' / `-' call
;;   `image-increase-size' / `image-decrease-size'.
;;   Otherwise the keys self-insert as usual.
;;   `C-+' / `C--' fall back to `text-scale-increase' / `text-scale-decrease'.
;;
;; Quick start:
;;
;;   (require 'org-image-paste)
;;   (add-hook 'org-mode-hook #'org-image-paste-mode)
;;
;; Or with `use-package':
;;
;;   (use-package org-image-paste
;;     :hook (org-mode . org-image-paste-mode))
;;
;; All bindings live in `org-image-paste-mode-map' so you are free to
;; rebind them.

;;; Code:

(require 'cl-lib)
(require 'image)
(require 'org)
(require 'org-element)

(defgroup org-image-paste nil
  "Paste images from the clipboard and resize them in Org mode."
  :group 'org
  :prefix "org-image-paste-")

;;;; Customization

(defcustom org-image-paste-default-width 800
  "Default image width (pixels) used when pasting from the clipboard."
  :type 'integer)

(defcustom org-image-paste-resize-step 50
  "Pixel step for `:width' adjustments via the keymap."
  :type 'integer)

(defcustom org-image-paste-latex-reference-width 800.0
  "Reference width used to derive `:width' for `#+ATTR_LATEX'.
The latex width is computed as PASTE-WIDTH / this value, capped at 1.0."
  :type 'number)

(defcustom org-image-paste-folder-format "%s.assets/"
  "Format string for the asset folder name.
%s is replaced with the buffer file's base name."
  :type 'string)

(defcustom org-image-paste-file-name-format "img_%s.png"
  "Format string for the saved image's file name.
%s is replaced with a timestamp from `format-time-string'."
  :type 'string)

(defcustom org-image-paste-time-format "%Y%m%d_%H%M%S"
  "Time format used to make pasted file names unique."
  :type 'string)

(defcustom org-image-paste-html-class "zoomImage"
  "CSS class added to the inserted `#+ATTR_HTML' block."
  :type 'string)

(defcustom org-image-paste-clipboard-command nil
  "Shell command template for reading a PNG from the clipboard.
The literal `%s' placeholder is replaced with the destination file
path (already shell-quoted).  When nil, a sensible default is chosen
based on `system-type'."
  :type '(choice (const :tag "Auto-detect" nil)
                 (string :tag "Command template")))

;;;; Internal helpers

(defun org-image-paste--clipboard-command (file)
  "Return the shell command to write clipboard PNG into FILE."
  (let ((quoted (shell-quote-argument file)))
    (cond
     (org-image-paste-clipboard-command
      (format org-image-paste-clipboard-command quoted))
     ((eq system-type 'darwin)
      (format "pngpaste %s" quoted))
     ((eq system-type 'gnu/linux)
      (format "xclip -selection clipboard -t image/png -o > %s" quoted))
     ((memq system-type '(windows-nt cygwin ms-dos))
      (format
       "powershell -NoProfile -Command \"$img = Get-Clipboard -Format Image; if ($img) { $img.Save('%s') }\""
       file))
     (t
      (user-error
       "No clipboard command for `system-type' %s; set `org-image-paste-clipboard-command'"
       system-type)))))

(defun org-image-paste--asset-dir ()
  "Return the asset directory (relative path) for the current buffer."
  (unless buffer-file-name
    (user-error "Buffer is not visiting a file; save it first"))
  (format org-image-paste-folder-format
          (file-name-base buffer-file-name)))

(defun org-image-paste--latex-width (width)
  "Compute the latex `\\linewidth' multiplier for an integer WIDTH."
  (let ((ratio (/ (float width) org-image-paste-latex-reference-width)))
    (if (>= ratio 1.0) "1.0" (number-to-string ratio))))

(defun org-image-paste--at-attr-line-p ()
  "Non-nil if the current line is an Org `#+ATTR_*' or `#+CAPTION' line."
  (save-excursion
    (beginning-of-line)
    (looking-at-p "^[ \t]*#\\+\\(?:ATTR_[A-Z]+\\|CAPTION\\):")))

;;;; Display refresh

(defun org-image-paste-display-subtree-images (&optional mode)
  "Refresh inline image display for the current subtree.
MODE is `on' to show, `off' to hide, or nil to toggle."
  (interactive "P")
  (save-excursion
    (save-restriction
      (condition-case nil
          (org-narrow-to-subtree)
        (error nil))
      (let* ((beg (point-min))
             (end (point-max))
             (image-overlays (cl-intersection
                              org-inline-image-overlays
                              (overlays-in beg end))))
        (cond
         ((eq mode 'on)
          (org-display-inline-images t t beg end))
         ((eq mode 'off)
          (org-remove-inline-images))
         (t
          (if image-overlays
              (org-remove-inline-images)
            (org-display-inline-images t t beg end))))))))

;;;; Pasting

;;;###autoload
(defun org-image-paste-from-clipboard (width)
  "Paste a PNG from the clipboard into the buffer's asset folder.

Creates `<basename>.assets/' (next to the visited file) if necessary,
saves a uniquely timestamped PNG into it, and inserts the matching
`#+DOWNLOADED', `#+CAPTION', `#+ATTR_ORG', `#+ATTR_LATEX',
`#+ATTR_HTML' lines plus a `file:' link.  WIDTH is the integer pixel
width used for the ATTR_ORG / ATTR_HTML lines."
  (interactive
   (list (read-number "Image width: " org-image-paste-default-width)))
  (let* ((folder (org-image-paste--asset-dir))
         (filename (format org-image-paste-file-name-format
                           (format-time-string org-image-paste-time-format)))
         (relative (concat folder filename)))
    (unless (file-exists-p folder)
      (make-directory folder t))
    (shell-command (org-image-paste--clipboard-command relative))
    (unless (and (file-exists-p relative)
                 (> (or (file-attribute-size (file-attributes relative)) 0) 0))
      (when (file-exists-p relative) (delete-file relative))
      (user-error "Could not paste an image from the clipboard"))
    (insert
     (format
      (concat "\n#+DOWNLOADED: screenshot @ %s"
              "\n#+CAPTION: "
              "\n#+ATTR_ORG: :width %d"
              "\n#+ATTR_LATEX: :width %s\\linewidth :float nil"
              "\n#+ATTR_HTML: :width %d :class %s :border 1"
              "\n[[file:%s]]\n")
      (format-time-string "%Y-%m-%d %a %H:%M:%S")
      width
      (org-image-paste--latex-width width)
      width
      org-image-paste-html-class
      relative))
    (org-image-paste-display-subtree-images 'on)))

;;;; Deletion

;;;###autoload
(defun org-image-paste-delete-link-and-file ()
  "Delete the file referenced by the link at point, and the link itself.

For PNG files inserted with `org-image-paste-from-clipboard', also
removes the surrounding `#+DOWNLOADED', `#+CAPTION' and `#+ATTR_*'
lines."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an Org buffer"))
  (org-with-point-at (point)
    (unless (org-in-regexp org-link-bracket-re 1)
      (user-error "Point is not on an Org link"))
    (let* ((link (org-element-context))
           (path (org-element-property :path link))
           (type (org-element-property :type link)))
      (unless (and (string= type "file") path (file-exists-p path))
        (user-error "Link does not point to an existing local file"))
      (when (yes-or-no-p (format "Delete local file %s? " path))
        (delete-file path)
        (when (org-in-regexp org-link-any-re)
          (replace-match "" t t))
        (when (string= (downcase (or (file-name-extension path) "")) "png")
          (save-excursion
            (let ((start (progn (forward-line -5) (line-beginning-position)))
                  (end   (progn (forward-line 4) (line-end-position))))
              (delete-region start end))))))))

;;;; Resizing

(defun org-image-paste--resize-attr (sign)
  "Adjust `:width N' values on the surrounding ATTR block by SIGN.
SIGN is `+' to grow or `-' to shrink.  Walks back up to three lines
to cover the typical ATTR_ORG / ATTR_LATEX / ATTR_HTML triple."
  (save-excursion
    (let ((start (line-beginning-position -3))
          (end   (line-end-position)))
      (goto-char start)
      (while (re-search-forward ":width \\([0-9]+\\)" end t)
        (let* ((width (string-to-number (match-string 1)))
               (new   (if (eq sign '+)
                          (+ width org-image-paste-resize-step)
                        (- width org-image-paste-resize-step))))
          (when (> new 0)
            (replace-match (number-to-string new) t t nil 1))))))
  (org-image-paste-display-subtree-images 'on))

(defun org-image-paste--resize (resize-func)
  "Resize images or adjust text scale based on context.
RESIZE-FUNC is called when point is on an inline image."
  (let ((sign (if (eq resize-func 'image-increase-size) '+ '-)))
    (cond
     ((org-image-paste--at-attr-line-p)
      (org-image-paste--resize-attr sign))
     ((image-at-point-p)
      (funcall resize-func))
     (t
      (call-interactively
       (if (eq sign '+) #'text-scale-increase #'text-scale-decrease))))))

;;;###autoload
(defun org-image-paste-increase ()
  "Grow the image, `:width' attr, or text scale at point."
  (interactive)
  (org-image-paste--resize 'image-increase-size))

;;;###autoload
(defun org-image-paste-decrease ()
  "Shrink the image, `:width' attr, or text scale at point."
  (interactive)
  (org-image-paste--resize 'image-decrease-size))

;;;###autoload
(defun org-image-paste-image-increase-or-self-insert ()
  "If point is on an image, call `image-increase-size'; else self-insert."
  (interactive)
  (if (image-at-point-p)
      (image-increase-size)
    (self-insert-command 1)))

;;;###autoload
(defun org-image-paste-image-decrease-or-self-insert ()
  "If point is on an image, call `image-decrease-size'; else self-insert."
  (interactive)
  (if (image-at-point-p)
      (image-decrease-size)
    (self-insert-command 1)))

;;;; Minor mode

(defvar org-image-paste-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "s-V")   #'org-image-paste-from-clipboard)
    (define-key map (kbd "C-c b") #'org-image-paste-delete-link-and-file)
    (define-key map (kbd "C-+")   #'org-image-paste-increase)
    (define-key map (kbd "C--")   #'org-image-paste-decrease)
    (define-key map (kbd "+")     #'org-image-paste-image-increase-or-self-insert)
    (define-key map (kbd "-")     #'org-image-paste-image-decrease-or-self-insert)
    map)
  "Keymap for `org-image-paste-mode'.")

;;;###autoload
(define-minor-mode org-image-paste-mode
  "Buffer-local minor mode that enables `org-image-paste' bindings.

\\{org-image-paste-mode-map}"
  :lighter " ImgPaste"
  :keymap org-image-paste-mode-map)

(provide 'org-image-paste)
;;; org-image-paste.el ends here
