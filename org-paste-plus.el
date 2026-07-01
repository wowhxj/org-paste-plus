;;; org-paste-plus.el --- Paste images and files, and resize images, in Org mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Randolph

;; Author: Randolph <xiaojianghuang@yahoo.com>
;; Maintainer: Randolph <xiaojianghuang@yahoo.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: multimedia, org, convenience
;; URL: https://github.com/randolph/org-paste-plus

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This package makes it easy to paste images and arbitrary files from
;; the system clipboard into Org buffers and to adjust how inline images
;; are displayed.
;;
;; Pasting:
;;   `org-paste-plus-dwim' (bound to `s-V') probes the clipboard: when a
;;   file manager has copied a file (e.g. a PDF) it copies that file into
;;   a `<basename>.assets/' folder next to the Org file and inserts a
;;   plain `file:' link; otherwise it grabs the clipboard image, writes a
;;   timestamped PNG into the same folder, and inserts a `#+DOWNLOADED' /
;;   `#+CAPTION' / `#+ATTR_ORG' / `#+ATTR_LATEX' / `#+ATTR_HTML' block
;;   plus a `file:' link.
;;
;;   The clipboard tools are selected automatically from `system-type':
;;     - macOS:      `pngpaste' / `osascript' (file URL)
;;     - GNU/Linux:  `xclip' (image/png or text/uri-list)
;;     - Windows:    PowerShell (Get-Clipboard Image or FileDropList)
;;   Override with `org-paste-plus-clipboard-command' or
;;   `org-paste-plus-clipboard-file-command' if needed.
;;
;; Resizing:
;;   When point is on an `#+ATTR_*' / `#+CAPTION' line, `+' / `-' bump
;;   every `:width N' value on that block by `org-paste-plus-resize-step'
;;   and refresh the inline display of the current subtree.
;;   When point is on an inline image, `+' / `-' call
;;   `image-increase-size' / `image-decrease-size'.
;;   Otherwise the keys self-insert as usual.
;;   `C-+' / `C--' fall back to `text-scale-increase' / `text-scale-decrease'.
;;
;; Quick start:
;;
;;   (require 'org-paste-plus)
;;   (add-hook 'org-mode-hook #'org-paste-plus-mode)
;;
;; Or with `use-package':
;;
;;   (use-package org-paste-plus
;;     :hook (org-mode . org-paste-plus-mode))
;;
;; All bindings live in `org-paste-plus-mode-map' so you are free to
;; rebind them.

;;; Code:

(require 'cl-lib)
(require 'image)
(require 'org)
(require 'org-element)
(require 'url-util)

(defgroup org-paste-plus nil
  "Paste images from the clipboard and resize them in Org mode."
  :group 'org
  :prefix "org-paste-plus-")

;;;; Customization

(defcustom org-paste-plus-default-width 800
  "Default image width (pixels) used when pasting from the clipboard."
  :type 'integer)

(defcustom org-paste-plus-resize-step 50
  "Pixel step for `:width' adjustments via the keymap."
  :type 'integer)

(defcustom org-paste-plus-latex-reference-width 800.0
  "Reference width used to derive `:width' for `#+ATTR_LATEX'.
The latex width is computed as PASTE-WIDTH / this value, capped at 1.0."
  :type 'number)

(defcustom org-paste-plus-folder-format "%s.assets/"
  "Format string for the asset folder name.
%s is replaced with the buffer file's base name."
  :type 'string)

(defcustom org-paste-plus-file-name-format "img_%s.png"
  "Format string for the saved image's file name.
%s is replaced with a timestamp from `format-time-string'."
  :type 'string)

(defcustom org-paste-plus-time-format "%Y%m%d_%H%M%S"
  "Time format used to make pasted file names unique."
  :type 'string)

(defcustom org-paste-plus-html-class "zoomImage"
  "CSS class added to the inserted `#+ATTR_HTML' block."
  :type 'string)

(defcustom org-paste-plus-clipboard-command nil
  "Shell command template for reading a PNG from the clipboard.
The literal `%s' placeholder is replaced with the destination file
path (already shell-quoted).  When nil, a sensible default is chosen
based on `system-type'."
  :type '(choice (const :tag "Auto-detect" nil)
                 (string :tag "Command template")))

(defcustom org-paste-plus-clipboard-file-command nil
  "Shell command that prints the path of a file copied to the clipboard.
The command must write one absolute path (macOS/Windows) or a
`file://' URI (Linux `text/uri-list') to stdout, and exit silently
when no file is on the clipboard.  When nil, a sensible default is
chosen based on `system-type'."
  :type '(choice (const :tag "Auto-detect" nil)
                 (string :tag "Command")))

;;;; Internal helpers

(defun org-paste-plus--clipboard-command (file)
  "Return the shell command to write clipboard PNG into FILE."
  (let ((quoted (shell-quote-argument file)))
    (cond
     (org-paste-plus-clipboard-command
      (format org-paste-plus-clipboard-command quoted))
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
       "No clipboard command for `system-type' %s; set `org-paste-plus-clipboard-command'"
       system-type)))))

(defun org-paste-plus--clipboard-file-command ()
  "Return the shell command that prints the clipboard file path, or nil."
  (cond
   (org-paste-plus-clipboard-file-command)
   ((eq system-type 'darwin)
    "osascript -e 'POSIX path of (the clipboard as «class furl»)' 2>/dev/null")
   ((eq system-type 'gnu/linux)
    "xclip -selection clipboard -t text/uri-list -o 2>/dev/null")
   ((memq system-type '(windows-nt cygwin ms-dos))
    "powershell -NoProfile -Command \"$f = Get-Clipboard -Format FileDropList; if ($f) { $f[0].FullName }\"")
   (t nil)))

(defun org-paste-plus--clipboard-file-path ()
  "Return the absolute path of a file on the clipboard, or nil.
Reads a file reference (not image data) put on the clipboard by the
system file manager.  Returns nil when no such file is present."
  (let ((cmd (org-paste-plus--clipboard-file-command)))
    (when cmd
      (let ((path (car (split-string
                        (string-trim (shell-command-to-string cmd))
                        "[\r\n]+" t))))
        (when (and path (string-prefix-p "file://" path))
          (setq path (url-unhex-string
                      (substring path (length "file://")))))
        (when (and path
                   (not (string-empty-p path))
                   (file-exists-p path)
                   (not (file-directory-p path)))
          path)))))

(defun org-paste-plus--asset-dir ()
  "Return the asset directory (relative path) for the current buffer."
  (unless buffer-file-name
    (user-error "Buffer is not visiting a file; save it first"))
  (format org-paste-plus-folder-format
          (file-name-base buffer-file-name)))

(defun org-paste-plus--latex-width (width)
  "Compute the latex `\\linewidth' multiplier for an integer WIDTH."
  (let ((ratio (/ (float width) org-paste-plus-latex-reference-width)))
    (if (>= ratio 1.0) "1.0" (number-to-string ratio))))

(defun org-paste-plus--at-attr-line-p ()
  "Non-nil if the current line is an Org `#+ATTR_*' or `#+CAPTION' line."
  (save-excursion
    (beginning-of-line)
    (looking-at-p "^[ \t]*#\\+\\(?:ATTR_[A-Z]+\\|CAPTION\\):")))

(defun org-paste-plus--at-image-link-p ()
  "Non-nil if point is on an Org link pointing to an image file.
Unlike `image-at-point-p', this works even when an `appear'-style
feature has deleted the inline image overlay (which removes the image
`display' property the cursor would otherwise sit on)."
  (let ((ctx (org-element-context)))
    (and (eq (org-element-type ctx) 'link)
         (member (org-element-property :type ctx) '("file" "attachment"))
         (let ((path (org-element-property :path ctx)))
           (and path (image-supported-file-p path))))))

;;;; Display refresh

(defun org-paste-plus-display-subtree-images (&optional mode)
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
(defun org-paste-plus-from-clipboard (width &optional src)
  "Paste a PNG from the clipboard into the buffer's asset folder.

Creates `<basename>.assets/' (next to the visited file) if necessary,
saves a uniquely timestamped PNG into it, and inserts the matching
`#+DOWNLOADED', `#+CAPTION', `#+ATTR_ORG', `#+ATTR_LATEX',
`#+ATTR_HTML' lines plus a `file:' link.  WIDTH is the integer pixel
width used for the ATTR_ORG / ATTR_HTML lines.

SRC, when non-nil, is an existing image file (e.g. a PNG copied in the
file manager) to copy in, preserving its extension, instead of reading
raw image data from the clipboard."
  (interactive
   (list (read-number "Image width: " org-paste-plus-default-width)))
  (let* ((folder (org-paste-plus--asset-dir))
         (filename (if src
                       (format "img_%s.%s"
                               (format-time-string org-paste-plus-time-format)
                               (or (file-name-extension src) "png"))
                     (format org-paste-plus-file-name-format
                             (format-time-string org-paste-plus-time-format))))
         (relative (concat folder filename)))
    (unless (file-exists-p folder)
      (make-directory folder t))
    (if src
        (copy-file src relative t)
      (shell-command (org-paste-plus--clipboard-command relative)))
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
      (org-paste-plus--latex-width width)
      width
      org-paste-plus-html-class
      relative))
    (org-paste-plus-display-subtree-images 'on)))

(defun org-paste-plus--unique-dest (folder name)
  "Return a destination path for NAME inside FOLDER, avoiding collisions.
Keeps NAME as-is when free; otherwise inserts a timestamp before the
extension."
  (let ((dest (concat folder name)))
    (if (file-exists-p dest)
        (let ((base (file-name-base name))
              (ext  (file-name-extension name)))
          (concat folder base "_"
                  (format-time-string org-paste-plus-time-format)
                  (if ext (concat "." ext) "")))
      dest)))

;;;###autoload
(defun org-paste-plus-file-from-clipboard (&optional src)
  "Copy a file from the clipboard into the buffer's asset folder.

When the system file manager has copied a file (e.g. a PDF), copy it
into `<basename>.assets/' (next to the visited file), preserving its
name, and insert a plain `file:' link to it.  SRC, when given, is the
source path; otherwise it is read from the clipboard."
  (interactive)
  (let ((src (or src (org-paste-plus--clipboard-file-path))))
    (unless src
      (user-error "No file found on the clipboard"))
    (let* ((folder (org-paste-plus--asset-dir))
           (relative (org-paste-plus--unique-dest
                      folder (file-name-nondirectory src))))
      (unless (file-exists-p folder)
        (make-directory folder t))
      (copy-file src relative)
      (insert (format "[[file:%s][%s]]"
                      relative (file-name-nondirectory relative))))))

;;;###autoload
(defun org-paste-plus-dwim ()
  "Paste from the clipboard: a non-image file if present, otherwise an image.
Probes the clipboard for a file reference first (the case when a file
manager copied a file); an image file is treated as an image paste so
the width prompt and `#+ATTR_*' block are produced.  Falls back to
`org-paste-plus-from-clipboard' for raw image data."
  (interactive)
  (let ((file (org-paste-plus--clipboard-file-path)))
    (cond
     ((and file (string-match-p (image-file-name-regexp) file))
      (org-paste-plus-from-clipboard
       (read-number "Image width: " org-paste-plus-default-width) file))
     (file (org-paste-plus-file-from-clipboard file))
     (t (call-interactively #'org-paste-plus-from-clipboard)))))

;;;; Deletion

;;;###autoload
(defun org-paste-plus--link-block-bounds ()
  "Return (START . END) for the image block containing the link at point.
Scans upward from the link line to collect contiguous #+DOWNLOADED:,
#+CAPTION:, and #+ATTR_* lines.  If the resulting block is wrapped
inside a #+begin_results … #+end_results / #+RESULTS: envelope,
expands the bounds to include that envelope too.
Returns nil if point is not on a bracket link."
  (save-excursion
    (unless (org-in-regexp org-link-bracket-re 1)
      (cl-return-from org-paste-plus--link-block-bounds nil))
    (beginning-of-line)
    (let ((link-end (line-end-position))
          (start    (line-beginning-position)))
      ;; Walk upward collecting DOWNLOADED / CAPTION / ATTR_* lines.
      (while (and (> (line-beginning-position) (point-min))
                  (progn
                    (forward-line -1)
                    (looking-at
                     "^[ \t]*#\\+\\(?:DOWNLOADED\\|CAPTION\\|ATTR_[A-Z]+\\):")))
        (setq start (line-beginning-position)))
      ;; Check whether the line just above `start' is #+begin_results.
      (let ((end (1+ link-end)))      ; +1 eats the trailing newline
        (save-excursion
          (goto-char start)
          (when (and (> (line-beginning-position) (point-min))
                     (progn (forward-line -1)
                            (looking-at "^[ \t]*#\\+begin_results")))
            (setq start (line-beginning-position))
            ;; Also pull in #+RESULTS: if it sits directly above.
            (when (and (> (line-beginning-position) (point-min))
                       (progn (forward-line -1)
                              (looking-at "^[ \t]*#\\+RESULTS:")))
              (setq start (line-beginning-position)))))
        ;; Check whether the line just after the link is #+end_results.
        (save-excursion
          (goto-char link-end)
          (when (and (< (point) (point-max))
                     (progn (forward-line 1)
                            (looking-at "^[ \t]*#\\+end_results")))
            (setq end (min (1+ (line-end-position)) (point-max)))))
        (cons start end)))))

(defun org-paste-plus-delete-link-and-file ()
  "Delete the file referenced by the link at point and the surrounding block.

Removes the `[[file:...]]' link line together with any contiguous
preceding `#+DOWNLOADED:', `#+CAPTION:', and `#+ATTR_*' lines."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an Org buffer"))
  (unless (org-in-regexp org-link-bracket-re 1)
    (user-error "Point is not on an Org link"))
  (let* ((link (org-element-context))
         (path (org-element-property :path link))
         (type (org-element-property :type link)))
    (unless (string= type "file")
      (user-error "Link is not a file link"))
    (unless path
      (user-error "Could not determine file path from link"))
    (when (yes-or-no-p (format "Delete %s%s? "
                               path
                               (if (file-exists-p path) "" " (file missing)")))
      (when (file-exists-p path)
        (delete-file path))
      ;; chatu-excalidraw exports SVGs from a same-named .excalidraw
      ;; source file; delete that source alongside the SVG.
      (when (string= (file-name-extension path) "svg")
        (let ((excalidraw-path (concat (file-name-sans-extension path) ".excalidraw")))
          (when (file-exists-p excalidraw-path)
            (delete-file excalidraw-path))))
      (let ((bounds (org-paste-plus--link-block-bounds)))
        (if bounds
            (delete-region (car bounds) (min (cdr bounds) (point-max)))
          ;; Fallback: just delete the link text on this line.
          (when (org-in-regexp org-link-any-re)
            (replace-match "" t t)))))))

;;;; Resizing

(defun org-paste-plus--resize-attr (sign)
  "Adjust `:width N' values on the surrounding ATTR block by SIGN.
SIGN is `+' to grow or `-' to shrink.  Walks back up to three lines
to cover the typical ATTR_ORG / ATTR_LATEX / ATTR_HTML triple.
ATTR_ORG and ATTR_HTML pixel widths are adjusted directly; the
ATTR_LATEX \\\\linewidth fraction is recomputed from the new pixel width."
  (save-excursion
    (let* ((reg-start (line-beginning-position -3))
           (reg-end   (copy-marker (line-end-position) t))
           new-pixel-width)
      ;; Pass 1: update integer :width on ORG and HTML lines only.
      ;; ATTR_LATEX uses a fractional \linewidth value, handled separately.
      (goto-char reg-start)
      (while (re-search-forward
              "^[ \t]*#\\+ATTR_\\(?:ORG\\|HTML\\):.*:width \\([0-9]+\\)" reg-end t)
        (let* ((width (string-to-number (match-string 1)))
               (new   (if (eq sign '+)
                          (+ width org-paste-plus-resize-step)
                        (- width org-paste-plus-resize-step))))
          (when (> new 0)
            (setq new-pixel-width new)
            (replace-match (number-to-string new) t t nil 1))))
      ;; Pass 2: recompute ATTR_LATEX \linewidth fraction from the new pixel width.
      (when new-pixel-width
        (goto-char reg-start)
        (when (re-search-forward
               "^[ \t]*#\\+ATTR_LATEX:.*:width \\([0-9.]+\\)\\\\linewidth" reg-end t)
          (replace-match
           (org-paste-plus--latex-width new-pixel-width) t t nil 1)))
      (set-marker reg-end nil)))
  (org-paste-plus-display-subtree-images 'on))

(defun org-paste-plus--resize (resize-func)
  "Resize images or adjust text scale based on context.
When on an ATTR/CAPTION line or inline image, updates the surrounding
`:width' block.  Otherwise adjusts text scale."
  (let ((sign (if (eq resize-func 'image-increase-size) '+ '-)))
    (cond
     ((or (org-paste-plus--at-attr-line-p)
          (image-at-point-p)
          (org-paste-plus--at-image-link-p))
      (org-paste-plus--resize-attr sign))
     (t
      (call-interactively
       (if (eq sign '+) #'text-scale-increase #'text-scale-decrease))))))

;;;###autoload
(defun org-paste-plus-increase ()
  "Grow the image, `:width' attr, or text scale at point."
  (interactive)
  (org-paste-plus--resize 'image-increase-size))

;;;###autoload
(defun org-paste-plus-decrease ()
  "Shrink the image, `:width' attr, or text scale at point."
  (interactive)
  (org-paste-plus--resize 'image-decrease-size))

;;;###autoload
(defun org-paste-plus-image-increase-or-self-insert ()
  "If point is on an image, call `image-increase-size'; else self-insert."
  (interactive)
  (if (image-at-point-p)
      (image-increase-size)
    (self-insert-command 1)))

;;;###autoload
(defun org-paste-plus-image-decrease-or-self-insert ()
  "If point is on an image, call `image-decrease-size'; else self-insert."
  (interactive)
  (if (image-at-point-p)
      (image-decrease-size)
    (self-insert-command 1)))

;;;; Minor mode

(defvar org-paste-plus-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "s-V")   #'org-paste-plus-dwim)
    (define-key map (kbd "C-c b") #'org-paste-plus-delete-link-and-file)
    (define-key map (kbd "C-+")   #'org-paste-plus-increase)
    (define-key map (kbd "C--")   #'org-paste-plus-decrease)
    (define-key map (kbd "+")     #'org-paste-plus-image-increase-or-self-insert)
    (define-key map (kbd "-")     #'org-paste-plus-image-decrease-or-self-insert)
    map)
  "Keymap for `org-paste-plus-mode'.")

;;;###autoload
(define-minor-mode org-paste-plus-mode
  "Buffer-local minor mode that enables `org-paste-plus' bindings.

\\{org-paste-plus-mode-map}"
  :lighter " PastePlus"
  :keymap org-paste-plus-mode-map)

(provide 'org-paste-plus)
;;; org-paste-plus.el ends here
