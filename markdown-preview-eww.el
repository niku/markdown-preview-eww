;;; markdown-preview-eww.el --- Realtime preview by eww -*- lexical-binding: t; -*-

;; Copyright (c) 2014, 2015, 2016 niku

;; Author: niku <niku@niku.name>
;; URL: https://github.com/niku/markdown-preview-eww
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.4"))


;; This file is not part of GNU Emacs.

;; The MIT License (MIT)

;; Permission is hereby granted, free of charge, to any person obtaining a copy of
;; this software and associated documentation files (the "Software"), to deal in
;; the Software without restriction, including without limitation the rights to
;; use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
;; the Software, and to permit persons to whom the Software is furnished to do so,
;; subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
;; FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
;; COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
;; IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:

;; This package provides the realtime markdown preview by eww.

;;; Code:
(require 'cl-lib)

(defvar markdown-preview-eww-waiting-idling-second 1
  "Seconds of convert waiting.")

(defcustom markdown-preview-eww-convert-command
  nil
  "Symbol of markdown convert function."
  :group 'markdown-preview-eww
  :type 'function)

(defcustom markdown-preview-eww-major-mode-default-dialect
  'markdown
  "Default Markdown dialect.

markdown: processor own default dialect
commonmark: CommonMark standard
markdown_strict: Original (markdown.pl)
markdown_phpextra: PHP Markdown Extra syntax
markdown_github: GFM (GitHub Flavored Markdown)
markdown_mmd: MultiMarkdown syntax"
  :group 'markdown-preview-eww
  :type '(options 'markdown 'commonmark 'markdown_strict 'markdown_phpextra 'markdown_github 'markdown_mmd))

(defcustom markdown-preview-eww-major-mode-dialect-alist
  (list '(markdown-mode . nil)
        '(gfm-mode . markdown_github))
  "Alist of markdown dialect by MAJOR-MODE."
  :group 'markdown-preview-eww)

(defcustom markdown-preview-eww-tmp-file-prefix
  "markdown-preview-eww_"
  "Prefix for generating preview HTML file."
  :group 'markdown-preview-eww)

(defun markdown-preview-eww-convert-command (output-file-name)
  "Return commandline argument by `OUTPUT-FILE-NAME'."
  (let ((command-function
         (or markdown-preview-eww-convert-command
             (if (executable-find "pandoc")
                 'markdown-preview-eww-convert-command-pandoc
               'markdown-preview-eww-convert-command-redcarpet))))
    (funcall command-function output-file-name)))

(defun markdown-preview-eww-convert-command-redcarpet (output-file-name)
  "Return ruby-redcarpet commandline argument to convert markdown by `OUTPUT-FILE-NAME'."
  (cl-values
   "ruby"
   (list
    "-r" "redcarpet"
    "-e"
    (format "
markdown = Redcarpet::Markdown.new(Redcarpet::Render::HTML)
doc = STDIN.read
File.write(\"%s\", markdown.render(doc))
" output-file-name))))

(defun markdown-preview-eww-convert-command-pandoc (output-file-name)
  "Return pandoc commandline argument to convert markdown by `OUTPUT-FILE-NAME'."
  (cl-values
   "pandoc"
   (list
    "-f" (symbol-name (markdown-preview-eww-dialect))
    "-t" "html"
    "-o" output-file-name)))

(defun markdown-preview-eww-dialect ()
  "Return markdown-dialect symbol by MAJOR-MODE."
  (or (cdr-safe (assq major-mode markdown-preview-eww-major-mode-dialect-alist))
      markdown-preview-eww-major-mode-default-dialect))

(defun markdown-preview--tmp-file (buffer-name)
  "Return tmporary file name by BUFFER-NAME."
  (concat temporary-file-directory markdown-preview-eww-tmp-file-prefix buffer-name ".html"))

(defun markdown-preview-eww--make-converter ()
  "Return closure for convert markdown and open eww buffer."
  (let ((preview-buffer nil)
        (last-line (line-number-at-pos (point)))
        (doc-buffer (current-buffer))
        (output-buffer (concat "*markdown-preview-eww " (buffer-name) "*"))
        (preview-temp-file (markdown-preview--tmp-file (buffer-name))))
    (cl-multiple-value-bind (command args)
        (markdown-preview-eww-convert-command preview-temp-file)
      (lambda ()
        (when (eq doc-buffer (current-buffer))
          (apply 'call-process-region (point-max) (point-min) command nil output-buffer nil args)
          (unless (file-readable-p preview-temp-file)
            (display-buffer output-buffer)
            (error "File not exists"))
          (eww-open-file preview-temp-file)
          (setq preview-buffer (get-buffer "*eww*"))
          ;; (unless (cl-loop for w in (window-list)
          ;;                  if (equal preview-buffer (window-buffer w))
          ;;                  return t)
          ;;   (display-buffer-use-some-window preview-buffer '((inhibit-same-window . t)))
          ;;   (pop-to-buffer preview-buffer))
          (with-current-buffer preview-buffer
            (goto-char (point-min))
            (forward-line last-line)
            (setq last-line (1- (line-number-at-pos (point))))
            (recenter)
            (message (number-to-string last-line)))
          (display-buffer doc-buffer)))
      )))

(defun markdown-preview-eww-disable ()
  "Disable realtime preview for current buffer."
  (interactive)
  (when (and (boundp 'markdown-prewiew--timer) markdown-prewiew--timer)
    (cancel-timer markdown-prewiew--timer)
    (setq markdown-prewiew--timer nil)))

;;;### autoload
(defun markdown-preview-eww ()
  "Start a realtime markdown preview."
  (interactive)
  (markdown-preview-eww-disable)
  (setq-local
   markdown-prewiew--timer
   (run-with-idle-timer markdown-preview-eww-waiting-idling-second t
                        (markdown-preview-eww--make-converter))))

(provide 'markdown-preview-eww)
;;; markdown-preview-eww.el ends here
