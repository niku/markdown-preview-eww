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

(defvar markdown-preview-eww-process-name "convert-from-md-to-html"
  "Process name of a converter.")

(defvar markdown-preview-eww-output-file-name "markdown-preview-eww-result.html"
  "Filename of converted html.")

(defvar markdown-preview-eww-waiting-idling-second 1
  "Seconds of convert waiting.")

(defcustom markdown-preview-eww-convert-command
  nil
  "Symbol of markdown convert function.")

(defcustom markdown-preview-eww-major-mode-default-dialect
  'markdown
  "Default Markdown dialect.

markdown: processor own default dialect
commonmark: CommonMark standard
markdown_strict: Original (markdown.pl)
markdown_phpextra: PHP Markdown Extra syntax
markdown_github: GFM (GitHub Flavored Markdown)
markdown_mmd: MultiMarkdown syntax"
  :type '(options 'markdown 'commonmark 'markdown_strict 'markdown_phpextra 'markdown_github 'markdown_mmd))

(defcustom markdown-preview-eww-major-mode-dialect-alist
  (list '(markdown-mode . nil)
        '(gfm-mode . markdown_github))
  "Alist of markdown dialect by MAJOR-MODE.")

(defun markdown-preview-eww-detect-convert-command (output-file-name)
  "Return commandline argument by `OUTPUT-FILE-NAME'."
  (let ((command-function (or markdown-preview-eww-convert-command
                              'markdown-preview-eww-convert-command-redcarpet)))
    (unless markdown-preview-eww-convert-command
      (setq-local markdown-preview-eww-convert-command command-function))
    (funcall command-function output-file-name)))

(defun markdown-preview-eww-convert-command-redcarpet (output-file-name)
  "Return ruby-redcarpet commandline argument to convert markdown by `OUTPUT-FILE-NAME'."
  (list
   "ruby" "-e"
   (format "require \"redcarpet\"

markdown = Redcarpet::Markdown.new(Redcarpet::Render::HTML)
while doc = gets(\"\\0\")
  doc.chomp!(\"\\0\")
  File.write(\"%s\", markdown.render(doc))
end
" output-file-name)))

(defun markdown-preview-eww-convert-command-pandoc (output-file-name)
  "Return pandoc commandline argument to convert markdown by `OUTPUT-FILE-NAME'."
  (list
   "pandoc"
   "-f" (symbol-name (markdown-preview-eww-dialect))
   "-t" "html"
   "-o" output-file-name))

(defun markdown-preview-eww-dialect ()
  "Return markdown-dialect symbol by MAJOR-MODE."
  (or (cdr-safe (assq major-mode markdown-preview-eww-major-mode-dialect-alist))
      markdown-preview-eww-major-mode-default-dialect))

(defun markdown-preview-eww--do-convert ()
  ""
  (let ((doc (buffer-substring-no-properties (point-min) (point-max)))
        (cb (current-buffer)))
    (process-send-string markdown-preview-eww-process-name (concat doc "\0"))
    (eww-open-file markdown-preview-eww-output-file-name)
    (switch-to-buffer cb)))

;;;### autoload
(defun markdown-preview-eww ()
  "Start a realtime markdown preview."
  (interactive)
  (let ((process-connection-type nil)
        (convert-command-args (markdown-preview-eww-convert-command markdown-preview-eww-output-file-name)))
    (apply 'start-process markdown-preview-eww-process-name nil convert-command-args)
    (run-with-idle-timer markdown-preview-eww-waiting-idling-second nil 'markdown-preview-eww--do-convert)))

(provide 'markdown-preview-eww)
;;; markdown-preview-eww.el ends here
