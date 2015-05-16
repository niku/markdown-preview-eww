;;; realtime-preview.el --- Realtime preview by eww -*- lexical-binding: t; -*-

;; Copyright (c) 2014 niku

;; Author: niku <niku@niku.name>
;; URL: https://github.com/niku/realtime-preview
;; Version 0.0.1
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

;; This package provides the realtime preview by eww.

;;; Code:

(defvar realtime-preview-process-name "convert-from-md-to-html"
  "Process name of a converter.")

(defvar realtime-preview-output-file-name "realtime-preview-result.html"
  "Filename of converted html.")

(defun realtime-preview-convert-command (output-file-name)
  (format "require \"redcarpet\"

markdown = Redcarpet::Markdown.new(Redcarpet::Render::HTML)
while doc = gets(\"\\0\")
  doc.chomp!(\"\\0\")
  File.write(\"%s\", markdown.render(doc))
end
" output-file-name))

(defun realtime-preview ()
  "Start realtime preview."
  (interactive)
  (let ((process-connection-type nil)
        (convert-command (realtime-preview-convert-command realtime-preview-output-file-name)))
    (start-process realtime-preview-process-name nil "ruby" "-e" convert-command)
    (add-hook 'after-change-functions
              '(lambda (beginning ending length)
                 (let ((doc (buffer-substring-no-properties (point-min) (point-max)))
                       (cb (current-buffer)))
                   (process-send-string realtime-preview-process-name (concat doc "\0"))
                   (eww-open-file realtime-preview-output-file-name)
                   (switch-to-buffer cb)))
              nil
              t)))

(provide 'realtime-preview)
;;; realtime-preview.el ends here
