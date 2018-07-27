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
  "Seconds of convert waiting")

(defun markdown-preview-eww-convert-command (output-file-name)
  (format "require \"redcarpet\"

markdown = Redcarpet::Markdown.new(Redcarpet::Render::HTML)
while doc = gets(\"\\0\")
  doc.chomp!(\"\\0\")
  File.write(\"%s\", markdown.render(doc))
end
" output-file-name))

(defvar-local markdown-preview-eww--timer nil
  "Idle timer used for markdown preview process.")

(defvar-local markdown-preview-eww--process nil
  "Markdown preview process.

This is buffer local because different buffers' processes might
write to different output files.")

(defvar-local markdown-preview-eww--buffer nil
  "eww buffer to use for markdown preview")

(defun markdown-preview-eww--do-convert ()
  (when markdown-preview-eww--process
    (process-send-string markdown-preview-eww--process
                         (concat (buffer-substring-no-properties
                                  (point-min) (point-max)) "\0"))
    (let ((eww-buffer (or markdown-preview-eww--buffer
                          (get-buffer "*eww*"))))
      ;; The with-selected-window form makes sure that we get back to
      ;; our actual window after eww-open-file has run.
      (with-selected-window (selected-window)
        (if eww-buffer
            ;; If there is already a candidate eww buffer, make sure
            ;; it's visible and set the current window to it. This
            ;; means that calling eww-open-file won't open on top of
            ;; the user's main window.
            (pop-to-buffer eww-buffer nil t)

          ;; Otherwise, switch to another window. If there is no other
          ;; window in the frame, try to make one by splitting this
          ;; one.
          (let ((other (or (other-window 1)
                           (split-window-sensibly))))
            ;; Assuming we ended up with a new window (which will be
            ;; true unless this is a really teensy frame), switch to
            ;; it.
            (select-window other nil)))

        ;; Display the preview in eww. This force-opens eww in the
        ;; current window, but we should have made a nice space for
        ;; that already.
        (eww-open-file markdown-preview-eww-output-file-name)

        ;; Finally, take a note of the eww buffer. When
        ;; with-selected-window returns (restoring us to the original
        ;; buffer), we can use this to set
        ;; markdown-preview-eww--buffer.
        (when (eq major-mode 'eww-mode)
          (setq eww-buffer (current-buffer))))

      (when eww-buffer
        (setq-local markdown-preview-eww--buffer eww-buffer)))))

;;;### autoload
(defun markdown-preview-eww ()
  "Start or stop continuous markdown preview."
  (interactive)
  (if markdown-preview-eww--timer
      (progn
        (message "Disabling eww markdown preview")
        (cancel-timer markdown-preview-eww--timer)
        (when markdown-preview-eww--process
          (delete-process markdown-preview-eww--process))
        (setq-local markdown-preview-eww--timer nil)
        (setq-local markdown-preview-eww--process nil)
        (setq-local markdown-preview-eww--buffer nil))
    (let ((process-connection-type nil))
      (setq-local markdown-preview-eww--process
                  (start-process markdown-preview-eww-process-name nil
                                 "ruby" "-e"
                                 (markdown-preview-eww-convert-command
                                  markdown-preview-eww-output-file-name))))
    (setq-local markdown-preview-eww--timer
                (run-with-idle-timer
                 markdown-preview-eww-waiting-idling-second
                 t 'markdown-preview-eww--do-convert))
    (message (concat "eww markdown preview enabled. "
                     "Run markdown-preview-eww again to disable."))))

(provide 'markdown-preview-eww)
;;; markdown-preview-eww.el ends here
