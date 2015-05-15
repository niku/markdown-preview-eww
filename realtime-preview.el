;;; realtime-preview.el --- Realtime preview by eww -*- lexical-binding: t; -*-

;; Author: niku <niku@niku.name>
;; URL: https://github.com/niku/realtime-preview
;; Version 0.0.1
;; Package-Requires: (emacs "24.4")

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
