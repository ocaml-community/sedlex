;;; sedlex-dot.el --- Render inline DOT graphs in sedlex expect tests -*- lexical-binding: t; -*-

(defun sedlex-render-dot-blocks ()
  "Render all DOT blocks in the current buffer as inline SVG images."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((count 0))
      (while (re-search-forward "^\\s-*DOT:" nil t)
        (let ((dot-start (line-beginning-position 2))
              (dot-end (if (re-search-forward "^\\s-*CODE:" nil t)
                           (line-beginning-position)
                         nil)))
          (when dot-end
            (let* ((dot-src (buffer-substring-no-properties dot-start dot-end))
                   (svg (with-temp-buffer
                          (insert dot-src)
                          (when (zerop (call-process-region
                                        (point-min) (point-max)
                                        "dot" t t nil "-Tsvg"))
                            (buffer-string)))))
              (when svg
                (goto-char dot-end)
                (let ((ov (make-overlay dot-start dot-end)))
                  (overlay-put ov 'display (create-image svg 'svg t))
                  (overlay-put ov 'after-string "\n")
                  (overlay-put ov 'sedlex-dot t)
                  (setq count (1+ count))))))))
      (message "Rendered %d DOT graph(s)" count))))

(defun sedlex-remove-dot-images ()
  "Remove all rendered DOT overlays."
  (interactive)
  (remove-overlays (point-min) (point-max) 'sedlex-dot t)
  (message "DOT overlays removed"))

(provide 'sedlex-dot)
;;; sedlex-dot.el ends here
