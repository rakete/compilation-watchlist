(require 'cl)

(defvar compilation-watchlist-position nil)
(defvar compilation-watchlist-prefix nil)
(defvar compilation-watchlist-map (make-hash-table :test 'eql))

(defun compilation-watchlist-update (beg end pre)
  (save-match-data
    (when (> end beg)
      (let* ((inhibit-modification-hooks t)
             (text (buffer-substring-no-properties beg end))
             (lines (split-string text "\n" t))
             (watch-pattern (if (> (length compilation-watchlist-prefix) 0)
                                (concat "\\(" compilation-watchlist-prefix "[^:]*\\):\\(..*\\)")
                              "")))
        (when (> (length watch-pattern) 0)
          (delete-matching-lines watch-pattern (point-min) (point-max)))
        (when compilation-watchlist-position
          (goto-char compilation-watchlist-position)
          (dolist (key (hash-table-keys compilation-watchlist-map))
            (let ((history (gethash key compilation-watchlist-map)))
              (insert (concat key ":" (mapconcat 'identity history " |") "\n")))))
        (dolist (line lines)
          (when (and (not compilation-watchlist-position)
                     (string-match "<<watchlist\\([^>]*\\)>>\\(.*\\)" line))
            (setq compilation-watchlist-position beg
                  compilation-watchlist-prefix (match-string 1 line))
            (let ((delete-end (+ beg (length line) (length (match-string 2 line)) 1)))
              (delete-region beg delete-end)))
          (when (and (> (length watch-pattern) 0)
                     (string-match watch-pattern line))
            ;; I am potentially throwing away some values here, especially just at the beginning,
            ;; instead of just putting the one change I got into the hashmap, I could scan the buffer
            ;; for everything matching the watch-pattern and add those, then delete them all
            (let* ((key (match-string 1 line))
                   (value (match-string 2 line))
                   (history (gethash key compilation-watchlist-map)))
              (when (and (> (length value) 0)
                         (> (length key) 0))
                (puthash key (remove-duplicates (append (list value) (remove-if-not 'identity (subseq history 0 10))) :test 'equal :from-end t) compilation-watchlist-map))))
          (when (string-match "Compilation finished" line)
            (print compilation-watchlist-position)
            (print compilation-watchlist-prefix)
            (print compilation-watchlist-map)
            (setq compilation-watchlist-position nil
                  compilation-watchlist-prefix nil
                  compilation-watchlist-map (make-hash-table :test 'equal)))))))
  (goto-char (point-max)))

(defun compilation-watchlist-init ()
  (save-match-data
    (let ((inhibit-modification-hooks t))
      (when (string-equal (buffer-name) "*compilation*")
        (add-hook 'after-change-functions 'compilation-watchlist-update t t)
        (setq compilation-watchlist-position nil
              compilation-watchlist-prefix nil
              compilation-watchlist-map (make-hash-table :test 'equal))))))

(eval-after-load 'compilation-watchlist
  (add-hook 'first-change-hook 'compilation-watchlist-init))

(provide 'compilation-watchlist)
