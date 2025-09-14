;;; spookfox-history -- Spookfox app for searching and opening browser history -*- lexical-binding: t -*-

;;; Commentary:
;; Search through browser history from Emacs and quickly open previously visited pages

;;; Code:
(require 'org)
(require 'cl-lib)
(require 'spookfox)
(require 'spookfox-tabs)

(defvar spookfox-history--msg-prefix "H_")

(defun spookfox-history--request (&rest args)
  "Make spookfox-request with CLIENT and ARGS but with prefixed NAME."
  (let ((spookfox--msg-prefix spookfox-history--msg-prefix))
    (apply #'spookfox-request args)))

(defun spookfox-history--search-history (&optional max-results)
  "Returns a vector containing up to MAX-RESULTS URLs/titles from browser
history, sorted in descending order of frecency score."
  (when-let ((client (cl-first spookfox--connected-clients)))
    (plist-get
     (spookfox--poll-response
      (spookfox-history--request
       client "GET_HISTORY"
       `(:maxResults ,(or max-results 300))))
     :payload)))

(defun spookfox-history--format-item (item)
  "Format a history ITEM for display in completion."
  (let ((title (plist-get item :title))
        (url (plist-get item :url))
        (visit-count (plist-get item :visitCount)))
    (format "%s [%d visits] — %s"
            (if (and title (not (string-empty-p title)))
                title
              url)
            visit-count
            url)))

(defun spookfox-history--open-url (url)
  "Open URL in browser tab."
  (dolist (client spookfox--connected-clients)
    (when client
      (let ((spookfox--msg-prefix "T_"))
        (spookfox-request client "OPEN_TAB" `(:url ,url))))))

;;;###autoload
(defun spookfox-search-history ()
  "Browse browser history with completing-read and open selected result."
  (interactive)
  (if (not spookfox--connected-clients)
      (message "Spookfox is not connected to any browser")
    (let* ((items-list (spookfox-history--search-history))
           (item-map (make-hash-table :test 'equal))
           (completion-candidates '()))

      ;; Build hash table of URL -> formatted completion list item
      (dotimes (i (length items-list))
        (let* ((item (aref items-list i))
               (formatted (spookfox-history--format-item item))
               (url (plist-get item :url)))
          (push formatted completion-candidates)
          (puthash formatted url item-map)))

      ;; Let user select from all results
      (let* ((selected (completing-read "Open history item: " completion-candidates nil t))
             (url (gethash selected item-map)))
        (if url
            (progn
              (spookfox-history--open-url url)
              (message "Opening: %s" url))
          (message "No URL found for selection"))))))


(provide 'spookfox-history)
;;; spookfox-history.el ends here