;; -*- lexical-binding: t; -*-
(require 'dom)
(require 'html2org)
(defgroup url2orgfile nil
  "Save http(s) page as org file")

(defcustom url2orgfile-store-dir "/home/lujun9972/github/emacs-document/raw/"
  "The directory to store org files"
  :type 'directory)

(defcustom url2orgfile-timeout 30
  "Timeout seconds"
  :type 'number)

(defun url2orgfile-get-dom (url)
  "Retrive `URL' and return the dom."
  (let ((buf (with-timeout
                 (url2orgfile-timeout
                  (error "fetch %s failed in %d seconds" url url2orgfile-timeout))
               (url-retrieve-synchronously url))))
    (prog1 (ignore-errors
             (with-current-buffer buf
               (goto-char url-http-end-of-headers)
               (libxml-parse-html-region (point) (point-max) url t)))
      (kill-buffer buf))))

(defun url2orgfile-save-dom (dom org-file &rest option-plist)
  "Write the DOM into ORG-FILE using org-mode format."
  (with-temp-file org-file
    (while option-plist
      (let ((option (format "%s" (pop option-plist)))
            (value (format "%s" (pop option-plist))))
        (when (string-prefix-p ":" option)
          (setq option (substring option 1))) ;handler format like :option
        (insert (format "#+%s: %s\n" option value))))
    (insert (html2org-transform-dom dom))))

;;;###autoload
(defun url2orgfile (&optional url org-file &rest option-plist)
  "Retrive URL and write the content into ORG-FILE using org-mode format.

This function will return the saved ORG-FILE path"
  (interactive)
  (let* ((url (or url (read-string "url: ")))
         (dom (url2orgfile-get-dom url))
         (title (dom-text (dom-by-tag dom 'title)))
         (org-file (or org-file (expand-file-name (concat title ".org") url2orgfile-store-dir))))
    (apply #'url2orgfile-save-dom dom org-file option-plist) 
    org-file))

(defun url2orgfile-asynchronously  (&optional url org-file &rest option-plist)
  "Retrive URL asynchronously and write the content into ORG-FILE using org-mode format when finished. "
  (interactive)
  (let ((url (or url (read-string "url: "))))
    (with-timeout (url2orgfile-timeout (error "fetch %s failed in %d seconds" url url2orgfile-timeout))
      (url-retrieve url (lambda (status)
                          (goto-char url-http-end-of-headers)
                          (let* ((dom (libxml-parse-html-region (point) (point-max)))
                                 (title (dom-text (dom-by-tag dom 'title)))
                                 (org-file (or org-file (expand-file-name (concat title ".org") url2orgfile-store-dir))))
                            (apply #'url2orgfile-save-dom dom org-file option-plist) 
                            (message "%s saved to %s" url org-file)))))))

(provide 'url2orgfile)
