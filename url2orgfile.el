;;; url2orgfile.el --- Retrive URL and save content as org-file  -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2018 DarkSun

;; Author: DarkSun <lujun9972@gmail.com>
;; URL: http://github.com/lujun9972/url2orgfile.el
;; Created: 2017-4-15
;; Version: 0.1
;; Keywords: convenience, html, org
;; Package-Requires: ((emacs "24.4")(html2org "0.1")

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Source code
;;
;; url2orgfile's code can be found here:
;;   http://github.com/lujun9972/url2orgfile.el


;;; Commentary:
;; 

;;; Code:

(require 'dom)
(require 'html2org)
(defgroup url2orgfile nil
  "Save http(s) page as org file")

(defcustom url2orgfile-store-dir "/home/lujun9972/github/emacs-document/raw/"
  "The default directory storing org files."
  :type 'directory)

(defcustom url2orgfile-timeout 30
  "Timeout seconds."
  :type 'number)

(defun url2orgfile-get-dom (url)
  "Retrive `URL' and return the dom."
  (let ((buf (with-timeout
                 (url2orgfile-timeout
                  (error "Fetch %s failed in %d seconds" url url2orgfile-timeout))
               (url-retrieve-synchronously url))))
    (prog1 (ignore-errors
             (with-current-buffer buf
               (goto-char url-http-end-of-headers)
               (libxml-parse-html-region (point) (point-max) url t)))
      (kill-buffer buf))))

(defun url2orgfile-save-dom (dom org-file &rest option-plist)
  "Write the DOM into ORG-FILE using ‘org-mode’ format.
OPTION-PLIST specify additional in-buffer settings."
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
  "Retrive URL and write the content into ORG-FILE using ‘org-mode’ format.

This function will return the saved ORG-FILE path
OPTION-PLIST specify additional in-buffer settings."
  (interactive)
  (let* ((option-plist (append (list :TITLE title) option-plist))
         (url (or url (read-string "url: ")))
         (dom (url2orgfile-get-dom url))
         (title (dom-text (dom-by-tag dom 'title)))
         (org-file (or org-file (expand-file-name (concat title ".org") url2orgfile-store-dir))))
    (apply #'url2orgfile-save-dom dom org-file "TITLE" title option-plist)
    org-file))

(defun url2orgfile-asynchronously  (&optional url org-file &rest option-plist)
  "Retrive URL asynchronously and save the content as ORG-FILE when finished.
OPTION-PLIST specify additional in-buffer settings."
  (interactive)
  (let ((option-plist (append (list :TITLE title) option-plist))
        (url (or url (read-string "url: "))))
    (with-timeout (url2orgfile-timeout (error "Fetch %s failed in %d seconds" url url2orgfile-timeout))
      (url-retrieve url (lambda (status)
                          (goto-char url-http-end-of-headers)
                          (let* ((dom (libxml-parse-html-region (point) (point-max)))
                                 (title (dom-text (dom-by-tag dom 'title)))
                                 (org-file (or org-file (expand-file-name (concat title ".org") url2orgfile-store-dir))))
                            (apply #'url2orgfile-save-dom dom org-file "TITLE" title option-plist)
                            (message "%s saved to %s" url org-file)))))))

(provide 'url2orgfile)

;;; url2orgfile.el ends here
