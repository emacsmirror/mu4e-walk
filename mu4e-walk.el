;;; mu4e-walk.el --- Send email addresses for a walk   -*- lexical-binding: t -*-

;; Copyright (C) 2025 Timm Lichte

;; Author: Timm Lichte <timm.lichte@uni-tuebingen.de>
;; URL: https://codeberg.org/timmli/mu4e-walk
;; Version: 1.0
;; Last modified: 2025-09-12 Fri 10:02:40
;; Package-Requires: ((mu4e "1.12"))
;; Keywords: mu4e

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use,
;; copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following
;; conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;; OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:

;; See https://codeberg.org/timmli/mu4e-walk

;;; Code:

(require 'mu4e)


;;====================
;;
;; Customization
;;
;;--------------------

(defvar description+email-regexp "[[:space:]]*\\(\"[^\"]*\"[[:space:]]*<[-+_.~a-zA-Z][-+_.~:a-zA-Z0-9]*@[-.a-zA-Z0-9]+>\\|[^,:\"]*<?[-+_.~a-zA-Z][-+_.~:a-zA-Z0-9]*@[-.a-zA-Z0-9]+>?\\)"
  "Regular expression of an email address including optional description.")


;;====================
;;
;; Identifcation and cleansing
;;
;;--------------------

(defun mu4e-walk--point-in-address-field-p ()
  "Non-nil if point is in an address field."
  (save-excursion
    (beginning-of-line)
    (and (message-point-in-header-p)
         (re-search-forward message-email-recipient-header-regexp 
                            (line-end-position) t))))

(defun mu4e-walk--email-address-at-point ()
  "Return the email address at point as a plist."
  (when (mu4e-walk--point-in-address-field-p)
    (if (region-active-p)
        `(:email ,(buffer-substring-no-properties (region-beginning) (region-end))
                 :start ,(region-beginning)
                 :end ,(region-end)
                 :relpos ,(- (region-end) (region-beginning))
                 :active t)
      (let ((email)
            (start)
            (end)
            (relpos)
            (active))
        (save-excursion
          (let ((point (point)))
            (cl-loop
             while (re-search-backward "[,:]" (line-beginning-position) t)
             do (setq start (+ (point) 1)
                      end (save-excursion
                            (re-search-forward description+email-regexp
                                               (line-end-position) t)))
             (when (<= point end)
               (setq email (string-trim
                            (string-replace
                             "\n" ""
                             (buffer-substring-no-properties start end))))
               (when (string-match (concat "^" description+email-regexp "$")
                                   email)
                 (setq relpos (max 0 (- end point)))
                 (cl-return `(:email ,email
                                     :start ,start
                                     :end ,end
                                     :relpos ,relpos
                                     :active ,active)))))))))))

(defun mu4e-walk--clean-address-field-at-point ()
  (let ((start (line-beginning-position))
        (end (line-end-position)))
    (replace-regexp-in-region "\\([:,]\\)[ ]*," "\\1 " start end)
    (replace-regexp-in-region "  " " " start end)
    (replace-regexp-in-region ",[ ]*$" "" start end)
    (replace-regexp-in-region ":$" ": " start end)))


;;====================
;;
;; Walking
;;
;;--------------------

(defun mu4e-walk--move-email-address-at-point (&optional direction)
  "Move email address to previous or next address field.
DIRECTION can be 'up, 'down, 'left, 'right."
  (let* ((email-plist (mu4e-walk--email-address-at-point))
         (email (plist-get email-plist :email))
         (start (plist-get email-plist :start))
         (end (plist-get email-plist :end))
         (relpos (plist-get email-plist :relpos))
         (active (plist-get email-plist :active)))
    (when (and (mu4e-walk--point-in-address-field-p)
               email)
      (cond
       ;; Move vertically
       ((or (eq direction 'up)
            (eq direction 'down))
        (let* ((search-function (if (eq direction 'up)
                                    're-search-backward 're-search-forward))
               (next-field-line-number (save-excursion
                                         (when (eq direction 'up) (beginning-of-line))
                                         (and (funcall search-function
                                                       message-email-recipient-header-regexp
                                                       nil t)
                                              (message-point-in-header-p)
                                              (line-number-at-pos)))))
          (when next-field-line-number
            (delete-region start end)
            (mu4e-walk--clean-address-field-at-point)
            ;; Goto target line
            (goto-char (point-min))
            (forward-line (1- next-field-line-number))
            (mu4e-walk--clean-address-field-at-point)
            (end-of-line)
            (unless (looking-back ": ") (insert ", "))
            (insert email)
            (when active
              (push-mark)
              (setq deactivate-mark nil)
              (setq mark-active t))
            (backward-char relpos))))
       ;; Move horizontally
       ((or (eq direction 'left)
            (eq direction 'right))
        (let ((next-email-plist (save-excursion
                                  (if (eq direction 'left)
                                      (progn 
                                        (goto-char start)
                                        (when (re-search-backward "@" (pos-bol) t)
                                          (setq mark-active nil)
                                          (mu4e-walk--email-address-at-point)))
                                    (progn 
                                      (goto-char end)
                                      (when (re-search-forward "@" (pos-eol) t)
                                        (setq mark-active nil)
                                        (mu4e-walk--email-address-at-point)))))))
          (when next-email-plist
            (if (eq direction 'left)
                (progn
                  (delete-region start end)
                  (goto-char (plist-get next-email-plist :start))
                  (insert " " email ", ")
                  (backward-char 2)
                  (when active
                    (push-mark))
                  (mu4e-walk--clean-address-field-at-point)
                  (when active
                    (setq deactivate-mark nil)
                    (setq mark-active t))
                  (backward-char relpos))
              (progn
                (goto-char (plist-get next-email-plist :end))
                (insert ", " email)
                (when active
                  (push-mark))
                (backward-char relpos)
                (delete-region start end)
                (mu4e-walk--clean-address-field-at-point)
                (when active
                  (setq deactivate-mark nil)
                  (setq mark-active t)))))))
       (t nil)))))

;;;###autoload
(defun mu4e-walk-up ()
  "Move email address up to previous address field."
  (interactive)
  (mu4e-walk--move-email-address-at-point 'up))

;;;###autoload
(defun mu4e-walk-down ()
  "Move email address down to next address field."
  (interactive)
  (mu4e-walk--move-email-address-at-point 'down))

;;;###autoload
(defun mu4e-walk-left ()
  "Move email address left within the address field."
  (interactive)
  (mu4e-walk--move-email-address-at-point 'left))

;;;###autoload
(defun mu4e-walk-right ()
  "Move email address right within the address field."
  (interactive)
  (mu4e-walk--move-email-address-at-point 'right))


;;====================
;;
;; Keybindings
;;
;;--------------------

(defun mu4e-walk-extend-key (key walk-fun)
  "Extend function bound to KEY with function WALK-FUN."
  (let ((oldfun (keymap-lookup nil key)))
    (keymap-local-set key 
                      `(lambda ()
                         (interactive)
                         (if (message-point-in-header-p)
                             (call-interactively (quote ,walk-fun))
                           (call-interactively (quote ,oldfun))
                           )))))

(defun mu4e-walk-add-keybindings-compose-mode ()
  "Function which is added to a mode hook."
  (use-local-map (copy-keymap mu4e-compose-mode-map))
  (mu4e-walk-extend-key "M-<up>" 'mu4e-walk-up)
  (mu4e-walk-extend-key "M-<down>" 'mu4e-walk-down)
  (mu4e-walk-extend-key "M-<left>" 'mu4e-walk-left)
  (mu4e-walk-extend-key "M-<right>" 'mu4e-walk-right))

;; (add-hook 'mu4e-compose-mode-hook 'mu4e-walk-add-keybindings-compose-mode)


(provide 'mu4e-walk)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; mu4e-walk.el ends here

