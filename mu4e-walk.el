;;; mu4e-walk.el --- Send email addresses for a walk   -*- lexical-binding: t -*-

;; Copyright (C) 2025 Timm Lichte

;; Author: Timm Lichte <timm.lichte@uni-tuebingen.de>
;; URL: https://codeberg.org/timmli/mu4e-walk
;; Version: 1.0
;; Last modified: 2025-11-27 Thu 22:33:42
;; Package-Requires: ((emacs "29.1") (mu4e "1.12"))
;; Keywords: convenience mail

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; When composing an email with mu4e, mu4e-walk allows for moving
;; around email addresses in the header with just one key stroke:

;; - Move email address "vertically" between address fields
;; - Move email address "horizontally" within an address field
;; - Also works with regions
;; - Moreover one can "vertically" and "horizontally" switch between email addresses.

;; The most important functions and default keybindings are:

;; | Function               | Keybinding  |
;; |------------------------+-------------|
;; | mu4e-walk-up           | M-<up>      |
;; | mu4e-walk-down         | M-<down>    |
;; | mu4e-walk-left         | M-<left>    |
;; | mu4e-walk-right        | M-<right>   |
;; | mu4e-walk-switch-up    | C-<up>      |
;; | mu4e-walk-switch-down  | C-<down>    |
;; | mu4e-walk-switch-left  | C-<left>    |
;; | mu4e-walk-switch-right | C-<right>   |

;; Installation note: This package depends on mu4e which must be installed
;; separately from package.el.

;;; Code:

(require 'mu4e)


;;====================
;;
;; Customization
;;
;;--------------------

(defvar mu4e-walk-description+email-regexp "[[:space:]]*\\(\"[^\"]*\"[[:space:]]*<[-+_.~a-zA-Z][-+_.~:a-zA-Z0-9]*@[-.a-zA-Z0-9]+>\\|[^,:\"]*<?[-+_.~a-zA-Z][-+_.~:a-zA-Z0-9]*@[-.a-zA-Z0-9]+>?\\)"
  "Regular expression of an email address including optional description.")

(defvar mu4e-walk-use-overlays t
  "If non-nil, use overlays.")


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
            (active nil))
        (save-excursion
          (let ((point (point)))
            (cl-loop
             while (re-search-backward "[,:]" (line-beginning-position) t)
             do (setq start (+ (point) 1)
                      end (save-excursion
                            (re-search-forward mu4e-walk-description+email-regexp
                                               (line-end-position) t)))
             (when (and end
                        (<= point end))
               (setq email (string-trim
                            (string-replace
                             "\n" ""
                             (buffer-substring-no-properties start end))))
               (when (string-match (concat "^" mu4e-walk-description+email-regexp "$")
                                   email)
                 (setq relpos (max 0 (- end point)))
                 (cl-return `(:email ,email
                                     :start ,start
                                     :end ,end
                                     :relpos ,relpos
                                     :active ,active)))))))))))

(defun mu4e-walk--clean-address-field-at-point ()
  "Clean address field at point."
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
DIRECTION can be \\='up, \\='down, \\='left, \\='right."
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
            (unless (looking-back ": " 2) (insert ", "))
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
;; Switching
;;
;;--------------------

(defun mu4e-walk--switch (&optional direction)
  "Switch to email address close by.
DIRECTION can be \\='up, \\='down, \\='left, \\='right."
  (let ((origin (mu4e-walk--email-address-at-point))
        (target-start nil)
        (target-end nil)
        (search-function nil))
    (save-excursion
      (when (eq direction 'up)
        (forward-line -1)
        (end-of-line)
        (setq direction 'left))
      (when (eq direction 'down)
        (forward-line 1)
        (beginning-of-line)
        (setq direction 'right))
      (when (eq direction 'left)
        (setq search-function 're-search-backward))
      (when (eq direction 'right)
        (setq search-function 're-search-forward))
      (cl-loop
       while (funcall search-function "@" nil t)
       do (let ((target (mu4e-walk--email-address-at-point)))
            (when (and target
                       (not (eq (when origin (plist-get origin :start))
                                (plist-get target :start))))
              (setq target-start (+ 1 (plist-get target :start)))
              (setq target-end (plist-get target :end))
              (cl-return t)))))
    (when target-start (progn
                         (goto-char target-start)
                         (when mu4e-walk-use-overlays
                           (let ((overlay (make-overlay target-start target-end)))
                             (overlay-put overlay 'face '(:box (:line-width 1)))
                             (run-at-time "0.5 sec" nil
                                          (lambda () (delete-overlay overlay)))))))))

;;;###autoload
(defun mu4e-walk-switch-up ()
  "Switch to next email address in the line above."
  (interactive)
  (mu4e-walk--switch 'up))

;;;###autoload
(defun mu4e-walk-switch-down ()
  "Switch to next email address in the line below."
  (interactive)
  (mu4e-walk--switch 'down))

;;;###autoload
(defun mu4e-walk-switch-left ()
  "Switch to next email address on the left."
  (interactive)
  (mu4e-walk--switch 'left))

;;;###autoload
(defun mu4e-walk-switch-right ()
  "Switch to next email address on th right."
  (interactive)
  (mu4e-walk--switch 'right))


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
                           (call-interactively (quote ,oldfun)))))))

(defun mu4e-walk-add-keybindings-compose-mode ()
  "Function which is added to a mode hook."
  (use-local-map (copy-keymap mu4e-compose-mode-map))
  (mu4e-walk-extend-key "M-<up>" 'mu4e-walk-up)
  (mu4e-walk-extend-key "M-<down>" 'mu4e-walk-down)
  (mu4e-walk-extend-key "M-<left>" 'mu4e-walk-left)
  (mu4e-walk-extend-key "M-<right>" 'mu4e-walk-right)
  (mu4e-walk-extend-key "C-<up>" 'mu4e-walk-switch-up)
  (mu4e-walk-extend-key "C-<down>" 'mu4e-walk-switch-down)
  (mu4e-walk-extend-key "C-<left>" 'mu4e-walk-switch-left)
  (mu4e-walk-extend-key "C-<right>" 'mu4e-walk-switch-right))

;; (add-hook 'mu4e-compose-mode-hook 'mu4e-walk-add-keybindings-compose-mode)


(provide 'mu4e-walk)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; mu4e-walk.el ends here

