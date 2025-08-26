;;; mu4e-walk.el --- Send email addresses for a walk   -*- lexical-binding: t -*-

;; Copyright (C) 2025 Timm Lichte

;; Author: Timm Lichte <timm.lichte@uni-tuebingen.de>
;; URL: 
;; Version: 0
;; Last modified: 2025-08-24 Sun 15:50:19
;; Package-Requires: ((mu4e "1.12.11"))
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

;;; Code:

(require 'mu4e)


;;====================
;;
;; Customization
;;
;;--------------------

(defvar description+email-regexp "[[:space:]]*\\(\"[^\"]*\"[[:space:]]*<[-+_.~a-zA-Z][-+_.~:a-zA-Z0-9]*@[-.a-zA-Z0-9]+>\\|[^,:\"]*<?[-+_.~a-zA-Z][-+_.~:a-zA-Z0-9]*@[-.a-zA-Z0-9]+>?\\)"
  "Regular expression of an email address including optional description.")

(defvar mu4e-walk--email-region-start nil)
(defvar mu4e-walk--email-region-end nil)
(defvar mu4e-walk--email-rel-pos nil)
(defvar mu4e-walk--region-active nil)


;;====================
;;
;; Identifcation and cleansing
;;
;;--------------------

(defun mu4e-walk--point-in-address-field-p ()
  "Non-nil if point is point is in an adress field."
  (save-excursion
    (beginning-of-line)
    (and (message-point-in-header-p)
         (re-search-forward message-email-recipient-header-regexp 
                            (line-end-position) t))))

(defun mu4e-walk--email-address-at-point ()
  "Return the email address as a string including the descriptor in a message header.

As a side effect, the position of the email address is stored in 
`mu4e-walk--email-region-start' and `mu4e-walk--email-region-end'."
  (when (and (message-point-in-header-p)
             (mu4e-walk--point-in-address-field-p)
             (not (or (eq ?, (char-after))
                      (eq ?: (char-after)))))
    (setq mu4e-walk--region-active nil)
    (if (region-active-p)
        (progn
          (setq mu4e-walk--email-region-start (region-beginning)
                mu4e-walk--email-region-end (region-end)
                mu4e-walk--region-active t
                mu4e-walk--email-rel-pos (- (region-end) (region-beginning)))
          (buffer-substring-no-properties (region-beginning) (region-end)))
      (save-excursion
        (let ((point (point)))
          (cl-loop
           while (re-search-backward "[,:]" (line-beginning-position) t)
           do (let ((start (+ (point) 1))
                    (end (re-search-forward description+email-regexp nil t)))
                (when (<= point end)
                  (let ((email (string-trim
                                (string-replace
                                 "\n" ""
                                 (buffer-substring-no-properties start end)))))
                    (when (string-match (concat "^" description+email-regexp "$")
                                        email)
                      (setq mu4e-walk--email-region-start start
                            mu4e-walk--email-region-end end
                            mu4e-walk--email-rel-pos (max 0 (- end point)))
                      (cl-return email))
                    ))
                (goto-char start) ;; Return to start position before again searching backward 
                )))))))

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
  "Move email address to previous or next address field. If DIRECTION is 'backward,
move it to the previous address field, else to the next one."
  (let* ((search-function (if (eq direction 'backward)
                              're-search-backward 're-search-forward))
         (next-field-line-number (save-excursion
                                   (when (eq direction 'backward) (beginning-of-line))
                                   (and (funcall search-function
                                                 message-email-recipient-header-regexp nil t)
                                        (message-point-in-header-p)
                                        (line-number-at-pos))))
         (email (mu4e-walk--email-address-at-point)))
    (when (and (mu4e-walk--point-in-address-field-p)
               next-field-line-number
               email)
      (delete-region mu4e-walk--email-region-start
                     mu4e-walk--email-region-end)
      (mu4e-walk--clean-address-field-at-point)
      ;; Goto target line
      (goto-char (point-min))
      (forward-line (1- next-field-line-number))
      (mu4e-walk--clean-address-field-at-point)
      (end-of-line)
      (unless (looking-back ": ") (insert ", "))
      (insert email)
      (when mu4e-walk--region-active
        (push-mark)
        (setq deactivate-mark nil)
        (setq mark-active t))
      (backward-char mu4e-walk--email-rel-pos)
      )))

;;;###autoload
(defun mu4e-walk-up ()
  "Move email address up to previous address field."
  (interactive)
  (mu4e-walk--move-email-address-at-point 'backward))

;;;###autoload
(defun mu4e-walk-down ()
  "Move email address down to next address field."
  (interactive)
  (mu4e-walk--move-email-address-at-point))

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
  (mu4e-walk-extend-key "M-<down>" 'mu4e-walk-down))

;; (add-hook 'mu4e-compose-mode-hook 'mu4e-walk-add-keybindings-compose-mode)


(provide 'mu4e-walk)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; mu4e-walk.el ends here

