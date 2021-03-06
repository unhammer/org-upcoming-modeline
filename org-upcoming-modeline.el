;;; org-upcoming-modeline.el --- Show next org event in mode line -*- lexical-binding: t -*-

;; Copyright (C) 2020 Kevin Brubeck Unhammer

;; Author: Kevin Brubeck Unhammer <unhammer@fsfe.org>
;; Version: 0.1.1
;; Package-Requires: ((emacs "26.1") (ts "0.2") (org-ql "0.5"))
;; URL: https://github.com/unhammer/org-upcoming-modeline
;; Keywords: convenience, calendar

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Just M-x org-upcoming-modeline-mode to turn it on.

;;; Code:

(require 'ts)
(require 'org-ql)

(defsubst org-upcoming-modeline--parse-ts (org-ts-string)
  "Get ts object for Org timestamp string ORG-TS-STRING but only if it has hour/minute part."
  (pcase-let* ((`(,second ,minute ,hour ,day ,month ,year)
                (save-match-data
                  (org-parse-time-string org-ts-string 'nodefault))))
    (and minute
         hour
         (make-ts :second 0 :minute minute :hour hour :day day :month month :year year))))

(defgroup org-upcoming-modeline nil
  "Options for showing upcoming org event in the mode line."
  :tag "Org Upcoming Modeline"
  :group 'org)

(defcustom org-upcoming-modeline-duration-threshold 3600
  "If less than this many seconds, print as duration instead of timestamp."
  :group 'org-upcoming-modeline
  :type 'integer)

(defcustom org-upcoming-modeline-l10n '((tomorrow . "tomorrow"))
  "Localisation."
  :group 'org-upcoming-modeline
  :type '(alist :value-type string))

(defcustom org-upcoming-modeline-recompute-after-idle 5
  "Minimum seconds of idle-time before updating upcoming event in the mode line."
  :group 'org-upcoming-modeline
  :type 'integer)

(defcustom org-upcoming-modeline-interval 5
  "Minimum seconds between updating event time in the mode line."
  :group 'org-upcoming-modeline
  :type 'integer)

(defcustom org-upcoming-modeline-days-ahead 1
  "How many days to look into the future for events."
  :group 'org-upcoming-modeline
  :type 'integer)

(defcustom org-upcoming-modeline-soon (* 15 60)
  "Number of seconds to consider an upcoming event \"close\"."
  :group 'org-upcoming-modeline
  :type 'integer)

(defface org-upcoming-modeline-normal-face
  '((default (:inherit mode-line-emphasis)))
  "Org Upcoming Modeline face for normal circumstances"
  :group 'org-upcoming-modeline)

(defface org-upcoming-modeline-soon-face
  '((default
      (:inherit org-upcoming-modeline-normal-face)
      (:foreground "red")))
  "Org Upcoming Modeline face for when an event is coming up soon."
  :group 'org-upcoming-modeline)

(defcustom org-upcoming-modeline-snooze-seconds (* 5 60)
  "How long to snooze when mouse-3-clicking the modeline.
Used by `org-upcoming-modeline-snooze'."
  :group 'org-upcoming-modeline
  :type 'integer)


(defconst org-upcoming-modeline-string nil)
;;;###autoload(put 'org-upcoming-modeline-string 'risky-local-variable t)
(put 'org-upcoming-modeline-string 'risky-local-variable t)

(defvar org-upcoming-modeline--current-event nil
  "Value from last `org-upcoming-modeline--find-upcoming'.")

(defvar org-upcoming-modeline--find-event-timer nil)
(defvar org-upcoming-modeline--set-string-timer nil)



(defun org-upcoming-modeline--set-string ()
  "Set `org-upcoming-modeline-string' based on `org-upcoming-modeline--current-event'."
  (setq
   org-upcoming-modeline-string
   (when org-upcoming-modeline--current-event
     (pcase-let* ((now (ts-now))
                  (`(,time ,heading ,marker) org-upcoming-modeline--current-event)
                  (seconds-until (ts-difference time now))
                  ;; NOTE: Using day of year to avoid end-of-month turnover in day number.
                  (days-until (- (ts-doy time)
                                 (ts-doy now)))
                  (time-string (cond ((<= seconds-until org-upcoming-modeline-duration-threshold)
                                      (ts-human-format-duration seconds-until 'abbreviate))
                                     ((= 0 days-until)
                                      (ts-format "%H:%M" time))
                                     ((= 1 days-until)
                                      (concat (cdr (assoc 'tomorrow org-upcoming-modeline-l10n))
                                              (ts-format " %H:%M" time)))
                                     (t      ; > 1 days-until
                                      (ts-format "%a %H:%M" time)))))
       (propertize (format " ⏰ %s: %s" time-string heading)
                   'face (if (<= 0 seconds-until org-upcoming-modeline-soon)
                             'org-upcoming-modeline-soon-face
                           'org-upcoming-modeline-normal-face)
                   'help-echo (format "%s left until %s (mouse-3 will snooze, mouse-1 will jump to task)"
                                      (ts-human-format-duration seconds-until)
                                      heading)
                   'org-upcoming-marker marker
                   'mouse-face 'mode-line-highlight
                   'local-map org-upcoming-modeline-map)))))

(defun org-upcoming-modeline--find-event ()
  "Find the first upcoming org event, with timestamp and marker.
Store it in `org-upcoming-modeline--current-event'."
  (setq
   org-upcoming-modeline--current-event
   (when-let*
       ((items (remove
                nil
                (org-ql-select (org-agenda-files)
                  `(ts-upcoming :from ,(ts-now)
                                :to ,(ts-adjust 'day org-upcoming-modeline-days-ahead
                                                (ts-now)))
                  :action '(when-let* ((mark (point-marker))
                                       (from-day (time-to-days (current-time)))
                                       (bound (save-excursion (outline-next-heading) (point)))
                                       (time (save-excursion
                                               (car
                                                (sort (cl-loop while (re-search-forward org-tsr-regexp bound 'noerror)
                                                               for org-ts-string = (match-string 1)
                                                               when org-ts-string
                                                               for time = (org-upcoming-modeline--parse-upcoming org-ts-string
                                                                                                                 from-day
                                                                                                                 #'org-upcoming-modeline--parse-ts)
                                                               when time
                                                               collect time)
                                                      #'ts<)))))
                             (list time mark))))))
     (pcase-let*
         ((`(,time ,marker . nil) (car (seq-sort-by #'car #'ts< items)))
          (heading (org-with-point-at marker
                     (org-link-display-format (nth 4 (org-heading-components))))))
       (list time heading marker)))))

;;;###autoload
(define-minor-mode org-upcoming-modeline-mode
  "Show next upcoming org-mode event in mode line"
  :group 'org-upcoming-modeline
  :global t
  (if org-upcoming-modeline-mode
      (progn (org-upcoming-modeline--enable)
             ;; Also compute immediately on first starting the mode, for that first-run feel:
             (org-upcoming-modeline--find-event))
    (org-upcoming-modeline--disable)))

(defun org-upcoming-modeline--enable ()
  "Add to mode line and start and store timers."
  (add-to-list 'global-mode-string 'org-upcoming-modeline-string 'append)
  (setq org-upcoming-modeline--find-event-timer (run-with-idle-timer
                                                 org-upcoming-modeline-recompute-after-idle
                                                 'repeat
                                                 #'org-upcoming-modeline--find-event))
  (setq org-upcoming-modeline--set-string-timer (run-with-timer
                                                 1
                                                 org-upcoming-modeline-interval
                                                 #'org-upcoming-modeline--set-string)))

(defun org-upcoming-modeline--disable ()
  "Remove from mode line and stop timers."
  (delq 'org-upcoming-modeline-string global-mode-string)
  (when (timerp org-upcoming-modeline--find-event-timer)
    (cancel-timer org-upcoming-modeline--find-event-timer))
  (when (timerp org-upcoming-modeline--set-string-timer)
    (cancel-timer org-upcoming-modeline--set-string-timer)))



(defun org-upcoming-modeline-goto (event)
  "Show upcoming org EVENT."
  (interactive "e")
  (when-let* ((text (car (cl-fifth (cadr event)))) ; TODO there's gotta be some event api for this
              (marker (get-text-property 0 'org-upcoming-marker text)))
    (switch-to-buffer (marker-buffer marker))
    (widen)
    (goto-char (marker-position marker))
    (org-show-entry)
    (org-back-to-heading t)
    (org-cycle-hide-drawers 'children)
    (org-reveal)))

(defun org-upcoming-modeline-snooze (event)
  "Hide it for five minutes, ignore EVENT."
  (interactive "e")
  (org-upcoming-modeline--disable)
  (force-mode-line-update)
  (run-with-timer org-upcoming-modeline-snooze-seconds
                  nil
                  (lambda () (when org-upcoming-modeline-mode
                          (org-upcoming-modeline--enable)))))

(defconst org-upcoming-modeline-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line down-mouse-1] 'org-upcoming-modeline-goto)
    (define-key map [mode-line down-mouse-2] 'org-upcoming-modeline-goto)
    (define-key map [mode-line down-mouse-3] 'org-upcoming-modeline-snooze)
    map))

(defun org-upcoming-modeline-ts-to-time (ts)
  "Turn a timestamp TS into format of `current-time'."
  (encode-time (list 0
                     0
                     12
                     (ts-d ts)
                     (ts-m ts)
                     (ts-year ts)
                     'ignored
                     (- 1)              ; guess
                     (ts-tz-offset ts))))

(defun org-upcoming-modeline--parse-upcoming (org-ts-string from-day ts-org-parser)
  "Parse org timestamp ORG-TS-STRING into ts structure using TS-ORG-PARSER.
If it has repeats, use the nearest instance at or after FROM-DAY."
  (if (string-match "\\+\\([0-9]+\\)\\([hdwmy]\\)" org-ts-string)
      (when-let* ((initial-ts (funcall ts-org-parser org-ts-string))
                  (initial-day (time-to-days (org-upcoming-modeline-ts-to-time initial-ts)))
                  (upcoming-day (org-time-string-to-absolute org-ts-string from-day 'future))
                  (adjustment (- upcoming-day initial-day)))
        (ts-adjust 'day adjustment initial-ts))
    ;; No repeats, just use the regular parse:
    (funcall ts-org-parser org-ts-string)))

(if (fboundp #'org-ql--defpred)
    ;; org-ql <=0.5.0
    (org-ql--defpred ts-upcoming
      (&key from to _on regexp (match-group 0) (limit (org-entry-end-position)))
      "As ts-active, but handle repeats by picking the one closest to FROM.
And because we don't have the hack in
`org-ql--pre-process-query', using this requires manually setting
FROM/TO to dates when calling org-ql."
      (let ((regexp org-tsr-regexp)
            (from-day (time-to-days (org-upcoming-modeline-ts-to-time
                                     from))))
        (cl-macrolet ((next-timestamp ()
                                      `(when (re-search-forward regexp limit t)
                                         (org-upcoming-modeline--parse-upcoming (match-string match-group)
                                                                                from-day
                                                                                #'ts-parse-org)))
                      (test-timestamps (pred-form)
                                       `(cl-loop for next-ts = (next-timestamp)
                                                 while next-ts
                                                 thereis ,pred-form)))
          (save-excursion
            (cond ((not (or from to)) (re-search-forward regexp limit t))
                  ((and from to) (test-timestamps (ts-in from to next-ts)))
                  (from (test-timestamps (ts<= from next-ts)))
                  (to (test-timestamps (ts<= next-ts to))))))))
  ;; org-ql >0.5.0
  (org-ql-defpred ts-upcoming
    (&key from to _on regexp (match-group 0) (limit (org-entry-end-position)))
    "As ts-active, but handle repeats by picking the one closest to FROM.
And no normalisers yet, so using this requires manually setting
FROM/TO to dates when calling org-ql."
    :body
    (let ((regexp org-tsr-regexp)
          (from-day (time-to-days (org-upcoming-modeline-ts-to-time
                                   from))))
      (cl-macrolet ((next-timestamp ()
                                    `(when (re-search-forward regexp limit t)
                                       (org-upcoming-modeline--parse-upcoming (match-string match-group)
                                                                              from-day
                                                                              #'ts-parse-org)))
                    (test-timestamps (pred-form)
                                     `(cl-loop for next-ts = (next-timestamp)
                                               while next-ts
                                               thereis ,pred-form)))
        (save-excursion
          (cond ((not (or from to)) (re-search-forward regexp limit t))
                ((and from to) (test-timestamps (ts-in from to next-ts)))
                (from (test-timestamps (ts<= from next-ts)))
                (to (test-timestamps (ts<= next-ts to)))))))))

(provide 'org-upcoming-modeline)

;;; org-upcoming-modeline.el ends here
