;;; org-upcoming-modeline.el --- Show next org event in mode line -*- lexical-binding: t -*-

;; Copyright (C) 2020--2023 Kevin Brubeck Unhammer

;; Author: Kevin Brubeck Unhammer <unhammer@fsfe.org>
;; Version: 0.1.5
;; Package-Requires: ((emacs "26.1") (ts "0.2") (org-ql "0.6"))
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

;; This package shows you the next upcoming org appointment in your
;; modeline.

;; You can left-click on it to go to it, right-click for a menu of
;; actions, middle-click to temporarily hide it.

;; After installing, enable with:

;; (use-package org-upcoming-modeline
;;   :after org
;;   :config (org-upcoming-modeline-mode))

;; See also M-x customize-group RET org-upcoming-modeline.

;;; Code:

(require 'ts)
(require 'org-ql)

(defsubst org-upcoming-modeline--parse-ts (org-ts-string)
  "Get ts object for Org timestamp string ORG-TS-STRING.
Returns nil if no hour/minute part."
  (pcase-let* ((`(,_second ,minute ,hour ,day ,month ,year)
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

(defcustom org-upcoming-modeline-keep-late 900
  "Show this many seconds after the event has begun, unless we're clocked into it."
  :group 'org-upcoming-modeline
  :type 'integer)

(defcustom org-upcoming-modeline-trim 20
  "Trim the org headline to this many characters.
No trimming if set to nil."
  :group 'org-upcoming-modeline
  :type '(choice (integer :tag "Show at most this many characters")
                 (const :tag "Never trim the string" nil)))

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

(defcustom org-upcoming-modeline-ignored-keywords nil
  "Which keywords to ignores (e.g. DONE)."
  :group 'org-upcoming-modeline
  :type '(repeat string))

(defcustom org-upcoming-modeline-format #'org-upcoming-modeline-default-format
  "A function to turn time-string and heading into a mode-line string."
  :group 'org-upcoming-modeline
  :type 'function)

(defface org-upcoming-modeline-normal-face
  '((default (:inherit mode-line-emphasis)))
  "Org Upcoming Modeline face for normal circumstances."
  :group 'org-upcoming-modeline)

(defface org-upcoming-modeline-soon-face
  '((default
      (:inherit org-upcoming-modeline-normal-face)
      (:foreground "red")))
  "Org Upcoming Modeline face for when an event is coming up soon."
  :group 'org-upcoming-modeline)

(defcustom org-upcoming-modeline-snooze-seconds (* 5 60)
  "How long to snooze when mouse-2-clicking the modeline.
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



(defun org-upcoming-modeline--trim (heading)
  "Trim HEADING to `org-upcoming-modeline-trim' if set and necessary."
  (if (and org-upcoming-modeline-trim
           (> (length heading)
              org-upcoming-modeline-trim))
      (concat (string-trim (substring heading
                                      0
                                      org-upcoming-modeline-trim))
              "…")
    heading))

(defvar org-upcoming-modeline-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line down-mouse-1] 'org-upcoming-modeline-goto)
    (define-key map [mode-line down-mouse-2] 'org-upcoming-modeline-snooze)
    (define-key map [mode-line down-mouse-3] 'org-upcoming-modeline-popup-menu)
    map))


(defun org-upcoming-modeline--encode-ts (ts)
  "Turn a TS struct into an Emacs TIME value."
  (encode-time (list (ts-second ts) (ts-minute ts) (ts-hour ts) (ts-day ts) (ts-month ts) (ts-year ts))))

(defun org-upcoming-modeline--days-between (d1 d2)
  "Count number of days occurring between D1 and earlier D2.
Ignores any TZ/DST info."
  (- (time-to-days (org-upcoming-modeline--encode-ts d1))
     (time-to-days (org-upcoming-modeline--encode-ts d2))))

(defun org-upcoming-modeline--format-ts (time now)
  "Human readable description of time left until TIME for display in mode-line.
NOW should be `ts-now' (an argument for ease of testing)."
  (let* ((seconds-until (ts-difference time now))
         (days-until (org-upcoming-modeline--days-between time now)))
    (cond ((ts< time now)
           (concat "-" (ts-human-format-duration (- seconds-until) 'abbreviate)))
          ((<= seconds-until org-upcoming-modeline-duration-threshold) ; "1m32s"
           (ts-human-format-duration seconds-until 'abbreviate))
          ((= days-until 0)             ; "7:45"
           (ts-format "%H:%M" time))
          ((= days-until 1)             ; "tomorrow 7:45"
           (concat (cdr (assoc 'tomorrow org-upcoming-modeline-l10n))
                   (ts-format " %H:%M" time)))
          ((< days-until 7)             ; "Fri 7:45"
           (ts-format "%a %H:%M" time))
          ((< days-until 28)            ; "Fri 7, 7:45"
           (ts-format "%a %-e, %H:%M" time))
          ((< days-until 365)           ; "7 July, 7:45"
           ;; TODO: would like to have "%x %X" but without year and timezone
           (ts-format "%-e %b %H:%M" time))
          (t
           (ts-format "%-e %b %Y, %H:%M" time)))))

(defun org-upcoming-modeline--set-string ()
  "Set the modeline string to the next upcoming event.
Sets `org-upcoming-modeline-string' based on
`org-upcoming-modeline--current-event'."
  (setq
   org-upcoming-modeline-string
   (when org-upcoming-modeline--current-event
     (pcase-let* ((`(,time ,heading ,marker) org-upcoming-modeline--current-event)
                  (now (ts-now))
                  (seconds-until (ts-difference time now))
                  (time-string (org-upcoming-modeline--format-ts time now)))
       (propertize (funcall org-upcoming-modeline-format
                            time-string
                            (org-upcoming-modeline--trim heading))
                   'face (if (<= 0 seconds-until org-upcoming-modeline-soon)
                             'org-upcoming-modeline-soon-face
                           'org-upcoming-modeline-normal-face)
                   'help-echo (format "%s left until %s (mouse-1: goto, mouse-2: snooze, mouse-3: menu)"
                                      (ts-human-format-duration seconds-until)
                                      heading)
                   'org-upcoming-marker marker
                   'mouse-face 'mode-line-highlight
                   'local-map org-upcoming-modeline-map)))))

(defun org-upcoming-modeline-default-format (time-string heading)
  "Format TIME-STRING and HEADING as a string for displaying in the mode-line.
Used as default for `org-upcoming-modeline-format'."
  (format " ⏰ %s: %s" time-string heading))

(defun org-upcoming-modeline--find-event ()
  "Find the first upcoming org event, with timestamp and marker.
Store it in `org-upcoming-modeline--current-event'.

Does nothing if `org-agenda-files' is nil."
  (setq
   org-upcoming-modeline--current-event
   (when-let*
       ((org-files (org-agenda-files))
        (start-time (ts-adjust 'second (- org-upcoming-modeline-keep-late)
                               (ts-now)))
        (end-time (ts-adjust 'day org-upcoming-modeline-days-ahead
                             (ts-now)))
        (items (remove
                nil
                (org-ql-select org-files
                  `(and (ts-upcoming :from ,start-time
                                     :to ,end-time)
                        (not ,@(if org-upcoming-modeline-ignored-keywords
                                   `((todo ,@org-upcoming-modeline-ignored-keywords))
                                 '(nil))))
                  :action `(when-let* ((mark (point-marker))
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
                                                               when (and time
                                                                         (ts<= ,start-time time))
                                                               collect time)
                                                      #'ts<)))))
                             (list time mark))))))
     (pcase-let*
         ((`(,time ,marker . nil) (car (seq-sort-by #'car #'ts< items)))
          (heading (org-with-point-at marker
                     (org-link-display-format (nth 4 (org-heading-components))))))
       (list time heading marker)))))


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

(org-ql-defpred ts-upcoming
  (&key from to _on _regexp (match-group 0) (limit (org-entry-end-position)))
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
              (to (test-timestamps (ts<= next-ts to))))))))



;;;###autoload
(define-minor-mode org-upcoming-modeline-mode
  "Show next upcoming `org-mode' event in mode line."
  :group 'org-upcoming-modeline
  :global t
  (if org-upcoming-modeline-mode
      (progn (org-upcoming-modeline--enable)
             ;; Also compute immediately on first starting the mode, for that first-run feel:
             (org-upcoming-modeline--find-event))
    (org-upcoming-modeline--disable)))

(defun org-upcoming-modeline--enable ()
  "Add to mode line and start and store timers."
  (if global-mode-string
      (add-to-list 'global-mode-string 'org-upcoming-modeline-string 'append)
    (setq global-mode-string '("" org-upcoming-modeline-string)))
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Right click events, menu: ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-upcoming-modeline--get-marker (event)
  "Get the marker stored at EVENT text.
Fallback to marker of `org-upcoming-modeline-string'."
  (when-let* ((text (or (car (cl-fifth (cadr event))) ; TODO there's gotta be some event api for this
                        org-upcoming-modeline-string)))
    (get-text-property 0 'org-upcoming-marker text)))

(defun org-upcoming-modeline-goto (event)
  "Show upcoming org EVENT."
  (interactive "e")
  (when-let* ((marker (org-upcoming-modeline--get-marker event)))
    (select-window (display-buffer (marker-buffer marker)))
    (widen)
    (goto-char (marker-position marker))
    (org-show-entry)
    (org-back-to-heading t)
    (org-cycle-hide-drawers 'children)
    (org-reveal)))

(defun org-upcoming-modeline-clock-in (event)
  "Clock in to upcoming org EVENT."
  (interactive "e")
  (when-let* ((marker (org-upcoming-modeline--get-marker event)))
    (with-current-buffer (org-base-buffer (marker-buffer marker))
      (org-with-wide-buffer
       (goto-char (marker-position marker))
       (org-clock-in)))))

(defun org-upcoming-modeline-snooze (_event)
  "Hide it for five minutes, ignore EVENT."
  (interactive "e")
  (message "Disabling org-upcoming-modeline for five minutes")
  (org-upcoming-modeline--disable)
  (force-mode-line-update)
  (run-with-timer org-upcoming-modeline-snooze-seconds
                  nil
                  (lambda () (when org-upcoming-modeline-mode
                          (org-upcoming-modeline--enable)))))

(easy-menu-define org-upcoming-modeline--menu nil "Dynamic Menu."
  '(
    "Org Upcoming Modeline"
    ["Go to event" org-upcoming-modeline-goto]
    ["Snooze for five minutes" org-upcoming-modeline-snooze]
    ["Clock in" org-upcoming-modeline-clock-in]))

(defun org-upcoming-modeline-popup-menu (event &optional prefix)
  "Popup a context menu for EVENT, passing on optional PREFIX."
  (interactive "@e \nP")
  (popup-menu org-upcoming-modeline--menu event prefix))




(provide 'org-upcoming-modeline)

;;; org-upcoming-modeline.el ends here
