;;; org-upcoming-modeline-test.el --- Tests for org-upcoming-modeline

;; Copyright (C) 2023 Kevin Brubeck Unhammer

;; Author: Kevin Brubeck Unhammer <unhammer@fsfe.org>

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

(require 'org-upcoming-modeline nil t)

(ert-deftest org-upcoming-modeline-format-time ()
  (let* ((org-upcoming-modeline-l10n '((tomorrow . "tomorrow")))
         (system-time-locale "C.UTF-8")
         (org-upcoming-modeline-duration-threshold 3600)
         (now       (make-ts :hour 10 :minute 0 :second 0 :day 1 :month 1 :year 2000))
         (past90s   (ts-adjust 'second (- 90) now))
         (in90s     (ts-adjust 'second 90 now))
         (in1hour   (ts-adjust 'hour 1 now))
         (tomorrow9 (ts-adjust 'day 1 'hour (- 1) now))
         (in2days   (ts-adjust 'day 2 now))
         (in6days   (ts-adjust 'day 6 now))
         (in7days   (ts-adjust 'day 7 now))
         (in10days  (ts-adjust 'day 10 now))
         (in28days  (ts-adjust 'day 28 now))
         (in60days  (ts-adjust 'day 60 now))
         (in1year   (ts-adjust 'year 1 now)))
    (should (equal (org-upcoming-modeline--format-ts  past90s now)   "-1m30s"))
    (should (equal (org-upcoming-modeline--format-ts  in90s now)     "1m30s"))
    (should (equal (org-upcoming-modeline--format-ts  in1hour now)   "1h"))
    (should (equal (org-upcoming-modeline--format-ts  tomorrow9 now) "tomorrow 09:00"))
    (should (equal (org-upcoming-modeline--format-ts  in2days now)   "Mon 10:00"))
    (should (equal (org-upcoming-modeline--format-ts  in6days now)   "Fri 10:00"))
    (should (equal (org-upcoming-modeline--format-ts  in7days now)   "Sat 8, 10:00"))
    (should (equal (org-upcoming-modeline--format-ts  in10days now)  "Tue 11, 10:00"))
    (should (equal (org-upcoming-modeline--format-ts  in28days now)  "29 Jan 10:00"))
    (should (equal (org-upcoming-modeline--format-ts  in60days now)  "1 Mar 10:00"))
    (should (equal (org-upcoming-modeline--format-ts  in1year now)   "1 Jan 2001, 10:00"))))

(ert-deftest org-upcoming-modeline-format-time-newyear ()
  (let* ((org-upcoming-modeline-l10n '((tomorrow . "tomorrow")))
         (system-time-locale "C.UTF-8")
         (org-upcoming-modeline-duration-threshold 3600)
         (now       (make-ts :hour 10 :minute 0 :second 0 :day 31 :month 12 :year 1999))
         (past90s   (ts-adjust 'second (- 90) now))
         (in90s     (ts-adjust 'second 90 now))
         (in1hour   (ts-adjust 'hour 1 now))
         (tomorrow9 (ts-adjust 'day 1 'hour (- 1) now))
         (in2days   (ts-adjust 'day 2 now))
         (in6days   (ts-adjust 'day 6 now))
         (in7days   (ts-adjust 'day 7 now))
         (in10days  (ts-adjust 'day 10 now))
         (in28days  (ts-adjust 'day 28 now))
         (in60days  (ts-adjust 'day 60 now))
         (in1year   (ts-adjust 'year 1 now)))
    (should (equal (org-upcoming-modeline--format-ts  past90s now)   "-1m30s"))
    (should (equal (org-upcoming-modeline--format-ts  in90s now)     "1m30s"))
    (should (equal (org-upcoming-modeline--format-ts  in1hour now)   "1h"))
    (should (equal (org-upcoming-modeline--format-ts  tomorrow9 now) "tomorrow 09:00"))
    (should (equal (org-upcoming-modeline--format-ts  in2days now)   "Sun 10:00"))
    (should (equal (org-upcoming-modeline--format-ts  in6days now)   "Thu 10:00"))
    (should (equal (org-upcoming-modeline--format-ts  in7days now)   "Fri 7, 10:00"))
    (should (equal (org-upcoming-modeline--format-ts  in10days now)  "Mon 10, 10:00"))
    (should (equal (org-upcoming-modeline--format-ts  in28days now)  "28 Jan 10:00"))
    (should (equal (org-upcoming-modeline--format-ts  in60days now)  "29 Feb 10:00"))
    (should (equal (org-upcoming-modeline--format-ts  in1year now)   "31 Dec 2000, 10:00"))))

(ert-deftest org-upcoming-modeline-refreshes-expired-event-before-rendering ()
  "An idle-timer delay must not expose a negative cached countdown."
  (let* ((now (make-ts :hour 10 :minute 0 :second 5 :day 1 :month 1 :year 2000))
         (old-time (ts-adjust 'second (- 5) now))
         (new-time (ts-adjust 'minute 30 now))
         (org-upcoming-modeline--current-event (list old-time "old" nil))
         (org-upcoming-modeline-format (lambda (time heading)
                                         (format "%s:%s" time heading)))
         refreshed)
    (cl-letf (((symbol-function 'ts-now) (lambda () now))
              ((symbol-function 'org-upcoming-modeline--find-event)
               (lambda ()
                 (setq refreshed t
                       org-upcoming-modeline--current-event
                       (list new-time "current" nil)))))
      (org-upcoming-modeline--set-string))
    (should refreshed)
    (should (equal (substring-no-properties org-upcoming-modeline-string)
                   "30m:current"))))

(ert-deftest org-upcoming-modeline-finds-later-timestamp-under-heading ()
  (let ((file (make-temp-file "org-upcoming-modeline-" nil ".org"))
        (now (make-ts :hour 17 :minute 50 :second 0
                      :day 21 :month 9 :year 2026)))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "* Event\n"
                    "<2026-09-21 Mon 15:00-16:00>\n"
                    "<2026-09-21 Mon 17:45-18:30>\n"))
          (let ((org-agenda-files (list file))
                (org-upcoming-modeline-show-running t)
                (org-upcoming-modeline-only-show-soon t))
            (cl-letf (((symbol-function 'ts-now) (lambda () now)))
              (org-upcoming-modeline--find-event))
            (should org-upcoming-modeline-running-p)
            (should (equal (ts-format "%H:%M"
                                      (car org-upcoming-modeline--current-event))
                           "18:30"))))
      (delete-file file))))

(ert-deftest org-upcoming-modeline-pick-upcoming ()
  (let* ((now (make-ts :hour 10 :minute 0 :day 1 :month 1 :year 2000))
         (soon (list (ts-adjust 'minute 10 now) 'soon))
         (later (list (ts-adjust 'minute 30 now) 'later)))
    (let ((org-upcoming-modeline-only-show-soon nil))
      (should (eq (cadr (org-upcoming-modeline--pick-upcoming
                         (list later soon) now))
                  'soon)))
    (let ((org-upcoming-modeline-only-show-soon t)
          (org-upcoming-modeline-soon (* 15 60)))
      (should (eq (cadr (org-upcoming-modeline--pick-upcoming
                         (list later soon) now))
                  'soon))
      (should-not (org-upcoming-modeline--pick-upcoming (list later) now)))))

(defun org-upcoming-modeline-test--event (start-h start-m end-h end-m)
  "Make a test event starting at START-H:START-M and ending at END-H:END-M."
  (let ((start (make-ts :hour start-h :minute start-m :second 0
                        :day 1 :month 1 :year 2000)))
    (list start
          (and end-h (ts-apply :hour end-h :minute end-m start))
          start-h)))

(ert-deftest org-upcoming-modeline-pick-running-event ()
  (let* ((org-upcoming-modeline-show-running t)
         (org-upcoming-modeline-only-show-soon t)
         (org-upcoming-modeline-soon (* 15 60))
         (events (list (org-upcoming-modeline-test--event 10 0 11 30)
                       (org-upcoming-modeline-test--event 12 0 13 0)))
         (at (lambda (hour minute)
               (org-upcoming-modeline--pick-event
                events (make-ts :hour hour :minute minute :second 0
                                :day 1 :month 1 :year 2000)))))
    (should (equal (funcall at 10 30) (list (nth 1 (car events)) 10 t)))
    (should-not (funcall at 11 40))
    (should (equal (funcall at 11 50) (list (car (cadr events)) 12 nil)))))

(ert-deftest org-upcoming-modeline-upcoming-event-takes-precedence ()
  (let* ((org-upcoming-modeline-show-running t)
         (org-upcoming-modeline-only-show-soon t)
         (org-upcoming-modeline-soon (* 15 60))
         (events (list (org-upcoming-modeline-test--event 10 0 11 30)
                       (org-upcoming-modeline-test--event 11 20 12 0)))
         (now (make-ts :hour 11 :minute 10 :second 0
                       :day 1 :month 1 :year 2000)))
    (should (equal (org-upcoming-modeline--pick-event events now)
                   (list (car (cadr events)) 11 nil)))))

(ert-deftest org-upcoming-modeline-show-running-default-preserves-behavior ()
  (let* ((org-upcoming-modeline-show-running nil)
         (org-upcoming-modeline-only-show-soon nil)
         (event (org-upcoming-modeline-test--event 10 0 11 30))
         (now (make-ts :hour 10 :minute 30 :second 0
                       :day 1 :month 1 :year 2000)))
    (should (equal (org-upcoming-modeline--pick-event (list event) now)
                   (list (car event) 10 nil)))))

(ert-deftest org-upcoming-modeline-range-end ()
  (let ((start (make-ts :hour 10 :minute 0 :second 0
                        :day 5 :month 5 :year 2024)))
    (should (equal (ts-format "%H:%M" (org-upcoming-modeline--range-end
                                      "<2024-05-05 Sun 10:00-11:30>" start))
                   "11:30"))
    (should (equal (ts-format "%F %H:%M" (org-upcoming-modeline--range-end
                                          "<2024-05-05 Sun 23:00-01:00>" start))
                   "2024-05-06 01:00"))
    (should-not (org-upcoming-modeline--range-end
                 "<2024-05-05 Sun 10:00>" start))))

(provide 'org-upcoming-modeline-test)
