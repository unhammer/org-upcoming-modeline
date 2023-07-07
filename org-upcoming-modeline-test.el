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

(provide 'org-upcoming-modeline-test)
