;;; Code:

(require 'dash)
(require 'ts)
(require 'org-ql)

(defsubst org-upcoming-modeline-parse-ts (org-ts-string)
  "Get ts object for Org timestamp string ORG-TS-STRING but only if it has hour/minute part."
  (pcase-let* ((`(,second ,minute ,hour ,day ,month ,year)
                (save-match-data
                  (org-parse-time-string org-ts-string 'nodefault))))
    (and minute
         hour
         (make-ts :second 0 :minute minute :hour hour :day day :month month :year year))))

(defvar org-upcoming-modeline-duration-threshold 3600
  "If less than this many seconds, print as duration instead of timestamp.")

(defun org-upcoming-modeline ()
  "Make a modeline string of the first upcoming org timestamp."
  (when-let* ((now (ts-now))
              (tomorrow (ts-format "%Y-%m-%d" (ts-adjust 'day 1 now)))
              (tasks (org-ql-select (org-agenda-files)
                       `(ts-active :from today :to ,tomorrow)
                       :action (lambda ()
                                 (-let* (((&alist "ITEM" headline
		                                  "TIMESTAMP" timestamp
		                                  "DEADLINE" deadline
		                                  "SCHEDULED" scheduled)
	                                  (org-entry-properties)))
                                   (seq-uniq (cl-loop
                                              for org-ts-string in (list timestamp deadline scheduled)
                                              when org-ts-string
                                              for time = (org-upcoming-modeline-parse-ts org-ts-string)
                                              when (and time
                                                        (ts> time now))
                                              collect
                                              (cons time headline)))))))
              (sorted-tasks (seq-sort-by #'car #'ts< (apply 'append tasks)))
              (task (car sorted-tasks))
              (event-time (car task))
              (event-title (cdr task))
              (seconds-until (ts-difference event-time now))
              (days-until (- (ts-day event-time)
                             (ts-day now)))
              (event-time-string (cond ((<= seconds-until org-upcoming-modeline-duration-threshold)
                                        (ts-human-format-duration seconds-until 'abbreviate))
                                       ((= 0 days-until)
                                        (ts-format "%H:%M" event-time))
                                       ((= 1 days-until)
                                        (ts-format "tomorrow %H:%M" event-time))
                                       (t ; > 1 days-until
                                        (ts-format "%a %H:%M" event-time)))) )
    (propertize
     (format " ⏰ %s: %s" event-time-string event-title)
     'face 'org-level-4
     'help-echo (format "%s at <%s>"
                        event-title
                        (ts-format "%Y-%m-%d %H:%M" event-time)))))

(add-to-list 'global-mode-string '(:eval (org-upcoming-modeline)) 'append)

(provide 'org-upcoming-modeline)

;;; org-upcoming-modeline.el ends here
