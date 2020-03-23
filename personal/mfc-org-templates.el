(defun shifted-day (event-input days)
  (format-time-string
   (car org-time-stamp-formats)
   (time-add event-input (seconds-to-time (* 3600 24 days)))))

(defun mfc-schedule-template (tasks)
  "Capture template for events"
  (let* ((evt (org-completing-read-no-i "Event: " nil))
         (date-input (org-read-date nil t nil "Start Date: "))
         (date (format-time-string
                (car org-time-stamp-formats)
                date-input)))
    ((lambda (lst) (apply #'concat
                     (-interleave lst
                                  (-repeat (length lst) "\n"))))
        (cons (format "*** TODO %s" evt)
              (-mapcat
               (lambda (info)
                 (apply
                  (lambda (step day &optional subtasks deadline)
                    (-concat (list (format-message "**** TODO %s: %s %s" evt step (if subtasks "[%]" ""))
                                   (format-message "   SCHEDULED: %s %s" (shifted-day date-input day)
                                                   (if deadline (format "DEADLINE: %s" (shifted-day date-input deadline)) "")))
                             (if subtasks (-map (lambda (tsk) (format "    - [ ] %s" tsk)) subtasks) '())))
                  info))
               tasks)))))

(defun mfc-C1-Seed-Train-Schedule ()
  (interactive)
  (insert (mfc-schedule-template
           '(("Thaw" 0)
             ("Expand to 1L" 3 ("Verify pre-sample" "LIMS" "Set up" "Expand" "post sample"))
             ("Expand to 10L" 5 ("Verify pre-sample" "LIMS" "Set up" "Expand" "post sample"))
             ("Set Up 50L" 6 ("Inflate bag" "Acquire media" "Charge" "Check Setpoints" "sparge/span"))
             ("Expand to 50L" 7 ("Verify pre-sample" "LIMS" "Base bag" "Expand" "post sample"))
             ("Set Up 250L" 9 ("Inflate bag" "Acquire media" "Charge" "Check Setpoints" "sparge/span"))
             ("Expand to 250L" 10 ("Verify pre-sample" "LIMS" "Base bag" "Expand" "post sample" "quit 50"))))))


(defun mfc-C1-ST-Tox-Doc-Schedule ()
  (interactive)
  (cl-letf
   ((gather-info (tasks) (-concat tasks '("fill out" "upload" "sign off" "submit"))))
   (insert (mfc-schedule-template
            `(("100mL Thaw" 0 ,(gather-info '("Vial number" "incubator id")))
              ("2L WB Prep And Charge" 3 ,(gather-info '("Bag ID" "Rocker ID")))
              ("2L WB Inoculation" 3 ,(gather-info '("Pre-Sample VCC and Viability")))
              ("20L WB Prep And Charge" 5 ,(gather-info '("Bag ID" "Rocker ID")))
              ("20L WB Inoculation" 5 ,(gather-info '("Pre-Sample VCC and Viability")))
              ("50L Prep And Charge" 6 ,(gather-info '("Bag ID" "Base bag lot" "media lot" "ph probes" "do probes" "sparge")))
              ("50L Inoculation" 7 ,(gather-info '("Pre-Sample VCC and Viability")))
              ("250L Prep And Charge" 10 ,(gather-info '("Bag ID" "Base bag lot" "media lot" "ph probes" "do probes" "sparge")))
              ("250L Inoculation" 10 ,(gather-info '("Pre-Sample VCC and Viability"))))))))

(defun mfc-C1-Production-Schedule ()
  (interactive)
  (insert
   (mfc-schedule-template
    `(("Set up" -1 ("Inflate bag" "Acquire Media" "Charge" "Check Setpoints" "Sparge/Span" "Make Glucose Barrel"))
     ("Inoc" 0 ("Verify pre-sample" "LIMS" "DOX" "Weld Glucose" "Weld AF" "Weld Base" "Expand" "post sample" "Add AF Bolus" "quit 250"))
     ("D3 Feed" 3 ("Acquire 18.65L RCF1MD, 2.15L Phos, 1.12L Cys" "Feed" "Weld new filter" "Start AF Auto feed"))
     ("Tyr Feed" 4 ("Acquire 4.97L" "Feed" "Weld new filter"))
     ("D5 Feed" 5 ("Acquire 18.65L RCF1MD, 2.15L Phos, 1.12L Cys" "Feed" "Weld new filter"))
     ("D7 Feed" 7 ("Acquire 18.65L RCF1MD, 2.15L Phos, 1.12L Cys, 7.76L RS5" "Feed" "Weld new filter" "Increase AF Auto feed"))
     ("D9 Feed" 9 ("7.76L RS5" "Feed" ))
     ("Steam Line" 11)
     ("Harvest" 12 ("Lower Temp" "Clipster Bags" "Prime line" "print sticker" "quit bag"))))))

(defun multi-step (mols steps)
  (-mapcat (lambda (step)
             (-map (lambda (mol) (format-message "%s: %s" mol step))
                   mols)) steps))


(defun mfc-Research-Molecule (molecules)
  (interactive "MCell Lines (seperated by ,): ")
  (let ((mols (split-string molecules ",")))
    (insert
     (mfc-schedule-template
      `(("Thaw" 0 ,(cons "Warm Media" (multi-step mols '("Thaw"))))
        ("LIMS Seed Train" 0 ,(multi-step mols '("LIMS" "Sheet" "Sticker")) 3))))))





(defun mfc-ELN-Schedule ()
  (interactive)
  (insert
   (mfc-schedule-template
    '(("Associations" 0 ("Base?" "Glucose" "Bag" "Raman Probe?" "Antifoam" "Dox" "Phos" "Tyr" "Feed") 10)
      ("Batch Sheets" 12 ("Upload glucose" "upload pcv" "scan and store") )
      ("ELN" 30 ("Get Titer" "Get Quality" "Sheets" "Template" "Upload") 60)))))


(provide 'mfc-org-templates)

