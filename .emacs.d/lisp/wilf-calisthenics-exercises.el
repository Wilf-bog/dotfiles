;;; wilf-calisthenics-exercises.el --- Hybrid Calisthenics exercises data -*- lexical-binding: t; -*-
;;; Commentary:
;; This file defines the list of Hybrid Calisthenics exercises and helper
;; functions for use in Org capture templates or other Emacs tools.

;;; Code:

(defvar wilf-calisthenics-exercises-alist
  '(("Pushups"
     . ("Wall Pushups"
        "Incline Pushups"
        "Advanced Incline Pushups"
        "Knee Pushups"
        "Full Pushups"
        "Narrow Pushups"
        "Side-Staggered Pushups"
        "Archer Pushups"
        "Sliding One-Arm Pushups"
        "One-Arm Pushups"
        "Advanced One-Arm Pushups"))
    ("Pullups"
     . ("Wall Pullups"
        "Horizontal Pullups"
        "Advanced Horizontal Pullups"
        "Jackknife Pullups"
        "Full Pullups"
        "Narrow Pullups"
        "One Hand Pullups"
        "Advanced One Hand Pullups"
        "Archer Pullups"
        "One Arm Pullups"))
    ("Squats"
     . ("Jackknife Squats"
        "Assisted Squats"
        "Half Squats"
        "Full Squats"
        "Narrow Squats"
        "Side Staggered Squats"
        "Front Staggered Squats"
        "Assisted One Leg Squats"
        "One Leg Chair Squats"
        "One Leg Squats"))
    ("Leg Raises"
     . ("Knee Raises"
        "Advanced Knee Raises"
        "Alternating Leg Raises"
        "Full Leg Raises"
        "Tuck Plow Raises"
        "Plow Raises"
        "Hanging Knee Raises"
        "Advanced Hanging Knee Raises"
        "Hanging Leg Raises"
        "Toe to Bars"))
    ("Bridges"
     . ("Glute Bridges"
        "Straight Bridges"
        "Wall Bridges"
        "Incline Bridges"
        "Head Bridges"
        "Full Bridges"
        "Wheel Bridges"
        "Tap Bridges"
        "Wall Walking Bridges"
        "Stand to Stand Bridges"))
    ("Twists"
     . ("Straight Leg Twists"
        "Bent Leg Twists"
        "Full Twists"))
    ("Dips"
     . ("Bench Dips"
        "Bar Dips"
        "Straight Bar Dips"
        "Korean Dips"
        "Ring Dips"
        "Assisted Dips"
        "Full Dips"
        "Weighted Dips"))
    ("Calf raises"
     . ("Calf raises"
        "One Leg Calf Raise"
        "Weighted One Leg Calf Raise"
        "Suspended Calf Raise"
        "Suspended One Leg Calf Raise"))
    ("Grips"
     . ("Towel Hangs"
        "Dead Hangs"
        "Assisted Towel Hangs"
        "One Arm Hangs"
        "Double Towel Hangs"
        "Rope Style Hangs"
        "One Arm Towel Hangs"
        "Advanced One Arm Tower Hangs")))
  "Alist of categories → list of Hybrid Calisthenics exercises.")

;;;###autoload
(defun wilf/get-exercises-for (category)
  "Return the list of exercises for CATEGORY from `wilf-calisthenics-exercises-alist`."
  (cdr (assoc category wilf-calisthenics-exercises-alist)))

;;;###autoload
(defun wilf/org-capture-exercice-entry ()
  "Capture dynamique pour un exercice Hybrid Calisthenics.
Propose une catégorie et un exercice, puis construit une entrée Org formatée."
  (let* ((cats (mapcar #'car wilf-calisthenics-exercises-alist))
         (chosen-cat (completing-read "Catégorie : " cats nil t))
         (ex-list (wilf/get-exercises-for chosen-cat))
         (chosen-ex (completing-read (format "Exercice (%s) : " chosen-cat)
                                     ex-list nil t))
         (sets (read-string "Sets : "))
         (reps (read-string "Répétitions : "))
         (comment (read-string "Commentaire : ")))
    (format "** %s — %s\n:PROPERTIES:\n:Category: %s\n:Sets: %s\n:Reps: %s\n:END:\n%s"
            chosen-cat chosen-ex chosen-cat sets reps comment)))

(provide 'wilf-calisthenics-exercises)
;;; wilf-calisthenics-exercises.el ends here
