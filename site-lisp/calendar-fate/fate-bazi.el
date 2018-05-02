;;; fate-bazi.el --- calendar util for another chinese calender system

;; Copyright (C) 2018 Thule Qaid

;; Author: Thule Qaid <thuleqaid@163.com>

;; fate-bazi is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; fate-bazi is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with calendar-fate.  If not, see http://www.gnu.org/licenses.

;;; Commentary:
;;; (fate-user-add)
;;; (fate-user-remove)
;;; (fate-user-choose)
;;; (fate-user-recalculate)

;;; Code:

(defun bazi-user-calculate ()
  (let* ((birthday (plist-get fate-user-current 'birthday))
         (birthday-a (calendar-fate-chinese-datetime birthday))
         (gender (plist-get fate-user-current 'male))
         (sinfo (fate-solar-info birthday))                     ;; 阳历数据
         (yinyang (mod (+ (nth 1 sinfo) (if gender 1 0)) 2))    ;; 阳男阴女为0,反之为1
         (termyear (- (nth 2 birthday) (mod (- (nth 2 birthday) (nth 1 sinfo) -1) 2)))  ;; 定大运的节气年份
         (termindex (if (= (mod (nth 5 sinfo) 2) 0)
                        (if (= yinyang 0) (1+ (nth 5 sinfo)) (1- (nth 5 sinfo)))
                      (if (= yinyang 0) (+ (nth 5 sinfo) 2) (nth 5 sinfo))))        ;; 定大运的节气序号
         term-a
         termdiff
         yun-year yun-month yun-day yun-datetime yun-a
         )
    ;; 调整节气年份
    (when (> termindex 24)
      (setq termindex (- termindex 24)
            termyear (1+ termyear)))
    (setq term-a (calendar-fate-chinese-datetime (fate-solar-item-info termyear termindex))
          term-diff (if (= yinyang 0) (- term-a birthday-a) (- birthday-a term-a))
          yun-year (floor (/ term-diff 3))
          term-diff (* (mod term-diff 3) 4)
          yun-month (floor term-diff)
          yun-day (* (- term-diff yun-month) 30)
          yun-datetime (list (+ yun-month (nth 0 birthday) (if (> (+ yun-month (nth 0 birthday)) 12) -12 0))
                             (nth 1 birthday)
                             (+ yun-year (nth 2 birthday) (if (> (+ yun-month (nth 0 birthday)) 12) 1 0))
                             (nth 3 birthday)
                             (nth 4 birthday)
                             (nth 5 birthday)
                             )
          yun-a (+ (calendar-fate-chinese-datetime yun-datetime) yun-day)
          yun-datetime (calendar-fate-gregorian-from-absolute yun-a)
          )
    (setq fate-user-current (plist-put fate-user-current 'bazi-kaiyun yun-datetime))
    )
  )

(add-to-list 'fate-user-calculate 'bazi-user-calculate)
(if calendar-fate-show-chinese
    (easy-menu-add-item
     nil '("Fate")
     '("四柱"
;;       ["排盘" fate-user-recalculate t]
       )
     )
  (easy-menu-add-item
   nil '("Fate")
   '("BaZi"
;;     ["MingPan" fate-user-recalculate t]
     )
   )
  )

(provide 'fate-bazi)
;;; fate-bazi.el ends here
