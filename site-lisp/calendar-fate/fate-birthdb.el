;;; fate-birthdb.el --- calendar util for another chinese calender system

;; Copyright (C) 2017 Thule Qaid

;; Author: Thule Qaid <thuleqaid@163.com>

;; fate-birthdb is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; fate-birthdb is distributed in the hope that it will be useful, but WITHOUT
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
;; 用户一览数据文件
(defconst fate-user-list-file (concat calendar-fate-data-path "userlist"))
;; 各日期时差数据文件
(defconst fate-timediff-date-file (concat calendar-fate-source-dir "timediff_date.txt"))
;; 各城市时差数据文件
(defconst fate-timediff-pos-file (concat calendar-fate-source-dir "timediff_pos.txt"))

;; 新增用户时是否需要指定出生位置
(defvar fate-user-specify-pos nil)
;; 当前用户信息
(defvar fate-user-current '())
;; 扩展用户信息计算函数列表
(defvar fate-user-calculate '())
;; 用户一览
(defvar fate-user-list '())
(defvar fate-user-list-choice '())
(defvar fate-user-list-map '())
;; 各日期时差
(defvar fate-timediff-date '())
;; 各城市时差
(defvar fate-timediff-pos '())
(defvar fate-timediff-pos-choice '())

;; 增加用户数据
(defun fate-user-add ()
  (interactive)
  (let (name male birth0 city poslong)
    (unless fate-timediff-pos-choice
      (load_timediff)
      )
    (setq name (string-trim (read-from-minibuffer "Name: "))
          male (y-or-n-p "Are you male?")
          birth0 (safe-date-to-time (org-read-date t))
          )
    ;; 读取出生日期（输入必须包括时间）
    (while (and (= (nth 0 birth0) 0) (= (nth 1 birth0) 0))
      (setq birth0 (safe-date-to-time (org-read-date t))
            )
      )
    (if fate-user-specify-pos
        (progn
          ;; 读取出生城市
          (setq city (if (fboundp 'ivy-completing-read)
                         (ivy-completing-read "Birth City: " fate-timediff-pos-choice nil t)
                       (completing-read "Birth City: " fate-timediff-pos-choice nil t)))
          (if (string= city (car fate-timediff-pos-choice))
              (setq poslong (string-to-number (read-string "Longitude: ")))
            (setq poslong 0)
            )
          )
      (setq city (car fate-timediff-pos-choice)
            poslong 120)
      )
    (fate_user_add_dummy name male birth0 city poslong)
    (fate_user_list_save)
    )
  )
;; 设置当前用户信息
(defun fate-user-choose ()
  (interactive)
  (let* ((user (if (fboundp 'ivy-completing-read)
                   (ivy-completing-read "Choose user: " fate-user-list-choice nil t)
                 (completing-read "Choose user: " fate-user-list-choice nil t)))
         (idx (nth 1 (assoc user fate-user-list-map)))
         )
    (setq fate-user-current (nth idx fate-user-list))
    )
  )
;; 删除用户信息
(defun fate-user-remove ()
  (interactive)
  (when (> (length fate-user-list) 1)
    (let* ((user (if (fboundp 'ivy-completing-read)
                     (ivy-completing-read "Choose user: " fate-user-list-choice nil t)
                   (completing-read "Choose user: " fate-user-list-choice nil t)))
           (idx (nth 1 (assoc user fate-user-list-map)))
           )
      (setq fate-user-current (nth idx fate-user-list)
            fate-user-list (remove fate-user-current fate-user-list)
            )
      (fate_user_list_save)
      (setq fate-user-current (nth 0 fate-user-list))
      )
    )
  )
;; 重新计算所有用户的扩展数据
(defun fate-user-recalculate ()
  (interactive)
  (let ((basiclist '())
        item item2 tmpi)
    (dotimes (tmpi (length fate-user-list))
      (setq item (nth tmpi fate-user-list)
            item2 '()
            item2 (plist-put item2 'name (plist-get item 'name))
            item2 (plist-put item2 'male (plist-get item 'male))
            item2 (plist-put item2 'city (plist-get item 'city))
            item2 (plist-put item2 'delta1 (plist-get item 'delta1))
            item2 (plist-put item2 'delta2 (plist-get item 'delta2))
            item2 (plist-put item2 'birthday (plist-get item 'birthday))
            item2 (plist-put item2 'birthday-fix (plist-get item 'birthday-fix))
            basiclist (cons item2 basiclist))
      )
    (setq fate-user-list '())
    (dotimes (tmpi (length basiclist))
      (setq fate-user-current (nth tmpi basiclist))
      (fate_user_calculate_current)
      )
    (fate_user_list_save)
    )
  )

;; 读取用户列表文件
(defun fate_user_list_load ()
  (if (file-exists-p fate-user-list-file)
      (load-file fate-user-list-file)
    (with-current-buffer (find-file-noselect fate-user-list-file)
      (insert "(setq fate-user-list '())")
      (save-buffer)
      (kill-buffer (current-buffer))
      )
    )
  (unless fate-timediff-pos-choice
    (load_timediff)
    )
  (if (<= (length fate-user-list) 0)
      (progn
        (fate_user_add_dummy "Anonymous" t (date-to-time (current-time-string)) (car fate-timediff-pos-choice) 120)
        (fate_user_list_save)
        )
    (progn
      (setq fate-user-current (car fate-user-list))
      (fate_user_list_choice)
      )
    )
  )
;; 保存用户列表文件
(defun fate_user_list_save ()
  (with-current-buffer (find-file-noselect fate-user-list-file)
    (let ((cb (current-buffer))
          i)
      (erase-buffer)
      (setq buffer-file-coding-system 'utf-8)
      (insert "(setq fate-user-list (list")
      (dotimes (i (length fate-user-list))
        (insert "\n'")
        (prin1 (nth i fate-user-list) cb)
        )
      (insert "\n))")
      )
    (save-buffer)
    (kill-buffer (current-buffer))
    (fate_user_list_choice)
    )
  )
;; 初始化可选的用户列表
(defun fate_user_list_choice ()
  (let (i item birthday)
    (setq fate-user-list-choice '()
          fate-user-list-map '())
    (dotimes (i (length fate-user-list))
      (setq item (nth i fate-user-list)
            birthday (plist-get item 'birthday)
            title (format "%s %s %d/%d/%d %d:%d:%d"
                          (plist-get item 'name)
                          (if (plist-get item 'male) "M" "F")
                          (nth 2 birthday) (nth 0 birthday) (nth 1 birthday)
                          (nth 3 birthday) (nth 4 birthday) (nth 5 birthday)
                          )
            )
      (add-to-list 'fate-user-list-choice title t)
      (add-to-list 'fate-user-list-map (list title i) t)
      )
    )
  )

;; 读取时差
(defun load_timediff ()
  (setq fate-timediff-date '()
        fate-timediff-pos '()
        fate-timediff-pos-choice '())
  ;; 读取各日期时差
  (with-current-buffer (find-file-noselect fate-timediff-date-file)
    (goto-char (point-min))
    (while (> (line-end-position) (point))
      (setq fate-timediff-date (cons (split-string (buffer-substring-no-properties (point) (line-end-position))) fate-timediff-date))
      (beginning-of-line 2)
      )
    (kill-buffer (current-buffer))
    )
  ;; 读取各城市时差
  (with-current-buffer (find-file-noselect fate-timediff-pos-file)
    (goto-char (point-min))
    (while (> (line-end-position) (point))
      (setq fate-timediff-pos (cons (split-string (buffer-substring-no-properties (point) (line-end-position))) fate-timediff-pos)
            fate-timediff-pos-choice (cons (caar fate-timediff-pos) fate-timediff-pos-choice))
      (beginning-of-line 2)
      )
    (setq fate-timediff-pos-choice (cons "Other" fate-timediff-pos-choice))
    (kill-buffer (current-buffer))
    )
  )
;; 添加用户（详细字段设置）
(defun fate_user_add_dummy (name male birth0 city poslong)
  (let* ((birth1 (decode-time birth0))
         birth2
         delta1 delta2
         birthday birthday2)
    (unless fate-timediff-pos-choice
      (load_timediff)
      )
    ;; 计算时差（根据输入的出生日期）
    (setq delta1 (string-to-number (nth 1 (assoc
                                           (format "%d-%d"
                                                   (nth 4 birth1)
                                                   (nth 3 birth1))
                                           fate-timediff-date))))
    ;; 计算时差（通过手动指定出生城市的经度或者选择列表中城市的数据）
    (if (string= city (car fate-timediff-pos-choice))
        (setq delta2 (truncate (* (- poslong 120) 240)))
      (setq delta2 (string-to-number (nth 3 (assoc city fate-timediff-pos))))
      )
    ;; 调整出生时间
    (setq birth0 (list (nth 0 birth0) (+ (nth 1 birth0) delta1 delta2))
          birth2 (decode-time birth0))
    (setq birthday (list (nth 4 birth1) (nth 3 birth1) (nth 5 birth1) (nth 2 birth1) (nth 1 birth1) (nth 0 birth1))
          birthday2 (list (nth 4 birth2) (nth 3 birth2) (nth 5 birth2) (nth 2 birth2) (nth 1 birth2) (nth 0 birth2))
          )
    ;; 基本信息
    (setq fate-user-current '())
    (setq fate-user-current (plist-put fate-user-current 'name name))
    (setq fate-user-current (plist-put fate-user-current 'male male))
    (setq fate-user-current (plist-put fate-user-current 'city city))
    (setq fate-user-current (plist-put fate-user-current 'delta1 delta1))
    (setq fate-user-current (plist-put fate-user-current 'delta2 delta2))
    (setq fate-user-current (plist-put fate-user-current 'birthday birthday))
    (setq fate-user-current (plist-put fate-user-current 'birthday-fix birthday2))
    (fate_user_calculate_current)
    )
  )
;; 计算当前用户的扩展数据
(defun fate_user_calculate_current ()
  (dotimes (calcfunc (length fate-user-calculate))
    (funcall (nth calcfunc fate-user-calculate))
    )
    (add-to-list 'fate-user-list fate-user-current t)
    ;(setq fate-user-list (cons fate-user-current fate-user-list))
  )

(fate_user_list_load)

(if calendar-fate-show-chinese
    (easy-menu-add-item
     nil '("Fate")
     '("用户管理"
       ("Current" :label (plist-get fate-user-current 'name) :active nil)
       ["选择用户" fate-user-choose t]
       ["增加用户" fate-user-add t]
       ["删除用户" fate-user-remove t]
       ["更新用户信息" fate-user-recalculate t]
       )
     )
  (easy-menu-add-item
   nil '("Fate")
   '("User"
     ("Current" :label (plist-get fate-user-current 'name) :active nil)
     ["Choose" fate-user-choose t]
     ["Add" fate-user-add t]
     ["Remove" fate-user-remove t]
     ["Update" fate-user-recalculate t]
     )
   )
  )

(provide 'fate-birthdb)
;;; fate-birthdb.el ends here
