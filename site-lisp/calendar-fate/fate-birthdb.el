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
;;; (fate-add-user)
;;; (fate-remove-user)
;;; (fate-choose-current-user)
;;; Manually execute (fate-recalculate-user) after modifying 'fate-calculate-current-user

;;; Code:
;; 当前文件路径
(defconst fate-root-dir (file-name-directory #$))
;; 当前用户信息
(defvar fate-current-user '())
;; 当前用户信息
(defvar fate-user-calculate '())
;; 用户一览
(defvar fate-user-list '())
(defvar fate-user-list-choice '())
(defvar fate-user-list-map '())
(defconst fate-user-list-file (concat fate-root-dir "userlist"))
;; 读取用户列表文件
(defun fate_load_user_list ()
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
        (fate-add-dummy-user "Anonymous" t (date-to-time (current-time-string)) (car fate-timediff-pos-choice) 120)
        (fate_save_user_list)
        )
    (progn
      (setq fate-current-user (car fate-user-list))
      (fate_user_list_choice)
      )
    )
  )
;; 保存用户列表文件
(defun fate_save_user_list ()
  (with-current-buffer (find-file-noselect fate-user-list-file)
    (let ((cb (current-buffer))
          i)
      (erase-buffer)
      (insert "(setq fate-user-list (list")
      (dotimes (i (length fate-user-list))
        (insert "\n'")
        (prin1 (nth i fate-user-list) cb)
        )
      (insert "))")
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
            fate-user-list-choice (cons title fate-user-list-choice)
            fate-user-list-map (cons (list title i) fate-user-list-map)
            )
      )
    )
  )
;; 各日期时差
(defconst fate-timediff-date-file (concat fate-root-dir "timediff_date.txt"))
(defvar fate-timediff-date '())
;; 各城市时差
(defconst fate-timediff-pos-file (concat fate-root-dir "timediff_pos.txt"))
(defvar fate-timediff-pos '())
(defvar fate-timediff-pos-choice '())
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
;; 增加用户数据
(defun fate-add-user ()
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
    ;; 读取出生城市
    (setq city (completing-read "Birth City: " fate-timediff-pos-choice nil t))
    (if (string= city (car fate-timediff-pos-choice))
        (setq poslong (string-to-number (read-string "Longitude: ")))
      (setq poslong 0)
      )
    (fate-add-dummy-user name male birth0 city poslong)
    (fate_save_user_list)
    )
  )
;; 添加用户（详细字段设置）
(defun fate-add-dummy-user (name male birth0 city poslong)
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
    (setq fate-current-user '())
    (setq fate-current-user (plist-put fate-current-user 'name name))
    (setq fate-current-user (plist-put fate-current-user 'male male))
    (setq fate-current-user (plist-put fate-current-user 'city city))
    (setq fate-current-user (plist-put fate-current-user 'delta1 delta1))
    (setq fate-current-user (plist-put fate-current-user 'delta2 delta2))
    (setq fate-current-user (plist-put fate-current-user 'birthday birthday))
    (setq fate-current-user (plist-put fate-current-user 'birthday-fix birthday2))
    (fate-calculate-current-user)
    )
  )
;; 计算当前用户的扩展数据
(defun fate-calculate-current-user ()
  (dotimes (calcfunc (length fate-user-calculate))
    (funcall (nth calcfunc fate-user-calculate))
    )
  ;(let ((birthday1 (plist-get fate-current-user 'birthday))
        ;(birthday2 (plist-get fate-current-user 'birthday-fix))
        ;(ziwei-ju-tbl '((2 2 6 6 3 3 5 5 4 4 6 6)
                        ;(6 6 5 5 4 4 3 3 2 2 5 5)
                        ;(5 5 3 3 2 2 4 4 6 6 3 3)
                        ;(3 3 4 4 6 6 2 2 5 5 4 4)
                        ;(4 4 2 2 5 5 6 6 3 3 2 2)))
        ;heluo lunar)
    ; 河洛相关信息
    ;(setq heluo (heluo_basic_calc (plist-get fate-current-user 'male) birthday1))
    ;(setq fate-current-user (plist-put fate-current-user 'heluo-tianshu (nth 0 heluo)))
    ;(setq fate-current-user (plist-put fate-current-user 'heluo-dishu (nth 1 heluo)))
    ;(setq fate-current-user (plist-put fate-current-user 'heluo-gua1 (nth 2 heluo)))
    ;(setq fate-current-user (plist-put fate-current-user 'heluo-yao1 (nth 3 heluo)))
    ;(setq fate-current-user (plist-put fate-current-user 'heluo-gua2 (nth 4 heluo)))
    ;(setq fate-current-user (plist-put fate-current-user 'heluo-yao2 (nth 5 heluo)))
    ;(setq fate-current-user (plist-put fate-current-user 'heluo-year-min (nth 6 heluo)))
    ;(setq fate-current-user (plist-put fate-current-user 'heluo-year-mid (nth 7 heluo)))
    ;(setq fate-current-user (plist-put fate-current-user 'heluo-year-max (nth 8 heluo)))

    ; 紫微相关信息
    ;(setq lunar (fate-lunar-info birthday2))
    ; 阴历年干支，年，月，日，时
    ;(setq fate-current-user (plist-put fate-current-user 'ziwei-birthday
                                       ;(list (cadr lunar)
                                             ;(if (= (cadr lunar) (1+ (mod (- (nth 2 birthday2) 1984) 60)))
                                                 ;(nth 2 birthday2)
                                               ;(1- (nth 2 birthday2)))
                                             ;(nth 2 lunar)
                                             ;(nth 3 lunar)
                                             ;(if (>= (nth 3 birthday2) 23)
                                                 ;1
                                               ;(1+ (floor (/ (1+ (nth 3 birthday2)) 2)))
                                               ;)
                                             ;)))
    ; 命宫地支
    ;(setq fate-current-user (plist-put fate-current-user 'ziwei-ming
                                       ;(1+ (mod (-
                                                 ;(floor (+ 0.9 (nth 2 (plist-get fate-current-user 'ziwei-birthday))))
                                                 ;(floor (+ 0.9 (nth 4 (plist-get fate-current-user 'ziwei-birthday))))
                                                 ;-2)
                                                ;12))))
    ; 身宫地支
    ;(setq fate-current-user (plist-put fate-current-user 'ziwei-shen
                                       ;(1+ (mod (+
                                                 ;(floor (+ 0.9 (nth 2 (plist-get fate-current-user 'ziwei-birthday))))
                                                 ;(floor (+ 0.9 (nth 4 (plist-get fate-current-user 'ziwei-birthday)))))
                                                ;12))))
    ; 五行局
    ;(setq fate-current-user (plist-put fate-current-user 'ziwei-ju
                                       ;(nth (1- (plist-get fate-current-user 'ziwei-ming))
                                            ;(nth (mod (1- (nth 0 (plist-get fate-current-user 'ziwei-birthday))) 5)
                                                 ;ziwei-ju-tbl))))

    ;(setq fate-user-list (cons fate-current-user fate-user-list))
    ;)
    (setq fate-user-list (cons fate-current-user fate-user-list))
  )
;; 重新计算所有用户的扩展数据
(defun fate-recalculate-user ()
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
      (setq fate-current-user (nth tmpi basiclist))
      (fate-calculate-current-user)
      )
    (fate_save_user_list)
    )
  )
;; 设置当前用户信息
(defun fate-choose-current-user ()
  (interactive)
  (let* ((user (completing-read "Choose user: " fate-user-list-choice nil t))
         (idx (nth 1 (assoc user fate-user-list-map)))
         )
    (setq fate-current-user (nth idx fate-user-list))
    )
  )
;; 删除用户信息
(defun fate-remove-user ()
  (interactive)
  (when (> (length fate-user-list) 1)
    (let* ((user (completing-read "Choose user: " fate-user-list-choice nil t))
           (idx (nth 1 (assoc user fate-user-list-map)))
           )
      (setq fate-current-user (nth idx fate-user-list)
            fate-user-list (remove fate-current-user fate-user-list)
            )
      (fate_save_user_list)
      (setq fate-current-user (nth 0 fate-user-list))
      )
    )
  )

(fate_load_user_list)

(provide 'fate-birthdb)
;;; fate-birthdb.el ends here
