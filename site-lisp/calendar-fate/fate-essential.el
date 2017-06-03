;;; fate-essential.el --- calendar util for another chinese calender system

;; Copyright (C) 2017 Thule Qaid

;; Author: Thule Qaid <thuleqaid@163.com>

;; fate-essential is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; fate-essential is distributed in the hope that it will be useful, but WITHOUT
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
;;; (ziwei-show)
;;; (heluo-show &optional year)
;;; (liuyao-show)
;;; (liuyao-new)
;;; (qimen-taigong)
;;; Manually execute (fate-recalculate-user) after modifying 'fate-calculate-current-user

;;; Code:
;; 公历转阳历
;; I: '(month day year hour minute second)
;; O: '(yun year month day hour last-24term-index)
;; Memo: '(hour minute second)[I] is optional, default is '(0 0 0)
(defun fate-solar-info (&optional date)
  (calendar-fate-chinese-from-absolute
   (calendar-fate-chinese-datetime date)))

;; 计算指定节气的详细时间
;; I: year, index
;; O: '(month day year hour minute second)
;; Memo: -1<=index[I]<=24
(defun fate-solar-item-info (year index)
  (let ((result (make-list 6 0))
        absolute)
    (cond
     ((< index -1)
      ;; 范围外
      (setq absolute nil)
      )
     ((<= index 22)
      ;; 使用year对应数据
      (setq absolute (cadr (assoc index (calendar-fate-chinese-year year))))
      )
     ((<= index 24)
      ;; 使用year+1对应数据
      (setq absolute (cadr (assoc (- index 24) (calendar-fate-chinese-year (1+ year)))))
      )
     (t
      ;; 范围外
      (setq absolute nil)
      )
     )
    (when absolute
      (setq result (calendar-fate-gregorian-from-absolute absolute))
      )
    result
    )
  )

;; 公历转阴历
;; I: '(month day year hour minute second)
;; O: '(cycle year month day)
;; Memo: '(hour minute second)[I] won't be used for calculating, and can be omitted
;; Memo: month[O] will be a float number for a leap month
(defun fate-lunar-info (date)
  (calendar-chinese-from-absolute
   (calendar-absolute-from-gregorian date)))

;; 当前文件路径
(defconst fate-root-dir (file-name-directory #$))
;; 当前用户信息
(defvar fate-current-user '())
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
  (let ((birthday1 (plist-get fate-current-user 'birthday))
        (birthday2 (plist-get fate-current-user 'birthday-fix))
        (ziwei-ju-tbl '((2 2 6 6 3 3 5 5 4 4 6 6)
                        (6 6 5 5 4 4 3 3 2 2 5 5)
                        (5 5 3 3 2 2 4 4 6 6 3 3)
                        (3 3 4 4 6 6 2 2 5 5 4 4)
                        (4 4 2 2 5 5 6 6 3 3 2 2)))
        heluo lunar)
    ;; 河洛相关信息
    (setq heluo (heluo_basic_calc (plist-get fate-current-user 'male) birthday1))
    (setq fate-current-user (plist-put fate-current-user 'heluo-tianshu (nth 0 heluo)))
    (setq fate-current-user (plist-put fate-current-user 'heluo-dishu (nth 1 heluo)))
    (setq fate-current-user (plist-put fate-current-user 'heluo-gua1 (nth 2 heluo)))
    (setq fate-current-user (plist-put fate-current-user 'heluo-yao1 (nth 3 heluo)))
    (setq fate-current-user (plist-put fate-current-user 'heluo-gua2 (nth 4 heluo)))
    (setq fate-current-user (plist-put fate-current-user 'heluo-yao2 (nth 5 heluo)))
    (setq fate-current-user (plist-put fate-current-user 'heluo-year-min (nth 6 heluo)))
    (setq fate-current-user (plist-put fate-current-user 'heluo-year-mid (nth 7 heluo)))
    (setq fate-current-user (plist-put fate-current-user 'heluo-year-max (nth 8 heluo)))

    ;; 紫微相关信息
    (setq lunar (fate-lunar-info birthday2))
    ;; 阴历年干支，年，月，日，时
    (setq fate-current-user (plist-put fate-current-user 'ziwei-birthday
                                       (list (cadr lunar)
                                             (if (= (cadr lunar) (1+ (mod (- (nth 2 birthday2) 1984) 60)))
                                                 (nth 2 birthday2)
                                               (1- (nth 2 birthday2)))
                                             (nth 2 lunar)
                                             (nth 3 lunar)
                                             (if (>= (nth 3 birthday2) 23)
                                                 1
                                               (1+ (floor (/ (1+ (nth 3 birthday2)) 2)))
                                               )
                                             )))
    ;; 命宫地支
    (setq fate-current-user (plist-put fate-current-user 'ziwei-ming
                                       (1+ (mod (-
                                                 (nth 2 (plist-get fate-current-user 'ziwei-birthday))
                                                 (floor (+ 0.9 (nth 4 (plist-get fate-current-user 'ziwei-birthday))))
                                                 -2)
                                                12))))
    ;; 身宫地支
    (setq fate-current-user (plist-put fate-current-user 'ziwei-shen
                                       (1+ (mod (+
                                                 (nth 2 (plist-get fate-current-user 'ziwei-birthday))
                                                 (floor (+ 0.9 (nth 4 (plist-get fate-current-user 'ziwei-birthday)))))
                                                12))))
    ;; 五行局
    (setq fate-current-user (plist-put fate-current-user 'ziwei-ju
                                       (nth (1- (plist-get fate-current-user 'ziwei-ming))
                                            (nth (mod (1- (nth 0 (plist-get fate-current-user 'ziwei-birthday))) 5)
                                                 ziwei-ju-tbl))))

    (setq fate-user-list (cons fate-current-user fate-user-list))
    )
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

;; 八卦：后天卦序
;; 一数坎兮二数坤
;; 三震四巽数中分
;; 五寄中宫六是乾
;; 七兑八艮九离门
(defconst gua_tbl_after [-1 2 0 4 3 -1 7 6 1 5])
;; 分解卦爻情报
;; I: gua 0~63
;; O: '[count-of-yang 1st-yao 2nd-yao 3rd-yao 4th-yao 5th-yao 6th-yao]
;; Memo: nth-yao[O] is 1 for yang and 0 for yin
(defun gua_bit_sts (gua)
  (let ((result (make-vector 7 0))
        (i 0)
        (count 0))
    (while (< i 6)
      (setq count (+ count (mod gua 2)))
      (aset result (- 6 i) (mod gua 2))
      (setq gua (/ gua 2)
            i (1+ i))
      )
    (aset result 0 count)
    result
    ))

;;; 河洛
;; 河洛批言路径
(defconst fate-heluo-file (concat fate-root-dir "heluo.txt"))
;; 十天干对应的天地数
;; 戊一乙癸二，庚三辛四同。壬甲从六数，丁七丙八宫。己九无差别，五数寄于中。
(defconst heluo_tbl_tianshu [0 6 0 2 0 8 7 0 1 0 9 0 3 0 0 4 0 6 0 2])
;; 十二地支对应的天地数
;; 亥子一六水，寅卯三八真。巳午二七火，申酉四九金。辰戌丑未土，五十总生成。
(defconst heluo_tbl_dishu [1 6 5 10 3 8 3 8 5 10 7 2 7 2 5 10 9 4 9 4 5 10 1 6])
(defun heluo_basic_calc (gender &optional birthday)
  (let* ((sinfo (fate-solar-info birthday)) ; 阳历数据
         ts ds                              ; 天数，地数
         tg dg                              ; 天卦，地卦
         gua1 gua2                          ; 先天卦，后天卦
         yao1 yao2                          ; 先天卦元堂，后天卦元堂
         year1 year2 year3                  ; 先天卦起始年，后天卦起始年，后天卦结束年
         i tmp1 tmp2 shizhi yinyang         ; 临时变量
         )
    ;; 计算天数和地数
    (setq i 1 ts 0 ds 0)
    (while (< i 5)
      (setq tmp1 (* 2 (mod (1- (nth i sinfo)) 10))
            tmp2 (* 2 (mod (1- (nth i sinfo)) 12)))
      (setq ts (+ ts (aref heluo_tbl_tianshu tmp1)
                  (aref heluo_tbl_dishu tmp2)))
      (setq ds (+ ds (aref heluo_tbl_tianshu (1+ tmp1))
                  (aref heluo_tbl_dishu (1+ tmp2))))
      (setq i (1+ i)))
    ;; 计算天卦和地卦
    (setq tg (1+ (mod (1- ts) 25))
          dg (1+ (mod (1- ds) 30)))
    (if (= (mod tg 10) 0)
        (setq tg (/ tg 10))
      (setq tg (mod tg 10)))
    (if (= (mod dg 10) 0)
        (setq dg (/ dg 10))
      (setq dg (mod dg 10)))
    ;; 阳男阴女为0,反之为1
    (if gender (setq tmp1 (mod (1+ (nth 1 sinfo)) 2) )
      (setq tmp1 (mod (nth 1 sinfo) 2)))
    (when (or (= tg 5) (= dg 5))
      ;; 上元男艮女为坤。女兑男离属下元。中元阴女阳男艮。阳女阴男亦寄坤。
      (setq i (car sinfo))
      (cond ((<= i 3)
             (if gender
                 (progn (when (= tg 5) (setq tg 8))
                        (when (= dg 5) (setq dg 8)))
               (progn (when (= tg 5) (setq tg 2))
                      (when (= dg 5) (setq dg 2)))
               )
             )
            ((<= i 6)
             (if (= 0 tmp1)
                 (progn (when (= tg 5) (setq tg 8))
                        (when (= dg 5) (setq dg 8)))
               (progn (when (= tg 5) (setq tg 2))
                      (when (= dg 5) (setq dg 2)))
               )
             )
            (t
             (if gender
                 (progn (when (= tg 5) (setq tg 9))
                        (when (= dg 5) (setq dg 9)))
               (progn (when (= tg 5) (setq tg 7))
                      (when (= dg 5) (setq dg 7)))
               )
             ))
      )
    ;; 八卦相荡成卦 阳男阴女，天数在上、地数在下成卦；阴男阳女，天数在下、地数在上成卦。
    (setq tg (aref gua_tbl_after tg)
          dg (aref gua_tbl_after dg))
    (if (= 0 tmp1)
        (setq gua1 (+ (* 8 dg) tg))
      (setq gua1 (+ (* 8 tg) dg)))
    ;; 计算先天卦的元堂
    (setq tmp2 (gua_bit_sts gua1))
    (setq year2 (+ (* (aref tmp2 0) 3) 36))
    (setq shizhi (1+ (mod (1- (nth 4 sinfo)) 12)))
    (if (<= shizhi 6)
        (setq yinyang 1)
      (progn (setq yinyang 0
                   shizhi (- shizhi 6))
             (aset tmp2 0 (- 6 (aref tmp2 0))))
      )
    (cond
     ((and (>= (aref tmp2 0) 1) (<= (aref tmp2 0) 3))
      (if (<= shizhi (* 2 (aref tmp2 0)))
          (progn
            (when (> shizhi (aref tmp2 0))
              (setq shizhi (- shizhi (aref tmp2 0))))
            (setq i 1)
            (while (<= i 6)
              (when (= (aref tmp2 i) yinyang)
                (setq shizhi (1- shizhi)))
              (when (<= shizhi 0)
                (setq yao1 i
                      i 6)
                )
              (setq i (1+ i))
              )
            )
        (progn
          (setq shizhi (- shizhi (* 2 (aref tmp2 0)))
                i 1)
          (while (<= i 6)
            (when (/= (aref tmp2 i) yinyang)
              (setq shizhi (1- shizhi)))
            (when (<= shizhi 0)
              (setq yao1 i
                    i 6)
              )
            (setq i (1+ i))
            )
          ))
      )
     ((and (> (aref tmp2 0) 3) (< (aref tmp2 0) 6))
      (if (<= shizhi (aref tmp2 0))
          (progn
            (setq i 1)
            (while (<= i 6)
              (when (= (aref tmp2 i) yinyang)
                (setq shizhi (1- shizhi)))
              (when (<= shizhi 0)
                (setq yao1 i
                      i 6)
                )
              (setq i (1+ i))
              )
            )
        (progn
          (setq shizhi (- shizhi (aref tmp2 0))
                i 1)
          (while (<= i 6)
            (when (/= (aref tmp2 i) yinyang)
              (setq shizhi (1- shizhi)))
            (when (<= shizhi 0)
              (setq yao1 i
                    i 6)
              )
            (setq i (1+ i))
            )
          ))
      )
     (t
      (if (= gua1 63)
          (if gender
              (if (= 1 yinyang)
                  (if (> shizhi 3)
                      (setq yao1 (- shizhi 3))
                    (setq yao1 shizhi))
                (if (> shizhi 3)
                    (setq yao1 shizhi)
                  (setq yao1 (+ shizhi 3))))
            (if (and (>= (nth 5 sinfo) 10) (< (nth 5 sinfo) 22))
                (if (= 1 yinyang)
                    (if (> shizhi 3)
                        (setq yao1 (- shizhi 3))
                      (setq yao1 shizhi))
                  (if (> shizhi 3)
                      (setq yao1 shizhi)
                    (setq yao1 (+ shizhi 3))))
              (if (= 1 yinyang)
                  (if (> shizhi 3)
                      (setq yao1 (- 10 shizhi))
                    (setq yao1 (- 7 shizhi)))
                (if (> shizhi 3)
                    (setq yao1 (- 7 shizhi))
                  (setq yao1 (- 4 shizhi))))
              )
            )
        (if (not gender)
            (if (= 1 yinyang)
                (if (> shizhi 3)
                    (setq yao1 (- shizhi 3))
                  (setq yao1 shizhi))
              (if (> shizhi 3)
                  (setq yao1 shizhi)
                (setq yao1 (+ shizhi 3))))
          (if (and (>= (nth 5 sinfo) 10) (< (nth 5 sinfo) 22))
              (if (= 1 yinyang)
                  (if (> shizhi 3)
                      (setq yao1 (- 10 shizhi))
                    (setq yao1 (- 7 shizhi)))
                (if (> shizhi 3)
                    (setq yao1 (- 7 shizhi))
                  (setq yao1 (- 4 shizhi))))
            (if (= 1 yinyang)
                (if (> shizhi 3)
                    (setq yao1 (- shizhi 3))
                  (setq yao1 shizhi))
              (if (> shizhi 3)
                  (setq yao1 shizhi)
                (setq yao1 (+ shizhi 3))))
            )
          )
        )
      ))
    ;; 计算后天卦及元堂
    (setq gua2 (logxor gua1 (ash 1 (- 6 yao1)))
          yao2 yao1)
    (if (and (or (= gua1 10) (= gua1 18) (= gua1 34))
             (> yao2 4))
        (if (= (mod (nth 2 sinfo) 2) 1)
            (when (= yao2 5)
              (setq yao2 (- yao2 3)
                    gua2 (+ (/ gua2 8) (* (mod gua2 8) 8))
                    ))
          (when (= yao2 6)
            (setq yao2 (- yao2 3)
                  gua2 (+ (/ gua2 8) (* (mod gua2 8) 8))
                  ))
          )
      (setq yao2 (1+ (mod (+ yao2 2) 6))
            gua2 (+ (/ gua2 8) (* (mod gua2 8) 8)))
      )
    (setq tmp2 (gua_bit_sts gua2))
    (setq year3 (+ (* (aref tmp2 0) 3) 36))
    ;; 计算河洛批言的年限范围
    (if (= 1 (mod (+ (nth 1 sinfo) (nth 2 birthday)) 2))
        (setq year1 (nth 2 birthday))
      (setq year1 (1- (nth 2 birthday)))
      )
    (setq year2 (+ year1 year2))
    (setq year3 (+ year2 year3 -1))
    (list ts ds gua1 yao1 gua2 yao2 year1 year2 year3)
    )
  )

;; 计算指定年份的卦爻信息
;; I: year
;; O: '(gua-half-life yao-half-life year-min-half-life year-max-half-life
;;      gua-da-yun yao-da-yun year-min-da-yun year-max-da-yun
;;      gua-year yao-year year
;;      gua-month-1 yao-month-1
;;      gua-month-2 yao-month-2
;;      ...
;;      gua-month-12 yao-month-12)
(defun heluo_year_info (year)
  (let* ((year-min (plist-get fate-current-user 'heluo-year-min))
         (year-mid (plist-get fate-current-user 'heluo-year-mid))
         (year-max (plist-get fate-current-user 'heluo-year-max))
         (year-cur (if (< year year-min) year-min
                     (if (> year year-max) year-max
                       year)))
         (result (if (< year-cur year-mid)
                     (list (plist-get fate-current-user 'heluo-gua1)
                           (plist-get fate-current-user 'heluo-yao1)
                           year-min
                           (1- year-mid))
                   (list (plist-get fate-current-user 'heluo-gua2)
                         (plist-get fate-current-user 'heluo-yao2)
                         year-mid
                         year-max)))
         (bitsts (gua_bit_sts (car result)))
         (age (- year-cur (nth 2 result)))
         (curyao (nth 1 result))
         i yaoyears curgua curgua2 curyao2
         )
    ;; 计算大运
    (setq i 0)
    (while (< i 6)
      (setq yaoyears (+ (* (aref bitsts curyao) 3) 6))
      (when (> yaoyears age)
        (setq result (append result (list (car result)
                                          curyao
                                          (- year-cur age)
                                          (+ (- year-cur age 1) yaoyears))))
        (setq i 6)
        )
      (setq age (- age yaoyears)
            i (1+ i)
            curyao (if (>= curyao 6) 1 (1+ curyao)))
      )
    (setq age (+ age yaoyears)
          curyao (if (<= curyao 1) 6 (1- curyao))
          curgua (car result))
    ;; 计算岁运
    (if (> yaoyears 6)
        (progn
          (when (= (mod (- year-cur age) 2) 1)
            (setq curgua (logxor curgua (ash 1 (- 6 curyao))))
            )
          (when (> age 0)
            (if (> curyao 3)
                (setq curgua (logxor curgua (ash 1 (- 9 curyao))))
              (setq curgua (logxor curgua (ash 1 (- 3 curyao))))
              )
            (setq age (1- age)
                  curyao (1- curyao)
                  i 0)
            (while (< i age)
              (setq curyao (if (>= curyao 6) 1 (1+ curyao))
                    curgua (logxor curgua (ash 1 (- 6 curyao)))
                    i (1+ i)
                    )
              )
            )
          )
      (progn
        (setq curyao (1- curyao)
              i 0)
        (while (<= i age)
          (setq curyao (if (>= curyao 6) 1 (1+ curyao))
                curgua (logxor curgua (ash 1 (- 6 curyao)))
                i (1+ i)
                )
          )
        )
      )
    (setq result (append result (list curgua
                                      curyao
                                      year-cur)))
    ;; 计算月运
    (setq i 0)
    (while (< i 6)
      (setq curyao (if (>= curyao 6) 1 (1+ curyao))
            curgua (logxor curgua (ash 1 (- 6 curyao)))
            curyao2 (if (> curyao 3) (- curyao 3) (+ curyao 3))
            curgua2 (logxor curgua (ash 1 (- 6 curyao2)))
            result (append result (list curgua curyao curgua2 curyao2))
            i (1+ i)
            )
      )
    result
    )
  )

;; 取得指定卦爻的批言
;; I: gua([0,63]) yao([0,6])
;; O: '(卦名爻序号
;;      易经爻辞
;;      批言小诗1
;;      ...
;;      批言小诗n)
(defun heluo_msg (gua yao)
  (if (and (integerp gua)
           (integerp yao)
           (>= gua 0)
           (<= gua 63)
           (>= yao 0)
           (<= yao 6))
      (with-current-buffer (find-file-noselect fate-heluo-file)
        (let ((prefix1 (format "^%d-%d#" gua 0))
              (prefix2 (format "^%d-%d#" gua yao))
              (suffix1 "卦")
              (suffix2 "爻")
              point11 point12 point21 point22
              result
              )
          (goto-char (point-min))
          (setq point11 (search-forward-regexp prefix1)
                point12 (search-forward-regexp suffix1)
                point21 (if (> yao 0) (search-forward-regexp prefix2) point11)
                point22 (if (> yao 0) (search-forward-regexp suffix2) point12)
                result (cons (if (> point21 point11)
                                 (format "%s%s" (buffer-substring-no-properties point11 point12)
                                         (buffer-substring-no-properties point21 point22))
                               (buffer-substring-no-properties point11 point12)
                               )
                             (if (> point21 point11)
                                 (split-string (format "%s%s" (buffer-substring-no-properties point11 point12)
                                                       (buffer-substring-no-properties point21 (line-end-position))) ":")
                               (split-string (buffer-substring-no-properties point21 (line-end-position)) ":")
                               )
                             ))
          result
          )
        )
    (list "Error")
    )
  )

;; 输出指定年份的批言到当前buffer
;; I: year
;; Memo: 采用org mode格式，输出全年及每个月的批言
(defun heluo_msg_output (year)
  (let* ((guayao (heluo_year_info year))
         (curgua (nth 8 guayao))
         (curyao (nth 9 guayao))
         (tmpd1 (fate-solar-item-info (nth 10 guayao) 1))
         (tmpd2 (fate-solar-item-info (1+ (nth 10 guayao)) 1))
         (curtext (heluo_msg curgua curyao))
         (pat1 "** %d/%d/%d %d:%d:%d -- %d/%d/%d %d:%d:%d %s\n")
         (pat2 "   + %s\n")
         tmpi tmpj
         )
    ;; 输出全年批言
    (insert (format pat1 (nth 2 tmpd1) (nth 0 tmpd1) (nth 1 tmpd1)
                    (nth 3 tmpd1) (nth 4 tmpd1) (nth 5 tmpd1)
                    (nth 2 tmpd2) (nth 0 tmpd2) (nth 1 tmpd2)
                    (nth 3 tmpd2) (nth 4 tmpd2) (nth 5 tmpd2)
                    (nth 0 curtext)
                    ))
    (setq tmpi 1)
    (while (< tmpi (length curtext))
      (insert (format pat2 (nth tmpi curtext)))
      (setq tmpi (1+ tmpi))
      )
    ;; 输出每个月的批言
    (dotimes (tmpj 12)
      (setq curgua (nth (+ (* tmpj 2) 11) guayao)
            curyao (nth (+ (* tmpj 2) 12) guayao)
            tmpd1 (fate-solar-item-info (nth 10 guayao) (1+ (* tmpj 2)))
            tmpd2 (if (>= tmpj 11)
                      (fate-solar-item-info (1+ (nth 10 guayao)) 1)
                    (fate-solar-item-info (nth 10 guayao) (+ 3 (* tmpj 2)))
                    )
            curtext (heluo_msg curgua curyao)
            )
      (insert "*")
      (insert (format pat1 (nth 2 tmpd1) (nth 0 tmpd1) (nth 1 tmpd1)
                      (nth 3 tmpd1) (nth 4 tmpd1) (nth 5 tmpd1)
                      (nth 2 tmpd2) (nth 0 tmpd2) (nth 1 tmpd2)
                      (nth 3 tmpd2) (nth 4 tmpd2) (nth 5 tmpd2)
                      (nth 0 curtext)
                      ))
      (setq tmpi 1)
      (while (< tmpi (length curtext))
        (insert " ")
        (insert (format pat2 (nth tmpi curtext)))
        (setq tmpi (1+ tmpi))
        )
      )
    )
  )

;; 输出批言到"fate-heluo"buffer
;; I: year
;; Memo: 如果省略year，则使用当前时间对应的年份（以立春为界）
(defun heluo-show (&optional year)
  (interactive "nYear:")
  (let* ((now (decode-time))
         (now-formatted (list (nth 4 now) (nth 3 now) (nth 5 now)
                              (nth 2 now) (nth 1 now) (nth 0 now)))
         (year-cur (or year
                       (if (< (calendar-fate-chinese-datetime now-formatted)
                              (calendar-fate-chinese-datetime (fate-solar-item-info (calendar-extract-year now-formatted) 1)))
                           (1- (calendar-extract-year now-formatted))
                         (calendar-extract-year now-formatted)
                         )))
         (year-start (1- year-cur))
         (logbuffer (get-buffer-create "fate-heluo"))
         cnt
         )
    (set-buffer logbuffer)
    (erase-buffer)
    (org-mode)
    (insert (format "* %s\n" (plist-get fate-current-user 'name)))
    (dotimes (cnt 4)
      (heluo_msg_output (+ year-start cnt))
      )
    (org-content 2)
    (switch-to-buffer logbuffer)
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

;; 卦名 6爻干支 5爻干支 ... 1爻干支 世爻位置 本卦 伏神mask
(defconst gua-64 '(
                   ("坤" 10 60 50 52 42 32 6 0 0)
                   ("剥" 3 13 23 52 42 32 5 63 2)
                   ("比" 25 35 45 52 42 32 3 0 0)
                   ("观" 28 18 8 52 42 32 4 63 34)
                   ("豫" 47 57 7 52 42 32 1 36 32)
                   ("晋" 6 56 46 52 42 32 4 63 32)
                   ("萃" 44 34 24 52 42 32 2 54 0)
                   ("否" 59 9 19 52 42 32 3 63 32)
                   ("谦" 10 60 50 33 43 53 5 54 16)
                   ("艮" 3 13 23 33 43 53 6 9 0)
                   ("蹇" 25 35 45 33 43 53 4 54 16)
                   ("渐" 28 18 8 33 43 53 3 9 2)
                   ("小过" 47 57 7 33 43 53 4 54 20)
                   ("旅" 6 56 46 33 43 53 1 45 40)
                   ("咸" 44 34 24 33 43 53 3 54 16)
                   ("遯" 59 9 19 33 43 53 2 63 48)
                   ("师" 10 60 50 55 5 15 3 18 0)
                   ("蒙" 3 13 23 55 5 15 4 45 4)
                   ("坎" 25 35 45 55 5 15 6 18 0)
                   ("涣" 28 18 8 55 5 15 5 45 12)
                   ("解" 47 57 7 55 5 15 2 36 32)
                   ("未济" 6 56 46 55 5 15 3 45 8)
                   ("困" 44 34 24 55 5 15 1 54 0)
                   ("讼" 59 9 19 55 5 15 4 45 8)
                   ("升" 10 60 50 58 48 38 4 36 20)
                   ("蛊" 3 13 23 58 48 38 3 27 2)
                   ("井" 25 35 45 58 48 38 5 36 20)
                   ("巽" 28 18 8 58 48 38 6 27 0)
                   ("恒" 47 57 7 58 48 38 3 36 16)
                   ("鼎" 6 56 46 58 48 38 2 45 32)
                   ("大过" 44 34 24 58 48 38 4 36 20)
                   ("姤" 59 9 19 58 48 38 1 63 16)
                   ("复" 10 60 50 17 27 37 1 0 16)
                   ("颐" 3 13 23 17 27 37 4 27 10)
                   ("屯" 25 35 45 17 27 37 2 18 8)
                   ("益" 28 18 8 17 27 37 3 27 8)
                   ("震" 47 57 7 17 27 37 6 36 0)
                   ("噬嗑" 6 56 46 17 27 37 5 27 0)
                   ("随" 44 34 24 17 27 37 3 36 4)
                   ("无妄" 59 9 19 17 27 37 4 27 0)
                   ("明夷" 10 60 50 36 26 16 4 18 8)
                   ("贲" 3 13 23 36 26 16 1 9 24)
                   ("既济" 25 35 45 36 26 16 3 18 8)
                   ("家人" 28 18 8 36 26 16 2 27 8)
                   ("丰" 47 57 7 36 26 16 5 18 0)
                   ("离" 6 56 46 36 26 16 6 45 0)
                   ("革" 44 34 24 36 26 16 4 18 8)
                   ("同人" 59 9 19 36 26 16 3 45 0)
                   ("临" 10 60 50 14 4 54 2 0 0)
                   ("损" 3 13 23 14 4 54 3 9 8)
                   ("节" 25 35 45 14 4 54 1 18 0)
                   ("中孚" 28 18 8 14 4 54 4 9 10)
                   ("归妹" 47 57 7 14 4 54 3 54 4)
                   ("睽" 6 56 46 14 4 54 4 9 2)
                   ("兑" 44 34 24 14 4 54 6 54 0)
                   ("履" 59 9 19 14 4 54 5 9 2)
                   ("泰" 10 60 50 41 51 1 3 0 16)
                   ("大畜" 3 13 23 41 51 1 2 9 24)
                   ("需" 25 35 45 41 51 1 4 0 16)
                   ("小畜" 28 18 8 41 51 1 1 27 8)
                   ("大壮" 47 57 7 41 51 1 4 0 0)
                   ("大有" 6 56 46 41 51 1 3 63 0)
                   ("夬" 44 34 24 41 51 1 5 0 16)
                   ("乾" 59 9 19 41 51 1 6 63 0)
                   ))
;; 生成变量的python3代码
;; # -*- coding:utf-8 -*-
;; names = (
;;     "坤", "剥", "比", "观", "豫", "晋", "萃", "否",
;;     "谦", "艮", "蹇", "渐", "小过", "旅", "咸", "遯",
;;     "师", "蒙", "坎", "涣", "解", "未济", "困", "讼",
;;     "升", "蛊", "井", "巽", "恒", "鼎", "大过", "姤",
;;     "复", "颐", "屯", "益", "震", "噬嗑", "随", "无妄",
;;     "明夷", "贲", "既济", "家人", "丰", "离", "革", "同人",
;;     "临", "损", "节", "中孚", "归妹", "睽", "兑", "履",
;;     "泰", "大畜", "需", "小畜", "大壮", "大有", "夬", "乾"
;; )
;; base = (
;;     (10,60,50,52,42,32),
;;     (3,13,23,33,43,53),
;;     (25,35,45,55,5,15),
;;     (28,18,8,58,48,38),
;;     (47,57,7,17,27,37),
;;     (6,56,46,36,26,16),
;;     (44,34,24,14,4,54),
;;     (59,9,19,41,51,1)
;; )
;; dzwx = (1,1,4,2,2,4,3,3,4,5,5,4)
;; mask = (0,32,16,8,4,2,4,56)
;; pos = (6,1,2,3,4,5,4,3)
;; fh = open('test.txt', 'w', encoding='utf-8')
;; for i in range(64):
;;     # push name first
;;     outlist = [names[i]]
;;     # push ganzhi for each yao(6->1)
;;     outlist.extend(base[i%8][0:3])
;;     outlist.extend(base[i//8][3:])
;;     wxset = set([dzwx[x % 12] for x in outlist[1:]])
;;     k = i
;;     for j in range(len(mask)):
;;         k ^= mask[j]
;;         if k%9==0:
;;             break
;;     # push self position
;;     outlist.append(pos[j])
;;     # push base gua
;;     outlist.append(k)
;;     # calculate invisible wx
;;     k = k // 9
;;     l = 0
;;     for j in range(6):
;;         if dzwx[base[k][j] % 12] not in wxset:
;;             l |= 1 << j
;;     outlist.append(l)
;;     fh.write('("{}" {})\n'.format(outlist[0], ' '.join([str(x) for x in outlist[1:]])))
;; fh.close()
;; 八卦类象
(defconst gua-8-xiang '("地" "山" "水" "风" "雷" "火" "泽" "天"))
;; 八卦五行
(defconst gua-8-wuxing '(4 4 1 2 2 3 5 5))
;; 天干五行（癸，甲，乙，。。。，壬）
(defconst tiangan-wuxing '(1 2 2 3 3 4 4 5 5 1))
;; 地支五行（亥，子，丑，。。。，戌）
(defconst dizhi-wuxing '(1 1 4 2 2 4 3 3 4 5 5 4))
;; 六亲
(defconst liuqin-name '("兄" "孙" "财" "官" "父"))
;; 六神
(defconst liushen-name '("青龙" "朱雀" "勾陈" "滕蛇" "白虎" "玄武"))
;; 计算卦的六亲
(defun liuqin (gua &optional basegua)
  (let (
        (basewx (nth (floor (/ (nth 8 (nth (or basegua gua) gua-64)) 9)) gua-8-wuxing))
        (dizhilen (length dizhi-wuxing))
        (liuqinlen (length liuqin-name))
        )
    (list (mod (- (nth (mod (nth 1 (nth gua gua-64)) dizhilen) dizhi-wuxing) basewx) liuqinlen)
          (mod (- (nth (mod (nth 2 (nth gua gua-64)) dizhilen) dizhi-wuxing) basewx) liuqinlen)
          (mod (- (nth (mod (nth 3 (nth gua gua-64)) dizhilen) dizhi-wuxing) basewx) liuqinlen)
          (mod (- (nth (mod (nth 4 (nth gua gua-64)) dizhilen) dizhi-wuxing) basewx) liuqinlen)
          (mod (- (nth (mod (nth 5 (nth gua gua-64)) dizhilen) dizhi-wuxing) basewx) liuqinlen)
          (mod (- (nth (mod (nth 6 (nth gua gua-64)) dizhilen) dizhi-wuxing) basewx) liuqinlen)
          )
    )
  )
;; 六爻排盘文字
(defconst liuyao-text '("-- --"
                        "-----"
                        "世"
                        "应"
                        "  "))
;; 六爻装卦
(defun liuyao_zhuanggua (gua1 gua2 &optional date)
  (let* ((gua0 (nth 8 (nth gua1 gua-64)))                       ;; 宫卦
         (basewx (nth (floor (/ gua0 9)) gua-8-wuxing))         ;; 宫卦五行
         (lq0 (liuqin gua0 gua0))                               ;; 宫卦六亲
         (lq1 (liuqin gua1 gua0))                               ;; 本卦六亲
         (lq2 (liuqin gua2 gua0))                               ;; 变卦六亲
         (mask0 (nth 9 (nth gua1 gua-64)))                      ;; 伏神mask
         (mask2 (logxor gua1 gua2))                             ;; 变爻mask
         (pos0 6)                                               ;; 宫卦世爻位置
         (pos1 (nth 7 (nth gua1 gua-64)))                       ;; 本卦世爻位置
         (pos2 (nth 7 (nth gua2 gua-64)))                       ;; 变卦世爻位置
         (adate (calendar-fate-chinese-datetime date))          ;; 卜卦时间（绝对时间）
         (gdate (calendar-fate-gregorian-from-absolute adate))  ;; 卜卦时间（公历）
         (sdate (calendar-fate-chinese-from-absolute adate))    ;; 卜卦时间（阳历）
         (kong1 (cond ((> (nth 3 sdate) 50) 1)                  ;; 日空1
                      ((> (nth 3 sdate) 40) 51)
                      ((> (nth 3 sdate) 30) 41)
                      ((> (nth 3 sdate) 20) 31)
                      ((> (nth 3 sdate) 10) 21)
                      (t 11)
                      ))
         (kong2 (1+ kong1))                                     ;; 日空2
         (ls (cond ((< (mod (1- (nth 3 sdate)) 10) 2) 0)        ;; 初爻六神
                   ((< (mod (1- (nth 3 sdate)) 10) 4) 1)
                   ((< (mod (1- (nth 3 sdate)) 10) 5) 2)
                   ((< (mod (1- (nth 3 sdate)) 10) 6) 3)
                   ((< (mod (1- (nth 3 sdate)) 10) 8) 4)
                   (t 5)
                   ))
         tmpi tmpj tmpk                                         ;; 临时变量
         txt0 txt1 txt2                                         ;; 临时变量
         )
    (unless (= (point) (line-beginning-position))
      (insert "\n")
      )
    ;; 输出公历时间
    (insert (format "%d-%d-%d %d:%d"
                    (nth 2 gdate) (nth 0 gdate) (nth 1 gdate)
                    (nth 3 gdate) (nth 4 gdate)
                    ))
    ;; 输出卦码（上卦、下卦、动爻）
    (setq tmpk (- 8 (mod gua1 8))
          tmpk (+ (* tmpk 10) (- 8 (lsh gua1 -3)))
          tmpj (logxor gua1 gua2)
          )
    (dotimes (tmpi 6)
      (when (> (logand tmpj (ash 32 (- 0 tmpi))) 0)
        (setq tmpk (+ (* tmpk 10) tmpi 1))
        )
      )
    (insert (format "  %d\n占事：\n" tmpk))
    ;; 输出阳历时间
    (insert (format "%s %s %s %s 空%s%s\n"
                    (calendar-fate-chinese-sexagesimal-name (nth 1 sdate))
                    (calendar-fate-chinese-sexagesimal-name (nth 2 sdate))
                    (calendar-fate-chinese-sexagesimal-name (nth 3 sdate))
                    (calendar-fate-chinese-sexagesimal-name (nth 4 sdate))
                    (aref chinese-fate-calendar-terrestrial-branch (% (1- kong1) 12))
                    (aref chinese-fate-calendar-terrestrial-branch (% (1- kong2) 12))
                    ))
    ;; 输出日期六亲
    (insert (format "  %s   %s   %s   %s\n"
                    (nth (mod (- (nth (mod (nth 1 sdate) (length dizhi-wuxing)) dizhi-wuxing) basewx) (length liuqin-name)) liuqin-name)
                    (nth (mod (- (nth (mod (nth 2 sdate) (length dizhi-wuxing)) dizhi-wuxing) basewx) (length liuqin-name)) liuqin-name)
                    (nth (mod (- (nth (mod (nth 3 sdate) (length dizhi-wuxing)) dizhi-wuxing) basewx) (length liuqin-name)) liuqin-name)
                    (nth (mod (- (nth (mod (nth 4 sdate) (length dizhi-wuxing)) dizhi-wuxing) basewx) (length liuqin-name)) liuqin-name)
                    ))
    (dotimes (tmpi 6)
      ;; 设置宫卦、本卦、变卦当前爻的文字
      (setq txt0 (format "%s%s%s%s"
                         (nth (logand (ash gua0 (- 0 tmpi)) 1) liuyao-text)
                         (cond ((= (- 6 tmpi) pos0) (nth 2 liuyao-text))
                               ((= (mod (- 6 tmpi) 6) (mod (+ pos0 3) 6)) (nth 3 liuyao-text))
                               (t (nth 4 liuyao-text)))
                         ;; (calendar-fate-chinese-sexagesimal-name (nth (1+ tmpi) (nth gua0 gua-64)))
                         (aref chinese-fate-calendar-terrestrial-branch (% (1- (nth (1+ tmpi) (nth gua0 gua-64))) 12))
                         (nth (nth tmpi lq0) liuqin-name)
                         )
            txt1 (format "%s%s%s%s"
                         (nth (logand (ash gua1 (- 0 tmpi)) 1) liuyao-text)
                         (cond ((= (- 6 tmpi) pos1) (nth 0 (nth gua0 gua-64)))
                               ((= (mod (- 6 tmpi) 6) (mod (+ pos1 3) 6)) (nth 3 liuyao-text))
                               (t (nth 4 liuyao-text)))
                         ;; (calendar-fate-chinese-sexagesimal-name (nth (1+ tmpi) (nth gua1 gua-64)))
                         (aref chinese-fate-calendar-terrestrial-branch (% (1- (nth (1+ tmpi) (nth gua1 gua-64))) 12))
                         (nth (nth tmpi lq1) liuqin-name)
                         )
            txt2 (format "%s%s%s%s"
                         (nth (logand (ash gua2 (- 0 tmpi)) 1) liuyao-text)
                         (cond ((= (- 6 tmpi) pos2) (nth 0 (nth (nth 8 (nth gua2 gua-64)) gua-64)))
                               ((= (mod (- 6 tmpi) 6) (mod (+ pos2 3) 6)) (nth 3 liuyao-text))
                               (t (nth 4 liuyao-text)))
                         ;; (calendar-fate-chinese-sexagesimal-name (nth (1+ tmpi) (nth gua2 gua-64)))
                         (aref chinese-fate-calendar-terrestrial-branch (% (1- (nth (1+ tmpi) (nth gua2 gua-64))) 12))
                         (nth (nth tmpi lq2) liuqin-name)
                         )
            )
      ;; 输出当前爻六神
      (insert (format "%s  " (nth (mod (- ls tmpi -5) (length liushen-name)) liushen-name)))
      ;; 输出宫卦当前爻文字
      (setq tmpj (point))
      (if (> (logand mask0 (ash 1 tmpi)) 0)
          (progn
            (insert txt0)
            (add-face-text-property tmpj (point) '(:foreground "blue") nil)
            )
        (progn
          (insert txt0)
          (add-face-text-property tmpj (point) '(:foreground "gray") nil)
          )
        )
      (insert "  ")
      ;; 输出本卦当前爻文字
      (setq tmpj (point))
      (if (> (logand mask2 (ash 1 tmpi)) 0)
          (progn
            (insert txt1)
            (add-face-text-property tmpj (point) '(:foreground "red") nil)
            )
        (progn
          (insert txt1)
          (add-face-text-property tmpj (point) '(:foreground "black") nil)
          )
        )
      (insert "  ")
      ;; 输出变卦当前爻文字
      (setq tmpj (point))
      (if (> (logand mask2 (ash 1 tmpi)) 0)
          (progn
            (insert txt2)
            (add-face-text-property tmpj (point) '(:foreground "red") nil)
            )
        (progn
          (insert txt2)
          (add-face-text-property tmpj (point) '(:foreground "gray") nil)
          )
        )
      (insert "\n")
      (when (>= tmpi 5)
        (setq pos0 (+ (string-width (car liushen-name)) 2)
              pos1 (+ (string-width txt0) 2)
              pos2 (+ (string-width txt1) 2)
              )
        )
      )
    ;; 输出卦名
    (setq tmpi (mod gua0 8)
          tmpj (lsh gua0 -3))
    (if (= tmpi tmpj)
        (setq txt0 (format "%s为%s" (nth tmpi gua-8-xiang) (nth 0 (nth gua0 gua-64))))
      (setq txt0 (format "%s%s%s" (nth tmpi gua-8-xiang) (nth tmpj gua-8-xiang) (nth 0 (nth gua0 gua-64))))
      )
    (setq tmpi (mod gua1 8)
          tmpj (lsh gua1 -3))
    (if (= tmpi tmpj)
        (setq txt1 (format "%s为%s" (nth tmpi gua-8-xiang) (nth 0 (nth gua1 gua-64))))
      (setq txt1 (format "%s%s%s" (nth tmpi gua-8-xiang) (nth tmpj gua-8-xiang) (nth 0 (nth gua1 gua-64))))
      )
    (setq tmpi (mod gua2 8)
          tmpj (lsh gua2 -3))
    (if (= tmpi tmpj)
        (setq txt2 (format "%s为%s" (nth tmpi gua-8-xiang) (nth 0 (nth gua2 gua-64))))
      (setq txt2 (format "%s%s%s" (nth tmpi gua-8-xiang) (nth tmpj gua-8-xiang) (nth 0 (nth gua2 gua-64))))
      )
    (insert (format "%s%s%s%s%s%s\n" (make-string pos0 ? ) txt0
                    (make-string (- pos1 (string-width txt0)) ? ) txt1
                    (make-string (- pos2 (string-width txt1)) ? ) txt2))
    ;; 输出爻辞
    (setq tmpk (list gua0))
    (add-to-list 'tmpk gua1 t)
    (add-to-list 'tmpk gua2 t)
    (setq pos2 '())
    (with-current-buffer (find-file-noselect fate-heluo-file)
      (dotimes (tmpi (length tmpk))
        (goto-char (point-min))
        (setq pos2 (cons "" pos2))
        (dotimes (tmpj 7)
          (setq pos0 (search-forward-regexp (format "^%d-%d#" (nth tmpi tmpk) tmpj))
                pos1 (buffer-substring-no-properties pos0 (line-end-position))
                pos2 (cons (car (split-string pos1 ":")) pos2)
                )
          )
        )
      )
    (setq pos2 (reverse pos2))
    (dotimes (tmpi (length pos2))
      (insert (nth tmpi pos2) "\n")
      )
    (goto-char (point-min))
    (end-of-line 2)
    )
  )
;; 六爻排盘
(defun liuyao-show ()
  (interactive)
  (let* ((input (string-to-number (read-string "硬币背面个数（3硬币6次）："))) ;; 用户输入
         (yao (list (mod (mod input 10) 4)
                    (mod (mod (floor (/ input 10)) 10) 4)
                    (mod (mod (floor (/ input 100)) 10) 4)
                    (mod (mod (floor (/ input 1000)) 10) 4)
                    (mod (mod (floor (/ input 10000)) 10) 4)
                    (mod (mod (floor (/ input 100000)) 10) 4)
                    ))                                                         ;; 各爻数据
         (gua1 0)                                                              ;; 本卦
         (gua2 0)                                                              ;; 变卦
         (xtime (safe-date-to-time (org-read-date t)))                         ;; 用户输入时间
         (ytime (decode-time xtime))                                           ;; 转换后时间
         (logbuffer (get-buffer-create "fate-liuyao"))                         ;; 输出buffer
         tmpi tmpj                                                             ;; 临时变量
         )
    ;; 计算本卦及变卦
    (dotimes (tmpi 6)
      (setq tmpj (nth tmpi yao)
            gua1 (+ gua1 (ash (logand tmpj 1) tmpi))
            gua2 (+ gua2 (ash (if (> (logxor (logand tmpj 1) (lsh tmpj -1)) 0)
                                  (logand tmpj 1)
                                (- 1 (logand tmpj 1))
                                ) tmpi))
            )
      )
    ;; 读取日期（输入必须包括时间）
    (while (and (= (nth 0 xtime) 0) (= (nth 1 xtime) 0))
      (setq xtime (safe-date-to-time (org-read-date t))
            ytime (decode-time xtime)
            )
      )
    (set-buffer logbuffer)
    (erase-buffer)
    (liuyao_zhuanggua gua1 gua2 (list (nth 4 ytime) (nth 3 ytime) (nth 5 ytime) (nth 2 ytime) (nth 1 ytime) (nth 0 ytime)))
    (switch-to-buffer logbuffer)
    )
  )
;; 随机起六爻卦
(defun liuyao-new ()
  (interactive)
  (let ((gua1 0)                                        ;; 本卦
        (gua2 0)                                        ;; 变卦
        (logbuffer (get-buffer-create "fate-liuyao"))   ;; 输出buffer
        tmpi tmpj tmpm tmpn                             ;; 临时变量
        )
    (random t)
    (dotimes (tmpi 6)
      (setq tmpj (mod (random) 8))
      (cond ((= tmpj 0) (setq tmpm 0 tmpn 1))
            ((= tmpj 7) (setq tmpm 1 tmpn 0))
            ((= tmpj 1) (setq tmpm 1 tmpn 1))
            ((= tmpj 2) (setq tmpm 1 tmpn 1))
            ((= tmpj 4) (setq tmpm 1 tmpn 1))
            ((= tmpj 3) (setq tmpm 0 tmpn 0))
            ((= tmpj 5) (setq tmpm 0 tmpn 0))
            ((= tmpj 6) (setq tmpm 0 tmpn 0))
            )
      (setq gua1 (+ (* gua1 2) tmpm)
            gua2 (+ (* gua2 2) tmpn))
      )
    (set-buffer logbuffer)
    (erase-buffer)
    (liuyao_zhuanggua gua1 gua2)
    (switch-to-buffer logbuffer)
    )
  )

;; 紫微各星曜信息列表（代号，名称，颜色，计算公式）
(defconst ziwei_star '(;; 14主星
                       (S0N01 "紫微" "red" '(nth (1- xday) (nth (- xju 2) '((2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10 11 11 12 12 1 1 2 2 3 3 4 4 5)
                                                                            (5 2 3 6 3 4 7 4 5 8 5 6 9 6 7 10 7 8 11 8 9 12 9 10 1 10 11 2 11 12)
                                                                            (12 5 2 3 1 6 3 4 2 7 4 5 3 8 5 6 4 9 6 7 5 10 7 8 6 11 8 9 7 12)
                                                                            (7 12 5 2 3 8 1 6 3 4 9 2 7 4 5 10 3 8 5 6 11 4 9 6 7 12 5 10 7 8)
                                                                            (10 7 12 5 2 3 11 8 1 6 3 4 12 9 2 7 4 5 1 10 3 8 5 6 2 11 4 9 6 7)
                                                                            ))))
                       (S0N02 "天机" "red" '(1+ (mod (- (plist-get info 'S0N01) 2) 12)))
                       (S0N03 "太阳" "red" '(1+ (mod (- (plist-get info 'S0N01) 4) 12)))
                       (S0N04 "武曲" "red" '(1+ (mod (- (plist-get info 'S0N01) 5) 12)))
                       (S0N05 "天同" "red" '(1+ (mod (- (plist-get info 'S0N01) 6) 12)))
                       (S0N06 "廉贞" "red" '(1+ (mod (- (plist-get info 'S0N01) 9) 12)))
                       (S0N11 "天府" "red" '(1+ (mod (- 17 (plist-get info 'S0N01)) 12)))
                       (S0N12 "太阴" "red" '(1+ (mod (plist-get info 'S0N11) 12)))
                       (S0N13 "贪狼" "red" '(1+ (mod (+ (plist-get info 'S0N11) 1) 12)))
                       (S0N14 "巨门" "red" '(1+ (mod (+ (plist-get info 'S0N11) 2) 12)))
                       (S0N15 "天相" "red" '(1+ (mod (+ (plist-get info 'S0N11) 3) 12)))
                       (S0N16 "天梁" "red" '(1+ (mod (+ (plist-get info 'S0N11) 4) 12)))
                       (S0N17 "七杀" "red" '(1+ (mod (+ (plist-get info 'S0N11) 5) 12)))
                       (S0N18 "破军" "red" '(1+ (mod (+ (plist-get info 'S0N11) 9) 12)))
                       ;; 甲级星
                       (S1N01 "禄存" "blue" '(nth (mod xgz 10) '(1 3 4 6 7 6 7 9 10 12)))
                       (S1N02 "擎羊" "blue" '(1+ (mod (plist-get info 'S1N01) 12)))
                       (S1N03 "陀罗" "blue" '(1+ (mod (- (plist-get info 'S1N01) 2) 12)))
                       (S1N04 "天魁" "blue" '(nth (mod xgz 10) '(4 2 1 12 12 2 1 2 7 4)))
                       (S1N05 "天钺" "blue" '(nth (mod xgz 10) '(6 8 9 10 10 8 9 8 3 6)))
                       (S1N11 "天马" "blue" '(nth (mod xgz 12) '(6 3 12 9 6 3 12 9 6 3 12 9)))
                       (S1N21 "左辅" "blue" '(1+ (mod (+ xmonth 3) 12)))
                       (S1N22 "右弼" "blue" '(1+ (mod (- 11 xmonth) 12)))
                       (S1N41 "文昌" "blue" '(1+ (mod (- 11 xhour) 12)))
                       (S1N42 "文曲" "blue" '(1+ (mod (+ xhour 3) 12)))
                       (S1N43 "火星" "blue" '(1+ (mod (+ (nth (mod xgz 12)
                                                              '(10 3 4 2 10 3 4 2 10 3 4 2))
                                                         xhour -2) 12)))
                       (S1N44 "铃星" "blue" '(1+ (mod (+ (nth (mod xgz 12)
                                                              '(11 11 11 4 11 11 11 4 11 11 11 4))
                                                         xhour -2) 12)))
                       (S1N45 "地劫" "blue" '(1+ (mod (+ xhour 10) 12)))
                       (S1N46 "地空" "blue" '(1+ (mod (- 12 xhour) 12)))
                       (S1N47 "台辅" "blue" '(1+ (mod (+ xhour 5) 12)))
                       (S1N48 "封诰" "blue" '(1+ (mod (+ xhour 1) 12)))
                       ;; 乙级星
                       (S2N01 "天官" "gray" '(nth (mod xgz 10) '(7 8 5 6 3 4 10 12 10 11)))
                       (S2N02 "天福" "gray" '(nth (mod xgz 10) '(6 10 9 1 12 4 3 7 6 7)))
                       (S2N03 "天厨" "gray" '(nth (mod xgz 10) '(12 6 7 1 6 7 9 3 7 10)))
                       (S2N11 "天空" "gray" '(1+ (mod (mod xgz 12) 12)))
                       (S2N12 "天哭" "gray" '(1+ (mod (- 7 (mod xgz 12)) 12)))
                       (S2N13 "天虚" "gray" '(1+ (mod (+ 5 (mod xgz 12)) 12)))
                       (S2N14 "龙池" "gray" '(1+ (mod (+ 3 (mod xgz 12)) 12)))
                       (S2N15 "凤阁" "gray" '(1+ (mod (- 11 (mod xgz 12)) 12)))
                       (S2N16 "红鸾" "gray" '(1+ (mod (- 4 (mod xgz 12)) 12)))
                       (S2N17 "天喜" "gray" '(1+ (mod (- 10 (mod xgz 12)) 12)))
                       (S2N18 "孤辰" "gray" '(nth (mod xgz 12) '(3 3 3 6 6 6 9 9 9 12 12 12)))
                       (S2N19 "寡宿" "gray" '(nth (mod xgz 12) '(11 11 11 2 2 2 5 5 5 8 8 8)))
                       (S2N1A "蜚廉" "gray" '(nth (mod xgz 12) '(2 9 10 11 6 7 8 3 4 5 12 1)))
                       (S2N1B "破碎" "gray" '(nth (mod xgz 12) '(10 6 2 10 6 2 10 6 2 10 6 2)))
                       (S2N1C "华盖" "gray" '(nth (mod xgz 12) '(8 5 2 11 8 5 2 11 8 5 2 11)))
                       (S2N1D "咸池" "gray" '(nth (mod xgz 12) '(1 10 7 4 1 10 7 4 1 10 7 4)))
                       (S2N1E "天德" "gray" '(1+ (mod (+ 8 (mod xgz 12)) 12)))
                       (S2N1F "月德" "gray" '(1+ (mod (+ 4 (mod xgz 12)) 12)))
                       (S2N1G "天才" "gray" '(1+ (mod (+ xming -2 (mod xgz 12)) 12)))
                       (S2N1H "天寿" "gray" '(1+ (mod (+ xshen -2 (mod xgz 12)) 12)))
                       (S2N21 "天刑" "gray" '(1+ (mod (+ xmonth 8) 12)))
                       (S2N22 "天姚" "gray" '(1+ (mod xmonth 12)))
                       (S2N23 "解神" "gray" '(nth (mod xmonth 12) '(7 9 9 11 11 1 1 3 3 5 5 7)))
                       (S2N24 "天巫" "gray" '(nth (mod xmonth 12) '(12 6 9 3 12 6 9 3 12 6 9 3)))
                       (S2N25 "天月" "gray" '(nth (mod xmonth 12) '(3 11 6 5 3 8 4 12 8 3 7 11)))
                       (S2N26 "阴煞" "gray" '(nth (mod xmonth 12) '(5 3 1 11 9 7 5 3 1 11 9 7)))
                       (S2N31 "三台" "gray" '(1+ (mod (+ xday (plist-get info 'S1N21) -2) 12)))
                       (S2N32 "八座" "gray" '(1+ (mod (- (plist-get info 'S1N22) xday) 12)))
                       (S2N33 "恩光" "gray" '(1+ (mod (+ xday (plist-get info 'S1N41) -3) 12)))
                       (S2N34 "天贵" "gray" '(1+ (mod (+ xday (plist-get info 'S1N42) -3) 12)))
                       ))
(defconst ziwei_star2 '((S9N01 "将星" "black" '(nth (mod (1- sgz) 12) '(1 10 7 4 1 10 7 4 1 10 7 4)))
                        (S9N02 "攀鞍" "black" '(1+ (mod (plist-get info2 'S9N01) 12)))
                        (S9N03 "岁驿" "black" '(1+ (mod (+ (plist-get info2 'S9N01) 1) 12)))
                        (S9N04 "息神" "black" '(1+ (mod (+ (plist-get info2 'S9N01) 2) 12)))
                        (S9N05 "华盖" "black" '(1+ (mod (+ (plist-get info2 'S9N01) 3) 12)))
                        (S9N06 "劫煞" "black" '(1+ (mod (+ (plist-get info2 'S9N01) 4) 12)))
                        (S9N07 "灾煞" "black" '(1+ (mod (+ (plist-get info2 'S9N01) 5) 12)))
                        (S9N08 "天煞" "black" '(1+ (mod (+ (plist-get info2 'S9N01) 6) 12)))
                        (S9N09 "指背" "black" '(1+ (mod (+ (plist-get info2 'S9N01) 7) 12)))
                        (S9N10 "咸池" "black" '(1+ (mod (+ (plist-get info2 'S9N01) 8) 12)))
                        (S9N11 "月煞" "black" '(1+ (mod (+ (plist-get info2 'S9N01) 9) 12)))
                        (S9N12 "亡神" "black" '(1+ (mod (+ (plist-get info2 'S9N01) 10) 12)))
                        (S9N21 "岁建" "black" '(1+ (mod (1- sgz) 12)))
                        (S9N22 "晦气" "black" '(1+ (mod (plist-get info2 'S9N21) 12)))
                        (S9N23 "丧门" "black" '(1+ (mod (+ (plist-get info2 'S9N21) 1) 12)))
                        (S9N24 "贯索" "black" '(1+ (mod (+ (plist-get info2 'S9N21) 2) 12)))
                        (S9N25 "官符" "black" '(1+ (mod (+ (plist-get info2 'S9N21) 3) 12)))
                        (S9N26 "小耗" "black" '(1+ (mod (+ (plist-get info2 'S9N21) 4) 12)))
                        (S9N27 "大耗" "black" '(1+ (mod (+ (plist-get info2 'S9N21) 5) 12)))
                        (S9N28 "龙德" "black" '(1+ (mod (+ (plist-get info2 'S9N21) 6) 12)))
                        (S9N29 "白虎" "black" '(1+ (mod (+ (plist-get info2 'S9N21) 7) 12)))
                        (S9N30 "天德" "black" '(1+ (mod (+ (plist-get info2 'S9N21) 8) 12)))
                        (S9N31 "吊客" "black" '(1+ (mod (+ (plist-get info2 'S9N21) 9) 12)))
                        (S9N32 "病符" "black" '(1+ (mod (+ (plist-get info2 'S9N21) 10) 12)))
                        (S9N41 "博士" "black" '(plist-get info 'S1N01))
                        (S9N42 "力士" "black" '(1+ (mod (- (plist-get info2 'S9N41) (* (- (* xnv 2) 3) 1) 1) 12)))
                        (S9N43 "青龙" "black" '(1+ (mod (- (plist-get info2 'S9N41) (* (- (* xnv 2) 3) 2) 1) 12)))
                        (S9N44 "小耗" "black" '(1+ (mod (- (plist-get info2 'S9N41) (* (- (* xnv 2) 3) 3) 1) 12)))
                        (S9N45 "将军" "black" '(1+ (mod (- (plist-get info2 'S9N41) (* (- (* xnv 2) 3) 4) 1) 12)))
                        (S9N46 "奏书" "black" '(1+ (mod (- (plist-get info2 'S9N41) (* (- (* xnv 2) 3) 5) 1) 12)))
                        (S9N47 "飞廉" "black" '(1+ (mod (- (plist-get info2 'S9N41) (* (- (* xnv 2) 3) 6) 1) 12)))
                        (S9N48 "喜神" "black" '(1+ (mod (- (plist-get info2 'S9N41) (* (- (* xnv 2) 3) 7) 1) 12)))
                        (S9N49 "病符" "black" '(1+ (mod (- (plist-get info2 'S9N41) (* (- (* xnv 2) 3) 8) 1) 12)))
                        (S9N50 "大耗" "black" '(1+ (mod (- (plist-get info2 'S9N41) (* (- (* xnv 2) 3) 9) 1) 12)))
                        (S9N51 "伏兵" "black" '(1+ (mod (- (plist-get info2 'S9N41) (* (- (* xnv 2) 3) 10) 1) 12)))
                        (S9N52 "官府" "black" '(1+ (mod (- (plist-get info2 'S9N41) (* (- (* xnv 2) 3) 11) 1) 12)))
                        ))
;; 阴历12月文字
(defconst lunar_monthname '("正" "二" "三" "四" "五" "六"
                            "七" "八" "九" "十" "冬" "腊"
                            "月" "闰"
                            ))
;; 紫微12宫文字
(defconst ziwei_12gong '("命宫" "兄弟" "夫妻" "子女" "财帛" "疾厄" "迁移" "交友" "官禄" "田宅" "福德" "父母" "身"))
;; 四化文字
(defconst ziwei_4change '("禄" "权" "科" "忌"))
;; 四化表格
(defconst ziwei_4change_tbl '((S0N06 S0N18 S0N04 S0N03)
                              (S0N02 S0N16 S0N01 S0N12)
                              (S0N05 S0N02 S1N41 S0N06)
                              (S0N12 S0N05 S0N02 S0N14)
                              (S0N13 S0N12 S1N22 S0N02)
                              (S0N04 S0N13 S0N16 S1N42)
                              (S0N03 S0N04 S0N12 S0N05)
                              (S0N14 S0N03 S1N42 S1N41)
                              (S0N16 S0N01 S1N21 S0N04)
                              (S0N18 S0N14 S0N12 S0N13)
                              ))
;; 四化对应颜色（命盘、大运、流年）
(defconst ziwei_color '(("white" "blue")
                        ("white" "orange")
                        ("black" "cyan")))

;; 清除字符串数组的属性（所有属性，包括颜色）
(defun clear-text-properties (obj)
  (let (tmpi)
    (cond ((eq 'string (type-of obj))
           (set-text-properties 0 (length obj) nil obj)
           )
          ((eq 'cons (type-of obj))
           (dotimes (tmpi (length obj))
             (clear-text-properties (nth tmpi obj))
             )
           )
          )
    )
  )

;; 计算紫微各星曜的位置
(defun ziwei_calculate (&optional mode year)
  (let* ((info '())                                                      ;; 临时保存各星曜的位置
         (result (make-vector 12 '()))                                   ;; 按宫位记录各星曜的代号
         (info2 '())                                                     ;; 临时保存各星曜的位置（流年星曜）
         (result2 (make-vector 12 '()))                                  ;; 按宫位记录各星曜的代号（流年星曜）
         (xgz (car (plist-get fate-current-user 'ziwei-birthday)))       ;; 基本信息（生年干支）
         (xyear (nth 1 (plist-get fate-current-user 'ziwei-birthday)))   ;; 基本信息（出生年份）
         (xmonth (nth 2 (plist-get fate-current-user 'ziwei-birthday)))  ;; 基本信息（出生月份）
         (xday (nth 3 (plist-get fate-current-user 'ziwei-birthday)))    ;; 基本信息（出生日期）
         (xhour (nth 4 (plist-get fate-current-user 'ziwei-birthday)))   ;; 基本信息（出生时辰）
         (xming (plist-get fate-current-user 'ziwei-ming))               ;; 基本信息（命宫位置）
         (xshen (plist-get fate-current-user 'ziwei-shen))               ;; 基本信息（身宫位置）
         (xju (plist-get fate-current-user 'ziwei-ju))                   ;; 基本信息（五行局）
         (xnv (1+ (mod (+ xgz
                          (if (plist-get fate-current-user 'male) 1 2))
                       2)))                                              ;; 基本信息（阳男阴女1，反之2）
         (smode (1+ (mod (1- (or mode 1)) 3)))                            ;; 指定模式（1:命盘 2:大运 3:流年）
         (syear (max (or year (nth 5 (decode-time))) xyear))             ;; 指定年份
         (sage (- (if (>= syear xyear)
                      syear
                    xyear)
                  xyear -1))                                             ;; 制定年龄
         (sgz (1+ (mod (+ xgz sage -2) 60)))                             ;; 指定年干支
         (gz3 (+ 3 (* 12 (mod (1- xgz) 5))))                             ;; 寅宫干支
         (direction (- (* 2 xnv) 3))                                     ;; 排宫方向
         (gong (list xming
                     (1+ (mod (1- (- xming (* direction (floor (/ (- sage xju) 10))))) 12))
                     (1+ (mod (1- (1+ (mod (- syear 1984) 60))) 12))
                     ))                                                  ;; 命宫位置（命盘，大运，流年）
         (firstmonth (1+ (mod (1-
                               (+ (- (nth 2 gong) (floor (+ xmonth 0.9)))
                                  xhour))
                              12)))                                      ;; 流正月宫位
         (sgz3 (+ 3 (* 12 (mod (1- sgz) 5))))                            ;; 流正月干支
         (hua (list xgz
                    (+ gz3 (nth 1 gong) (if (>= (nth 1 gong) 3) -3 9))
                    (1+ (mod (- syear 1984) 60))
                    ))                                                   ;; 4化年份（命盘，大运，流年）
         (rows-min 5)                                                    ;; 一个区块的最小行数
         (cols-min 12)                                                   ;; 一个区块的最小宽度（一个汉字的宽度为2）
         (rows (max rows-min 8))                                         ;; 一个区块的行数
         (cols (max cols-min 20))                                        ;; 一个区块的宽度
         (block (make-vector rows ""))                                   ;; 一个区块的文字
         (txt '())                                                       ;; 组装好的各区块的文字
         starcode starinfo starname starcolor                            ;; 星耀信息
         item tmpi tmpj tmpk                                             ;; 临时变量
         )
    ;; 计算各星座的位置
    (dotimes (tmpi (length ziwei_star))
      (setq item (nth tmpi ziwei_star)
            tmpj (eval (eval (car (last item))))
            info (plist-put info (car item) tmpj)
            )
      (aset result (1- tmpj) (append (aref result (1- tmpj)) (list (car item))))
      )
    (dotimes (tmpi (length ziwei_star2))
      (setq item (nth tmpi ziwei_star2)
            tmpj (eval (eval (car (last item))))
            info2 (plist-put info2 (car item) tmpj)
            )
      (aset result2 (1- tmpj) (append (aref result2 (1- tmpj)) (list (car item))))
      )
    ;; 设置中央区块文字
    (setq block (list (format "姓名：%s" (plist-get fate-current-user 'name))
                      (format "公历：%d年%d月%d日 %d:%d:%d"
                              (nth 2 (plist-get fate-current-user 'birthday-fix))
                              (nth 0 (plist-get fate-current-user 'birthday-fix))
                              (nth 1 (plist-get fate-current-user 'birthday-fix))
                              (nth 3 (plist-get fate-current-user 'birthday-fix))
                              (nth 4 (plist-get fate-current-user 'birthday-fix))
                              (nth 5 (plist-get fate-current-user 'birthday-fix))
                              )
                      (format "阴历：%d年%d月%d日%s时" xyear xmonth xday (aref chinese-fate-calendar-terrestrial-branch (mod (1- xhour) 12)))
                      (format "八字：%s %s %s %s"
                              (calendar-fate-chinese-sexagesimal-name (nth 1 (fate-solar-info (plist-get fate-current-user 'birthday-fix))))
                              (calendar-fate-chinese-sexagesimal-name (nth 2 (fate-solar-info (plist-get fate-current-user 'birthday-fix))))
                              (calendar-fate-chinese-sexagesimal-name (nth 3 (fate-solar-info (plist-get fate-current-user 'birthday-fix))))
                              (calendar-fate-chinese-sexagesimal-name (nth 4 (fate-solar-info (plist-get fate-current-user 'birthday-fix))))
                              )
                      ))
    (when (> smode 1)
      (setq block (append block (list (format "流年：%d年" syear)))))
    (setq txt (cons block txt))
    ;; 设置12宫文字
    (dotimes (tmpi 12)
      (setq item (aref result tmpi)
            block (make-vector rows ""))
      ;; 设置星耀及四化
      (dotimes (tmpj (min (/ cols 2) (length item)))
        (setq starcode (nth tmpj item)
              starinfo (assoc starcode ziwei_star)
              starname (nth 1 starinfo)
              starcolor (nth 2 starinfo)
              )
        (add-face-text-property 0 2 (list :foreground starcolor) nil starname)
        (aset block 0 (concat (substring starname 0 1) (aref block 0)))
        (aset block 1 (concat (substring starname 1 2) (aref block 1)))
        ;; 标记四化
        (dotimes (tmpk (min smode 3 (- rows rows-min -1)))
          (let* ((sub4change (memq starcode (nth (mod (1- (nth tmpk hua)) 10) ziwei_4change_tbl)))
                 (subidx (- 4 (length sub4change)))
                 (subtxt (when (< subidx 4) (nth subidx ziwei_4change)))
                 )
            (if (< subidx 4)
                (progn
                  (add-face-text-property 0 1 (list :foreground (nth 0 (nth tmpk ziwei_color)) :background (nth 1 (nth tmpk ziwei_color))) nil subtxt)
                  )
              (setq subtxt (make-string (string-width (nth 0 ziwei_4change)) ? ))
              )
            (aset block (+ 2 tmpk) (concat subtxt (aref block (+ 2 tmpk))))
            )
          )
        )
      ;; 12宫干支
      (setq tmpj (if (< tmpi 2) (+ gz3 tmpi 10) (+ gz3 tmpi -2))
            tmpk (calendar-fate-chinese-sexagesimal-name tmpj))
      (aset block (- rows 2) (substring tmpk 0 1))
      (aset block (1- rows) (substring tmpk 1 2))
      ;; 12宫名字
      (dotimes (tmpj (min smode 3 (max 2 (- rows rows-min))))
        (let* ((gongname (nth (mod (- (nth tmpj gong) tmpi 1) 12) ziwei_12gong))
               (gongpos (floor (/ (- cols (string-width gongname)) 2)))
               (suffix "")
               )
          ;; 宫名
          (if (and (= 0 tmpj) (= tmpi (1- xshen)))
              (setq gongname (concat (car (last ziwei_12gong)) (substring gongname 0 1)))
            )
          (add-face-text-property 0 2 (list :foreground (nth 0 (nth tmpj ziwei_color)) :background (nth 1 (nth tmpj ziwei_color))) nil gongname)
          ;; 宫名后情报
          (cond ((= tmpj 0)
                 (setq suffix (format "%d-" (+ xju (* 10 (mod (* direction (- xming tmpi 1)) 12)))))
                 )
                ((= tmpj 1)
                 (setq suffix (format "%d" (+ syear (mod (- tmpi firstmonth -1) 12))))
                 )
                ((= tmpj 2)
                 (setq suffix (concat (nth (mod (- tmpi firstmonth -1) 12) lunar_monthname)
                              (aref chinese-fate-calendar-celestial-stem (mod (+ sgz3 (mod (- tmpi firstmonth -1) 12) -1) 10))))
                 ))
          ;; 组装宫名及宫名后情报
          (setq gongpos (- gongpos (string-width (aref block (- rows tmpj 1)))))
          (if (>= gongpos (string-width suffix))
              (setq gongname (concat gongname suffix (make-string (- gongpos (string-width suffix)) ? )))
            (setq gongname (concat gongname (make-string gongpos ? )))
            )
          (aset block (- rows tmpj 1) (concat gongname (aref block (- rows tmpj 1))))
          )
        )
      ;; 流年星耀
      (setq item (aref result2 tmpi))
      (dotimes (tmpj (min (if (> smode 2) 999 0) (length item) (max 2 (- rows rows-min))))
        (setq starcode (nth tmpj item)
              starinfo (assoc starcode ziwei_star2)
              starname (nth 1 starinfo)
              starcolor (nth 2 starinfo)
              )
        (add-face-text-property 0 2 (list :foreground starcolor) nil starname)
        (aset block (- rows tmpj 1) (concat
                                     starname
                                     (make-string (- cols (string-width starname) (string-width (aref block (- rows tmpj 1)))) ? )
                                     (aref block (- rows tmpj 1))))
        )
      ;; 补足宽度
      (dotimes (tmpj rows)
        (aset block tmpj (concat (make-string (- cols (string-width (aref block tmpj))) ? ) (aref block tmpj)))
        )
      (setq txt (append txt (list (append block nil))))
      )
    (clear-text-properties ziwei_12gong)
    (ziwei_draw txt)
    )
  )

;; 画紫微盘
;; I: blocks
;; Memo: blocks[I]是各个区块的文字
;;       blocks[i](1<=i<=12)表示12宫的文字列数组
;;       blocks[i][j]表示对应宫位j行的文字
;;       要求blocks[i](1<=i<=12)中包含的文字列行数一致，并且所有文字列宽度一致
;;       blocks[0]表示中央区块的文字列数组
;;       要求length(blocks[0])<=2*length(blocks[1])+1，string-width(blocks[0][0])<=2*string-width(blocks[1][0])+1
(defun ziwei_draw (blocks)
  (let* ((cols (string-width (nth 0 (nth 1 blocks))))
         (rows (length (nth 1 blocks)))
         (marktype nil)
         (mark06 (if marktype "┃" "|"))
         (mark39 (if marktype "━" "-"))
         (mark03 (if marktype "┗" "+"))
         (mark09 (if marktype "┛" "+"))
         (mark36 (if marktype "┏" "+"))
         (mark69 (if marktype "┓" "+"))
         (mark036 (if marktype "┣" "+"))
         (mark369 (if marktype "┳" "+"))
         (mark069 (if marktype "┫" "+"))
         (mark039 (if marktype "┻" "+"))
         (mark0369 (if marktype "╋" "+"))
         (markwidth (string-width mark06))
         (dmytxt (make-string (+ (* 2 cols) markwidth) ? ))
         (hline (make-string (/ cols markwidth) (string-to-char mark39)))
         (logbuffer (get-buffer-create "fate-ziwei"))
         tmpi
         )
    (set-buffer logbuffer)
    (erase-buffer)
    (insert mark36 hline mark369 hline mark369 hline mark369 hline mark69 "\n")
    ;; 输出第一行
    (dotimes (tmpi rows)
      (insert mark06 (nth tmpi (nth 6 blocks))
              mark06 (nth tmpi (nth 7 blocks))
              mark06 (nth tmpi (nth 8 blocks))
              mark06 (nth tmpi (nth 9 blocks))
              mark06 "\n"
              )
      )
    (insert mark036 hline mark0369 hline mark039 hline mark0369 hline mark069 "\n")
    ;; 输出第二行
    (dotimes (tmpi rows)
      (insert mark06 (nth tmpi (nth 5 blocks)) mark06)
      (if (<= (length (nth 0 blocks)) tmpi)
          (insert dmytxt)
        (insert (nth tmpi (nth 0 blocks)) (make-string (- (string-width dmytxt) (string-width (nth tmpi (nth 0 blocks)))) ? ))
        )
      (insert mark06 (nth tmpi (nth 10 blocks)) mark06 "\n")
      )
    (insert mark036 hline mark069)
    ;; 第二行和第三行的分割线位置，需要输出中央区块的内容
    (if (<= (length (nth 0 blocks)) rows)
        (insert dmytxt)
      (insert (nth rows (nth 0 blocks)) (make-string (- (string-width dmytxt) (string-width (nth rows (nth 0 blocks)))) ? ))
      )
    (insert mark036 hline mark069 "\n")
    ;; 输出第三行
    (dotimes (tmpi rows)
      (insert mark06 (nth tmpi (nth 4 blocks)) mark06)
      (if (<= (length (nth 0 blocks)) (+ tmpi rows 1))
          (insert dmytxt)
        (insert (nth (+ tmpi rows 1) (nth 0 blocks)) (make-string (- (string-width dmytxt) (string-width (nth (+ tmpi rows 1) (nth 0 blocks)))) ? ))
        )
      (insert mark06 (nth tmpi (nth 11 blocks)) mark06 "\n")
      )
    (insert mark036 hline mark0369 hline mark369 hline mark0369 hline mark069 "\n")
    ;; 输出第四行
    (dotimes (tmpi rows)
      (insert mark06 (nth tmpi (nth 3 blocks))
              mark06 (nth tmpi (nth 2 blocks))
              mark06 (nth tmpi (nth 1 blocks))
              mark06 (nth tmpi (nth 12 blocks))
              mark06 "\n"
              )
      )
    (insert mark03 hline mark039 hline mark039 hline mark039 hline mark09 "\n")
    (switch-to-buffer logbuffer)
    )
  )
;; 紫微排盘
(defun ziwei-show ()
  (interactive)
  (let* ((mode-flag (y-or-n-p "查看命盘？"))
         mode-flag2 specific-year
         )
    (if mode-flag
        (ziwei_calculate)
      (progn
        (setq mode-flag2 (y-or-n-p "查看流年盘？")
              specific-year (string-to-number (read-from-minibuffer "年份：")))
        (if mode-flag2
            (ziwei_calculate 3 specific-year)
          (ziwei_calculate 2 specific-year)
          )
        )
      )
    )
  )

(fate_load_user_list)

;; 太公奇门排盘
(defun qimen-taigong ()
  (interactive)
  (let* ((adate (calendar-fate-chinese-datetime))               ;; 绝对时间
         (gdate (calendar-fate-gregorian-from-absolute adate))  ;; 公历时间
         (sdate (calendar-fate-chinese-from-absolute adate))    ;; 阳历时间
         (ldate (fate-lunar-info gdate))                        ;; 阴历时间
         (lmonth (floor (nth 2 ldate)))                         ;; 阴历月
         (lday (nth 3 ldate))                                   ;; 阴历日
         (dz-month (1+ (mod (1- (nth 2 sdate)) 12)))            ;; 阳历月地支
         (tg-day (1+ (mod (1- (nth 3 sdate)) 10)))              ;; 阳历日天干
         (tg-hour (1+ (mod (1- (nth 4 sdate)) 10)))             ;; 阳历时天干
         (shour (1+ (mod (1- (nth 4 sdate)) 12)))               ;; 时辰
         (sminute (+ 1
                     (floor (/ (nth 4 gdate) 10))
                     (if (= (mod (nth 3 gdate) 2) 0) 6 0)))     ;; 刻（1时辰12刻）
         (pos-min (mod (+ lmonth lday shour -2) 8))             ;; 命宫位置（坎宫起1，顺数）
         (pos-yun (mod (+ pos-min sminute -1) 8))               ;; 运宫位置（坎宫起1，顺数）
         (dz-yun (cond ((= pos-yun 0) 11)
                       ((= pos-yun 1)  1)
                       ((= pos-yun 2)  2)
                       ((= pos-yun 3)  4)
                       ((= pos-yun 4)  5)
                       ((= pos-yun 5)  7)
                       ((= pos-yun 6)  8)
                       ((= pos-yun 7) 10)
                       ))                                       ;; 运宫地支位置
         (pos-8shen (cond ((= tg-day 2) 5)
                          ((= tg-day 3) 2)
                          ((= tg-day 4) 7)
                          ((= tg-day 5) 1)
                          ((= tg-day 6) 6)
                          ((= tg-day 7) 3)
                          ((= tg-day 8) 4)
                          ((= tg-day 10) 0)
                          (t (cond ((= (floor (/ dz-month 3)) 1) 4)
                                   ((= (floor (/ dz-month 3)) 2) 6)
                                   ((= (floor (/ dz-month 3)) 2) 0)
                                   (t 2)
                                   ))
                          ))                                    ;; 青龙（八神）所在位置（坎宫起1，顺数）
         (pos-9xing (cond ((= tg-hour 1) 1)
                          ((= tg-hour 2) 9)
                          ((= tg-hour 3) 8)
                          ((= tg-hour 4) 7)
                          ((= tg-hour 5) 1)
                          ((= tg-hour 6) 2)
                          ((= tg-hour 7) 3)
                          ((= tg-hour 8) 4)
                          ((= tg-hour 9) 5)
                          ((= tg-hour 10) 6)
                          ))                                    ;; 一白（九星）所在后天宫位
         (txt-9xing '("一白" "二黑" "三碧"
                      "四绿" "五黄" "六白"
                      "七赤" "八白" "九紫"))
         (txt-8shen '("青龙" "朱雀" "勾陈" "腾蛇"
                      "白虎" "玄武" "地运" "天运"))
         (txt-8men '("合门" "天门" "阴门" "凶门"
                     "官门" "恶门" "阳门" "地门"))
         (txt '())
         (info '((5  27 4 3)  ;; 巽宫地支、卦、后天方位、八门
                 (8  0  2 5)  ;; 坤宫地支、卦、后天方位、八门
                 (11 63 6 7)  ;; 乾宫地支、卦、后天方位、八门
                 (2  9  8 1)  ;; 艮宫地支、卦、后天方位、八门
                 (4  36 3 2)  ;; 震宫地支、卦、后天方位、八门
                 (7  45 9 4)  ;; 离宫地支、卦、后天方位、八门
                 (10 54 7 6)  ;; 兑宫地支、卦、后天方位、八门
                 (1  18 1 0)  ;; 坎宫地支、卦、后天方位、八门
                 ))
         (color-min "red")
         (color-yun '("white" "blue"))
         cur-dz cur-64gua cur-dir cur-8men
         cur-9xing cur-8shen cur-yun
         block tmpi tmpj tmpk
         )
    ;; 设置各宫文字
    (dotimes (tmpi (length info))
      (setq cur-dz (nth 0 (nth tmpi info))
            cur-64gua (nth 1 (nth tmpi info))
            cur-dir (nth 2 (nth tmpi info))
            cur-8men (nth 3 (nth tmpi info))
            )
      (setq cur-9xing (mod (+ cur-dir pos-9xing -2) 9)
            cur-8shen (mod (+ pos-8shen -1 cur-8men) 8)
            cur-yun (mod (- dz-yun cur-dz) 12))
      (setq cur-64gua (car (nth cur-64gua gua-64))
            tmpj (nth cur-yun ziwei_12gong)
            tmpk (nth (mod (1- cur-yun) 12) ziwei_12gong)
            )
      (when (= (mod (1+ cur-8men) 8) pos-min)
        (add-face-text-property 0 (length cur-64gua) (list :foreground color-min) nil cur-64gua)
        )
      (when (memq cur-yun '(0 4 8))
        (add-face-text-property 0 (length tmpj) (list :foreground (car color-yun) :background (cadr color-yun)) nil tmpj)
        )
      (when (memq (mod (1- cur-yun) 12) '(0 4 8))
        (add-face-text-property 0 (length tmpk) (list :foreground (car color-yun) :background (cadr color-yun)) nil tmpk)
        )
      (if (< tmpi 4)
          (if (< tmpi 2)
              (setq block (list (nth cur-9xing txt-9xing)
                                (nth cur-8shen txt-8shen)
                                cur-64gua
                                (nth cur-8men txt-8men)
                                (format "%s%s  %s%s"
                                        (aref chinese-fate-calendar-terrestrial-branch (1- cur-dz))
                                        tmpj
                                        (aref chinese-fate-calendar-terrestrial-branch cur-dz)
                                        tmpk
                                        )
                                ))
            (setq block (list (nth cur-9xing txt-9xing)
                              (nth cur-8shen txt-8shen)
                              cur-64gua
                              (nth cur-8men txt-8men)
                              (format "%s%s  %s%s"
                                      (aref chinese-fate-calendar-terrestrial-branch cur-dz)
                                      tmpk
                                      (aref chinese-fate-calendar-terrestrial-branch (1- cur-dz))
                                      tmpj
                                      )
                              ))
            )
        (setq block (list (nth cur-9xing txt-9xing)
                          (nth cur-8shen txt-8shen)
                          cur-64gua
                          (nth cur-8men txt-8men)
                          (format "%s%s"
                                  (aref chinese-fate-calendar-terrestrial-branch (1- cur-dz))
                                  tmpj
                                  )
                          ))
        )
      (setq txt (cons block txt))
      )
    (clear-text-properties ziwei_12gong)
    ;; 设置中宫文字
    (setq cur-dir 5
          cur-9xing (mod (+ cur-dir pos-9xing -2) 9)
          block (list (nth cur-9xing txt-9xing)
                      (format "%s年%04d%02d%02d" (calendar-fate-chinese-sexagesimal-name (nth 1 sdate)) (nth 2 gdate) (nth 0 gdate) (nth 1 gdate))
                      (format "%s月%02d:%02d:%02d" (calendar-fate-chinese-sexagesimal-name (nth 2 sdate)) (nth 3 gdate) (nth 4 gdate) (nth 5 gdate))
                      (format "%s日  %s%s月" (calendar-fate-chinese-sexagesimal-name (nth 3 sdate))
                              (if (> (nth 2 ldate) lmonth) (nth 13 lunar_monthname) "  ")
                              (nth (1- lmonth) lunar_monthname))
                      (format "%s时  %s%s日" (calendar-fate-chinese-sexagesimal-name (nth 4 sdate))
                              (cond ((= lday 30) "三")
                                    ((> lday 20) "廿")
                                    ((= lday 20) "二")
                                    ((> lday 10) "十")
                                    (t "  ")
                                    )
                              (cond ((= 1 (mod lday 10)) "一")
                                    ((= 2 (mod lday 10)) "二")
                                    ((= 3 (mod lday 10)) "三")
                                    ((= 4 (mod lday 10)) "四")
                                    ((= 5 (mod lday 10)) "五")
                                    ((= 6 (mod lday 10)) "六")
                                    ((= 7 (mod lday 10)) "七")
                                    ((= 8 (mod lday 10)) "八")
                                    ((= 9 (mod lday 10)) "九")
                                    ((= 0 (mod lday 10)) "十")
                                    )
                              )
                      ))
    ;; 调整九宫文字顺序（洛书顺序）
    (setq txt (list (nth 0 txt)
                    (nth 6 txt)
                    (nth 3 txt)
                    (nth 7 txt)
                    block
                    (nth 5 txt)
                    (nth 1 txt)
                    (nth 4 txt)
                    (nth 2 txt)))
    ;; 画九宫
    (qimen_draw txt)
    ))
(defun qimen_draw (blocks)
  (let* ((rows-min 5)
         (cols-min 14)
         (rows (max rows-min 5))
         (cols (max cols-min 14))
         (block-rows (length (nth 0 blocks)))
         (row-start (floor (/ (- rows block-rows) 2)))
         (row-end (+ row-start block-rows))
         (marktype nil)
         (mark06 (if marktype "┃" "|"))
         (mark39 (if marktype "━" "-"))
         (mark03 (if marktype "┗" "+"))
         (mark09 (if marktype "┛" "+"))
         (mark36 (if marktype "┏" "+"))
         (mark69 (if marktype "┓" "+"))
         (mark036 (if marktype "┣" "+"))
         (mark369 (if marktype "┳" "+"))
         (mark069 (if marktype "┫" "+"))
         (mark039 (if marktype "┻" "+"))
         (mark0369 (if marktype "╋" "+"))
         (markwidth (string-width mark06))
         (hline (make-string (/ cols markwidth) (string-to-char mark39)))
         (eline (make-string (/ cols markwidth) ? ))
         (logbuffer (get-buffer-create "fate-qimen"))
         tmpi tmpj tmpk tmptxt
         )
    (set-buffer logbuffer)
    (erase-buffer)
    (insert mark36 hline mark369 hline mark369 hline mark69 "\n")
    ;; 输出第一行
    (dotimes (tmpi rows)
      (cond ((< tmpi row-start)
             (insert mark06 eline mark06 eline mark06 eline mark06 "\n")
             )
            ((< tmpi row-end)
             (insert mark06)
             (setq tmptxt (nth (- tmpi row-start) (nth 3 blocks))
                   tmpk (- cols (string-width tmptxt))
                   tmpj (floor (/ tmpk 2))
                   tmpk (- tmpk tmpj)
                   tmptxt (format "%s%s%s" (make-string tmpj ? ) tmptxt (make-string tmpk ? ))
                   )
             (insert tmptxt)
             (insert mark06)
             (setq tmptxt (nth (- tmpi row-start) (nth 8 blocks))
                   tmpk (- cols (string-width tmptxt))
                   tmpj (floor (/ tmpk 2))
                   tmpk (- tmpk tmpj)
                   tmptxt (format "%s%s%s" (make-string tmpj ? ) tmptxt (make-string tmpk ? ))
                   )
             (insert tmptxt)
             (insert mark06)
             (setq tmptxt (nth (- tmpi row-start) (nth 1 blocks))
                   tmpk (- cols (string-width tmptxt))
                   tmpj (floor (/ tmpk 2))
                   tmpk (- tmpk tmpj)
                   tmptxt (format "%s%s%s" (make-string tmpj ? ) tmptxt (make-string tmpk ? ))
                   )
             (insert tmptxt)
             (insert mark06 "\n")
             )
            (t
             (insert mark06 eline mark06 eline mark06 eline mark06 "\n")
             )
            )
      )
    (insert mark36 hline mark369 hline mark369 hline mark69 "\n")
    ;; 输出第二行
    (dotimes (tmpi rows)
      (cond ((< tmpi row-start)
             (insert mark06 eline mark06 eline mark06 eline mark06 "\n")
             )
            ((< tmpi row-end)
             (insert mark06)
             (setq tmptxt (nth (- tmpi row-start) (nth 2 blocks))
                   tmpk (- cols (string-width tmptxt))
                   tmpj (floor (/ tmpk 2))
                   tmpk (- tmpk tmpj)
                   tmptxt (format "%s%s%s" (make-string tmpj ? ) tmptxt (make-string tmpk ? ))
                   )
             (insert tmptxt)
             (insert mark06)
             (setq tmptxt (nth (- tmpi row-start) (nth 4 blocks))
                   tmpk (- cols (string-width tmptxt))
                   tmpj (floor (/ tmpk 2))
                   tmpk (- tmpk tmpj)
                   tmptxt (format "%s%s%s" (make-string tmpj ? ) tmptxt (make-string tmpk ? ))
                   )
             (insert tmptxt)
             (insert mark06)
             (setq tmptxt (nth (- tmpi row-start) (nth 6 blocks))
                   tmpk (- cols (string-width tmptxt))
                   tmpj (floor (/ tmpk 2))
                   tmpk (- tmpk tmpj)
                   tmptxt (format "%s%s%s" (make-string tmpj ? ) tmptxt (make-string tmpk ? ))
                   )
             (insert tmptxt)
             (insert mark06 "\n")
             )
            (t
             (insert mark06 eline mark06 eline mark06 eline mark06 "\n")
             )
            )
      )
    (insert mark36 hline mark369 hline mark369 hline mark69 "\n")
    ;; 输出第三行
    (dotimes (tmpi rows)
      (cond ((< tmpi row-start)
             (insert mark06 eline mark06 eline mark06 eline mark06 "\n")
             )
            ((< tmpi row-end)
             (insert mark06)
             (setq tmptxt (nth (- tmpi row-start) (nth 7 blocks))
                   tmpk (- cols (string-width tmptxt))
                   tmpj (floor (/ tmpk 2))
                   tmpk (- tmpk tmpj)
                   tmptxt (format "%s%s%s" (make-string tmpj ? ) tmptxt (make-string tmpk ? ))
                   )
             (insert tmptxt)
             (insert mark06)
             (setq tmptxt (nth (- tmpi row-start) (nth 0 blocks))
                   tmpk (- cols (string-width tmptxt))
                   tmpj (floor (/ tmpk 2))
                   tmpk (- tmpk tmpj)
                   tmptxt (format "%s%s%s" (make-string tmpj ? ) tmptxt (make-string tmpk ? ))
                   )
             (insert tmptxt)
             (insert mark06)
             (setq tmptxt (nth (- tmpi row-start) (nth 5 blocks))
                   tmpk (- cols (string-width tmptxt))
                   tmpj (floor (/ tmpk 2))
                   tmpk (- tmpk tmpj)
                   tmptxt (format "%s%s%s" (make-string tmpj ? ) tmptxt (make-string tmpk ? ))
                   )
             (insert tmptxt)
             (insert mark06 "\n")
             )
            (t
             (insert mark06 eline mark06 eline mark06 eline mark06 "\n")
             )
            )
      )
    (insert mark36 hline mark369 hline mark369 hline mark69 "\n")
    (switch-to-buffer logbuffer)
    ))

(provide 'fate-essential)
;;; fate-essential.el ends here
