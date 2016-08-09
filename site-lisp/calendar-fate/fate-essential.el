;;; fate-essential.el --- calendar util for another chinese calender system

;; Copyright (C) 2016 Thule Qaid

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
;;; (heluo-show &optional year)
;;; (liuyao-show)
;;; (liuyao-new)

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
(defun fate-lunar-info (&optional date)
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
      (fate-add-dummy-user "Anonymous" t (date-to-time (current-time-string)) (car fate-timediff-pos-choice) 120)
    (setq fate-current-user (car fate-user-list))
    )
  (fate_user_list_choice)
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
    )
  )
;; 添加用户（详细字段设置）
(defun fate-add-dummy-user (name male birth0 city poslong)
  (let* ((birth1 (decode-time birth0))
         birth2
         delta1 delta2
         birthday birthday2
         x)
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
          x (heluo_basic_calc male birthday)
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
    ;; 河洛相关信息
    (setq fate-current-user (plist-put fate-current-user 'heluo-tianshu (nth 0 x)))
    (setq fate-current-user (plist-put fate-current-user 'heluo-dishu (nth 1 x)))
    (setq fate-current-user (plist-put fate-current-user 'heluo-gua1 (nth 2 x)))
    (setq fate-current-user (plist-put fate-current-user 'heluo-yao1 (nth 3 x)))
    (setq fate-current-user (plist-put fate-current-user 'heluo-gua2 (nth 4 x)))
    (setq fate-current-user (plist-put fate-current-user 'heluo-yao2 (nth 5 x)))
    (setq fate-current-user (plist-put fate-current-user 'heluo-year-min (nth 6 x)))
    (setq fate-current-user (plist-put fate-current-user 'heluo-year-mid (nth 7 x)))
    (setq fate-current-user (plist-put fate-current-user 'heluo-year-max (nth 8 x)))
    (setq fate-user-list (cons fate-current-user fate-user-list))
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
     ((annd (> (aref tmp2 0) 3) (< (aref tmp2 0) 6))
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
         (pat1 "* %d/%d/%d %d:%d:%d -- %d/%d/%d %d:%d:%d %s\n")
         (pat2 "  + %s\n")
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
    (dotimes (cnt 4)
      (heluo_msg_output (+ year-start cnt))
      )
    (org-overview)
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

(fate_load_user_list)

(provide 'fate-essential)
;;; fate-essential.el ends here
