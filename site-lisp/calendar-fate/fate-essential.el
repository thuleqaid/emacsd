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

;;; Code:
;; 公历转阳历
;; I: '(month day year hour minute second)
;; O: '(yun year month day hour last-24term-index)
;; Memo: '(hour minute second)[I] is optional, default is '(0 0 0)
(defun fate-solar-info (&optional date)
  (calendar-fate-chinese-from-absolute
   (calendar-fate-chinese-datetime date)))

;; 公历转阴历
;; I: '(month day year hour minute second)
;; O: '(cycle year month day)
;; Memo: '(hour minute second)[I] won't be used for calculating, and can be omitted
;; Memo: month[O] will be a float number for a leap month
(defun fate-lunar-info (&optional date)
  (calendar-chinese-from-absolute
   (calendar-absolute-from-gregorian date)))

;; 当前用户信息
(defvar fate-current-user '())

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

;; 设置当前用户信息
(defun fate-set-current-user (name male birthday)
  (let ((x (heluo_basic_calc male birthday)))
    ;; 基本信息
    (setq fate-current-user (plist-put fate-current-user 'name name))
    (setq fate-current-user (plist-put fate-current-user 'male male))
    (setq fate-current-user (plist-put fate-current-user 'birthday birthday))
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
    )
  )

(fate-set-current-user "anonymous" t '(1 1 1984 0 0 0))

(provide 'fate-essential)
;;; fate-essential.el ends here
