;;; fate-heluo.el --- calendar util for another chinese calender system

;; Copyright (C) 2017 Thule Qaid

;; Author: Thule Qaid <thuleqaid@163.com>

;; fate-heluo is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; fate-heluo is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with calendar-fate.  If not, see http://www.gnu.org/licenses.

;;; Commentary:
;;; (heluo-show &optional year)

;;; Code:
;;; 河洛
;; 河洛批言路径
(defconst fate-heluo-file (concat fate-root-dir "heluo.txt"))
(defconst fate-heluo-ext-file (concat fate-root-dir "heluo_ext.txt"))
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
  (let* ((year-min (plist-get fate-user-current 'heluo-year-min))
         (year-mid (plist-get fate-user-current 'heluo-year-mid))
         (year-max (plist-get fate-user-current 'heluo-year-max))
         (year-cur (if (< year year-min) year-min
                     (if (> year year-max) year-max
                       year)))
         (result (if (< year-cur year-mid)
                     (list (plist-get fate-user-current 'heluo-gua1)
                           (plist-get fate-user-current 'heluo-yao1)
                           year-min
                           (1- year-mid))
                   (list (plist-get fate-user-current 'heluo-gua2)
                         (plist-get fate-user-current 'heluo-yao2)
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
                  curyao (if (<= curyao 1) 6 (1- curyao))
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
        (setq curyao (if (<= curyao 1) 6 (1- curyao))
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
;; 取得指定卦爻的岁运
;; I: gua([0,63]) yao([0,6])
;; O: '(批言)
(defun heluo_ext_msg (gua yao)
  (if (and (integerp gua)
           (integerp yao)
           (>= gua 0)
           (<= gua 63)
           (>= yao 0)
           (<= yao 6))
      (with-current-buffer (find-file-noselect fate-heluo-ext-file)
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
(defun heluo_msg_output (year &optional lastguayao)
  (let* ((guayao (heluo_year_info year))
         (curgua (nth 8 guayao))
         (curyao (nth 9 guayao))
         (tmpd1 (fate-solar-item-info (nth 10 guayao) 1))
         (tmpd2 (fate-solar-item-info (1+ (nth 10 guayao)) 1))
         (pat1 "* %d -- %d %s\n")
         (pat2 "  + %s\n")
         (pat3 "*** %d/%d/%d %d:%d:%d -- %d/%d/%d %d:%d:%d %s\n")
         (pat4 "    + %s\n")
         (pat5 "    %s\n")
         tmpi tmpj curtext exttext tmptext
         )
    ;; 输出先天/后天运批言
    (unless (and lastguayao (= (nth 2 lastguayao) (nth 2 guayao)))
      (setq curtext (heluo_msg (nth 0 guayao) (nth 1 guayao)))
      (insert (format pat1 (nth 2 guayao) (nth 3 guayao)
                      (nth 0 curtext)
                      ))
      (setq tmpi 1)
      (while (< tmpi (length curtext))
        (insert (format pat2 (nth tmpi curtext)))
        (setq tmpi (1+ tmpi))
        )
      )
    ;; 输出大运批言
    (unless (and lastguayao (= (nth 6 lastguayao) (nth 6 guayao)))
      (setq curtext (heluo_msg (nth 4 guayao) (nth 5 guayao)))
      (insert "*")
      (insert (format pat1 (nth 6 guayao) (nth 7 guayao)
                      (nth 0 curtext)
                      ))
      (setq tmpi 1)
      (while (< tmpi (length curtext))
        (insert " ")
        (insert (format pat2 (nth tmpi curtext)))
        (setq tmpi (1+ tmpi))
        )
      )
    ;; 输出全年批言
    (setq curtext (heluo_msg curgua curyao))
    (insert (format pat3 (nth 2 tmpd1) (nth 0 tmpd1) (nth 1 tmpd1)
                    (nth 3 tmpd1) (nth 4 tmpd1) (nth 5 tmpd1)
                    (nth 2 tmpd2) (nth 0 tmpd2) (nth 1 tmpd2)
                    (nth 3 tmpd2) (nth 4 tmpd2) (nth 5 tmpd2)
                    (nth 0 curtext)
                    ))
    (setq exttext (heluo_ext_msg curgua curyao))
    (setq tmpi 1)
    (while (< tmpi (length exttext))
      (setq tmptext (format pat5 (nth tmpi exttext))
            tmpj (floor (/ (string-width tmptext) 80))
            tmpi (1+ tmpi)
        )
      (insert tmptext)
      ;; 自动换行
      (when (> tmpj 0)
        (beginning-of-line 0)
        (while (> tmpj 0)
          (move-to-column 80)
          (insert "\n")
          (setq tmpj (1- tmpj))
          )
        (beginning-of-line 2)
        )
      )
    (setq tmpi 1)
    (while (< tmpi (length curtext))
      (insert (format pat4 (nth tmpi curtext)))
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
      (insert (format pat3 (nth 2 tmpd1) (nth 0 tmpd1) (nth 1 tmpd1)
                      (nth 3 tmpd1) (nth 4 tmpd1) (nth 5 tmpd1)
                      (nth 2 tmpd2) (nth 0 tmpd2) (nth 1 tmpd2)
                      (nth 3 tmpd2) (nth 4 tmpd2) (nth 5 tmpd2)
                      (nth 0 curtext)
                      ))
      (setq tmpi 1)
      (while (< tmpi (length curtext))
        (insert " ")
        (insert (format pat4 (nth tmpi curtext)))
        (setq tmpi (1+ tmpi))
        )
      )
    guayao
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
         (lastguayao nil)
         )
    (set-buffer logbuffer)
    (erase-buffer)
    (org-mode)
    (insert (format "#+Title: %s\n" (plist-get fate-user-current 'name)))
    (dotimes (cnt 4)
      (setq lastguayao (heluo_msg_output (+ year-start cnt) lastguayao))
      )
    (org-content 3)
    (switch-to-buffer logbuffer)
    )
  )

(defun heluo-user-calculate ()
  (let ((birthday (plist-get fate-user-current 'birthday))
        heluo)
    ;; 河洛相关信息
    (setq heluo (heluo_basic_calc (plist-get fate-user-current 'male) birthday))
    (setq fate-user-current (plist-put fate-user-current 'heluo-tianshu (nth 0 heluo)))
    (setq fate-user-current (plist-put fate-user-current 'heluo-dishu (nth 1 heluo)))
    (setq fate-user-current (plist-put fate-user-current 'heluo-gua1 (nth 2 heluo)))
    (setq fate-user-current (plist-put fate-user-current 'heluo-yao1 (nth 3 heluo)))
    (setq fate-user-current (plist-put fate-user-current 'heluo-gua2 (nth 4 heluo)))
    (setq fate-user-current (plist-put fate-user-current 'heluo-yao2 (nth 5 heluo)))
    (setq fate-user-current (plist-put fate-user-current 'heluo-year-min (nth 6 heluo)))
    (setq fate-user-current (plist-put fate-user-current 'heluo-year-mid (nth 7 heluo)))
    (setq fate-user-current (plist-put fate-user-current 'heluo-year-max (nth 8 heluo)))
    )
  )

(defun heluo-export ( )
  (interactive)
  (fate-export-org)
  )

(add-to-list 'fate-user-calculate 'heluo-user-calculate)

(add-to-list 'fate-buffer-list "fate-heluo")
(add-to-list 'fate-buffer-list "heluo.txt")
(add-to-list 'fate-buffer-list "heluo_ext.txt")
(if calendar-fate-show-chinese
    (easy-menu-add-item
     nil '("Fate")
     '("河洛真数"
       ["流年" (heluo-show (nth 5 (decode-time))) t]
       ["指定年份" heluo-show t]
       "---"
       ["保存" heluo-export t]
       )
     )
  (easy-menu-add-item
   nil '("Fate")
   '("HeLuo"
     ["Current Year" (heluo-show (nth 5 (decode-time))) t]
     ["Specified Year" heluo-show t]
     "---"
     ["Save" heluo-export t]
     )
   )
  )

(provide 'fate-heluo)
;;; fate-heluo.el ends here
