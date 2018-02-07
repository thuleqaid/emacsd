;;; fate-qimen.el --- Qimen paipan util

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
;; Usage
;; qimen-taigong

;;; Code:
(defun qimen-taigong ()
  (interactive)
  (let ((adate (calendar-fate-chinese-datetime)))
    (qimen_clearbuffer)
    (qimen_taigong adate)
    )
  )
(defun qimen-normal ()
  (interactive)
  (let ((adate (calendar-fate-chinese-datetime)))
    (qimen_clearbuffer)
    (qimen_normal adate)
    )
  )

(defvar qimen_normal_qiju 2)   ;; 定局方法 1:拆补(1节气15日), 2:手表时(1节气5分钟)
(defvar qimen_normal_9xing t)  ;; 九星排法 t:转盘, nil:飞宫
(defvar qimen_normal_8men t)   ;; 八门排法 t:转盘, nil:飞宫

(defun qimen_normal_setting1 (flag_qiju)
  (setq qimen_normal_qiju flag_qiju)
  )
(defun qimen_normal_setting2 (flag_9xing flag_8men)
  (setq qimen_normal_9xing flag_9xing
        qimen_normal_8men flag_8men)
  )

;; 太公奇门排盘
(defun qimen_taigong (adate)
  (let* ((gdate (calendar-fate-gregorian-from-absolute adate))  ;; 公历时间
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
                     (if (= (mod (nth 3 gdate) 2) 0) 6 0)))     ;; 刻地支（1时辰12刻）
         (tg-minute (1+ (mod (+ (* 2 tg-hour) -1 sminute) 10))) ;; 刻天干
         (empty-year (1+ (mod (- (nth 1 sdate) (mod (1- (nth 1 sdate)) 10) -9) 12)))  ;; 年空
         (empty-month (1+ (mod (- (nth 2 sdate) (mod (1- (nth 2 sdate)) 10) -9) 12))) ;; 月空
         (empty-day (1+ (mod (- (nth 3 sdate) (mod (1- (nth 3 sdate)) 10) -9) 12)))   ;; 日空
         (empty-hour (1+ (mod (- (nth 4 sdate) (mod (1- (nth 4 sdate)) 10) -9) 12)))  ;; 时空
         (empty-minute (mod (- sminute -11 tg-minute) 12))                            ;; 刻空
         (empty-year2 (1+ empty-year))
         (empty-month2 (1+ empty-month))
         (empty-day2 (1+ empty-day))
         (empty-hour2 (1+ empty-hour))
         (empty-minute2 (1+ empty-minute))
         (empty-txt-year "年空")
         (empty-txt-month "月空")
         (empty-txt-day "日空")
         (empty-txt-hour "时空")
         (empty-txt-minute "刻空")
         (pos-min (mod (+ lmonth lday shour -2) 8))             ;; 命宫位置（坎宫起1，顺数）
         (pos-yun (mod (+ pos-min sminute -1) 8))               ;; 运宫位置（坎宫起1，顺数）
         (dz-yun (cond ((= pos-yun 0) (if (= (mod tg-hour 2) 0) 12 11))
                       ((= pos-yun 1)  1)
                       ((= pos-yun 2) (if (= (mod tg-hour 2) 0) 2 3))
                       ((= pos-yun 3)  4)
                       ((= pos-yun 4) (if (= (mod tg-hour 2) 0) 6 5))
                       ((= pos-yun 5)  7)
                       ((= pos-yun 6) (if (= (mod tg-hour 2) 0) 8 9))
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
                                   ((= (floor (/ dz-month 3)) 3) 0)
                                   (t 2)
                                   ))
                          ))                                    ;; 青龙（八神）所在位置（坎宫起1，顺数）
         (pos-9xing (cond ((= tg-minute 1) 1)                     ;; 按刻定九星
                          ((= tg-minute 2) 9)
                          ((= tg-minute 3) 8)
                          ((= tg-minute 4) 7)
                          ((= tg-minute 5) 1)
                          ((= tg-minute 6) 2)
                          ((= tg-minute 7) 3)
                          ((= tg-minute 8) 4)
                          ((= tg-minute 9) 5)
                          ((= tg-minute 10) 6)
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
    (add-face-text-property 0 (length empty-txt-year) (list :foreground "gray") nil empty-txt-year)
    (add-face-text-property 0 (length empty-txt-month) (list :foreground "gray") nil empty-txt-month)
    (add-face-text-property 0 (length empty-txt-day) (list :foreground "gray") nil empty-txt-day)
    (add-face-text-property 0 (length empty-txt-hour) (list :foreground "gray") nil empty-txt-hour)
    (add-face-text-property 0 (length empty-txt-minute) (list :foreground "gray") nil empty-txt-minute)
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
      (clear-text-properties cur-64gua)
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
              (setq block (list (format "%s %s %s"
                                        (if (or (= cur-dz empty-year) (= cur-dz empty-year2)) empty-txt-year "    ")
                                        (nth cur-9xing txt-9xing)
                                        (if (or (= (1+ cur-dz) empty-year) (= (1+ cur-dz) empty-year2)) empty-txt-year "    "))
                                (format "%s %s %s"
                                        (if (or (= cur-dz empty-month) (= cur-dz empty-month2)) empty-txt-month "    ")
                                        (nth cur-8shen txt-8shen)
                                        (if (or (= (1+ cur-dz) empty-month) (= (1+ cur-dz) empty-month2)) empty-txt-month "    "))
                                (format "%s  %s  %s"
                                        (if (or (= cur-dz empty-day) (= cur-dz empty-day2)) empty-txt-day "    ")
                                        cur-64gua
                                        (if (or (= (1+ cur-dz) empty-day) (= (1+ cur-dz) empty-day2)) empty-txt-day "    "))
                                (format "%s %s %s"
                                        (if (or (= cur-dz empty-hour) (= cur-dz empty-hour2)) empty-txt-hour "    ")
                                        (nth cur-8men txt-8men)
                                        (if (or (= (1+ cur-dz) empty-hour) (= (1+ cur-dz) empty-hour2)) empty-txt-hour "    "))
                                (format "%s      %s"
                                        (if (or (= cur-dz empty-minute) (= cur-dz empty-minute2)) empty-txt-minute "    ")
                                        (if (or (= (1+ cur-dz) empty-minute) (= (1+ cur-dz) empty-minute2)) empty-txt-minute "    "))
                                (format "%s%s  %s%s"
                                        (aref chinese-fate-calendar-terrestrial-branch (1- cur-dz))
                                        tmpj
                                        (aref chinese-fate-calendar-terrestrial-branch cur-dz)
                                        tmpk
                                        )
                                ))
            (setq block (list (format "%s %s %s"
                                      (if (or (= (1+ cur-dz) empty-year) (= (1+ cur-dz) empty-year2)) empty-txt-year "    ")
                                      (nth cur-9xing txt-9xing)
                                      (if (or (= cur-dz empty-year) (= cur-dz empty-year2)) empty-txt-year "    "))
                              (format "%s %s %s"
                                      (if (or (= (1+ cur-dz) empty-month) (= (1+ cur-dz) empty-month2)) empty-txt-month "    ")
                                      (nth cur-8shen txt-8shen)
                                      (if (or (= cur-dz empty-month) (= cur-dz empty-month2)) empty-txt-month "    "))
                              (format "%s  %s  %s"
                                      (if (or (= (1+ cur-dz) empty-day) (= (1+ cur-dz) empty-day2)) empty-txt-day "    ")
                                      cur-64gua
                                      (if (or (= cur-dz empty-day) (= cur-dz empty-day2)) empty-txt-day "    "))
                              (format "%s %s %s"
                                      (if (or (= (1+ cur-dz) empty-hour) (= (1+ cur-dz) empty-hour2)) empty-txt-hour "    ")
                                      (nth cur-8men txt-8men)
                                      (if (or (= cur-dz empty-hour) (= cur-dz empty-hour2)) empty-txt-hour "    "))
                              (format "%s      %s"
                                      (if (or (= (1+ cur-dz) empty-minute) (= (1+ cur-dz) empty-minute2)) empty-txt-minute "    ")
                                      (if (or (= cur-dz empty-minute) (= cur-dz empty-minute2)) empty-txt-minute "    "))
                              (format "%s%s  %s%s"
                                      (aref chinese-fate-calendar-terrestrial-branch cur-dz)
                                      tmpk
                                      (aref chinese-fate-calendar-terrestrial-branch (1- cur-dz))
                                      tmpj
                                      )
                              ))
            )
        (cond ((= tmpi 4)
               (setq block (list (format "%s %s     "
                                         (if (or (= cur-dz empty-year) (= cur-dz empty-year2)) empty-txt-year "    ")
                                         (nth cur-9xing txt-9xing)
                                         )
                                 (format "%s %s     "
                                         (if (or (= cur-dz empty-month) (= cur-dz empty-month2)) empty-txt-month "    ")
                                         (nth cur-8shen txt-8shen)
                                         )
                                 (format "%s  %s      "
                                         (if (or (= cur-dz empty-day) (= cur-dz empty-day2)) empty-txt-day "    ")
                                         cur-64gua
                                         )
                                 (format "%s %s     "
                                         (if (or (= cur-dz empty-hour) (= cur-dz empty-hour2)) empty-txt-hour "    ")
                                         (nth cur-8men txt-8men)
                                         )
                                 (format "%s          "
                                         (if (or (= cur-dz empty-minute) (= cur-dz empty-minute2)) empty-txt-minute "    ")
                                         )
                                 (format "%s%s        "
                                         (aref chinese-fate-calendar-terrestrial-branch (1- cur-dz))
                                         tmpj
                                         )
                                 ))
               )
              ((= tmpi 6)
               (setq block (list (format "     %s %s"
                                         (nth cur-9xing txt-9xing)
                                         (if (or (= cur-dz empty-year) (= cur-dz empty-year2)) empty-txt-year "    ")
                                         )
                                 (format "     %s %s"
                                         (nth cur-8shen txt-8shen)
                                         (if (or (= cur-dz empty-month) (= cur-dz empty-month2)) empty-txt-month "    ")
                                         )
                                 (format "      %s  %s"
                                         cur-64gua
                                         (if (or (= cur-dz empty-day) (= cur-dz empty-day2)) empty-txt-day "    ")
                                         )
                                 (format "     %s %s"
                                         (nth cur-8men txt-8men)
                                         (if (or (= cur-dz empty-hour) (= cur-dz empty-hour2)) empty-txt-hour "    ")
                                         )
                                 (format "          %s"
                                         (if (or (= cur-dz empty-minute) (= cur-dz empty-minute2)) empty-txt-minute "    ")
                                         )
                                 (format "        %s%s"
                                         (aref chinese-fate-calendar-terrestrial-branch (1- cur-dz))
                                         tmpj
                                         )
                                 ))
               )
              (t
                              (setq block (list (format "     %s %s"
                                         (nth cur-9xing txt-9xing)
                                         (if (or (= cur-dz empty-year) (= cur-dz empty-year2)) empty-txt-year "    ")
                                         )
                                 (format "     %s %s"
                                         (nth cur-8shen txt-8shen)
                                         (if (or (= cur-dz empty-month) (= cur-dz empty-month2)) empty-txt-month "    ")
                                         )
                                 (format "      %s  %s"
                                         cur-64gua
                                         (if (or (= cur-dz empty-day) (= cur-dz empty-day2)) empty-txt-day "    ")
                                         )
                                 (format "     %s %s"
                                         (nth cur-8men txt-8men)
                                         (if (or (= cur-dz empty-hour) (= cur-dz empty-hour2)) empty-txt-hour "    ")
                                         )
                                 (format "          %s"
                                         (if (or (= cur-dz empty-minute) (= cur-dz empty-minute2)) empty-txt-minute "    ")
                                         )
                                 (format "%s%s"
                                         (aref chinese-fate-calendar-terrestrial-branch (1- cur-dz))
                                         tmpj
                                         )
                                 ))
               )
              )
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
                      (format "%s日        " (calendar-fate-chinese-sexagesimal-name (nth 3 sdate)))
                      (format "%s时  %s%s月" (calendar-fate-chinese-sexagesimal-name (nth 4 sdate))
                              (if (> (nth 2 ldate) lmonth) (nth 13 lunar_monthname) "  ")
                              (nth (1- lmonth) lunar_monthname)
                              )
                      (format "%s%s刻  %s%s日"
                              (aref chinese-fate-calendar-celestial-stem (mod (1- tg-minute) 10))
                              (aref chinese-fate-calendar-terrestrial-branch (mod (1- sminute) 12))
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
;; 普通奇门排盘
(defun qimen_normal (adate)
  (let* ((flag-ju qimen_normal_qiju)
         (flag-9xing qimen_normal_9xing)
         (flag-8men qimen_normal_8men)
         (logbuffer (get-buffer-create "fate-qimen"))
         ;; 阴阳遁歌诀
         (yinyang-table '((8 5 2) (9 6 3) (1 7 4)
                          (3 9 6) (4 1 7) (5 2 8)
                          (4 1 7) (5 2 8) (6 3 9)
                          (9 3 6) (8 2 5) (7 1 4)
                          (2 5 8) (1 4 7) (9 3 6)
                          (7 1 4) (6 9 3) (5 8 2)
                          (6 9 3) (5 8 2) (4 7 1)
                          (1 7 4) (2 8 5) (3 9 6)))
         (gdate (calendar-fate-gregorian-from-absolute adate)) ;; 公历时间
         (sdate (calendar-fate-chinese-from-absolute adate)) ;; 阳历时间
         (org (+ (floor (/ (1- (nth 4 sdate)) 10)) 5)) ;; 甲的遁干
         (org2 (1+ (mod (1- (nth 4 sdate)) 10)))       ;; 时干
         (termidx (nth 5 sdate))                       ;; 上一个节气
         (ju-yinyang (if (or (>= termidx 22) (< termidx 10)) t nil)) ;; 阴遁/阳遁（定局方法1） t:阳遁, nil:阴遁
         (ju (nth (floor (/ (mod (1- (nth 3 sdate)) 15) 5)) (nth (1- termidx) yinyang-table))) ;; 局数（定局方法1）
         (info-tiangan '(5 6 7 8 9 10 4 3 2))
         ;; 转盘顺序
         (order11 '(0 7 2 3 8 1 6 5))    ;; 转盘的宫位顺序
         (order12 '(0 5 2 3 5 7 6 1 4)) ;; 宫位(1~9)到order11的index
         (txt-9xing1 '("天蓬" "天任" "天冲" "天辅"
                       "天英" "天芮" "天柱" "天心"))
         (txt-8men1 '("休门" "生门" "伤门" "杜门"
                      "景门" "死门" "惊门" "开门"))
         (txt-8shen1 '("值符" "腾蛇" "太阴" "六合"
                       "白虎" "玄武" "九地" "九天"))
         ;; 飞宫顺序
         (txt-9xing2 '("天蓬" "天芮" "天冲"
                       "天辅" "天禽" "天心"
                       "天柱" "天任" "天英"))
         (txt-8men2 '("休门" "死门" "伤门"
                      "杜门" "中门" "开门"
                      "惊门" "生门" "景门"))
         (txt-8shen2 '("值符" "腾蛇" "太阴"
                       "六合" "勾陈" "太常"
                       "朱雀" "九地" "九天"))
         (txt '())
         (dipan (make-vector 9 ""))
         (renpan (make-vector 9 ""))
         (tianpan (make-vector 9 ""))
         (shenpan (make-vector 9 ""))
         (shenpan_base (make-vector 9 ""))
         block basegong rengong tiangong
         zhifu zhishi
         tmpi tmpj tmpk
         )
    (clear-text-properties txt-9xing1)
    (clear-text-properties txt-9xing2)
    (clear-text-properties txt-8men1)
    (clear-text-properties txt-8men2)
    (clear-text-properties txt-8shen1)
    (clear-text-properties txt-8shen2)
    (add-face-text-property 0 (length (nth 0 txt-8shen1)) (list :foreground "red") nil (nth 0 txt-8shen1))
    (add-face-text-property 0 (length (nth 0 txt-8shen2)) (list :foreground "red") nil (nth 0 txt-8shen2))
    ;; 调整阴阳遁及局数
    (cond ((= flag-ju 2)
           (setq ju-yinyang (if (= (mod (nth 3 gdate) 2) 1) t nil)
                 termidx (1+ (mod (+ (if ju-yinyang 0 12) (floor (/ (nth 4 gdate) 5)) 21) 24))
                 ju (nth (floor (/ (mod (+ (* (nth 4 gdate) 60) (nth 5 gdate)) 300) 100)) (nth (1- termidx) yinyang-table))
                 )
           )
          )
    ;; 计算地盘天干
    (dotimes (tmpi 9)
      (aset dipan (mod (+ ju -1 (* (if ju-yinyang 1 -1) tmpi)) 9)
            (aref chinese-fate-calendar-celestial-stem (mod (1- (nth tmpi info-tiangan)) 10)))
      (when (= (nth tmpi info-tiangan) org)
        ;; 设置值符/值使所在原始宫位
        (setq basegong (1+ (mod (+ ju -1 (* (if ju-yinyang 1 -1) tmpi)) 9)))
        )
      (when (= (nth tmpi info-tiangan) org2)
        ;; 设置值符所在新宫位
        (setq tiangong (1+ (mod (+ ju -1 (* (if ju-yinyang 1 -1) tmpi)) 9)))
        )
      )
    (when (= org2 1) (setq tiangong basegong))
    (setq rengong (1+ (mod (+ basegong -2 (* (if ju-yinyang 1 -1) org2)) 9)))
    ;; 计算人盘
    (setq zhishi basegong)
    (if flag-8men
        (progn
          (when (= rengong 5) (setq rengong 2))
          (setq tmpj (nth (1- rengong) order12)   ;; 值使新宫位
                tmpk (nth (1- basegong) order12)) ;; 值使旧宫位
          (add-face-text-property 0 (length (nth tmpk txt-8men1)) (list :foreground "red") nil (nth tmpk txt-8men1))
          (dotimes (tmpi 8)
            (aset renpan (nth (mod (+ tmpi tmpj) 8) order11) (nth (mod (+ tmpi tmpk) 8) txt-8men1))
            )
          )
      (progn
        (setq tmpj (1- rengong)   ;; 值使新宫位
              tmpk (1- basegong)) ;; 值使旧宫位
        (add-face-text-property 0 (length (nth tmpk txt-8men2)) (list :foreground "red") nil (nth tmpk txt-8men2))
        (dotimes (tmpi 9)
          (aset renpan (mod (+ tmpi tmpj) 9) (nth (mod (+ tmpi tmpk) 9) txt-8men2))
          )
        ))
    ;; 计算天盘、神盘
    (setq zhifu basegong)
    (if flag-9xing
        (progn
          (when (= tiangong 5) (setq tiangong 2))
          (setq tmpj (nth (1- tiangong) order12)  ;; 值符新宫位
                tmpk (nth (1- basegong) order12)) ;; 值符旧宫位
          (unless (= 5 basegong)
            (add-face-text-property 0 (length (nth tmpk txt-9xing1)) (list :foreground "red") nil (nth tmpk txt-9xing1)))
          (dotimes (tmpi 8)
            (aset tianpan (nth (mod (+ tmpi tmpj) 8) order11) (format "%s %s" (nth (mod (+ tmpi tmpk) 8) txt-9xing1) (aref dipan (nth (mod (+ tmpi tmpk) 8) order11))))
            (aset shenpan (nth (mod (+ tmpi tmpj) 8) order11) (nth (mod (* (if ju-yinyang 1 -1) tmpi) 8) txt-8shen1))
            (aset shenpan_base (nth (mod (+ tmpi tmpk) 8) order11) (nth (mod (* (if ju-yinyang 1 -1) tmpi) 8) txt-8shen1))
            )
          )
      (progn
        (when (= tiangong 5) (setq tiangong 2))
        (setq tmpj (1- tiangong)  ;; 值符新宫位
              tmpk (1- basegong)) ;; 值符旧宫位
        (add-face-text-property 0 (length (nth tmpk txt-9xing2)) (list :foreground "red") nil (nth tmpk txt-9xing2))
        (dotimes (tmpi 9)
          (aset tianpan (mod (+ tmpi tmpj) 9) (format "%s %s" (nth (mod (+ tmpi tmpk) 9) txt-9xing2) (aref dipan (mod (+ tmpi tmpk) 9))))
          (aset shenpan (mod (+ tmpi tmpj) 9) (nth (mod (* (if ju-yinyang 1 -1) tmpi) 9) txt-8shen2))
          (aset shenpan_base (mod (+ tmpi tmpk) 9) (nth (mod (* (if ju-yinyang 1 -1) tmpi) 9) txt-8shen2))
          )
        ))
    ;; 设置各宫文字
    (dotimes (tmpi 9)
      (setq block (list (format "%s" (aref shenpan tmpi))
                        (format "%s" (aref tianpan tmpi))
                        (format "%s %s" (aref renpan tmpi) (aref dipan tmpi))
                        (format "%s" (aref shenpan_base tmpi))
                        ))
      (add-to-list 'txt block t)
      )
    (set-buffer logbuffer)
    (insert (format "起盘时间：%d/%02d/%02d %02d:%02d:%02d	%s遁%d局\n" (nth 2 gdate) (nth 0 gdate) (nth 1 gdate) (nth 3 gdate) (nth 4 gdate) (nth 5 gdate) (if ju-yinyang "阳" "阴") ju))
    (insert (format "起盘方法：%s\n"
                    (if flag-9xing
                        (if flag-8men "转盘" "星转门飞")
                      (if flag-8men "星飞门转" "飞盘"))))
    (insert (format "四柱：%s   %s   %s   %s\n"
                    (calendar-fate-chinese-sexagesimal-name (nth 1 sdate))
                    (calendar-fate-chinese-sexagesimal-name (nth 2 sdate))
                    (calendar-fate-chinese-sexagesimal-name (nth 3 sdate))
                    (calendar-fate-chinese-sexagesimal-name (nth 4 sdate))
                    ))
    (insert (format "旬空：%s%s   %s%s   %s%s   %s%s\n"
                    (aref chinese-fate-calendar-terrestrial-branch (mod (* 10 (floor (/ (+ (nth 1 sdate) 9) 10))) 12))
                    (aref chinese-fate-calendar-terrestrial-branch (mod (1+ (* 10 (floor (/ (+ (nth 1 sdate) 9) 10)))) 12))
                    (aref chinese-fate-calendar-terrestrial-branch (mod (* 10 (floor (/ (+ (nth 2 sdate) 9) 10))) 12))
                    (aref chinese-fate-calendar-terrestrial-branch (mod (1+ (* 10 (floor (/ (+ (nth 2 sdate) 9) 10)))) 12))
                    (aref chinese-fate-calendar-terrestrial-branch (mod (* 10 (floor (/ (+ (nth 3 sdate) 9) 10))) 12))
                    (aref chinese-fate-calendar-terrestrial-branch (mod (1+ (* 10 (floor (/ (+ (nth 3 sdate) 9) 10)))) 12))
                    (aref chinese-fate-calendar-terrestrial-branch (mod (* 10 (floor (/ (+ (nth 4 sdate) 9) 10))) 12))
                    (aref chinese-fate-calendar-terrestrial-branch (mod (1+ (* 10 (floor (/ (+ (nth 4 sdate) 9) 10)))) 12))
                    ))
    (insert (format "旬首：%s%s %s%s %s%s %s%s\n"
                    (calendar-fate-chinese-sexagesimal-name (- (* 10 (floor (/ (+ (nth 1 sdate) 9) 10))) 9))
                    (aref chinese-fate-calendar-celestial-stem (+ (floor (/ (1- (nth 1 sdate)) 10)) 4))
                    (calendar-fate-chinese-sexagesimal-name (- (* 10 (floor (/ (+ (nth 2 sdate) 9) 10))) 9))
                    (aref chinese-fate-calendar-celestial-stem (+ (floor (/ (1- (nth 2 sdate)) 10)) 4))
                    (calendar-fate-chinese-sexagesimal-name (- (* 10 (floor (/ (+ (nth 3 sdate) 9) 10))) 9))
                    (aref chinese-fate-calendar-celestial-stem (+ (floor (/ (1- (nth 3 sdate)) 10)) 4))
                    (calendar-fate-chinese-sexagesimal-name (- (* 10 (floor (/ (+ (nth 4 sdate) 9) 10))) 9))
                    (aref chinese-fate-calendar-celestial-stem (+ (floor (/ (1- (nth 4 sdate)) 10)) 4))
                    ))
    ;; 画九宫
    (qimen_draw txt)
    ))

(defun qimen_clearbuffer ()
  (let ((logbuffer (get-buffer-create "fate-qimen")))
    (set-buffer logbuffer)
    (erase-buffer)
    )
  )
(defun qimen_draw (blocks)
  (let* ((rows-min 6)
         (cols-min 14)
         (rows (max rows-min 6))
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

(add-to-list 'fate-buffer-list "fate-qimen")
(if calendar-fate-show-chinese
    (easy-menu-add-item
     nil '("Fate")
     '("奇门遁甲"
       ["起普通奇门盘" qimen-normal t]
       "---"
       ["起太公奇门盘" qimen-taigong t]
       "---"
       "普通奇门盘设定：起局"
       ["1节气15日" (qimen_normal_setting1 1) :style toggle :selected (= qimen_normal_qiju 1)]
       ["1节气5分钟" (qimen_normal_setting1 2) :style toggle :selected (= qimen_normal_qiju 2)]
       "普通奇门盘设定：排盘"
       ["飞盘" (qimen_normal_setting2 nil nil) :style toggle :selected (and (not qimen_normal_9xing) (not qimen_normal_8men))]
       ["转盘" (qimen_normal_setting2 t t) :style toggle :selected (and qimen_normal_9xing qimen_normal_8men)]
       ["星飞门转" (qimen_normal_setting2 nil t) :style toggle :selected (and (not qimen_normal_9xing) qimen_normal_8men)]
       )
     )
  (easy-menu-add-item
   nil '("Fate")
   '("QiMen"
     ["Normal" qimen-normal t]
     "---"
     ["TaiGong" qimen-taigong t]
     "---"
     "Normal Setting: Ju"
     ["15 Days" (qimen_normal_setting1 1) :style toggle :selected (= qimen_normal_qiju 1)]
     ["5 Minutes" (qimen_normal_setting1 2) :style toggle :selected (= qimen_normal_qiju 2)]
     "Normal Setting: Pan"
     ["Jump" (qimen_normal_setting2 nil nil) :style toggle :selected (and (not qimen_normal_9xing) (not qimen_normal_8men))]
     ["Rotate" (qimen_normal_setting2 t t) :style toggle :selected (and qimen_normal_9xing qimen_normal_8men)]
     ["Xing Jump & Men Rotate" (qimen_normal_setting2 nil t) :style toggle :selected (and (not qimen_normal_9xing) qimen_normal_8men)]
     )
   )
  )

(provide 'fate-qimen)
;;; fate-qimen.el ends here
