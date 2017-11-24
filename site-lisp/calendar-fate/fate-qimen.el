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
                     (if (= (mod (nth 3 gdate) 2) 0) 6 0)))     ;; 刻地支（1时辰12刻）
         (tg-minute (1+ (mod (+ (* 2 tg-hour) -3 sminute) 10))) ;; 刻天干
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

(provide 'fate-qimen)
;;; fate-qimen.el ends here
