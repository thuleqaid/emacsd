;;; fate-liuyao.el --- liuyao paipan util

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
;; Usage 1
;; a. liuyao-quick
;; Usage 2
;; a. liuyao-empty
;; b. liuyao-qigua
;; Usage 3
;; a. liuyao-empty
;; b. input memo
;; c. liuyao-qigua
;; Usage 4
;; a. liuyao-empty
;; b. input memo
;; c. liuyao-qigua-with-time
;; Usage 5
;; a. liuyao-empty
;; b. specify ganzhi datetime and gua code
;; c. liuyao-update

;;; Code:
(require 'cl)
(require 'subr-x)
;; 六亲
(defconst liuqin-name '("兄" "孙" "财" "官" "父"))
;; 六神
(defconst liushen-name '("青龙" "朱雀" "勾陈" "滕蛇" "白虎" "玄武"))
;; 当前排盘的导出文件名
(defvar liuyao-current-filename nil)
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
;; 新建空挂单
(defun liuyao-empty ()
  (interactive)
  (let* ((logbuffer (get-buffer-create "fate-liuyao"))
         poslist
        )
    (setq liuyao-current-filename nil)
    (set-buffer logbuffer)
    (erase-buffer)
    ;; 输出第一部分（起卦特征）
    (insert "#######################################[\n")
    (insert "---------------------------------------]\n")
    ;; 输出第二部分（起卦信息）
    (insert "占事：\n")
    (insert "背景：\n")
    (insert "问卦人：\n")
    (insert "自占/代占：\n")
    (insert "起卦方法：\n")
    (insert "----------------------------------------\n")
    ;; 输出第三部分（装卦）
    (insert "----------------------------------------\n")
    ;; 输出第四部分（断卦）
    (insert "断卦：\n\n")
    (insert "反馈：\n\n")
    (insert "总结：\n\n")
    (insert "========================================\n")
    ;; 输出第五部分（自动提示）
    (insert "----------------------------------------\n")
    ;; 输出第六部分（易经）
    (insert "----------------------------------------\n")

    ;; 输出第一部分
    (liuyao-qigua-empty)
    ;; 根据第一部分生成第三，五，六部分
    )
  )
;; 新建空挂单并快速起卦
(defun liuyao-quick ()
  (interactive)
  (let* ((logbuffer (get-buffer-create "fate-liuyao"))
         poslist
         )
    (setq liuyao-current-filename nil)
    (set-buffer logbuffer)
    (erase-buffer)
    ;; 输出第一部分（起卦特征）
    (insert "#######################################[\n")
    (insert "---------------------------------------]\n")
    ;; 输出第二部分（起卦信息）
    (insert "占事：\n")
    (insert "背景：\n")
    (insert "问卦人：\n")
    (insert "自占/代占：\n")
    (insert "起卦方法：\n")
    (insert "----------------------------------------\n")
    ;; 输出第三部分（装卦）
    (insert "----------------------------------------\n")
    ;; 输出第四部分（断卦）
    (insert "断卦：\n\n")
    (insert "反馈：\n\n")
    (insert "总结：\n\n")
    (insert "========================================\n")
    ;; 输出第五部分（自动提示）
    (insert "----------------------------------------\n")
    ;; 输出第六部分（易经）
    (insert "----------------------------------------\n")

    ;; 输出第一部分
    (liuyao-qigua)
    ;; 根据第一部分生成第三，五，六部分
    (liuyao-update)
    (goto-char (point-min))
    (switch-to-buffer logbuffer)
    )
  )
;; 使用当前时间起卦
(defun liuyao-qigua ()
  (interactive)
  (when (get-buffer "fate-liuyao")
    (let* ((logbuffer (get-buffer "fate-liuyao"))
           (adate (- (calendar-fate-chinese-datetime)
                     (/ (- (/ (car (current-time-zone)) 3600) 8)
                        24.0))) ;; 卜卦时间（绝对时间 GMT+8）
           (gdate (calendar-fate-gregorian-from-absolute adate)) ;; 卜卦时间（公历）
           (sdate (calendar-fate-chinese-from-absolute adate))   ;; 卜卦时间（阳历）
           (poslist (fate-parts "fate-liuyao"))
           (stopwatch (liuyao-stopwatch))
           )
      (set-buffer logbuffer)
      ;; 输出第一部分
      (goto-char (nth 1 poslist))
      ;; 输出公历时间
      (insert (format "%d-%d-%d %d:%d"
                      (nth 2 gdate) (nth 0 gdate) (nth 1 gdate)
                      (nth 3 gdate) (nth 4 gdate)
                      ))
      ;; 输出阳历时间
      (insert (format "  %d,%d,%d,%d" (nth 1 sdate)
                      (nth 2 sdate)
                      (nth 3 sdate)
                      (nth 4 sdate)))
      ;; 输出卦码（上卦、下卦、动爻）
      (insert (format "  %s\n" stopwatch))
      ;; 删除第一部分
      (goto-char (car poslist))
      (forward-line 1)
      (delete-region (point) (nth 1 poslist))
      (liuyao-update)
      (goto-char (point-min))
      (switch-to-buffer logbuffer)
      )
    )
  )
;; 指定时间起卦
(defun liuyao-qigua-with-time ()
  (interactive)
  (when (get-buffer "fate-liuyao")
    (let* ((logbuffer (get-buffer "fate-liuyao"))
           (poslist (fate-parts "fate-liuyao"))
           (xtime (safe-date-to-time (fate-read-date t)))                         ;; 用户输入时间
           ytime adate
           stopwatch gdate sdate
           )
      ;; 读取日期（输入必须包括时间）
      (while (and (= (nth 0 xtime) 0) (= (nth 1 xtime) 0))
        (setq xtime (safe-date-to-time (fate-read-date t))
              )
        )
      (setq ytime (decode-time xtime)
            adate (calendar-fate-chinese-datetime (list (nth 4 ytime)
                                                        (nth 3 ytime)
                                                        (nth 5 ytime)
                                                        (nth 2 ytime)
                                                        (nth 1 ytime)
                                                        (nth 0 ytime)
                                                        )))
      (setq gdate (calendar-fate-gregorian-from-absolute adate) ;; 卜卦时间（公历）
            sdate (calendar-fate-chinese-from-absolute adate)   ;; 卜卦时间（阳历）
            stopwatch (liuyao-stopwatch)
            )
      (set-buffer logbuffer)
      ;; 输出第一部分
      (goto-char (nth 1 poslist))
      ;; 输出公历时间
      (insert (format "%d-%d-%d %d:%d"
                      (nth 2 gdate) (nth 0 gdate) (nth 1 gdate)
                      (nth 3 gdate) (nth 4 gdate)
                      ))
      ;; 输出阳历时间
      (insert (format "  %d,%d,%d,%d" (nth 1 sdate)
                      (nth 2 sdate)
                      (nth 3 sdate)
                      (nth 4 sdate)))
      ;; 输出卦码（上卦、下卦、动爻）
      (insert (format "  %s\n" stopwatch))
      ;; 删除第一部分
      (goto-char (car poslist))
      (forward-line 1)
      (delete-region (point) (nth 1 poslist))
      (liuyao-update)
      (goto-char (point-min))
      (switch-to-buffer logbuffer)
      )
    )
  )
;; 重新排盘
(defun liuyao-update ()
  (interactive)
  (when (get-buffer "fate-liuyao")
    (let* ((logbuffer (get-buffer "fate-liuyao"))
           (poslist (fate-parts "fate-liuyao"))
           (info (liuyao-parse-part1))
           (gzdt (car info))
           (gua0 (nth 1 info))
           (gua1 (nth 2 info))
           (gua2 (nth 3 info))
           )
      (set-buffer logbuffer)
      ;; 删除第六部分
      (goto-char (nth 5 poslist))
      (beginning-of-line 2)
      (delete-region (point) (nth 6 poslist))
      ;; 删除第五部分
      (goto-char (nth 4 poslist))
      (beginning-of-line 2)
      (delete-region (point) (nth 5 poslist))
      ;; 删除第三部分
      (goto-char (nth 2 poslist))
      (beginning-of-line 2)
      (delete-region (point) (nth 3 poslist))

      (setq poslist (fate-parts "fate-liuyao"))
      ;; 输出第六部分
      (goto-char (nth 5 poslist))
      (beginning-of-line 2)
      (liuyao-yi (list gua0 gua1 gua2))
      ;; 输出第五部分
      (goto-char (nth 4 poslist))
      (beginning-of-line 2)
      (liuyao-calc-part5 gua0 gua1 gua2 gzdt)
      ;; 输出第三部分
      (goto-char (nth 2 poslist))
      (beginning-of-line 2)
      (liuyao-zhuanggua gua0 gua1 gua2 gzdt)
      )
    )
  )
(defun liuyao-qigua-empty ()
  (when (get-buffer "fate-liuyao")
    (let* ((logbuffer (get-buffer "fate-liuyao"))
           (adate (- (calendar-fate-chinese-datetime)
                     (/ (- (/ (car (current-time-zone)) 3600) 8)
                        24.0))) ;; 卜卦时间（绝对时间 GMT+8）
           (gdate (calendar-fate-gregorian-from-absolute adate)) ;; 卜卦时间（公历）
           (sdate (calendar-fate-chinese-from-absolute adate))   ;; 卜卦时间（阳历）
           (poslist (fate-parts "fate-liuyao"))
           )
      (set-buffer logbuffer)
      ;; 输出第一部分
      (goto-char (nth 1 poslist))
      ;; 输出公历时间
      (insert (format "%d-%d-%d %d:%d"
                      (nth 2 gdate) (nth 0 gdate) (nth 1 gdate)
                      (nth 3 gdate) (nth 4 gdate)
                      ))
      ;; 输出阳历时间
      (insert (format "  %d,%d,%d,%d" (nth 1 sdate)
                      (nth 2 sdate)
                      (nth 3 sdate)
                      (nth 4 sdate)))
      ;; 输出卦码（上卦、下卦、动爻）
      (insert (format "  GC11\n"))
      ;; 删除第一部分
      (goto-char (car poslist))
      (forward-line 1)
      (delete-region (point) (nth 1 poslist))
      (liuyao-update)
      (goto-char (point-min))
      (switch-to-buffer logbuffer)
      )
    )
  )
(defun liuyao-zhuanggua (gua0 gua1 gua2 sdate)
  (let* ((basewx (nth (floor (/ gua0 9)) gua-8-wuxing))         ;; 宫卦五行
         (lq0 (liuqin gua0 gua0))                               ;; 宫卦六亲
         (lq1 (liuqin gua1 gua0))                               ;; 本卦六亲
         (lq2 (liuqin gua2 gua0))                               ;; 变卦六亲
         (mask0 (nth 9 (nth gua1 gua-64)))                      ;; 伏神mask
         (mask2 (logxor gua1 gua2))                             ;; 变爻mask
         (pos0 6)                                               ;; 宫卦世爻位置
         (pos1 (nth 7 (nth gua1 gua-64)))                       ;; 本卦世爻位置
         (pos2 (nth 7 (nth gua2 gua-64)))                       ;; 变卦世爻位置
         (kong1 (cond ((> (nth 2 sdate) 60) 0)                  ;; 日空1
                      ((> (nth 2 sdate) 50) 1)
                      ((> (nth 2 sdate) 40) 51)
                      ((> (nth 2 sdate) 30) 41)
                      ((> (nth 2 sdate) 20) 31)
                      ((> (nth 2 sdate) 10) 21)
                      ((> (nth 2 sdate) 0) 11)
                      (t 0)
                      ))
         (kong2 (if (> kong1 0) (1+ kong1) 0))                  ;; 日空2
         (ls (cond ((> (nth 2 sdate) 60) 0)                     ;; 初爻六神
                   ((< (nth 2 sdate) 1) 0)
                   ((< (mod (1- (nth 2 sdate)) 10) 2) 1)
                   ((< (mod (1- (nth 2 sdate)) 10) 4) 2)
                   ((< (mod (1- (nth 2 sdate)) 10) 5) 3)
                   ((< (mod (1- (nth 2 sdate)) 10) 6) 4)
                   ((< (mod (1- (nth 2 sdate)) 10) 8) 5)
                   (t 6)
                   ))
         tmpi tmpj tmpk                                         ;; 临时变量
         txt0 txt1 txt2                                         ;; 临时变量
         )
    ;; 输出阳历时间
    (setq tmpi (cond ((> (nth 0 sdate) 60) (format "  %s年" (aref chinese-fate-calendar-terrestrial-branch (% (1- (nth 0 sdate)) 12))))
                     ((< (nth 0 sdate) 1) "      ")
                     (t (format "%s年" (calendar-fate-chinese-sexagesimal-name (nth 0 sdate)))))
          tmpj (cond ((> (nth 1 sdate) 60) (format "  %s月" (aref chinese-fate-calendar-terrestrial-branch (% (1- (nth 1 sdate)) 12))))
                     ((< (nth 1 sdate) 1) "      ")
                     (t (format "%s月" (calendar-fate-chinese-sexagesimal-name (nth 1 sdate)))))
          tmpk (cond ((> (nth 2 sdate) 60) (format "  %s日" (aref chinese-fate-calendar-terrestrial-branch (% (1- (nth 2 sdate)) 12))))
                     ((< (nth 2 sdate) 1) "      ")
                     (t (format "%s日" (calendar-fate-chinese-sexagesimal-name (nth 2 sdate)))))
          txt0 (cond ((> (nth 3 sdate) 60) (format "  %s时" (aref chinese-fate-calendar-terrestrial-branch (% (1- (nth 3 sdate)) 12))))
                     ((< (nth 3 sdate) 1) "      ")
                     (t (format "%s时" (calendar-fate-chinese-sexagesimal-name (nth 3 sdate)))))
          txt1 (if (> kong1 0) (format "空%s%s"
                                       (aref chinese-fate-calendar-terrestrial-branch (% (1- kong1) 12))
                                       (aref chinese-fate-calendar-terrestrial-branch (% (1- kong2) 12)))
                 ""
                 )
          txt2 (string-trim-right (format "%s %s %s %s %s" tmpi tmpj tmpk txt0 txt1))
          )
    (insert (format "%s\n" txt2))
    ;; 输出日期六亲
    (setq tmpi (if (> (nth 0 sdate) 0) (nth (mod (- (nth (mod (nth 0 sdate) (length dizhi-wuxing)) dizhi-wuxing) basewx) (length liuqin-name)) liuqin-name) "  ")
          tmpj (if (> (nth 1 sdate) 0) (nth (mod (- (nth (mod (nth 1 sdate) (length dizhi-wuxing)) dizhi-wuxing) basewx) (length liuqin-name)) liuqin-name) "  ")
          tmpk (if (> (nth 2 sdate) 0) (nth (mod (- (nth (mod (nth 2 sdate) (length dizhi-wuxing)) dizhi-wuxing) basewx) (length liuqin-name)) liuqin-name) "  ")
          txt0 (if (> (nth 3 sdate) 0) (nth (mod (- (nth (mod (nth 3 sdate) (length dizhi-wuxing)) dizhi-wuxing) basewx) (length liuqin-name)) liuqin-name) "  ")
          txt1 (format "  %s     %s     %s     %s" tmpi tmpj tmpk txt0)
          txt2 (string-trim-right txt1)
          )
    (insert (format "%s\n" txt2))
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
      (if (> ls 0)
          (insert (format "%s  " (nth (mod (- ls tmpi -4) (length liushen-name)) liushen-name)))
        (insert "      ")
        )
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
    )
  )
(defun liuyao-yi (gualist)
  (let (tmpi tmpj tmpk pos0 pos1 pos)
    ;; 输出爻辞
    (setq tmpk (list))
    (dolist (tmpi gualist)
      (add-to-list 'tmpk tmpi t)
      )
    (setq pos '())
    (with-current-buffer (find-file-noselect fate-heluo-file)
      (dotimes (tmpi (length tmpk))
        (goto-char (point-min))
        (setq pos (cons "" pos))
        (dotimes (tmpj 7)
          (setq pos0 (search-forward-regexp (format "^%d-%d#" (nth tmpi tmpk) tmpj))
                pos1 (buffer-substring-no-properties pos0 (line-end-position))
                pos (cons (car (split-string pos1 ":")) pos)
                )
          )
        )
      )
    (setq pos (cdr (reverse pos)))
    (dotimes (tmpi (length pos))
      (insert (nth tmpi pos) "\n")
      )
    )
  )
(defun liuyao-parse-part1 ()
  (save-excursion
    (let* ((logbuffer (get-buffer-create "fate-liuyao"))
           pos tmptxt tmppart
           gzdt gua1 gua2
           )
      (set-buffer logbuffer)
      (goto-char (point-min))
      (forward-line 1)
      (setq pos (line-end-position)
            tmptxt (buffer-substring-no-properties (point) pos)
            tmppart (split-string tmptxt "  ")
            gzdt (mapcar 'string-to-number (split-string (nth 1 tmppart) ","))
            tmptxt (nth 2 tmppart)
            pos (substring-no-properties tmptxt 2)
            )
      (cond ((string-prefix-p "GC" tmptxt) ;; 卦码
             (progn
               (setq gua1 (+ (- 8 (string-to-number (substring-no-properties pos 0 1)))
                             (* 8 (- 8 (string-to-number (substring-no-properties pos 1 2))))
                             )
                     pos (mapcar (lambda (x)
                                   (lsh 1 (- 6 (- x 48))))
                                 (coerce (substring-no-properties pos 2) 'list))
                     gua2 (logxor gua1 (reduce '+ pos))
                     )
               )
             )
            ((string-prefix-p "CT" tmptxt) ;; 硬币背面
             (progn
               (setq pos (mapcar (lambda (x) (- x 48)) (coerce (substring-no-properties pos 0 6) 'list))
                     gua1 0
                     gua2 0)
               (dolist (x pos)
                 (cond ((= 0 x) (setq gua1 (+ (* gua1 2) 0)
                                      gua2 (+ (* gua2 2) 1)))
                       ((= 1 x) (setq gua1 (+ (* gua1 2) 1)
                                      gua2 (+ (* gua2 2) 1)))
                       ((= 2 x) (setq gua1 (+ (* gua1 2) 0)
                                      gua2 (+ (* gua2 2) 0)))
                       ((= 3 x) (setq gua1 (+ (* gua1 2) 1)
                                      gua2 (+ (* gua2 2) 0)))
                       ))
               )
             )
            ((string-prefix-p "SW" tmptxt) ;; 秒表
             (progn
               (setq pos (mapcar 'string-to-number (split-string pos ","))
                     gua1 0
                     gua2 0)
               (dotimes (x 6)
                 (setq tmptxt (round (/ (nth (1+ x) pos) (nth 0 pos)))
                       tmptxt (mod tmptxt 8))
                 (cond ((= 0 tmptxt) (setq gua1 (+ (* gua1 2) 0)
                                           gua2 (+ (* gua2 2) 1)))
                       ((= 1 tmptxt) (setq gua1 (+ (* gua1 2) 1)
                                           gua2 (+ (* gua2 2) 1)))
                       ((= 2 tmptxt) (setq gua1 (+ (* gua1 2) 1)
                                           gua2 (+ (* gua2 2) 1)))
                       ((= 3 tmptxt) (setq gua1 (+ (* gua1 2) 0)
                                           gua2 (+ (* gua2 2) 0)))
                       ((= 4 tmptxt) (setq gua1 (+ (* gua1 2) 1)
                                           gua2 (+ (* gua2 2) 1)))
                       ((= 5 tmptxt) (setq gua1 (+ (* gua1 2) 0)
                                           gua2 (+ (* gua2 2) 0)))
                       ((= 6 tmptxt) (setq gua1 (+ (* gua1 2) 0)
                                           gua2 (+ (* gua2 2) 0)))
                       ((= 7 tmptxt) (setq gua1 (+ (* gua1 2) 1)
                                           gua2 (+ (* gua2 2) 0)))
                       )
                 )
               )
             )
            )
      (list gzdt (nth 8 (nth gua1 gua-64)) gua1 gua2)
      )
    )
  )
(defun liuyao-calc-part5-one (result-mask yaogz-self able-self yaogz-other able-other label-other)
  (let* ((wuxing-self (nth (mod yaogz-self (length dizhi-wuxing)) dizhi-wuxing))
         (wuxing-other (nth (mod yaogz-other (length dizhi-wuxing)) dizhi-wuxing))
         (wuxing-rel (mod (- wuxing-other wuxing-self) 5))
         (result '())
         )
    ;; 生克
    (cond ((= wuxing-rel 0)  ;; 同五行
           )
          ((= wuxing-rel 1)  ;; self生other
           (when (and (> (logand result-mask #x80) 0) able-self)
             (setq result (cons (format "生%s" label-other) result))
             )
           )
          ((= wuxing-rel 2)  ;; self克other
           (when (and (> (logand result-mask #x40) 0) able-self)
             (setq result (cons (format "克%s" label-other) result))
             )
           )
          ((= wuxing-rel 3)  ;; self被other克
           (when (and (> (logand result-mask #x80) 0) able-other)
             (setq result (cons (format "被%s克" label-other) result))
             )
           )
          ((= wuxing-rel 4)  ;; self被other生
           (when (and (> (logand result-mask #x40) 0) able-other)
             (setq result (cons (format "被%s生" label-other) result))
             )
           )
          )
    ;; 冲
    (when (> (logand result-mask #x20) 0)
      (when (= (mod (- yaogz-self yaogz-other) (length dizhi-wuxing)) 6)
        (when able-self
          (setq result (cons (format "冲%s" label-other) result))
          )
        (when able-other
          (setq result (cons (format "被%s冲" label-other) result))
          )
        )
      )
    ;; 合
    (when (> (logand result-mask #x10) 0)
      (when (= (mod (+ yaogz-self yaogz-other) (length dizhi-wuxing)) 3)
        (when able-self
          (setq result (cons (format "合%s" label-other) result))
          )
        (when able-other
          (setq result (cons (format "被%s合" label-other) result))
          )
        )
      )
    ;; 生旺墓绝
    (when (and (> (logand result-mask #x0F) 0) able-other)
      (cond ((or (= wuxing-self 1) (= wuxing-self 4)) ;; tmpi爻为水、土
             (cond ((and (> (logand result-mask #x08) 0) (= (mod yaogz-other (length dizhi-wuxing)) 9))
                    (setq result (cons (format "长生于%s" label-other) result))
                    )
                   ((and (> (logand result-mask #x04) 0) (= (mod yaogz-other (length dizhi-wuxing)) 1))
                    (setq result (cons (format "帝旺于%s" label-other) result))
                    )
                   ((and (> (logand result-mask #x02) 0) (= (mod yaogz-other (length dizhi-wuxing)) 5))
                    (setq result (cons (format "墓于%s" label-other) result))
                    )
                   ((and (> (logand result-mask #x01) 0) (= (mod yaogz-other (length dizhi-wuxing)) 6))
                    (setq result (cons (format "绝于%s" label-other) result))
                    )
                   )
             )
            ((= wuxing-self 2) ;; tmpi爻为木
             (cond ((and (> (logand result-mask #x08) 0) (= (mod yaogz-other (length dizhi-wuxing)) 0))
                    (setq result (cons (format "长生于%s" label-other) result))
                    )
                   ((and (> (logand result-mask #x04) 0) (= (mod yaogz-other (length dizhi-wuxing)) 4))
                    (setq result (cons (format "帝旺于%s" label-other) result))
                    )
                   ((and (> (logand result-mask #x02) 0) (= (mod yaogz-other (length dizhi-wuxing)) 8))
                    (setq result (cons (format "墓于%s" label-other) result))
                    )
                   ((and (> (logand result-mask #x01) 0) (= (mod yaogz-other (length dizhi-wuxing)) 9))
                    (setq result (cons (format "绝于%s" label-other) result))
                    )
                   )
             )
            ((= wuxing-self 3) ;; tmpi爻为火
             (cond ((and (> (logand result-mask #x08) 0) (= (mod yaogz-other (length dizhi-wuxing)) 3))
                    (setq result (cons (format "长生于%s" label-other) result))
                    )
                   ((and (> (logand result-mask #x04) 0) (= (mod yaogz-other (length dizhi-wuxing)) 7))
                    (setq result (cons (format "帝旺于%s" label-other) result))
                    )
                   ((and (> (logand result-mask #x02) 0) (= (mod yaogz-other (length dizhi-wuxing)) 11))
                    (setq result (cons (format "墓于%s" label-other) result))
                    )
                   ((and (> (logand result-mask #x01) 0) (= (mod yaogz-other (length dizhi-wuxing)) 0))
                    (setq result (cons (format "绝于%s" label-other) result))
                    )
                   )
             )
            ((= wuxing-self 5) ;; tmpi爻为金
             (cond ((and (> (logand result-mask #x08) 0) (= (mod yaogz-other (length dizhi-wuxing)) 6))
                    (setq result (cons (format "长生于%s" label-other) result))
                    )
                   ((and (> (logand result-mask #x04) 0) (= (mod yaogz-other (length dizhi-wuxing)) 10))
                    (setq result (cons (format "帝旺于%s" label-other) result))
                    )
                   ((and (> (logand result-mask #x02) 0) (= (mod yaogz-other (length dizhi-wuxing)) 2))
                    (setq result (cons (format "墓于%s" label-other) result))
                    )
                   ((and (> (logand result-mask #x01) 0) (= (mod yaogz-other (length dizhi-wuxing)) 3))
                    (setq result (cons (format "绝于%s" label-other) result))
                    )
                   )
             )
            )
      )
    (reverse result)
    )
  )
(defun liuyao-calc-part5 (gua0 gua1 gua2 sdate)
  (let* ((basewx (nth (floor (/ gua0 9)) gua-8-wuxing))         ;; 宫卦五行
         (lq0 (liuqin gua0 gua0))                               ;; 宫卦六亲
         (lq1 (liuqin gua1 gua0))                               ;; 本卦六亲
         (lq2 (liuqin gua2 gua0))                               ;; 变卦六亲
         (mask0 (nth 9 (nth gua1 gua-64)))                      ;; 伏神mask
         (mask2 (logxor gua1 gua2))                             ;; 变爻mask
         (dzm (nth 1 sdate))
         (dzd (nth 2 sdate))
         (kong1 (cond ((> (nth 2 sdate) 60) 0)                  ;; 日空1
                      ((> (nth 2 sdate) 50) 1)
                      ((> (nth 2 sdate) 40) 51)
                      ((> (nth 2 sdate) 30) 41)
                      ((> (nth 2 sdate) 20) 31)
                      ((> (nth 2 sdate) 10) 21)
                      ((> (nth 2 sdate) 0) 11)
                      (t 0)
                      ))
         (kong2 (if (> kong1 0) (1+ kong1) 0))                  ;; 日空2
         (pos0 '())
         tmpi tmpj tmpk                                         ;; 临时变量
         txt0 txt1 txt2                                         ;; 临时变量
         pos1 pos2
         )
    ;; 提取动爻（包含日月所冲静爻）
    ;; pos0 爻位置(0~5)列表
    (dotimes (tmpi 6)
      (setq pos1 (nth (1+ tmpi) (nth gua1 gua-64)))             ;; 当前爻干支
      ;; 判断动爻
      (if (> (logand mask2 (ash 1 tmpi)) 0)
          (setq tmpk 1)
        (setq tmpk 0)
        )
      ;; 判断日月冲
      (when (= tmpk 0)
        (when (> dzm 0)
          (when (= (mod (- dzm pos1) (length dizhi-wuxing)) 6)
            (setq tmpk 1))
          )
        (when (> dzd 0)
          (when (= (mod (- dzd pos1) (length dizhi-wuxing)) 6)
            (setq tmpk 1))
          )
        )
      (when (> tmpk 0)
        (setq pos0 (cons (- 5 tmpi) pos0))
        )
      )
    (dotimes (tmpi 6)
      (setq txt1 (nth (- 6 tmpi) (nth gua1 gua-64))                    ;; 当前爻干支
            )
      (insert (format "%d爻%s%s:" (1+ tmpi)
                      (aref chinese-fate-calendar-terrestrial-branch (% (1- txt1) (length dizhi-wuxing)))
                      (nth (nth (- 5 tmpi) lq1) liuqin-name)
                      ))
      ;; 空亡
      (when (or (= (mod (- txt1 kong1) (length dizhi-wuxing)) 0)
                (= (mod (- txt1 kong2) (length dizhi-wuxing)) 0))
        (insert " 空亡")
        )
      ;; 与其它爻的生克冲合、生旺墓绝
      (dotimes (tmpj 6)
        (when (/= tmpi tmpj)
          (setq pos1 (nth (- 6 tmpj) (nth gua1 gua-64))                    ;; 当前爻干支
                txt2 (liuyao-calc-part5-one #x3F txt1 (member tmpi pos0)
                                            pos1 (member tmpj pos0)
                                            (format "%d爻%s%s" (1+ tmpj)
                                                    (aref chinese-fate-calendar-terrestrial-branch (% (1- pos1) (length dizhi-wuxing)))
                                                    (nth (nth (- 5 tmpj) lq1) liuqin-name)
                                                    ))
                )
          (when (> (length txt2) 0)
            (insert " ")
            (insert (string-join txt2 " "))
            )
          )
        )
      ;; 与变爻的生克冲合、生旺墓绝
      (when (> (logand mask2 (ash 1 (- 5 tmpi))) 0)
        (setq pos1 (nth (- 6 tmpi) (nth gua2 gua-64))                    ;; 变爻干支
              txt2 (liuyao-calc-part5-one #xFF txt1 nil pos1 t "变爻")
              )
        (when (> (length txt2) 0)
          (insert " ")
          (insert (string-join txt2 " "))
          )
        )
      ;; 与伏神的生克冲合
      (when (> (logand mask0 (ash 1 (- 5 tmpi))) 0)
        (setq pos1 (nth (- 6 tmpi) (nth gua0 gua-64))                    ;; 伏神干支
              txt2 (liuyao-calc-part5-one #xF0 txt1 t pos1 t "伏神")
              )
        (when (> (length txt2) 0)
          (insert " ")
          (insert (string-join txt2 " "))
          )
        )
      ;; 日月对当前爻的生克冲合、生旺墓绝、旬空
      (setq pos1 (liuyao-calc-part5-one #xFF txt1 nil dzd t "日辰")
            pos2 (liuyao-calc-part5-one #xFF txt1 nil dzm t "月建")
            )
      (when (> (length pos2) 0)
        (insert " ")
        (insert (string-join pos2 " "))
        )
      (when (> (length pos1) 0)
        (insert " ")
        (insert (string-join pos1 " "))
        )
      ;; 日月对变爻的生克冲合、生旺墓绝
      (when (> (logand mask2 (ash 1 (- 5 tmpi))) 0)
        (setq txt2 (nth (- 6 tmpi) (nth gua2 gua-64))
              pos1 (liuyao-calc-part5-one #xFF txt2 nil dzd t "日辰")
              pos2 (liuyao-calc-part5-one #xFF txt2 nil dzm t "月建")
              )
        (when (or (= (mod (- txt2 kong1) (length dizhi-wuxing)) 0)
                  (= (mod (- txt2 kong2) (length dizhi-wuxing)) 0)
                  (> (+ (length pos1) (length pos2)) 0)
                  )
          (insert "\n   变爻:")
          (when (or (= (mod (- txt2 kong1) (length dizhi-wuxing)) 0)
                    (= (mod (- txt2 kong1) (length dizhi-wuxing)) 0))
            (insert " 空亡")
            )
          (when (> (length pos2) 0)
            (insert " ")
            (insert (string-join pos2 " "))
            )
          (when (> (length pos1) 0)
            (insert " ")
            (insert (string-join pos1 " "))
            )
          )
        )
      ;; 日月对伏神的生克冲合、生旺墓绝
      (when (> (logand mask0 (ash 1 (- 5 tmpi))) 0)
        (setq txt2 (nth (- 6 tmpi) (nth gua0 gua-64))
              pos1 (liuyao-calc-part5-one #xFF txt2 nil dzd t "日辰")
              pos2 (liuyao-calc-part5-one #xFF txt2 nil dzm t "月建")
              )
        (when (or (= (mod (- txt2 kong1) (length dizhi-wuxing)) 0)
                  (= (mod (- txt2 kong2) (length dizhi-wuxing)) 0)
                  (> (+ (length pos1) (length pos2)) 0)
                  )
          (insert "\n   伏神:")
          (when (or (= (mod (- txt2 kong1) (length dizhi-wuxing)) 0)
                    (= (mod (- txt2 kong2) (length dizhi-wuxing)) 0))
            (insert " 空亡")
            )
          (when (> (length pos2) 0)
            (insert " ")
            (insert (string-join pos2 " "))
            )
          (when (> (length pos1) 0)
            (insert " ")
            (insert (string-join pos1 " "))
            )
          )
        )
      (insert "\n")
      )
    )
  )
(defun liuyao-stopwatch ()
  (let (time0
        time1 time2 time3 time4 time5 time6
        diff1 diff2 diff3 diff4 diff5 diff6
        )
    (read-key "Press any key to start...")
    (setq time0 (current-time))
    (read-key "Press any key to tag 1...")
    (setq time1 (current-time))
    (read-key "Press any key to tag 2...")
    (setq time2 (current-time))
    (read-key "Press any key to tag 3...")
    (setq time3 (current-time))
    (read-key "Press any key to tag 4...")
    (setq time4 (current-time))
    (read-key "Press any key to tag 5...")
    (setq time5 (current-time))
    (read-key "Press any key to tag 6...")
    (setq time6 (current-time))
    (setq diff1 (+ (* (- (nth 0 time1) (nth 0 time0)) 65536) (- (nth 1 time1) (nth 1 time0)) (/ (- (nth 2 time1) (nth 2 time0)) 1000000.0))
          diff2 (+ (* (- (nth 0 time2) (nth 0 time0)) 65536) (- (nth 1 time2) (nth 1 time0)) (/ (- (nth 2 time2) (nth 2 time0)) 1000000.0))
          diff3 (+ (* (- (nth 0 time3) (nth 0 time0)) 65536) (- (nth 1 time3) (nth 1 time0)) (/ (- (nth 2 time3) (nth 2 time0)) 1000000.0))
          diff4 (+ (* (- (nth 0 time4) (nth 0 time0)) 65536) (- (nth 1 time4) (nth 1 time0)) (/ (- (nth 2 time4) (nth 2 time0)) 1000000.0))
          diff5 (+ (* (- (nth 0 time5) (nth 0 time0)) 65536) (- (nth 1 time5) (nth 1 time0)) (/ (- (nth 2 time5) (nth 2 time0)) 1000000.0))
          diff6 (+ (* (- (nth 0 time6) (nth 0 time0)) 65536) (- (nth 1 time6) (nth 1 time0)) (/ (- (nth 2 time6) (nth 2 time0)) 1000000.0))
          )
    (format "SW0.001,%.3f,%.3f,%.3f,%.3f,%.3f,%.3f" diff1 diff2 diff3 diff4 diff5 diff6)
    )
  )

(defun liuyao-export ( )
  (interactive)
  (let* ((poslist (fate-parts "fate-liuyao"))
         part1 part2 part4
         )
    (goto-char (nth 0 poslist))
    (forward-line 1)
    (setq part1 (buffer-substring-no-properties (point) (nth 1 poslist)))
    (goto-char (nth 1 poslist))
    (forward-line 1)
    (setq part2 (buffer-substring-no-properties (point) (nth 2 poslist)))
    (goto-char (nth 3 poslist))
    (forward-line 1)
    (setq part4 (buffer-substring-no-properties (point) (nth 4 poslist)))
    (fate-export-html liuyao-current-filename nil "_liuyao"
                      (fate-dumps-b64 (list 'part1 part1 'part2 part2 'part4 part4)))
    )
  )

(defun liuyao-import ()
  (interactive)
  (let ((info (fate-import-html))
        (logbuffer (get-buffer-create "fate-liuyao"))
        )
    (setq liuyao-current-filename (plist-get info 'filepath))
    (set-buffer logbuffer)
    (erase-buffer)
    ;; 输出第一部分（起卦特征）
    (insert "#######################################[\n")
    (insert (plist-get info 'part1))
    (insert "---------------------------------------]\n")
    ;; 输出第二部分（起卦信息）
    (insert (plist-get info 'part2))
    (insert "----------------------------------------\n")
    ;; 输出第三部分（装卦）
    (insert "----------------------------------------\n")
    ;; 输出第四部分（断卦）
    (insert (plist-get info 'part4))
    (insert "========================================\n")
    ;; 输出第五部分（自动提示）
    (insert "----------------------------------------\n")
    ;; 输出第六部分（易经）
    (insert "----------------------------------------\n")

    ;; 根据第一部分生成第三，五，六部分
    (liuyao-update)
    (goto-char (point-min))
    (switch-to-buffer logbuffer)
    )
  )

(add-to-list 'fate-buffer-list "fate-liuyao")
(if calendar-fate-show-chinese
    (easy-menu-add-item
     nil '("Fate")
     '("六爻"
       ["快速起卦" liuyao-quick t]
       "---"
       ["新卦单" liuyao-empty t]
       ["当前时间起卦" liuyao-qigua t]
       ["指定时间起卦" liuyao-qigua-with-time t]
       ["重新排卦" liuyao-update t]
       "---"
       ["保存" liuyao-export t]
       ["读取" liuyao-import t]
       )
     )
  (easy-menu-add-item
   nil '("Fate")
   '("LiuYao"
     ["Quick" liuyao-quick t]
     "---"
     ["Empty" liuyao-empty t]
     ["QiGua" liuyao-qigua t]
     ["QiGua With Time" liuyao-qigua-with-time t]
     ["Update" liuyao-update t]
     "---"
     ["Save" liuyao-export t]
     ["Load" liuyao-import t]
     )
   )
  )

(provide 'fate-liuyao)
;;; fate-liuyao.el ends here
