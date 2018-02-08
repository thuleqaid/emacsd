;;; fate-ziwei.el --- calendar util for another chinese calender system

;; Copyright (C) 2017 Thule Qaid

;; Author: Thule Qaid <thuleqaid@163.com>

;; fate-ziwei is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; fate-ziwei is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with calendar-fate.  If not, see http://www.gnu.org/licenses.

;;; Commentary:
;;; (ziwei-show)

;;; Code:
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

;; 计算紫微各星曜的位置
(defun ziwei_calculate (&optional mode year)
  (let* ((info '())                                                                      ;; 临时保存各星曜的位置
         (result (make-vector 12 '()))                                                   ;; 按宫位记录各星曜的代号
         (info2 '())                                                                     ;; 临时保存各星曜的位置（流年星曜）
         (result2 (make-vector 12 '()))                                                  ;; 按宫位记录各星曜的代号（流年星曜）
         (xgz (car (plist-get fate-user-current 'ziwei-birthday)))                       ;; 基本信息（生年干支）
         (xyear (nth 1 (plist-get fate-user-current 'ziwei-birthday)))                   ;; 基本信息（出生年份）
         (xmonth (floor (+ 0.9 (nth 2 (plist-get fate-user-current 'ziwei-birthday)))))  ;; 基本信息（出生月份）
         (xday (nth 3 (plist-get fate-user-current 'ziwei-birthday)))                    ;; 基本信息（出生日期）
         (xhour (nth 4 (plist-get fate-user-current 'ziwei-birthday)))                   ;; 基本信息（出生时辰）
         (xming (plist-get fate-user-current 'ziwei-ming))                               ;; 基本信息（命宫位置）
         (xshen (plist-get fate-user-current 'ziwei-shen))                               ;; 基本信息（身宫位置）
         (xju (plist-get fate-user-current 'ziwei-ju))                                   ;; 基本信息（五行局）
         (xnv (1+ (mod (+ xgz
                          (if (plist-get fate-user-current 'male) 1 2))
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
                               (+ (- (nth 2 gong) xmonth)
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
    (setq block (list (format "姓名：%s" (plist-get fate-user-current 'name))
                      (format "公历：%d年%d月%d日 %d:%d:%d"
                              (nth 2 (plist-get fate-user-current 'birthday-fix))
                              (nth 0 (plist-get fate-user-current 'birthday-fix))
                              (nth 1 (plist-get fate-user-current 'birthday-fix))
                              (nth 3 (plist-get fate-user-current 'birthday-fix))
                              (nth 4 (plist-get fate-user-current 'birthday-fix))
                              (nth 5 (plist-get fate-user-current 'birthday-fix))
                              )
                      (if (< (nth 2 (plist-get fate-user-current 'ziwei-birthday)) xmonth)
                          (format "阴历：%d年闰%d月%d日%s时" xyear (1- xmonth) xday (aref chinese-fate-calendar-terrestrial-branch (mod (1- xhour) 12)))
                        (format "阴历：%d年%d月%d日%s时" xyear xmonth xday (aref chinese-fate-calendar-terrestrial-branch (mod (1- xhour) 12)))
                        )
                      (format "八字：%s %s %s %s"
                              (calendar-fate-chinese-sexagesimal-name (nth 1 (fate-solar-info (plist-get fate-user-current 'birthday-fix))))
                              (calendar-fate-chinese-sexagesimal-name (nth 2 (fate-solar-info (plist-get fate-user-current 'birthday-fix))))
                              (calendar-fate-chinese-sexagesimal-name (nth 3 (fate-solar-info (plist-get fate-user-current 'birthday-fix))))
                              (calendar-fate-chinese-sexagesimal-name (nth 4 (fate-solar-info (plist-get fate-user-current 'birthday-fix))))
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
         (marktype calendar-fate-border-type)
         ;;0,3,6,9:clock position. e.g 0:up, 3:right
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

(defun ziwei-user-calculate ()
  (let ((birthday (plist-get fate-user-current 'birthday-fix))
        (ziwei-ju-tbl '((2 2 6 6 3 3 5 5 4 4 6 6)
                        (6 6 5 5 4 4 3 3 2 2 5 5)
                        (5 5 3 3 2 2 4 4 6 6 3 3)
                        (3 3 4 4 6 6 2 2 5 5 4 4)
                        (4 4 2 2 5 5 6 6 3 3 2 2)))
        lunar)
    ;; 紫微相关信息
    (setq lunar (fate-lunar-info birthday))
    ;; 阴历年干支，年，月，日，时
    (setq fate-user-current (plist-put fate-user-current 'ziwei-birthday
                                       (list (cadr lunar)
                                             (if (= (cadr lunar) (1+ (mod (- (nth 2 birthday) 1984) 60)))
                                                 (nth 2 birthday)
                                               (1- (nth 2 birthday)))
                                             (nth 2 lunar)
                                             (nth 3 lunar)
                                             (if (>= (nth 3 birthday) 23)
                                                 1
                                               (1+ (floor (/ (1+ (nth 3 birthday)) 2)))
                                               )
                                             )))
    ;; 命宫地支
    (setq fate-user-current (plist-put fate-user-current 'ziwei-ming
                                       (1+ (mod (-
                                                 (floor (+ 0.9 (nth 2 (plist-get fate-user-current 'ziwei-birthday))))
                                                 (floor (+ 0.9 (nth 4 (plist-get fate-user-current 'ziwei-birthday))))
                                                 -2)
                                                12))))
    ;; 身宫地支
    (setq fate-user-current (plist-put fate-user-current 'ziwei-shen
                                       (1+ (mod (+
                                                 (floor (+ 0.9 (nth 2 (plist-get fate-user-current 'ziwei-birthday))))
                                                 (floor (+ 0.9 (nth 4 (plist-get fate-user-current 'ziwei-birthday)))))
                                                12))))
    ;; 五行局
    (setq fate-user-current (plist-put fate-user-current 'ziwei-ju
                                       (nth (1- (plist-get fate-user-current 'ziwei-ming))
                                            (nth (mod (1- (nth 0 (plist-get fate-user-current 'ziwei-birthday))) 5)
                                                 ziwei-ju-tbl))))
    )
  )

(defun ziwei-export ( )
  (interactive)
  (fate-export-html "ziwei_")
  )

(add-to-list 'fate-user-calculate 'ziwei-user-calculate)

(add-to-list 'fate-buffer-list "fate-ziwei")
(if calendar-fate-show-chinese
    (easy-menu-add-item
     nil '("Fate")
     '("紫微斗数"
       ["命盘" (ziwei_calculate) t]
       ["大运盘" (ziwei_calculate 2 (nth 5 (decode-time))) t]
       ["流年盘" (ziwei_calculate 3 (nth 5 (decode-time))) t]
       ["指定年份" ziwei-show t]
       "---"
       ["保存" ziwei-export t]
       )
     )
  (easy-menu-add-item
   nil '("Fate")
   '("ZiWei"
     ["MingPan" (ziwei_calculate) t]
     ["DaYun" (ziwei_calculate 2 (nth 5 (decode-time))) t]
     ["LiuNian" (ziwei_calculate 3 (nth 5 (decode-time))) t]
     ["Specified Year" ziwei-show t]
     "---"
     ["Save" ziwei-export t]
     )
   )
  )

(provide 'fate-ziwei)
;;; fate-ziwei.el ends here
