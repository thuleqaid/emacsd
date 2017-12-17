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

;; 阴历12月文字
(defconst lunar_monthname '("正" "二" "三" "四" "五" "六"
                            "七" "八" "九" "十" "冬" "腊"
                            "月" "闰"
                            ))

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

(provide 'fate-essential)
;;; fate-essential.el ends here
