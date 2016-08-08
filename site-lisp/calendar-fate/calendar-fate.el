;;; calendar-fate.el --- calendar util for another chinese calender system

;; Copyright (C) 2016 Thule Qaid

;; Author: Thule Qaid <thuleqaid@163.com>

;; calendar-fate is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; calendar-fate is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with calendar-fate.  If not, see http://www.gnu.org/licenses.

;;; Commentary:

;; Install:
;;  (require 'calendar-fate)
;;  (calendar-fate-chinese-character)
;;  (setq holiday-other-holidays '((holiday-chinese-terms)))

;;; Code:
(require 'calendar)
(require 'cal-china)

(defun calendar-fate-chinese-character ()
  (setq chinese-calendar-celestial-stem chinese-fate-calendar-celestial-stem)
  (setq chinese-calendar-terrestrial-branch chinese-fate-calendar-terrestrial-branch)
  (setq calendar-fate-chinese-24-term chinese-calendar-fate-chinese-24-term)
  (defun calendar-chinese-sexagesimal-name (n)
    (calendar-fate-chinese-sexagesimal-name n)
    )
  )

(defun calendar-fate-chinese-datetime (&optional date)
  (let* ((cur-datetime (decode-time))
         (g-date (or date
                     (list (nth 4 cur-datetime)
                           (nth 3 cur-datetime)
                           (nth 5 cur-datetime))))
         g-hour
         g-minute
         g-second
         )
    (if (> (length g-date) 3)
        (progn (setq g-hour (nth 3 g-date))
               (setq g-minute (or (nth 4 g-date) 0))
               (setq g-second (or (nth 5 g-date) 0))
               )
      (if (and (= (nth 2 g-date) (nth 5 cur-datetime))
               (= (car g-date) (nth 4 cur-datetime))
               (= (cadr g-date) (nth 3 cur-datetime)))
          (progn (setq g-hour (nth 2 cur-datetime))
                 (setq g-minute (cadr cur-datetime))
                 (setq g-second (car cur-datetime))
                 )
        (progn (setq g-hour 0)
               (setq g-minute 0)
               (setq g-second 0)
               )
        )
      )
    (+ (calendar-absolute-from-gregorian g-date)
       (/ (+ g-hour
             (/ (+ g-minute
                   (/ g-second 60.0)) 60.0)) 24.0))
    )
  )

;; 浮点型绝对日期转换成公历日期
;; calendar-gregorian-from-absolute函数只能转换整型绝对日期
;; 本函数反复使用decode-time和encode-time可以防止出现60秒的情况
;; 例如: 12:23:00会变成12:22:60
(defun calendar-fate-gregorian-from-absolute (date)
  (let* ((hour (* (- date (floor date)) 24))
         (minute (* (- hour (floor hour)) 60))
         (second (* (- minute (floor minute)) 60))
         (gdate1 (append
                  (calendar-gregorian-from-absolute (floor date))
                  (list (floor hour) (floor minute) (floor (+ second 0.5)))))
         (gdate2 (list (nth 5 gdate1) (nth 4 gdate1) (nth 3 gdate1)
                       (nth 1 gdate1) (nth 0 gdate1) (nth 2 gdate1)))
         (gdate3 (decode-time (apply 'encode-time gdate2)))
         (gdate4 (list (nth 4 gdate3) (nth 3 gdate3) (nth 5 gdate3)
                       (nth 2 gdate3) (nth 1 gdate3) (nth 0 gdate3)))
         )
    gdate4
    ))

(defun calendar-fate-chinese-from-absolute (date)
  (let* ((g-year (calendar-extract-year
                  (calendar-gregorian-from-absolute (floor date))))
         (list (calendar-fate-chinese-year g-year))
         termidx
         c-year)
    (while (and list (< (-(cadr (car list)) date) calendar-fate-1sec))
      (setq list (cdr list)))
    (if list
        ;; found item after given date
        (setq termidx (1- (caar list)))
      ;; given date in beyond last item
      (setq termidx 22))
    (setq c-year (- g-year 63 (if (< termidx 1) 1 0)))
    (list (1+ (mod (floor (/ (1- c-year) 20.0)) 9))
          (1+ (mod (1- c-year) 60))
          (1+ (mod (+ (* (- g-year 64) 12) (/ (+ termidx 3) 2)) 60))
          (1+ (mod (floor (+ date (/ 1 24.0) 14)) 60))
          (1+ (mod (+ (* (floor (+ date 14)) 12)
                      (/ (1+ (floor (* (- date (floor date)) 24))) 2)) 60))
          (1+ (mod (1- termidx) 24))
          )
    ))

(defun calendar-fate-chinese-date-string (&optional date)
  "String of Chinese date of Gregorian DATE.
Defaults to current date and current time if DATE is not given.
Input is a list of datetime (month day year hour minute second)."
  (let* ((a-date (calendar-fate-chinese-datetime date))
         (c-date (calendar-fate-chinese-from-absolute a-date))
         (yun (car c-date))
         (year (cadr c-date))
         (month (nth 2 c-date))
         (day (nth 3 c-date))
         (hour (nth 4 c-date))
         (termidx (nth 5 c-date)))
    (format "Yun %s, year %s (%s), month %s (%s), day %s (%s), hour %s (%s) term %s (%s)"
            yun
            year (calendar-chinese-sexagesimal-name year)
            month (calendar-chinese-sexagesimal-name month)
            day (calendar-chinese-sexagesimal-name day)
            hour (calendar-chinese-sexagesimal-name hour)
            termidx (calendar-fate-chinese-term-name termidx)
            )))

(defun calendar-fate-chinese-year (y)
  (let ((list (cdr (assoc y calendar-fate-chinese-year-cache))))
    (or list
        (setq list (calendar-fate-chinese-compute-year y)
              calendar-fate-chinese-year-cache (append calendar-fate-chinese-year-cache
                                                       (list (cons y list)))))
    list))

(defcustom calendar-fate-chinese-24-term
  ["LiChun" "YuShui" "JingZhe" "ChunFen" "QingMing" "GuYu"
   "LiXia" "XiaoMan" "MangZhong" "XiaZhi" "XiaoShu" "DaShu"
   "LiQiu" "ChuShu" "BaiLu" "QiuFen" "HanLu" "ShuangJiang"
   "LiDong" "XiaoXue" "DaXue" "DongZhi" "XiaoHan" "DaHan"
   ]
  "Prefixes used by `calendar-fate-chinese-term-name'."
  :group 'calendar-fate
  :type '(vector (string :tag "LiChun")
                 (string :tag "YuShui")
                 (string :tag "JingZhe")
                 (string :tag "ChunFen")
                 (string :tag "QingMing")
                 (string :tag "GuYu")
                 (string :tag "LiXia")
                 (string :tag "XiaoMan")
                 (string :tag "MangZhong")
                 (string :tag "XiaZhi")
                 (string :tag "XiaoShu")
                 (string :tag "DaShu")
                 (string :tag "LiQiu")
                 (string :tag "ChuShu")
                 (string :tag "BaiLu")
                 (string :tag "QiuFen")
                 (string :tag "HanLu")
                 (string :tag "ShuangJiang")
                 (string :tag "LiDong")
                 (string :tag "XiaoXue")
                 (string :tag "DaXue")
                 (string :tag "DongZhi")
                 (string :tag "XiaoHan")
                 (string :tag "DaHan")))

(defun calendar-fate-chinese-term-name (n)
  "The N-th name of the Chinese 24 terms.
N congruent to 1 gives the first name, N congruent to 2 gives the second name,
..., N congruent to 24 gives the last name."
  (format "%s" (aref calendar-fate-chinese-24-term (% (+ n 23) 24))))

(defun calendar-fate-chinese-sexagesimal-name (n)
  (format "%s%s"
          (aref chinese-fate-calendar-celestial-stem (% (1- n) 10))
          (aref chinese-fate-calendar-terrestrial-branch (% (1- n) 12))))

(defconst chinese-fate-calendar-celestial-stem
  ["甲" "乙" "丙" "丁" "戊" "己" "庚" "辛" "壬" "癸"])
(defconst chinese-fate-calendar-terrestrial-branch
  ["子" "丑" "寅" "卯" "辰" "巳" "午" "未" "申" "酉" "戌" "亥"])
(defconst chinese-calendar-fate-chinese-24-term
  ["立春" "雨水" "惊蛰" "春分" "清明" "谷雨"
   "立夏" "小满" "芒种" "夏至" "小暑" "大暑"
   "立秋" "处暑" "白露" "秋分" "寒露" "霜降"
   "立冬" "小雪" "大雪" "冬至" "小寒" "大寒"]
  )

(defconst calendar-fate-1sec (/ 1 86400.0) "Value for one second in absolute date.")

(defvar calendar-fate-chinese-year-cache
  '((2005 (-1 731951.5850475626) (0 731966.3063670793) (1 731981.0711194673)
          (2 731995.8968434334) (3 732010.822622776) (4 732025.856279532)
          (5 732041.0235619545) (6 732056.3174058595) (7 732071.7448506355)
          (8 732087.2827388444) (9 732102.9177041054) (10 732118.6150625544)
          (11 732134.3445448875) (12 732150.0695926347) (13 732165.7519475617)
          (14 732181.3643892603) (15 732196.8721502619) (16 732212.2655329704)
          (17 732227.5225968361) (18 732242.6539416313) (19 732257.6541482606)
          (20 732272.5517792702) (21 732287.3559263544) (22 732302.1074385643))
    (2006 (-1 732316.824165503) (0 732331.5520097413) (1 732346.3102453547)
          (2 732361.1422794657) (3 732376.0610661507) (4 732391.1005562143)
          (5 732406.2602807679) (6 732421.5593371391) (7 732436.9792081513)
          (8 732452.5215229988) (9 732468.1502178507) (10 732483.8508515358)
          (11 732499.576947371) (12 732515.3037276268) (13 732530.986400445)
          (14 732546.5989216166) (15 732562.1101953187) (16 732577.5021564164)
          (17 732592.7644645371) (18 732607.8930595713) (19 732622.8987577753)
          (20 732637.7924866676) (21 732652.6015213327) (22 732667.3482810655))
    (2007 (-1 732682.0691803293) (0 732696.7918995218) (1 732711.5540362992)
          (2 732726.3808887796) (3 732741.3039272623) (4 732756.3381482759)
          (5 732771.5029908814) (6 732786.7962457337) (7 732802.2222361565)
          (8 732817.7579795518) (9 732833.3934842744) (10 732849.0873468714)
          (11 732864.8201670647) (12 732880.5412322679) (13 732896.2295953431)
          (14 732911.8383425074) (15 732927.3535014787) (16 732942.7434916496)
          (17 732958.0076991715) (18 732973.1352620125) (19 732988.1412701607)
          (20 733003.0340851145) (21 733017.8425555229) (22 733032.5881778398))
    (2008 (-1 733047.308330059) (0 733062.0297776856) (1 733076.7914385796)
          (2 733091.617415905) (3 733106.5403590202) (4 733121.5748502412)
          (5 733136.7396133738) (6 733152.0350457826) (7 733167.4600914316)
          (8 733183.0001889863) (9 733198.6327780085) (10 733214.3326636949)
          (11 733230.0601433115) (12 733245.787885189) (13 733261.469317277)
          (14 733277.0845812159) (15 733292.5928190546) (16 733307.9889356294)
          (17 733323.247317791) (18 733338.3807059922) (19 733353.3819616633)
          (20 733368.2803953486) (21 733383.0844868021) (22 733397.8354744911))
    (2009 (-1 733412.551009337) (0 733427.2775581675) (1 733442.0341884294)
          (2 733456.8649986582) (3 733471.7827512422) (4 733486.8216770487)
          (5 733501.9815923371) (6 733517.2805533409) (7 733532.7017445564)
          (8 733548.2435507774) (9 733563.8740735054) (10 733579.5729419389)
          (11 733595.3007712364) (12 733611.0246109962) (13 733626.7088971138)
          (14 733642.318295002) (15 733657.831436316) (16 733673.2211235361)
          (17 733688.4858962693) (18 733703.6133707361) (19 733718.6220730143)
          (20 733733.5153568583) (21 733748.3274989128) (22 733763.0737341242))
    (2010 (-1 733777.7972877817) (0 733792.518766244) (1 733807.2827556925)
          (2 733822.1075498261) (3 733837.031732718) (4 733852.0635139146)
          (5 733867.2291830378) (6 733882.5204033852) (7 733897.947124958)
          (8 733913.4817786217) (9 733929.1175465584) (10 733944.8111866312)
          (11 733960.5430680909) (12 733976.2644203501) (13 733991.9505263963)
          (14 734007.5601070719) (15 734023.0725967088) (16 734038.4644544916)
          (17 734053.7267467179) (18 734068.8575193086) (19 734083.8627565699)
          (20 734098.7597756386) (21 734113.5680791535) (22 734128.3180178003))
    (2011 (-1 734143.0376454988) (0 734157.7625899315) (1 734172.5225570993)
          (2 734187.3506573038) (3 734202.2704135575) (4 734217.3057154017)
          (5 734232.4661393166) (6 734247.7617068291) (7 734263.1822374659)
          (8 734278.7226697602) (9 734294.3518325486) (10 734310.0527364411)
          (11 734325.7786901789) (12 734341.5077980356) (13 734357.1894456544)
          (14 734372.8056314783) (15 734388.3150525093) (16 734403.711209774)
          (17 734418.9711575508) (18 734434.1039964356) (19 734449.1071286201)
          (20 734464.0050139427) (21 734478.8113783197) (22 734493.5620948472))
    (2012 (-1 734508.2800854044) (0 734523.006388823) (1 734537.7651648521)
          (2 734552.5951317148) (3 734567.5142839747) (4 734582.551302433)
          (5 734597.711964766) (6 734613.008040905) (7 734628.4300586381)
          (8 734643.9687177339) (9 734659.6009720163) (10 734675.297314167)
          (11 734691.0279329615) (12 734706.7502547898) (13 734722.437599659)
          (14 734738.0461481409) (15 734753.5615544319) (16 734768.9504151344)
          (17 734784.2161246934) (18 734799.3424159684) (19 734814.350967248)
          (20 734829.2428205805) (21 734844.0544937449) (22 734858.7994891801))
    (2013 (-1 734873.5230428376) (0 734888.2439649897) (1 734903.0088683763)
          (2 734917.8339962959) (3 734932.7597835856) (4 734947.7925502458)
          (5 734962.9596896172) (6 734978.2520305314) (7 734993.6790947914)
          (8 735009.2147259712) (9 735024.8492700257) (10 735040.5440376597)
          (11 735056.2736233072) (12 735071.9967398643) (13 735087.6803336143)
          (14 735103.2923073769) (15 735118.8025161424) (16 735134.1968366303)
          (17 735149.4569443064) (18 735164.5897831917) (19 735179.5927316346)
          (20 735194.4914434748) (21 735209.2974184351) (22 735224.049018065))
    (2014 (-1 735238.766531785) (0 735253.4935097694) (1 735268.251801014)
          (2 735283.0824443498) (3 735298.0009846687) (4 735313.0390919046)
          (5 735328.1985462504) (6 735343.496529738) (7 735358.9159557023)
          (8 735374.4575314522) (9 735390.0852646828) (10 735405.7854533195)
          (11 735421.5100312233) (12 735437.2369227409) (13 735452.918109417)
          (14 735468.5317432084) (15 735484.0423334436) (16 735499.4366230965)
          (17 735514.6992888451) (18 735529.8310468988) (19 735544.8376272516)
          (20 735559.7345509529) (21 735574.5441578226) (22 735589.2933812141))
    (2015 (-1 735604.0138990083) (0 735618.7379851341) (1 735633.4986437159)
          (2 735648.3259332972) (3 735663.2467703819) (4 735678.2810072899)
          (5 735693.443584919) (6 735708.7369987168) (7 735724.1612089472)
          (8 735739.6973973908) (9 735755.3317894936) (10 735771.025993824)
          (11 735786.7582179704) (12 735802.4791003861) (13 735818.1673045158)
          (14 735833.7754238443) (15 735849.2910834947) (16 735864.6805729866)
          (17 735879.9462189674) (18 735895.0738762217) (19 735910.0821900368)
          (20 735924.9756328263) (21 735939.7867487269) (22 735954.5329680443))
    (2016 (-1 735969.255393187) (0 735983.976785183) (1 735998.7398041086)
          (2 736013.5647095041) (3 736028.4880420365) (4 736043.5205623307)
          (5 736058.6852380433) (6 736073.978341897) (7 736089.403537591)
          (8 736104.9416333833) (9 736120.5750479698) (10 736136.2736074128)
          (11 736152.0023268061) (12 736167.7292898493) (13 736183.4117312431)
          (14 736199.0264627137) (15 736214.5351297059) (16 736229.9309204416)
          (17 736245.1893820763) (18 736260.3228815394) (19 736275.3243597345)
          (20 736290.2234848337) (21 736305.0282279649) (22 736319.7803362207))
    (2017 (-1 736334.4967532158) (0 736349.2244067192) (1 736363.9816807108)
          (2 736378.8130860329) (3 736393.7307362556) (4 736408.769519011)
          (5 736423.9283852577) (6 736439.2267661095) (7 736454.6462883949)
          (8 736470.1878164606) (9 736485.8166941004) (10 736501.5162698426)
          (11 736517.2430103617) (12 736532.9685428934) (13 736548.6522479057)
          (14 736564.2635938325) (15 736579.7762026787) (16 736595.1673760414)
          (17 736610.431338151) (18 736625.5596311884) (19 736640.5672844248)
          (20 736655.4610848427) (21 736670.2722175913) (22 736685.0190965333))
    (2018 (-1 736699.7419031458) (0 736714.4643432298) (1 736729.2278399467)
          (2 736744.0537467003) (3 736758.9774368601) (4 736774.0100754099)
          (5 736789.1749904947) (6 736804.4664413133) (7 736819.8922012644)
          (8 736835.4264496164) (9 736851.0615897179) (10 736866.7546257973)
          (11 736882.48693641) (12 736898.2080025673) (13 736913.8957854905)
          (14 736929.5054535866) (15 736945.02029562) (16 736960.4121216135)
          (17 736975.6764960289) (18 736990.8065533638) (19 737005.8130462961)
          (20 737020.7084997492) (21 737035.5172721543) (22 737050.2650809288))
    (2019 (-1 737064.9848437309) (0 737079.707601706) (1 737094.4678956666)
          (2 737109.294160048) (3 737124.214703083) (4 737139.248614152)
          (5 737154.4101983705) (6 737169.7046055794) (7 737185.1263044672)
          (8 737200.6656241417) (9 737216.2956293421) (10 737231.9956749277)
          (11 737247.7222158113) (12 737263.451276938) (13 737279.1336000757)
          (14 737294.7509144144) (15 737310.2612175941) (16 737325.6593298912)
          (17 737340.9200167656) (18 737356.0547868409) (19 737371.057911078)
          (20 737385.9569010735) (21 737400.7621280351) (22 737415.5128455162))
    (2020 (-1 737430.2282055216) (0 737444.9536744752) (1 737459.7098031044)
          (2 737474.5387934046) (3 737489.4554661112) (4 737504.492079258)
          (5 737519.6508898735) (6 737534.9475302696) (7 737550.3683231669)
          (8 737565.9083580971) (9 737581.5397143364) (10 737597.2377413111)
          (11 737612.9676289558) (12 737628.6915949183) (13 737644.3786546388)
          (14 737659.9888799982) (15 737675.5048098564) (16 737690.8955311775)
          (17 737706.162504355) (18 737721.2906076112) (19 737736.3005178766)
          (20 737751.1936030388) (21 737766.005888144) (22 737780.7510027885))
    (2021 (-1 737795.473896821) (0 737810.1936507225) (1 737824.9566785493)
          (2 737839.779667695) (3 737854.7030002275) (4 737869.7333747544)
          (5 737884.8984955149) (6 737900.1889449754) (7 737915.6153019266)
          (8 737931.1498999596) (9 737946.7852864265) (10 737962.4796646433)
          (11 737978.211165587) (12 737993.9340993562) (13 738009.6200057664)
          (14 738025.2318364778) (15 738040.7444214821) (16 738056.1388373375)
          (17 738071.401376883) (18 738086.5346369743) (19 738101.5400172868)
          (20 738116.4391741753) (21 738131.2471915879) (22 738145.9986481667))
    (2022 (-1 738160.7172745066) (0 738175.4429252939) (1 738190.2010258036)
          (2 738205.0289750099) (3 738219.9460927644) (4 738234.9806068735)
          (5 738250.1380346613) (6 738265.4325779276) (7 738280.8503429093)
          (8 738296.3898364701) (9 738312.0169657068) (10 738327.7170987129)
          (11 738343.4420739808) (12 738359.1706503229) (13 738374.8526158333)
          (14 738390.4687848091) (15 738405.9799631434) (16 738421.3768982887)
          (17 738436.6398272514) (18 738451.7740807533) (19 738466.7807087898)
          (20 738481.6799610453) (21 738496.4894169169) (22 738511.2406827607))
    (2023 (-1 738525.9606124558) (0 738540.6860884028) (1 738555.4452935853)
          (2 738570.2729169526) (3 738585.1911155381) (4 738600.224517981)
          (5 738615.3833842278) (6 738630.6752483048) (7 738646.0954769449)
          (8 738661.6303540864) (9 738677.2617659569) (10 738692.9558033943)
          (11 738708.6870570183) (12 738724.4090768495) (13 738740.0983220735)
          (14 738755.7082613306) (15 738771.2260826426) (16 738786.6172092753)
          (17 738801.8851440744) (18 738817.0137629509) (19 738832.0239761667)
          (20 738846.9177209535) (21 738861.7302753129) (22 738876.4763515787))
    (2024 (-1 738891.1998098688) (0 738905.9206852913) (1 738920.6842923164)
          (2 738935.5081089335) (3 738950.4313699403) (4 738965.4618875184)
          (5 738980.6256810823) (6 738995.9157093363) (7 739011.3394586244)
          (8 739026.8738191919) (9 739042.5059940019) (10 739058.2012632685)
          (11 739073.9299111366) (12 739089.6551804543) (13 739105.3391486802)
          (14 739120.9541662531) (15 739136.4654866853) (16 739151.8628360429)
          (17 739167.1241030693) (18 739182.259342988) (19 739197.2629669504)
          (20 739212.1632126169) (21 739226.9691557884) (22 739241.7215571404))
    (2025 (-1 739256.4384121895) (0 739271.1656921702) (1 739285.922910531)
          (2 739300.7535370188) (3 739315.6707183518) (4 739330.7084202766)
          (5 739345.8662374811) (6 739361.1631957688) (7 739376.5808027582)
          (8 739392.1206857362) (9 739407.7468612986) (10 739423.4451257386)
          (11 739439.1691006022) (12 739454.8944900827) (13 739470.5764625864)
          (14 739486.1893820763) (15 739501.7018717127) (16 739517.0960650444)
          (17 739532.361107985) (18 739547.4929773011) (19 739562.5019100504)
          (20 739577.3988258042) (21 739592.2105228105) (22 739606.9594600992))))

(defun calendar-fate-chinese-compute-year (y)
  (let* ((year y)
         (calendar-time-zone (eval calendar-chinese-time-zone)) ; uses year
         (calendar-daylight-time-offset
          calendar-chinese-daylight-time-offset)
         (calendar-standard-time-zone-name
          calendar-chinese-standard-time-zone-name)
         (calendar-daylight-time-zone-name
          calendar-chinese-daylight-time-zone-name)
         (calendar-daylight-savings-starts
          calendar-chinese-daylight-saving-start)
         (calendar-daylight-savings-ends
          calendar-chinese-daylight-saving-end)
         (calendar-daylight-savings-starts-time
          calendar-chinese-daylight-saving-start-time)
         (calendar-daylight-savings-ends-time
          calendar-chinese-daylight-saving-end-time)
         (term-datetime (calendar-astro-to-absolute
                         (solar-date-next-longitude
                          (calendar-astro-from-absolute
                           (calendar-absolute-from-gregorian (list 1 1 year))) 15)))
         count
         outlist
         )
    (setq outlist (list (list -1 term-datetime)))
    (dotimes (count 23)
      (setq term-datetime (calendar-astro-to-absolute (solar-date-next-longitude (calendar-astro-from-absolute (+ (floor term-datetime) 10)) 15)))
      (setq outlist (append outlist (list (list count term-datetime))))
      )
    outlist
    )
  )
;; Maintainer use.
(defun calendar-fate-chinese-year-cache-init (year)
  "Insert an initialization value for `calendar-fate-chinese-year-cache' after point.
Computes values for 10 years either side of YEAR."
  (setq year (- year 10))
  (let (calendar-fate-chinese-year-cache end)
    (save-excursion
      (insert "'(")
      (dotimes (n 21)
        (princ (cons year (calendar-fate-chinese-compute-year year))
               (current-buffer))
        (insert (if (= n 20) ")" "\n"))
        (setq year (1+ year)))
      (setq end (point)))
    (save-excursion
      ;; fill-column -/+ 10.
      (while (and (< (point) end)
                  (re-search-forward "^.\\{60,80\\})" end t))
        (delete-char 1)
        (insert "\n")))
    (indent-region (point) end)))

(defun holiday-chinese-terms (&optional year)
  (let* ((g-year (or year displayed-year))
         (term-list (calendar-fate-chinese-year g-year))
         (holiday-list '()))
    (while term-list
      (setq holiday-list
            (append holiday-list
                    (list (list (calendar-gregorian-from-absolute
                                 (floor (cadr (car term-list))))
                                (calendar-fate-chinese-term-name
                                 (caar term-list))))))
      (setq term-list (cdr term-list)))
    holiday-list)
  )

(require 'fate-essential)

(provide 'calendar-fate)
;;; calendar-fate.el ends here
