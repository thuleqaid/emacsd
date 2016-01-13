(defun calendar-chinese-gz-zodiac-sign-on-or-after (d)
  "Absolute date of first new Zodiac sign on or after absolute date D.
The Zodiac signs begin when the sun's longitude is a multiple of 15 degrees."
  (let* ((year (calendar-extract-year (calendar-gregorian-from-absolute d)))
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
         (a-datetime (calendar-astro-to-absolute (solar-date-next-longitude (calendar-astro-from-absolute d) 15)))
         (a-date (calendar-gregorian-from-absolute (floor a-datetime)))
         )
    (list a-datetime (+ (* (car a-date) 2) (if (> (cadr a-date) 15) 2 1) -4))
    )
  )
(defun calendar-chinese-gz-from-absolute (date)
  "Compute Chinese date (yun year month day hour prev-term-idx) corresponding to absolute DATE.
The absolute date is the number of days elapsed since the (imaginary)
Gregorian date Sunday, December 31, 1 BC."
  (let* ((g-year (calendar-extract-year
                  (calendar-gregorian-from-absolute (floor date))))
         (next-term (calendar-chinese-gz-zodiac-sign-on-or-after (floor date)))
         (termidx (if (> (- (nth 0 next-term) date) 0.0000157) (1- (nth 1 next-term)) (nth 1 next-term)))
         (c-year (- g-year 63 (if (< termidx 1) 1 0)))
        )
    (list (1+ (mod (floor (/ (1- c-year) 20.0)) 9))
          (1+ (mod (1- c-year) 60))
          (1+ (mod (+ (* (- g-year 64) 12) (/ (+ termidx 3) 2)) 60))
          (1+ (mod (floor (+ date (/ 1 24.0) 14)) 60))
          (1+ (mod (+ (* (floor (+ date 14)) 12) (/ (1+ (floor (* (- date (floor date)) 24))) 2)) 60))
          (1+ (mod (1- termidx) 24))
    )))
(defun calendar-chinese-gz-date-string (&optional date)
  "String of Chinese date of Gregorian DATE.
Defaults to current date and current time if DATE is not given.
Input is a list of datetime (month day year hour minute second)."
  (let* ((cur-time (decode-time))
         (g-date (or date (list (nth 4 cur-time) (nth 3 cur-time) (nth 5 cur-time))))
         (a-date (+ (calendar-absolute-from-gregorian g-date) (/ (+ (or (nth 3 g-date) (nth 2 cur-time)) (/ (+ (or (nth 4 g-date) (cadr cur-time)) (/ (or (nth 5 g-date) (car cur-time)) 60.0)) 60.0)) 24.0)))
         (c-date (calendar-chinese-gz-from-absolute a-date))
         (yun (car c-date))
         (year (cadr c-date))
         (month (nth 2 c-date))
         (day (nth 3 c-date))
         (hour (nth 4 c-date)))
    (format "Yun %s, year %s (%s), month %s (%s), day %s (%s), hour %s (%s)"
            yun
            year (calendar-chinese-sexagesimal-name year)
            month (calendar-chinese-sexagesimal-name month)
            day (calendar-chinese-sexagesimal-name day)
            hour (calendar-chinese-sexagesimal-name hour)
            )))
