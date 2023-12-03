(defvar grid (get-lines))
(defvar xlim (len [grid 0]))
(defvar ylim (len grid))

(defun get-items-pos (regex)
  (mapcar (op tok-where @1 regex) grid))

(defun horizontal-span (range xlim)
  (rcons (max 0 (- (from range) 1))
         (min xlim (+ (to range) 1))))

(defun adjacent-strings (num-range row)
  (let ((span (horizontal-span num-range xlim)))
    (keep-if 'or
      (mapcar (op [grid @1] span)
              (range (max 0 (- row 1)) (min (- ylim 1) (+ row 1)))))))

(defun adjacent-numbers (gear-range row numbers)
  (let ((gear-beg (- (from gear-range) 1))
        (gear-end (to gear-range)))
    ;; flatten from stdlib is recursive, and doesn't work with nested cons cells
    ;; this is a hack to do just 1 level flattening
    (append-each
      ((x (mapcar 
            (lambda (row)
              (mapcar
                (op cons @1 row)
                (remove-if 
                  (lambda (num)
                    (let ((num-beg (from num)) 
                          (num-end (- (to num) 1)))
                      (or (< num-end gear-beg) 
                          (> num-beg gear-end))))
                  [numbers row])))
            (range (max 0 (- row 1)) (min (- ylim 1) (+ row 1))))))
      x)))

(defun is-part-number? (num row)
  (some (mapcar (op and [#/[^\d\.]/ @1]) 
                (adjacent-strings num row))))

(defun main ()
  (let* ((numbers (get-items-pos #/\d+/))
         (gears (get-items-pos #/\*/))
         (part1 0)
         (part2 0))
    
    (dotimes (row ylim)
      (each ((range [numbers row]))
        (when (is-part-number? range row)
          (inc part1 (num-str [[grid row] range])))))

    (dotimes (row ylim)
      (each ((gear [gears row]))
        (let ((part-numbers
               (keep-if
                 (op is-part-number? (car @1) (cdr @1))
                 (adjacent-numbers gear row numbers))))
          (when (= (length part-numbers) 2)
            (inc part2 (prod (mapcar (op num-str [[grid (cdr @1)] (car @1)]) part-numbers)))))))

    (put-line `Part1: @part1`)
    (put-line `Part2: @part2`)))

(main)
