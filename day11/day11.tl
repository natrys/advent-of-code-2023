(defstruct (point x y) ()
  x y
  (:method shift-right (self n) (inc self.x n))
  (:method shift-down (self n) (inc self.y n)))

(defun read-input ()
  (build
    (each ((line (get-lines)) (row (range 0)))
      (each ((c (list-str line)) (col (range 0)))
        (when (= c #\#)
          (add (new (point col row))))))))

(defun dist-sum (pairs)
  (sum-each ((pair pairs))
    (apply (op + (abs (- @1.x @2.x)) (abs (- @1.y @2.y))) pair)))

(defun expand-by (galaxies axis factor)
  (let ((max-val (find-max-key galaxies '> (op slot @1 axis)))
        (partition (hash))
        (shift (if (eq axis 'x) 'shift-right 'shift-down)))
    (each ((galaxy galaxies)) 
      (pushhash partition (slot galaxy axis) galaxy))
    (each ((val1 (range 0 max-val)))
      (unless (inhash partition val1)
        (each ((val2 (range val1 max-val)))
          (each ((galaxy (gethash partition val2))) 
            [(slot galaxy shift) galaxy (- factor 1)]))))
    (flatten* (hash-values partition))))

(defun solve (input factor)
  (lflow input
    (expand-by 'x factor)
    (expand-by 'y factor)
    (rcomb 2)
    (dist-sum)))

(let ((input (read-input)))
  (put-line `Part1: @(solve (mapcar 'copy-struct input) 2)`)
  (put-line `Part2: @(solve (mapcar 'copy-struct input) 1000000)`))
