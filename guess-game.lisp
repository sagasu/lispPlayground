(defparameter *small* 1)

(defparameter *big* 100)

(defun guess-my-number ()
  ;;; this is really interesting binary search technique on bit manipulation
  ;;; 11 in binary is 1011, if we shift it left it is 10110 and it means in decimal 22 which is x2
  ;;; and 11 shift to the left is in binary 101 which is 5 in decimal
  ;;; so it is a great technique for binary search
  (ash (+ *small* *big*) -1))

(defun smaller ()
  (setf *big* (1- (guess-my-number)))
  (guess-my-number))

(defun bigger ()
  (setf *small* (1+ (guess-my-number)))
  (guess-my-number))
