(defun test-case (test-case-name tests)
  (labels
    ((test-case-inner (test-case-name tests tests-passed tests-failed)
       (cond ((null tests)
              (format t "~d tests run: ~d passed, ~d failed~%"
                      (+ tests-passed tests-failed) tests-passed tests-failed))
             (t (let* ((test-name (cadar tests))
                       (assertions (caddar tests))
                       (test-passedp (test test-name assertions))
                       (tests-passed (+ tests-passed (if test-passedp 1 0)))
                       (tests-failed (+ tests-failed
                                        (if (not test-passedp) 1 0))))
                  (test-case-inner test-case-name (cdr tests)
                                   tests-passed tests-failed))))))
    (test-case-inner test-case-name tests 0 0)))

(defun test (test-name assertions)
  (labels
    ((test-inner (test-name assertions passedp)
       (cond ((null assertions) passedp)
             (t (let* ((assertion (caar assertions))
                       (argument1 (cadar assertions))
                       (argument2 (caddar assertions))
                       (assertion-passedp ((assert assertion)
                                           argument1 argument2))
                       (passedp (and passedp assertion-passedp)))
                  (test-inner test-name (cdr assertions) passedp))))))
    (test-inner test-name assertions t)))

(defun assert (assertion)
  (cond ((eq assertion 'assert-equal) assert-equal)))

(defun assert-equal (param1 param2)
  (let ((passedp (equal param1 param2)))
    (not (if (not passedp) (format t "fail: ~a != ~a~%" param1 param2)))))
