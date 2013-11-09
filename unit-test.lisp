(defun test-case (test-case-name tests)
  (labels
    ((test-case-inner (tests tests-passed tests-failed)
       (cond ((null tests)
              (format t "(~a) ~d tests run: ~d passed, ~d failed~%"
                      test-case-name (+ tests-passed tests-failed)
                      tests-passed tests-failed))
             (t (let* ((test-name (cadar tests))
                       (assertions (caddar tests))
                       (test-passedp (test test-case-name test-name assertions))
                       (tests-passed (+ tests-passed (if test-passedp 1 0)))
                       (tests-failed (+ tests-failed
                                        (if (not test-passedp) 1 0))))
                  (test-case-inner (cdr tests) tests-passed tests-failed))))))
    (test-case-inner tests 0 0)))

(defun test (test-case-name test-name assertions)
  (labels
    ((test-inner (assertions passedp)
       (cond ((null assertions) passedp)
             (t (let* ((assertion (caar assertions))
                       (argument1 (cadar assertions))
                       (argument2 (caddar assertions))
                       (assertion-passedp ((assert assertion)
                                           test-case-name test-name
                                           argument1 argument2))
                       (passedp (and passedp assertion-passedp)))
                  (test-inner (cdr assertions) passedp))))))
    (test-inner assertions t)))

(defun assert-equal (test-case-name test-name param1 param2)
  (let ((passedp (equal param1 param2)))
    (not (if (not passedp) (format t "(~a) ~a: ~a != ~a~%"
                                   test-case-name test-name param1 param2)))))

(defun assert (assertion)
  (cond ((eq assertion 'assert-equal) assert-equal)))
