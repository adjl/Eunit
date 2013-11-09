(load "unit-test.lisp")

(test-case 'test-case-unit-test
           '((test test-assert-equal-pass
                   ((assert-equal t t)
                    (assert-equal nil nil)))
             (test test-assert-equal-fail
                   ((assert-equal t nil)
                    (assert-equal nil t)))))
