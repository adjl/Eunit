(test-case 'test-case-unit-test
           '((test test-assert-equal
                   ((assert-equal 1 1)
                    (assert-equal 'x 'x)
                    (assert-equal '(x) '(x))
                    (assert-equal '(x (y)) '(x (y)))
                    (assert-equal 2 (+ 1 1))))))
