;;;; t/pike-vm-test.lisp
(in-package #:cl-regex-kit/test)

;; RUN-PIKE-VM (src/pike-vm.lisp) has no implementation yet. Replace this
;; placeholder with real specs -- thread deduplication, capture-slot
;; save/restore, leftmost-first priority at :SPLIT -- once a program actually
;; executes instead of unconditionally erroring.
(it "signals that it is not yet implemented"
  (signals error (cl-regex-kit::run-pike-vm #() "")))
