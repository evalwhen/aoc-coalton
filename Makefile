build:
	sbcl --load aoc-coalton.asd \
	     --eval '(ql:quickload :aoc-coalton)' \
         --eval "(sb-ext:save-lisp-and-die #p\"./aoc-coalton\" :toplevel #'aoc-coalton-2024-07:main :executable t)"
