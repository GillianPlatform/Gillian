for filename in $1/**/*.c; do
  echo "transforming $filename"
  sed -i '' -e '1s/^/#include <klee\/klee.h> /' $filename
  comby 'int :[[x]] = __builtin_annot_intval("symb_int", :[[x]]);' 'int :[x]; klee_make_symbolic(&:[x], sizeof(int), ":[x]");' -d $(dirname $filename) $(basename $filename) -i
  comby 'char :[[x]] = __builtin_annot_intval("symb_int", :[[x]]);' 'char :[x]; klee_make_symbolic(&:[x], sizeof(char), ":[x]");' -d $(dirname $filename) $(basename $filename) -i
  comby 'char :[[x]] = (char) __builtin_annot_intval("symb_int", :[[x]]);' 'char :[x]; klee_make_symbolic(&:[x], sizeof(char), ":[x]");' -d $(dirname $filename) $(basename $filename) -i
  comby ':[[x]] = __builtin_annot_intval("symb_int", :[[x]]);' 'klee_make_symbolic(&:[x], sizeof(int), ":[x]");' -d $(dirname $filename) $(basename $filename) -i # FIXME: timeout
  comby '*:[[x]] = __builtin_annot_intval("symb_int", *:[[x]]);' 'klee_make_symbolic(:[x], sizeof(int), ":[x]");' -d $(dirname $filename) $(basename $filename) -i
  comby 'ASSERT(:[assertion]);' 'klee_assert(:[assertion]);' -d $(dirname $filename) $(basename $filename) -i
  comby 'ASSUME(:[assertion]);' 'klee_assume(:[assertion]);' -d $(dirname $filename) $(basename $filename) -i
  comby 'ASSUME (:[assertion]);' 'klee_assume(:[assertion]);' -d $(dirname $filename) $(basename $filename) -i
done