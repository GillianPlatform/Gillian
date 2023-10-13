FINAL_RETURN=0

if [[ -z "${GITHUB_ACTIONS}" ]]; then
  esy x true > /dev/null 2>&1
  esy exec-env > exec.env
  source exec.env
fi

echo "--- basic_se.c basic symex (expect failure) ---"
time kanillian wpst wpst/basic_se.c -l disabled
rc=$?; if [[ $rc == 0 ]]; then FINAL_RETURN=1; fi
printf "\n\n"

echo "--- bool_test.c (expect success) ---"
time kanillian wpst wpst/bool_test.c -l disabled
rc=$?; if [[ $rc != 0 ]]; then FINAL_RETURN=1; fi
printf "\n\n"

echo "--- bycopy.c (expect success) ---"
time kanillian wpst wpst/bycopy.c -l disabled
rc=$?; if [[ $rc != 0 ]]; then FINAL_RETURN=1; fi
printf "\n\n"

echo "--- dyncall.c (expect success) ---"
time kanillian wpst wpst/dyncall.c -l disabled
rc=$?; if [[ $rc != 0 ]]; then FINAL_RETURN=1; fi
printf "\n\n"

echo "--- exprif.c (expect success) ---"
time kanillian wpst wpst/exprif.c -l disabled
rc=$?; if [[ $rc != 0 ]]; then FINAL_RETURN=1; fi
printf "\n\n"

echo "--- fcall.c (expect failure) ---"
time kanillian wpst wpst/fcall.c -l disabled
rc=$?; if [[ $rc == 0 ]]; then FINAL_RETURN=1; fi
printf "\n\n"

echo "--- globalvar.c (expect success) ---"
time kanillian wpst wpst/globalvar.c -l disabled
rc=$?; if [[ $rc != 0 ]]; then FINAL_RETURN=1; fi
printf "\n\n"

echo "--- globalvar_bad.c (expect failure) ---"
time kanillian wpst wpst/globalvar_bad.c -l disabled
rc=$?; if [[ $rc == 0 ]]; then FINAL_RETURN=1; fi
printf "\n\n"

echo "--- memory.c (expect failure) ---"
time kanillian wpst wpst/memory.c -l disabled
rc=$?; if [[ $rc == 0 ]]; then FINAL_RETURN=1; fi
printf "\n\n"

echo "--- return_by_copy.c (expect success) ---"
time kanillian wpst wpst/return_by_copy.c -l disabled
rc=$?; if [[ $rc != 0 ]]; then FINAL_RETURN=1; fi
printf "\n\n"

echo "--- return_by_copy_bad.c (expect failure) ---"
time kanillian wpst wpst/return_by_copy_bad.c -l disabled
rc=$?; if [[ $rc == 0 ]]; then FINAL_RETURN=1; fi
printf "\n\n"

echo "--- struct_expr.c (expect success) ---"
time kanillian wpst wpst/struct_expr.c -l disabled
rc=$?; if [[ $rc != 0 ]]; then FINAL_RETURN=1; fi
printf "\n\n"

echo "--- struct_expr_bad.c (expect failure) ---"
time kanillian wpst wpst/struct_expr_bad.c -l disabled
rc=$?; if [[ $rc == 0 ]]; then FINAL_RETURN=1; fi
printf "\n\n"

echo "--- struct_passing.c (expect success) ---"
time kanillian wpst wpst/struct_passing.c -l disabled
rc=$?; if [[ $rc != 0 ]]; then FINAL_RETURN=1; fi
printf "\n\n"

echo "--- struct_passing_bad.c (expect failure) ---"
time kanillian wpst wpst/struct_passing_bad.c -l disabled
rc=$?; if [[ $rc == 0 ]]; then FINAL_RETURN=1; fi
printf "\n\n"

echo "--- trivial.c (expect success) ---"
time kanillian wpst wpst/trivial.c -l disabled
rc=$?; if [[ $rc != 0 ]]; then FINAL_RETURN=1; fi
printf "\n\n"

exit $FINAL_RETURN
