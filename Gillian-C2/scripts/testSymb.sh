FINAL_RETURN=0

if [[ "${GITHUB_ACTIONS}" ]]; then
	GILLIAN_C2="gillian-c2"
else
  GILLIAN_C2="dune exec -- gillian-c2"
fi
WPST="time $GILLIAN_C2 wpst -l disabled"


echo "--- basic_se.c basic symex (expect failure) ---"
$WPST wpst/basic_se.c
rc=$?; if [[ $rc == 0 ]]; then FINAL_RETURN=1; fi
printf "\n\n"

echo "--- bool_test.c (expect success) ---"
$WPST wpst/bool_test.c
rc=$?; if [[ $rc != 0 ]]; then FINAL_RETURN=1; fi
printf "\n\n"

echo "--- bycopy.c (expect success) ---"
$WPST wpst/bycopy.c
rc=$?; if [[ $rc != 0 ]]; then FINAL_RETURN=1; fi
printf "\n\n"

echo "--- dyncall.c (expect success) ---"
$WPST wpst/dyncall.c
rc=$?; if [[ $rc != 0 ]]; then FINAL_RETURN=1; fi
printf "\n\n"

echo "--- exprif.c (expect success) ---"
$WPST wpst/exprif.c
rc=$?; if [[ $rc != 0 ]]; then FINAL_RETURN=1; fi
printf "\n\n"

echo "--- fcall.c (expect failure) ---"
$WPST wpst/fcall.c
rc=$?; if [[ $rc == 0 ]]; then FINAL_RETURN=1; fi
printf "\n\n"

echo "--- globalvar.c (expect success) ---"
$WPST wpst/globalvar.c
rc=$?; if [[ $rc != 0 ]]; then FINAL_RETURN=1; fi
printf "\n\n"

echo "--- globalvar_bad.c (expect failure) ---"
$WPST wpst/globalvar_bad.c
rc=$?; if [[ $rc == 0 ]]; then FINAL_RETURN=1; fi
printf "\n\n"

echo "--- memory.c (expect failure) ---"
$WPST wpst/memory.c
rc=$?; if [[ $rc == 0 ]]; then FINAL_RETURN=1; fi
printf "\n\n"

echo "--- return_by_copy.c (expect success) ---"
$WPST wpst/return_by_copy.c
rc=$?; if [[ $rc != 0 ]]; then FINAL_RETURN=1; fi
printf "\n\n"

echo "--- return_by_copy_bad.c (expect failure) ---"
$WPST wpst/return_by_copy_bad.c
rc=$?; if [[ $rc == 0 ]]; then FINAL_RETURN=1; fi
printf "\n\n"

echo "--- struct_expr.c (expect success) ---"
$WPST wpst/struct_expr.c
rc=$?; if [[ $rc != 0 ]]; then FINAL_RETURN=1; fi
printf "\n\n"

echo "--- struct_expr_bad.c (expect failure) ---"
$WPST wpst/struct_expr_bad.c
rc=$?; if [[ $rc == 0 ]]; then FINAL_RETURN=1; fi
printf "\n\n"

echo "--- struct_passing.c (expect success) ---"
$WPST wpst/struct_passing.c
rc=$?; if [[ $rc != 0 ]]; then FINAL_RETURN=1; fi
printf "\n\n"

echo "--- struct_passing_bad.c (expect failure) ---"
$WPST wpst/struct_passing_bad.c
rc=$?; if [[ $rc == 0 ]]; then FINAL_RETURN=1; fi
printf "\n\n"

echo "--- trivial.c (expect success) ---"
$WPST wpst/trivial.c
rc=$?; if [[ $rc != 0 ]]; then FINAL_RETURN=1; fi
printf "\n\n"

exit $FINAL_RETURN
