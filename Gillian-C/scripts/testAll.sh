#!/bin/bash

FINAL_RETURN=0

echo "------------------------------------------"
echo "----------- CONCRETE EXECUTION -----------"
echo "------------------------------------------"
./testConcrete.sh
rc=$?; if [[ $rc != 0 ]]; then FINAL_RETURN=1; fi
printf "\n\n"

# echo "------------------------------------------"
# echo "-------------- BI-ABDUCTION --------------"
# echo "------------------------------------------"
# ./testACT.sh
# rc=$?; if [[ $rc != 0 ]]; then FINAL_RETURN=1; fi
# printf "\n\n"

echo "------------------------------------------"
echo "------------ SYMBOLIC TESTING ------------"
echo "------------------------------------------"
./testSymb.sh
rc=$?; if [[ $rc != 0 ]]; then FINAL_RETURN=1; fi
printf "\n\n"

echo "------------------------------------------"
echo "-------------- VERIFICATION --------------"
echo "------------------------------------------"
./testVerif.sh
rc=$?; if [[ $rc != 0 ]]; then FINAL_RETURN=1; fi
printf "\n\n"

echo "------------------------------------------"
echo "-------------- MULTI-FILE ----------------"
echo "------------------------------------------"
./testMultifile.sh
rc=$?; if [[ $rc != 0 ]]; then FINAL_RETURN=1; fi
printf "\n\n"

exit $FINAL_RETURN