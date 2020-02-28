comby 'CHECK_EQUAL_C_INT(:[arg1], :[arg2]);' 'ASSERT(:[arg1] == :[arg2]);' -d $(dirname $1) $(basename $1) -i
comby 'CHECK_C(:[args])' 'ASSERT(:[args])' -d $(dirname $1) $(basename $1) -i
comby 'CHECK_EQUAL_C_POINTER(:[arg1], :[arg2]);' 'ASSERT(:[arg1] == :[arg2]);' -d $(dirname $1) $(basename $1) -i
comby 'CHECK_EQUAL_C_STRING(:[arg1], :[arg2]);' 'ASSERT(strcmp(:[arg1], :[arg2]) == 0);' -d $(dirname $1) $(basename $1) -i
