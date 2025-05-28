x=$(which idf.py 2>/dev/null)
if [ $? -ne 0 ]; then
   get_idf || exit -1
fi

idf.py build   || exit $?
idf.py flash   || exit $?
idf.py monitor || exit $?
