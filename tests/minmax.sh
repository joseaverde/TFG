for file in *.in; do
   printf "$file "
   awk -f minmax.awk < $file
done
