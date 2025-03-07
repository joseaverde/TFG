BEGIN {
   k=1; while (2^(k-1) < 2^k) k++; inf = 2^k; 
   min = inf; max = -inf;
   count = 0;
}

{
   if (count == 1) {
      for (i = 1; i <= NF; ++i) {
         if ($i > max) { max = $i; }
         if ($i < min) { min = $i; }
      }
   }
   count = 1;
}

END {
   printf "%f .. %f\n", min, max;
}
