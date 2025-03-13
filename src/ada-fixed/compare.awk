# USAGE: cat chbXY.batch chbXY.in | ./ftests | cat chbXY.out - | awk -f compare.awk

function abs (item) {
   return item < 0 ? -item : item
}

function error (expected, got) {
   return got == 0 ? expected : abs((expected - got) / expected);
}

function geterr (old, expected, got) {
   return error(expected, got) < old ? old : error(expected, got);
}

BEGIN {
   reading_expected = 1;
   expected_count = 0;
   got_count = 0;
}

/^Ada/ { reading_expected = 0; }

/^[FT]/ && reading_expected {
   expected_count++;
   expected["max_dist"][expected_count] = $6;
}

/^[FT]/ && ! reading_expected {
   got_count++;
   got["max_dist"][got_count] = $5;
}

END {
   if (got_count != expected_count) {
      printf "Expected %d values, got %d\n", expected_count, got_count;
      exit 1;
   }
   inf = 1e38.0;
   max_dist = -inf;
   for (i = 1; i < got_count; ++i) {
      max_dist = geterr(max_dist, expected["max_dist"][i], got["max_dist"][i]);
   }
   print max_dist
}
