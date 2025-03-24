# USAGE: cat chbXY.batch chbXY.in | ./ftests | cat chbXY.out - | awk -f compare.awk

function max (a, b) {
   return a > b ? a : b;
}

function abs (item) {
   return item < 0 ? -item : item;
}

function error (expected, got) {
   if (expected == 0) {
      return abs(got);
   } else if (got == 0) {
      return abs(epected);
   } else {
      return abs(expected - got) / max(abs(expected), abs(got));
   }
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
   expected["energy"][expected_count] = $7;
   expected["dtw"][expected_count] = $8;
}

/^[FT]/ && ! reading_expected {
   got_count++;
   got["max_dist"][got_count] = $5;
   got["energy"][got_count] = $6;
   got["dtw"][got_count] = $7;
}

END {
   if (got_count != expected_count) {
      printf "Expected %d values, got %d\n", expected_count, got_count;
      exit 1;
   }
   inf = 1e38.0;
   max_dist = -inf;
   energy = -inf;
   dtw = -inf;
   for (i = 1; i < got_count; ++i) {
      max_dist = geterr(max_dist, expected["max_dist"][i], got["max_dist"][i]);
      energy = geterr(energy, expected["energy"][i], got["energy"][i]);
      dtw = geterr(dtw, expected["dtw"][i], got["dtw"][i]);
   }
   print "Max distance error: ", max_dist
   print "Energy error: ", energy
   print "DTW error: ", dtw
}
