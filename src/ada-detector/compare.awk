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
   expected["psd_1"][expected_count] = $3;
   expected["psd_2"][expected_count] = $4;
   expected["psd_3"][expected_count] = $5;
   expected["max_dist"][expected_count] = $6;
   expected["energy"][expected_count] = $7;
   expected["dtw"][expected_count] = $8;
}

/^[FT]/ && ! reading_expected {
   got_count++;
   got["psd_1"][got_count] = $2;
   got["psd_2"][got_count] = $3;
   got["psd_3"][got_count] = $4;
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
   max_dist = 0;
   energy = 0;
   dtw = 0;
   psd_1 = 0;
   psd_2 = 0;
   psd_3 = 0;
   for (i = 1; i < got_count; ++i) {
      max_dist += error(expected["max_dist"][i], got["max_dist"][i]);
      energy += error(expected["energy"][i], got["energy"][i]);
      dtw += error(expected["dtw"][i], got["dtw"][i]);
      psd_1 += error(expected["psd_1"][i], got["psd_1"][i]);
      psd_2 += error(expected["psd_2"][i], got["psd_2"][i]);
      psd_3 += error(expected["psd_3"][i], got["psd_3"][i]);
   }
   print "Max distance error: ", max_dist / got_count
   print "Energy error: ", energy / got_count
   print "DTW error: ", dtw / got_count
   print "PSD 1 error: ", psd_1 / got_count
   print "PSD 2 error: ", psd_2 / got_count
   print "PSD 3 error: ", psd_3 / got_count
}
