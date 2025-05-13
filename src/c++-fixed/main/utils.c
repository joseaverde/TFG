void black_void (void*) {}

#define SCALE 32
#define SAMPLE_LAST 10000

static int curr_num = -SAMPLE_LAST;
void eeg_read_sample(int *value) {
  *value = curr_num;
  if (++curr_num > SAMPLE_LAST) { curr_num = -SAMPLE_LAST; }
}
