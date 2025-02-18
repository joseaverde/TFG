#include "common.hpp"

int main () {
  std::cout << "C++ ftests " << stride_size << " " << epochs_per_stride
    << REAL_NAME << std::endl;
  auto batch = read_batch();
  auto signal = read_signal();
  return 0;
}
