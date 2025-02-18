#include "common.hpp"

int main () {
  constexpr Real d_th = 1.05;
  std::cout << "C++ ftests " << stride_size << " " << epochs_per_stride
    << " " << REAL_NAME << std::endl;
  auto const batch = read_batch();
  auto const signal = read_signal();
  auto const psd = Seizure::psd_default_ranges<Real>(epoch_size);
  Seizure::DTW_params const dtw{epoch_size, stride_size, warping_window};
  std::vector<Real> dists(batch.Pj.size(), 0);
  for (auto && epoch : signal | Seizure::sliding_window_view(epoch_size, stride_size)) {
    auto const psds = Seizure::call_psd(epoch, epoch_size, stride_size, psd);
    auto const energy = Seizure::call_energy(epoch, epoch_size, stride_size);
    auto const max_dist = Seizure::call_max_dist(epoch, epoch_size, stride_size);
    bool any = false;
    for (std::size_t i = 0; i < batch.Pj.size(); ++i) {
      dists[i] = Seizure::get_distance_matrix<Seizure::DTW_method::Partial_euclidean_distance>(epoch, batch.Pj[i], dtw)[0];
      any = any or dists[i] <= batch.d_max_c * d_th;
    }
    bool const is_it = any
                   and Seizure::within(psds[0][0], batch.psd_1)
                   and Seizure::within(psds[1][0], batch.psd_2)
                   and Seizure::within(psds[2][0], batch.psd_3)
                   and Seizure::within(energy[0], batch.energy)
                   and Seizure::within(max_dist[0], batch.max_dist);
    std::cout << (is_it ? "True" : "False")
              << " " << psds[0][0]
              << " " << psds[1][0]
              << " " << psds[2][0]
              << " " << energy[0]
              << " " << max_dist[0];
    for (auto dist : dists) { std::cout << " " << dist; }
    std::cout << "\n";
  }
  return 0;
}
