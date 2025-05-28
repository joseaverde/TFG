#ifndef SEIZURE_BATCH_HPP
#define SEIZURE_BATCH_HPP
#include "concepts.hpp"

#include <limits>
#include <tuple>
#include "serial.hpp"

namespace Seizure {

  template <typename T>
  using Bounds = std::pair<T, T>;

  template <typename T, typename U>
    requires requires(T t, U u) {
      t <=> u;
      u <=> t;
    }
  bool within(T value, Bounds<U> bounds) {
    return value >= bounds.first and value <= bounds.second;
  }

  template <Real_type T, Input_channel C>
  struct Batch {
      Bounds<T> psd_1;
      Bounds<T> psd_2;
      Bounds<T> psd_3;
      Bounds<T> energy;
      Bounds<T> max_dist;
      T d_max_c{std::numeric_limits<T>::infinity()};
      C Pj;
  };

  // I/O for debugging

  template <typename T>
  std::ostream & operator<<(std::ostream & out, Bounds<T> const & bounds) {
    return out << "(" << bounds.first << ", " << bounds.second << ")";
  }

  template <typename T, typename C>
  std::ostream & operator<<(std::ostream & out, Batch<T, C> const & batch) {
    return out << "Batch [\n"
               << "   PSD (1)  : " << batch.psd_1 << "\n"
               << "   PSD (2)  : " << batch.psd_2 << "\n"
               << "   PSD (3)  : " << batch.psd_3 << "\n"
               << "   Distance : " << batch.max_dist << "\n"
               << "   Energy   : " << batch.energy << "\n"
               << "   D_max_c  : " << batch.d_max_c << "\n"
               << "];";
  }

  // Serialisation/Deserialisation

  template <Writable_stream Stream, typename T>
  bool write (Stream & out, Bounds<T> const & value) {
    return write(out, value.first) and write(out, value.second);
  }

  template <Writable_stream Stream, typename T, typename C>
  bool write (Stream & out, Batch<T, C> const & value) {
    return write(out, value.psd_1) and write(out, value.psd_2) and
           write(out, value.psd_3) and write(out, value.max_dist) and
           write(out, value.energy) and write(out, value.d_max_c) and
           write(out, value.Pj);
  }

  template <Readable_stream Stream, typename T>
  bool read (Stream & in, Bounds<T> & value) {
    return read(in, value.first) and read(in, value.second);
  }

  template <Readable_stream Stream, typename T, typename C>
  bool read (Stream & in, Batch<T, C> & value) {
    return read(in, value.psd_1) and read(in, value.psd_2) and
           read(in, value.psd_3) and read(in, value.max_dist) and
           read(in, value.energy) and read(in, value.d_max_c) and
           read(in, value.Pj.begin());
  }

  template <Readable_stream Stream, typename T>
  bool read (Stream & in, Batch<T, std::vector<T>> & value) {
    auto iter = std::back_inserter(value.Pj);
    return read(in, value.psd_1) and read(in, value.psd_2) and
           read(in, value.psd_3) and read(in, value.max_dist) and
           read(in, value.energy) and read(in, value.d_max_c) and
           read<Stream, T>(in, iter);
  }

}  // namespace Seizure

#endif  // SEIZURE_BATCH_HPP
