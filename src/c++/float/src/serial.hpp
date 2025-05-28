#ifndef SEIZURE_SERIAL_HPP
#define SEIZURE_SERIAL_HPP

#include <bit>
#include <concepts>
#include <iostream>
#include <array>
#include <cstdint>
#include <algorithm>

namespace Seizure {
  template <typename Stream>
  concept Writable_stream = requires (Stream & stream, const char * str, std::size_t size) {
    { stream.write(str, size) }; // -> std::convertible_to<bool>;
  };

  template <typename Stream>
  concept Readable_stream = requires (Stream & stream, char * str, std::size_t size) {
    { stream.read(str, size) }; // -> std::convertible_to<bool>;
  };

  // Write

  template <Writable_stream Stream, typename T>
  bool write (Stream & out, T const & value);

  template <Writable_stream Stream>
  bool write (Stream & out, std::byte const & value) {
    const char byte = std::bit_cast<char>(value);
    return static_cast<bool>(out.write(&byte, sizeof(char)));
  }

  template <Writable_stream Stream>
  bool write (Stream & out, bool const & value) {
    return write(out, static_cast<std::byte>(value));
  }

  template <Writable_stream Stream, std::integral T>
  bool write (Stream & out, T const & value) {
    std::array<std::int32_t, 1> const data{static_cast<std::int32_t>(value)};
    auto const bytes = std::as_bytes(std::span{data});
    if constexpr (std::endian::native == std::endian::big) {
      for (auto byte : std::ranges::reverse_view(bytes)) {
        if (not write(out, byte)) { return false; }
      }
    } else if constexpr (std::endian::native == std::endian::little) {
      for (auto byte : bytes) {
        if (not write(out, byte)) { return false; }
      }
    } else {
      // static_assert(false, "Mixed endianness machine");
      for (auto byte : std::ranges::reverse_view(bytes)) {
        if (not write(out, byte)) { return false; }
      }
    }
    return true;
  }

  template <Writable_stream Stream>
  bool write (Stream & out, float const & value) {
    return write(out, std::bit_cast<std::int32_t>(value));
  }

  template <Writable_stream Stream>
  bool write (Stream & out, double const & value) {
    return write(out, static_cast<float>(value));
  }

  template <Writable_stream Stream, Input_channel Channel>
  bool write (Stream & out, Channel const & value) {
    if (not write(out, std::ssize(value))) { return false; }
    for (auto const & item : value) {
      if (not write(out, item)) { return false; }
    }
    return true;
  }

  // Read

  template <Readable_stream Stream, typename T>
  bool read (Stream & in, T & value);

  template <Readable_stream Stream>
  bool read (Stream & in, std::byte & value) {
    char data = 0;
    bool const valid = static_cast<bool>(in.read(&data, sizeof(char)));
    value = std::bit_cast<std::byte>(data);
    return valid;
  }

  template <Readable_stream Stream>
  bool read (Stream & in, bool & value) {
    std::byte data{};
    if (read(in, data)) {
      value = data != std::byte{};
      return true;
    }
    return false;
  }

  template <Readable_stream Stream, std::integral T>
  bool read (Stream & in, T & value) {
    std::array<std::int32_t, 1> data{};
    auto const wview{std::as_writable_bytes(std::span{data})};
    if constexpr (std::endian::native == std::endian::big) {
      for (auto & byte : std::ranges::reverse_view(wview)) {
        if (not read(in, byte)) { return false; }
      }
    } else if constexpr (std::endian::native == std::endian::little) {
      for (auto & byte : wview) {
        if (not read(in, byte)) { return false; }
      }
    } else {
      // static_assert(false, "Mixed endianness machine");
      for (auto & byte : std::ranges::reverse_view(wview)) {
        if (not read(in, byte)) { return false; }
      }
    }
    value = static_cast<T>(*data.begin());
    return true;
  }

  template <Readable_stream Stream>
  bool read (Stream & in, float & value) {
    std::int32_t data = 0;
    if (read(in, data)) {
      value = std::bit_cast<float>(data);
      return true;
    }
    return false;
  }

  template <Readable_stream Stream>
  bool read (Stream & in, double & value) {
    float data = 0.0;
    if (read(in, data)) {
      value = static_cast<double>(data);
      return true;
    }
    return false;
  }

  template <Readable_stream Stream, typename T, typename Iterator>
    requires std::output_iterator<Iterator, T>
  bool read (Stream & in, Iterator & value) {
    std::ptrdiff_t size = 0;
    T buffer;
    if (not read(in, size)) { return false; }
    for (std::ptrdiff_t index = 0; index < size; ++index) {
      // std::cout << "Index = " << index << "/" << size << std::endl;
      if (not read(in, buffer)) { return false; }
      // std::cout << "  " << buffer << "\n";
      *value++ = buffer;
    }
    return true;
  }
}

#endif//SEIZURE_SERIAL_HPP
