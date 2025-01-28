(define (vector-reduce f vec index acc)
  (if (<= index

(define (simpson y dx)
  (letrec [(helper 
    using Real = Channel_value_type<decltype(y)>;
    constexpr Real denominator{12};
    constexpr Real weight_1{5};
    constexpr Real weight_2{8};
    Real result{};
    for (gsl::index i = 2; i < std::ssize(y); i += 2) { result += y[i - 2] + 4 * y[i - 1] + y[i]; }
    result = result * dx / 3;
    if (std::ssize(y) > 2 and std::ssize(y) % 2 == 0) {
      result += dx *
                (weight_1 * y[std::ssize(y) - 1] + weight_2 * y[std::ssize(y) - 2] -
                 y[std::ssize(y) - 3]) /
                denominator;
    }
    return result;
  ())
