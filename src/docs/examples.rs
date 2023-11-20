//! # Examples of `mechylang` code

/// # Summing Primes
///
/// ## Objective
///
/// Create a program that sums all prime numbers up to (and including) a given number.
///
/// ## Solution
/// ```rust
/// # mechylang::test_utils::test_eval_ok(r#"
/// main()
/// fn sum_primes(n) {
///     // Create a list of all primes found so far
///     let primes = [2];
///
///     if n < 2 {
///         return 0;
///     }
///
///     if n == 2 {
///         return 2;
///     }
///         
///
///     let sum = 2;
///
///     // Loop through all numbers from 3 to n
///     for i in (3..=n).iter().step_by(2) {
///         // Loop through all primes found so far
///         let is_prime = for prime in primes {
///             // If the number is divisible by a prime, it is not prime
///             if i % prime == 0 {
///                 break false;
///             }
///         } else {
///             // This is only reached if the loop is not broken
///             true
///         }
///
///         // If the number is prime, add it to the list
///         if is_prime {
///             primes.push(i);
///             sum += i;
///         }
///     }
///
///     sum
/// }
///
///
/// fn main() {
///     assert_eq(sum_primes(0), 0);
///     assert_eq(sum_primes(1), 0);
///     assert_eq(sum_primes(2), 2);
///     assert_eq(sum_primes(3), 5);
///     assert_eq(sum_primes(4), 5);
///     assert_eq(sum_primes(5), 10);
///     assert_eq(sum_primes(10), 17);
///     assert_eq(sum_primes(100), 1060);
///     assert_eq(sum_primes(1000), 76127);
/// }
/// # "#);
///

pub struct SummingPrimes;
