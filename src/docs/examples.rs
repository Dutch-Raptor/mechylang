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
/// }
/// # "#);
/// ```
pub struct SummingPrimes;

/// # FizzBuzz
///
/// ## Objective
///
/// Create a program that prints the numbers from 1 to 100. But for multiples of three print
/// “Fizz” instead of the number and for the multiples of five print “Buzz”. For numbers which are
/// multiples of both three and five print “FizzBuzz”.
///
/// ## Solution
///
/// ```rust
/// # mechylang::test_utils::test_eval_ok(r#"
/// fn fizzbuzz(n) {
///   let config = [
///     [3, "Fizz"],
///     [5, "Buzz"],
///   ];
///
///   let values = [];
///
///   for i in 1..=n {
///     let output = "";
///
///     for item in config {
///       let divisor = item[0];
///       let word = item[1];
///
///       if i % divisor == 0 {
///         output += word;
///       }
///     }
///
///     values.push(if output == "" { i; } else { output; });
///   }
///
///   return values;
/// };
///
/// assert_eq(
///     fizzbuzz(100),
///     [
///     1, 2, "Fizz", 4, "Buzz", "Fizz", 7, 8, "Fizz", "Buzz", 11, "Fizz", 13, 14,
///     "FizzBuzz", 16, 17, "Fizz", 19, "Buzz", "Fizz", 22, 23, "Fizz", "Buzz",
///     26, "Fizz", 28, 29, "FizzBuzz", 31, 32, "Fizz", 34, "Buzz", "Fizz", 37,
///     38, "Fizz", "Buzz", 41, "Fizz", 43, 44, "FizzBuzz", 46, 47, "Fizz", 49,
///     "Buzz", "Fizz", 52, 53, "Fizz", "Buzz", 56, "Fizz", 58, 59, "FizzBuzz",
///     61, 62, "Fizz", 64, "Buzz", "Fizz", 67, 68, "Fizz", "Buzz", 71, "Fizz",
///     73, 74, "FizzBuzz", 76, 77, "Fizz", 79, "Buzz", "Fizz", 82, 83, "Fizz",
///     "Buzz", 86, "Fizz", 88, 89, "FizzBuzz", 91, 92, "Fizz", 94, "Buzz",
///     "Fizz", 97, 98, "Fizz", "Buzz"
///     ]
/// );
/// # "#);
/// ```
pub struct FizzBuzz;
