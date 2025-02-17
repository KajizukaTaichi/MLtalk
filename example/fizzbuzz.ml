effect load stdlib;

let getFizzbuzz n =
match [n % 3, n % 5] with
    [0, 0] => "FizzBuzz",
    [0, _] => "Fizz",
    [_, 0] => "Buzz",
    [_, _] => n;
bind getFizzbuzz = fn(num -> num | str);

lazy let fizzbuzzList = List.map getFizzbuzz (1~101 as list);
effect print (List.join fizzbuzzList "\n")
