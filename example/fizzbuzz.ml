effect load stdlib;

let getFizzbuzz n =
begin
    let divisible = (\x. n % x == 0);
    if divisible 15 then "FizzBuzz"
    else if divisible 3 then "Fizz"
    else if divisible 5 then "Buzz"
    else n
end;
bind getFizzbuzz = fn(num -> num | str);

lazy let fizzbuzzList = List.map getFizzbuzz (1~101 as list);
effect print (List.join fizzbuzzList "\n")
