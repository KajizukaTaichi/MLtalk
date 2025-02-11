effect load stdlib;

let fizzbuzz n =
begin
    if n % 15 == 0 then "FizzBuzz"
    else if n % 3 == 0 then "Fizz"
    else if n % 5 == 0 then "Buzz"
    else n
end;
bind fizzbuzz = fn(num -> num | str);

effect print (List.join
    (List.map fizzbuzz (1~101 as list)) "\n")
