let fizzbuzz n =
begin
  if n % 15 == 0 then "FizzBuzz"
  else if n % 3 == 0 then "Fizz"
  else if n % 5 == 0 then "Buzz"
  else n
end;

for i = 1 ~ 101 do
  print f"{fizzbuzz i}\n"
