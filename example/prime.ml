effect load stdlib;
let solveDivisors n = List.filter (\x. n % x == 0) (1~(n+1) as list);

effect print "Primality checker\n";
while let n = input ">> " as num do
begin
    if let divisors = solveDivisors n then
    effect print
    begin
        if divisors == [1, n]
            then f"TRUE: {n} is prime number\n"
            else f"FALSE: {n} is diviable by {divisors}\n"
    end
end
