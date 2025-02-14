effect load stdlib;

let solveDivisors n =
    if let x =
        List.filter (\x. n % x == 0) (1~(n+1) as list)
    then x else [];
bind solveDivisors = fn(num -> list);

effect let main _ =
begin
    print "Primality checker\n";
    while let n = input ">> " as num do
    if divisors:=(solveDivisors n) == [1, n] then
        print f"TRUE: {n} is prime number\n"
    else
    begin
        let divisors = List.join divisors ", ";
        print f"FALSE: {n} is divisible by {divisors}\n"
    end
end
