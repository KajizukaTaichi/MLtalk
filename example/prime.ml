effect load stdlib;

let solveDivisors n = List.filter (\x. n % x == 0) (1~(n+1) as list);
bind solveDivisors = fn(num -> list);

effect let mainLoop _ =
begin
    print "Primality checker\n";
    while let n = input ">> " as num do
    begin
        if let divisors =
            solveDivisors n then
        print
        begin
            if divisors == [1, n]
                then f"TRUE: {n} is prime number\n"
                else f"FALSE: {n} is diviable by {divisors}\n"
        end
        else
            print "ERROR: input should be greater than 1\n"
    end
end;
