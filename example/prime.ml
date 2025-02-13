effect load stdlib;

let solveDivisors n =
    if let x =
        List.filter (\x. n % x == 0) (1~(n+1) as list)
    then x
    else [];
bind solveDivisors = fn(num -> list);

effect let main _ =
begin
    print "Primality checker\n";
    lazy let prompt = input ">> ";
    while let n = prompt as num do
    begin
        let divisors = solveDivisors n;
        print
        begin
            if divisors == [1, n] then
                f"TRUE: {n} is prime number\n"
            else
                f"FALSE: {n} is diviable by {List.join divisors ", "}\n"
        end
    end
end
