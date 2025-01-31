let solveDivisor n =
begin
    let divisorList = [];
    for i as num in
        1 ~ (n + 1) do
    begin
        let remainder = n % i;
        if remainder == 0 then
            divisorList += [i]
    end;
    divisorList
end;

print "Primality checker\n";
while let n =
    input ">> " as num loop
begin
    let divisors = solveDivisor n;
    print
    if divisors == [1, n]
        then f"TRUE: {n} is prime number"
        else f"FALSE: {n} is diviable by {divisors}"
    , "\n";
end
