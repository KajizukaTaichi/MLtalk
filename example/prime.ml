let solveDivisors n =
begin
    let divisorList = [];
    for i as num =
        1 ~ (n + 1) do
    begin
        let remainder = n % i;
        if remainder == 0 then
            divisorList += [i]
    end;
    divisorList
end;

effect let mainLoop _ =
begin
    print "Primality checker\n";
    while let n =
        input ">> " as num do
    f"{
        if let divisors =
            solveDivisors n then
        begin
            if divisors == [1, n]
                then f"TRUE: {n} is prime number"
                else f"FALSE: {n} is diviable by {divisors}"
        end
        else
            "ERROR: input should be greater than 1"
    }\n" |> print;
end;

effect mainLoop _
