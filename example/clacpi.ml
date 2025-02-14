let solvePi iter =
begin
    let [a, p] = [1.0] * 2;
    let b = 1.0 / (2 ^ 0.5);
    let t = 1.0 / 4.0;

    for _ = 0~iter do
    begin
        let an = (a + b) / 2;
        t -= (p * (a - an) ^ 2);
        b := (a * b ^ 0.5);
        a := an;
        p *= 2;
    end;
    (a+b ^ 2) / (4 * t)
end;

let pi = solvePi 1000;
effect print f"π = {pi}\n";
