let solvePi iter =
begin
    let [a, p] = [1.0] * 2;
    let b = 1.0 / (2 ^ 0.5);
    let t = 1.0 / 4.0;

    for _ = 0~iter do
    begin
        let aNext = (a + b) / 2;
        t -= (p * (a - aNext) ^ 2);
        b := (a * b ^ 0.5);
        a := aNext;
        p *= 2;
    end;
    (a+b ^ 2) / (4 * t)
end;

let pi = solvePi 999;
effect print f"π = {pi}\n";
