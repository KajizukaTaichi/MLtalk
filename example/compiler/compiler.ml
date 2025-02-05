effect load std;

let SPACE as list = [" ", "　", "\n", "\t", "\r"];
let INDENT as str = SPACE[0] * 12;

let parseExpr source =
begin
    let tokenList = tokenize SPACE source;
    let token = tokenList[length tokenList - 1];
    let token = begin
        if let number = token as num then
            { class: #Value, value: number }
        else if token[0] == "\(" & (token[length token - 1] == "\)") then
            parseExpr token[1 ~ (length token - 1)]
        else if token[0] == "λ" & token["."] then
        begin
            let token = token - "λ" / ".";
            let [args, body] = [token[0], List.join token[1 ~ length(token)] "."];
            { class: #Lambda, value: [args, parseExpr body] }
        end
        else if token[0] == "\\" & token["."] then
        begin
            let token = token - "\\" / ".";
            let [args, body] = [token[0], List.join token[1 ~ length(token)] "."];
            { class: #Lambda, value: [args, parseExpr body] }
        end
        else
            { class: #Symbol, value: token }
    end;
    if length tokenList >= 2 then
    begin
        let hasLhs x = parseExpr(List.join tokenList[0 ~ (length tokenList - x)] SPACE[0]);
        let operatorGen x = { class: x, value: [hasLhs 2, token] };
        let operToken = tokenList[length tokenList - 2];
        if operToken == "+" then
            operatorGen #Add
        else if operToken == "-" then
            operatorGen #Sub
        else if operToken == "*" then
            operatorGen #Mul
        else if operToken == "/" then
            operatorGen #Div
        else if operToken == "|>" then
            { class: #Apply, value: [token, hasLhs 2] }
        else
            { class: #Apply, value: [hasLhs 1, token] }
    end
    else
        token
end;

let tokenize delimiter source =
begin
    let [tokens, current] = [[], ""];
    let [nest, quote] = [0] * 2;

    for c = source do
    begin
        if ["\(", "\[", "\{"] :: [c] & (quote == 0) then
        begin
            current += c;
            nest += 1;
        end
        else if ["\)", "\]", "\}"] :: [c] & (quote == 0) then
        begin
            current += c;
            nest -= 1;
        end
        else if ["\"", "\'", "\`"] :: [c] then
        begin
            let quote = begin
                if quote == 1
                    then 0
                    else 1
            end;
            current += c;
        end
        else if delimiter :: [c] then
        begin
            if (nest == 0) & (quote == 0) then
            begin
                if current != "" then
                begin
                    tokens += [current];
                    let current = "";
                end
            end
            else
                current += c
        end
        else
            current += c;
    end;

    if current != "" then
        tokens += [current];

    tokens
end;

let codeGenOpr c f x =
begin
    [count, funcs, ast] := [c, f, x];
    if type ast == #Value then
        [count, funcs, ast.value as str]
    else if type ast == #Lambda then
    begin
        let [arg, body] = ast.value; count += 1;
        let [fncCnt, funcs, body] = codeGenOpr 0 funcs body;
        let name = f"@lambda_{count}";
        funcs += f"\n
        define i32 {name}(i32 %{arg}) \{
            {body}
            ret i32 %r{fncCnt}
        \}";
        [count, funcs, name]
    end
    else if type ast == #Symbol then
        [count, funcs, f"%{ast.value}"]
    else
    begin
        let [count, funcs, lhs] = codeGenOpr count funcs ast.value[0]; lhsCnt := count;
        let [count, funcs, rhs] = codeGenOpr count funcs ast.value[1]; rhsCnt := count;

        let prepareMnemonic x y z =
        begin
            let [term, cnt, beforeCode] = [x, y, z];
            if term["="] then
            begin
                beforeCode += f"{term}\n{INDENT}";
                let term = f"%r{cnt}";
            end;
            [term, beforeCode]
        end;

        count += 1;
        let beforeCode = "";
        let [lhs, beforeCode] = prepareMnemonic lhs lhsCnt beforeCode;
        let [rhs, beforeCode] = prepareMnemonic rhs rhsCnt beforeCode;

        [count, funcs, begin
            if type ast == #Apply then
                f"{beforeCode}%r{count} = call i32 {lhs}(i32 {rhs})"
            else
            begin
                let op = begin
                    if type ast == #Add then "add"
                    else if type ast == #Sub then "sub"
                    else if type ast == #Mul then "mul"
                    else if type ast == #Div then "sdiv"
                end;
                f"{beforeCode}%r{count} = {op} i32 {lhs}, {rhs}"
            end
        end]
    end
end;

let compile x =
begin
    let [c, funcs, ast] = codeGenOpr 0 "" (parseExpr x);
    let outputCode = f"
        @format_str = private constant [3 x i8] c\"%d\\0A\"
        declare i32 @printf(i8* %format, ...)
        {funcs}

        define i32 @main(i32 %argc, i8** %argv) \{
            {ast}
            call i32 @printf(i8* bitcast ([3 x i8]* @format_str to i8*), i32 %r{c})
            ret i32 0
        \}
    ";
    outputCode
end;

if let script = cmdLineArgs[0] then
    effect print compile (readFile script), "\n"
else
    effect print "Lamutac: the Lamuta\'s compiler\n"
