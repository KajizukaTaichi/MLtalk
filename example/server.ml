let httpResHead =
    "HTTP/1.1 200 OK\r\nContent-Type: text/html\r\n\r\n";

let handleHttp reqData =
begin
    let name = req/"HTTP"::0 - "GET /";
    f"
        {httpResHead}
        <html lang=\"ja\">
            <head>
                <title>MLtalk server</title>
                <meta charset=\"UTF-8\">
            </head>
            <body>
                <h1>Hello {name}!</h1>
                <p>from the MLtalk\'s server</p>
            </body>
        </html>
    "
end;

effect system.httpServer handleHttp
