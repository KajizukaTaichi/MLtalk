effect let handle req =
begin
    print req;
    let name = req/"HTTP" :: 0 - "GET /";
    f"
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

effect httpServer handle
