fs = require('fs')

// Start the Elm application.
var app = Elm.Main.init({
    node: document.getElementById('myapp')
});

app.ports.requestRead.subscribe(function(path) {
    fs.readFile(path, 'utf8' , (err, data) => {
        result = err?
            {"ok": false, "data": err} :
            {"ok": true, "data": data};
        // console.log(JSON.stringify(result));
        app.ports.receiveRead.send(JSON.stringify(result));
    })
});
