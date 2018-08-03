const WebSocket = require('ws');
const wss = new WebSocket.Server({ port: 7080 });
wss.on('connection', function connection(ws) {
  console.warn('connection')
  ws.on('message', function (raw) {
    const msg = JSON.parse(raw) 
    console.warn('message', msg)
    ws.send(JSON.stringify(msg));
  })
});
