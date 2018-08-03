import * as WebSocket from 'ws';
const port = 7080
const wss = new WebSocket.Server({port})
  .on('listening', () => console.log(`listening on port ${port}`))
wss.on('connection', ws => {
  console.warn('connection')
  ws.on('message', raw => {
    const msg = JSON.parse('' + raw) 
    console.warn('message', msg)
    ws.send(JSON.stringify(msg));
  })
});
