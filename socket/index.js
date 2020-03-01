const { Socket } = require('net');

const host = 'localhost';
const port = 4203;
const url = `${host}:${port}`;

const socket = new Socket(url);
socket.connect(port, host, () => {
  console.log(`Connected to ${url}`);

  const packet1 = {
    id: 1,
    action: 'connect',
    params: null
  };

  const packet2 = {
    id: 1,
    action: 'echo',
    params: "Hello, Socket!"
  };

  const packet3 = {
    id: 1,
    action: 'unknown command',
    params: null
  };

  const packet4 = 'invalid command structure';

  socket.write(JSON.stringify(packet1));
  setTimeout(() => {
    socket.write(JSON.stringify(packet2));
    setTimeout(() => {
      socket.write(JSON.stringify(packet3));
      setTimeout(() => {
        socket.write(JSON.stringify(packet4));
      }, 1000);
    }, 1000);
  }, 1000);
});

socket.on('data', msg => {
  console.log(msg + '');
});

socket.on('close', () => {
  console.log(`Connection closed ${url}`);
});
