const express = require('express');

const app = express();

const balances = {
  '39ToPLh2uWGFHNyZDMkVhw6GpXfu3WpSro': 37699570,
  '3DQYiHbMSHNEPdfMn7wo5tx6MuDnmBpKvf': 1491903,
  '33F7Ab1e7t82u8ZY8hCVFU8YDHXJQwF4nF': 0
};

app.get('/https://blockchain.info/q/addressbalance/:address', function(req, res) {
  const balance = balances[req.params.address] || 0;
  return res.set('content-type', 'text/plain').send(balance.toString());
});

app.listen(3939);
