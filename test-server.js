const express = require('express');

const app = express();

const balances = {
  '39ToPLh2uWGFHNyZDMkVhw6GpXfu3WpSro': 37699570,
  '3DQYiHbMSHNEPdfMn7wo5tx6MuDnmBpKvf': 1491903,
  '33F7Ab1e7t82u8ZY8hCVFU8YDHXJQwF4nF': 0,
  '38dhb2njTenb1U5BC3yW4vmGKSjqdUWeb2': 0,
  '3N9cQFpVFkicyMsWijS7KbNzkTwAXM95KB': 0,
  '3FQit9C3Sz8BzCzkuuDkm1m1zvXpNSPMeR': 0,
  '3NMaYkPgR2UGJJgnjDczmQ1D47TX26gUg1': 214397,
  '3Mh5wpFecbpWcvPaTcQL1Y4pkNAt9X2yJa': 0,
  '38xBfbhmCxL7MUhwLT7LVJAXQrm5tL89LS': 0,
  '3Fen57sz4Suxsy7zmkPddkk4mtENbp4VxU': 0,
  '3Hd7M5123pAVrPyeLg5cuMZy35kUTFyFMf': 0,
  '34gbTMxhTXXPVmyq9L2mgjfndY6KHUh9WR': 0,
  '3LcYQgPFJiDddpV4gxs4c8AW5mnkLjcCRN': 1
};

app.get('/https://blockchain.info/q/addressbalance/:address', function(req, res) {
  const balance = balances[req.params.address] || 0;
  return res.set('content-type', 'text/plain').send(balance.toString());
});

app.listen(3939);
