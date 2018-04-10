# !/bin/sh
npm run xhr2-test -- http://localhost:3939/
forever start test-server.js
pulp test
forever stop test-server.js
npm run xhr2-test -- --revert
