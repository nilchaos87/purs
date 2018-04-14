#!/bin/bash

set -e

cd app
git init

git config user.name "nilchaos87"
git config user.email "justthebit@inorbit.com"

git add .
git commit -m "Deploy to GitHub Pages"

git push --force --quiet "https://nilchaos87@github.com/nilchaos87/purs.git" master:gh-pages > /dev/null 2>&1
