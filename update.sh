#!/bin/bash

rm -f index.html main.bc.js
cp ../sdui4/_build/default/bin/{index.html,main.bc.js} ./ 
git add -A
git commit -m "_"
git push
