#!/usr/bin/env bash

stack clean && stack build && rm -rf _cache && rm -rf _site && stack exec site build

cd _site

git init
git config user.email 'nazarii@bardiuk.com'

git add .
git commit -m "manual deploy"

git remote add origin 'https://github.com/nbardiuk/nbardiuk.github.io.git'
git push --force origin master
