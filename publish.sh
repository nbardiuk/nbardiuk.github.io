#!/usr/bin/env bash

cd _site

git init
git config user.name  'CircleCI'
git config user.email 'job@circleci.com'

git add .
git commit -m "deployment for commit $CIRCLE_SHA1"

git remote add origin 'https://github.com/nbardiuk/nbardiuk.github.io.git'
git push --force origin master
