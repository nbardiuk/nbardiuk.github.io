#!/usr/bin/env bash

cd _site

git init
git config user.name  'CircleCI'
git config user.email 'job@circleci.com'

git add .
git commit -m "deployment for $CIRCLE_SHA1 [ci skip]"

git remote add origin 'https://github.com/nbardiuk/nbardiuk.github.io.git'
git push --force origin master
