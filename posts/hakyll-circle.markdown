---
title: "How to Hakyll CircleCI 2.0"
date:  2018-08-19 14:00:00
---

This post documents my
[CircleCI configuration](https://github.com/nbardiuk/nbardiuk.github.io/blob/a2745429997f4f66bd488e330f694c60c4624194/.circleci/config.yml)
that builds Hakyll site and publishes it to GitHub pages.

It is inspired by configurations found on the Internet.

* [Example of caching dependencies for stack](https://pbrisbin.com/posts/haskell_project_checklist/#circleyml) by Pat Brisbin
* [Old publishing script of haskellweekly blog](https://github.com/haskellweekly/haskellweekly.github.io/blob/b9e2f0e70aa0f8fe8d2a427fa6c64ec41a2b7965/tools/deploy.hs) by Taylor Fausak
* [Guide to publish GitHub page with Hakyll and CircleCI 1.0](https://www.stackbuilders.com/news/dr-hakyll-create-a-github-page-with-hakyll-and-circleci) by Juan Pedro Villa

## Overview

<!-- vim-markdown-toc GFM -->

* [Use FPComplete docker image](#use-fpcomplete-docker-image)
* [Cache compiled dependencies](#cache-compiled-dependencies)
* [Generate content](#generate-content)
* [Push `_site` content to GitHub pages](#push-_site-content-to-github-pages)
* [Referrers](#referrers)

<!-- vim-markdown-toc -->


## Use FPComplete docker image

Docker is the simplest [executor type](https://circleci.com/docs/2.0/executor-types/#overview) on CircleCI.

This configuration reuses [FPComplete's image](https://hub.docker.com/r/fpco/stack-build/) with stack to avoid custom installation scripts.

```yaml
    docker:
      - image: fpco/stack-build:lts
```

## Cache compiled dependencies

The job is going to recompile all dependencies on every build because it is
using clean docker image.

It takes around 20 minutes to build template Hakyll blog.
With [caching configuration](https://circleci.com/docs/2.0/caching/) building 
time drops to less than a minute.

```yaml
      - restore_cache:
          name: Restore Cached Dependencies
          keys:
            # find a cache for the same stack.yaml
            - stack-{{ .Branch }}-{{ checksum "stack.yaml" }}
            # when missing reuse from the same branch
            - stack-{{ .Branch }}-
            # when missing reuse the latest cache
            - stack-
      - run:
          name: Resolve/Update Dependencies
          command: stack setup
          command: stack build --dependencies-only
      - save_cache:
          name: Cache Dependencies
          key: stack-{{ .Branch }}-{{ checksum "stack.yaml" }}
          paths:
            - ~/.stack
            - ./.stack-work
```


## Generate content

Builds `site` application and uses it generate static content

```yaml
      - run:
          name: Build Site App
          command: stack build --pedantic
      - run:
          name: Generate Static Site
          command: stack exec site build
```


## Push `_site` content to GitHub pages

Here `master` branch is not used for version control but rather as production
environment so it makes more sense to keep it clean and wipe previous site 
content by `git push --force`

```yaml
      - run:
          name: Publish GitHub Pages
          working_directory: './_site'
          command: |
            # initalize repo
            git init
            git config user.name  'CircleCI'
            git config user.email 'job@circleci.com'
            # add generated files
            git add .
            git commit -m "publish $CIRCLE_SHA1 [ci skip]"
            # push to pages branch
            git remote add origin "$CIRCLE_REPOSITORY_URL"
            git push --force origin master
```

## Referrers

* [`@nbardiuk`](https://twitter.com/nbardiuk/status/1031151015175831553)
* [r/haskell](https://www.reddit.com/r/haskell/comments/98jtii/how_to_hakyll_circleci_20/)
