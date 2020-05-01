.PHONY: watch
watch: clean
	hugo server -D

.PHONY: publish
publish: build
	cd public && \
	git add --all && \
	git commit -m "Publishing to master" && \
	git push

.PHONY: build
build: setup
	hugo
	cd public && \
	git status -vv

.PHONY: setup
setup: clean
	git worktree add -B master public origin/master

.PHONY: clean
clean:
	rm -rf public
	git worktree prune
