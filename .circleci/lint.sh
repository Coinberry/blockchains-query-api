#!/usr/bin/env bash
git ls-files . | grep -e '.*\.hs' | grep -v -e "test-pure" -e "test-impure" | xargs /root/.local/bin/hlint "$@"
