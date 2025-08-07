#!/bin/zsh

git pull origin $(git branch --show-current)
docker compose down
docker compose up