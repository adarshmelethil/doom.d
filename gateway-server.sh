#!/usr/bin/env zsh

PROJECTS_DIR="${HOME}/work/src/github.com/adarsh-emburse"
GATEWAY_DIR="${PROJECTS_DIR}/cards-gateway"
LOCAL_DIR="${PROJECTS_DIR}/gateway-local"

cd $GATEWAY_DIR || exit 1
eval "$(direnv export bash 2>/dev/null)"

exec python "${LOCAL_DIR}/emacs/server.py" "${GATEWAY_DIR}"
