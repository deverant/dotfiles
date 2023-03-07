venv() {
  local VENVS=(.venv)
  for venv in ${VENVS};
  do
    local V="$(pwd)/${venv}"
    local ACTIVATE="${V}/bin/activate"
    if [ -r "${ACTIVATE}" ];
    then
      echo "Found a virtualenv (${V}), activating.."
      source "${ACTIVATE}"
      return 0
    fi
  done

  echo "No virtualenv found, creating a new one ($(pwd)/.venv).."
  python3 -mvenv .venv
  source "$(pwd)/.venv/bin/activate"
}
