#!/usr/bin/env bash
set -euo pipefail
REPO="$(cd "$(dirname "$0")/.." && pwd)"
RSCRIPT="$(command -v Rscript)"
PLIST="$HOME/Library/LaunchAgents/is.metill.sports.autoplace.plist"
TEMPLATE="$REPO/tools/launchd/is.metill.sports.autoplace.plist.template"

case "${1:-install}" in
  install)
    mkdir -p "$HOME/Library/LaunchAgents"
    sed -e "s#__REPO__#$REPO#g" -e "s#__RSCRIPT__#$RSCRIPT#g" \
        -e "s#__HOME__#$HOME#g" "$TEMPLATE" > "$PLIST"
    launchctl bootout "gui/$(id -u)/is.metill.sports.autoplace" 2>/dev/null || true
    launchctl bootstrap "gui/$(id -u)" "$PLIST"
    echo "Installed + loaded. Tail: ~/Library/Logs/sports-autoplace.log"
    echo "Kill switch: touch $REPO/data/AUTO_PLACE_DISABLED"
    ;;
  uninstall)
    launchctl bootout "gui/$(id -u)/is.metill.sports.autoplace" 2>/dev/null || true
    rm -f "$PLIST"
    echo "Uninstalled."
    ;;
  *) echo "usage: $0 [install|uninstall]"; exit 1 ;;
esac
