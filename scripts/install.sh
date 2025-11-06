#!/bin/bash

# Install ai-squads globally and locally
# This is a convenience script that runs both install_global.sh and install_in_repo.sh

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

echo "=========================================="
echo "Installing ai-squads"
echo "=========================================="
echo ""
echo "Step 1: Global installation"
echo "------------------------------------------"

# Run global installation
"$SCRIPT_DIR/install_global.sh"

echo ""
echo "Step 2: Local repository installation"
echo "------------------------------------------"

# Run local installation
"$SCRIPT_DIR/install_in_repo.sh"

echo ""
echo "=========================================="
echo "✓ Installation complete!"
echo "=========================================="
echo ""
echo "Next: Run 'Adopt Project' command in Cursor"
