#!/bin/bash
set -e

# Detect if running in Claude Code Web
if [ -n "$CLAUDE_CODE_REMOTE" ]; then
    echo "Setting up Erlang environment for Claude Code Web..."

    # Install build dependencies for Erlang
    echo "Installing build dependencies..."
    apt-get update -qq || true
    apt-get install -y build-essential libssl-dev libncurses5-dev unixodbc-dev curl git autoconf 2>&1 | grep -v "^Reading state information" || true

    # Install asdf if not present
    if [ ! -d "$HOME/.asdf" ]; then
        echo "Installing asdf..."
        git clone https://github.com/asdf-vm/asdf.git ~/.asdf --branch v0.14.1 2>&1 | grep -v "^Cloning" || true
    fi

    # Source asdf
    . ~/.asdf/asdf.sh

    # Add asdf to CLAUDE_ENV_FILE for persistence
    if [ -n "$CLAUDE_ENV_FILE" ]; then
        echo ". ~/.asdf/asdf.sh" >> "$CLAUDE_ENV_FILE"
        echo "export PATH=~/.local/bin:\$PATH" >> "$CLAUDE_ENV_FILE"
    fi

    # Add Erlang plugin if not present
    if ! asdf plugin list | grep -q erlang; then
        echo "Adding asdf Erlang plugin..."
        asdf plugin add erlang https://github.com/asdf-vm/asdf-erlang.git 2>/dev/null || true
    fi

    # Install Erlang versions from .tool-versions
    if [ -f .tool-versions ]; then
        ERLANG_VERSION=$(grep erlang .tool-versions | awk '{print $2}')
        if [ -n "$ERLANG_VERSION" ]; then
            if ! asdf list erlang 2>/dev/null | grep -q "$ERLANG_VERSION"; then
                echo "Installing Erlang $ERLANG_VERSION (this may take a while)..."
                asdf install erlang "$ERLANG_VERSION"
            else
                echo "Erlang $ERLANG_VERSION already installed"
            fi
        fi
    fi

    # Build rebar3 from source if not present in ~/.local/bin
    if [ ! -f ~/.local/bin/rebar3 ]; then
        echo "Building rebar3 from source..."
        cd /tmp
        rm -rf rebar3
        # Use shallow clone for faster download
        git clone --depth 1 --branch 3.26.0 https://github.com/erlang/rebar3.git 2>&1 | grep -v "^Cloning" || true
        cd rebar3
        ./bootstrap 2>&1 | grep -E "(Compiling|Building)" || true
        mkdir -p ~/.local/bin
        cp rebar3 ~/.local/bin/
        chmod +x ~/.local/bin/rebar3
        cd - > /dev/null
    else
        echo "Rebar3 already installed in ~/.local/bin"
    fi

    # Add ~/.local/bin to PATH
    export PATH=~/.local/bin:$PATH

    echo "Erlang environment setup complete!"

    # Verify installation
    echo "Erlang version: $(erl -noshell -eval 'io:format("~s", [erlang:system_info(otp_release)]), halt().' 2>/dev/null || echo 'not installed')"
    echo "Rebar3 version: $(rebar3 version 2>&1 | grep -E '^rebar [0-9]' || echo 'not installed')"
else
    echo "Not in Claude Code Web environment, skipping Erlang setup"
fi
