#!/bin/bash
#
# SessionStart hook for Claude Code on the web.
#
# The remote container ships without an Erlang toolchain, but this project
# needs OTP >= 27 (see rebar.config's minimum_otp_vsn) plus rebar3 to run
# `make build-test`, `make hank`, `make proper`, etc.
#
# GitHub release downloads are blocked by the egress policy, so we pull:
#   * a precompiled OTP from builds.hex.pm (reachable)
#   * the rebar3 escript from the official S3 CDN (reachable)
#
# The install is idempotent and the container state is cached after the hook
# completes, so later sessions reuse the already-installed toolchain.
set -euo pipefail

# Only run in the remote (Claude Code on the web) environment.
if [ "${CLAUDE_CODE_REMOTE:-}" != "true" ]; then
  exit 0
fi

OTP_VERSION="27.3.4.14"
OTP_DIR="/opt/otp"
# Ubuntu 24.04 (noble) is what the remote container runs.
OTP_BUILD="ubuntu-24.04"

log() { echo "[session-start] $*"; }

# --- Erlang/OTP -------------------------------------------------------------
if [ -x "$OTP_DIR/bin/erl" ] && \
   [ "$("$OTP_DIR/bin/erl" -noshell -eval 'io:format("~s",[erlang:system_info(otp_release)]),halt().' 2>/dev/null)" -ge 27 ] 2>/dev/null; then
  log "OTP already installed at $OTP_DIR, skipping."
else
  log "Installing Erlang/OTP $OTP_VERSION ..."
  tmp="$(mktemp -d)"
  curl -fsSL -o "$tmp/otp.tar.gz" \
    "https://builds.hex.pm/builds/otp/${OTP_BUILD}/OTP-${OTP_VERSION}.tar.gz"
  sudo rm -rf "$OTP_DIR"
  sudo mkdir -p "$OTP_DIR"
  sudo tar -xzf "$tmp/otp.tar.gz" -C "$OTP_DIR" --strip-components=1
  # Precompiled OTP is relocatable; its Install script fixes up ROOTDIR paths.
  ( cd "$OTP_DIR" && sudo ./Install -minimal "$OTP_DIR" >/dev/null )
  sudo ln -sf "$OTP_DIR/bin/erl"     /usr/local/bin/erl
  sudo ln -sf "$OTP_DIR/bin/erlc"    /usr/local/bin/erlc
  sudo ln -sf "$OTP_DIR/bin/escript" /usr/local/bin/escript
  rm -rf "$tmp"
  log "OTP installed."
fi

# --- rebar3 -----------------------------------------------------------------
if [ -x /usr/local/bin/rebar3 ] && \
   PATH="$OTP_DIR/bin:$PATH" /usr/local/bin/rebar3 version >/dev/null 2>&1; then
  log "rebar3 already installed, skipping."
else
  log "Installing rebar3 ..."
  tmp="$(mktemp -d)"
  curl -fsSL -o "$tmp/rebar3" "https://s3.amazonaws.com/rebar3/rebar3"
  chmod +x "$tmp/rebar3"
  sudo mv "$tmp/rebar3" /usr/local/bin/rebar3
  rm -rf "$tmp"
  log "rebar3 installed."
fi

# --- Persist PATH for the session -------------------------------------------
# Put OTP first so its erl/erlc win over any apt-provided (older) Erlang.
if [ -n "${CLAUDE_ENV_FILE:-}" ]; then
  echo "export PATH=\"$OTP_DIR/bin:\$PATH\"" >> "$CLAUDE_ENV_FILE"
fi

log "Toolchain ready: $(PATH="$OTP_DIR/bin:$PATH" erl -noshell -eval 'io:format("OTP ~s",[erlang:system_info(otp_release)]),halt().') / $(PATH="$OTP_DIR/bin:$PATH" /usr/local/bin/rebar3 version 2>/dev/null | head -1)"
