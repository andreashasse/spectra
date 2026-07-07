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
# SHA256 of the OTP-${OTP_VERSION}.tar.gz artifact, taken from builds.hex.pm's
# builds.txt manifest. Verified before extraction so we never install (and run
# the ./Install script from) a tampered or truncated download.
OTP_SHA256="0d3fc311e126856b9494f8b3e5c4630091968863186f59a7a187f49d5b878b3d"

# Clean up any temp dirs we create, even on failure.
TMPDIRS=()
cleanup() { local d; for d in "${TMPDIRS[@]:-}"; do [ -n "$d" ] && rm -rf "$d"; done; return 0; }
trap cleanup EXIT

log() { echo "[session-start] $*"; }

# curl with retries so a transient network blip doesn't hard-fail the session.
fetch() { curl -fsSL --retry 5 --retry-delay 2 --retry-connrefused "$@"; }

# Only reach for sudo when we're not already root; minimal containers (or a
# root session user) may not have sudo installed at all.
if [ "$(id -u)" -eq 0 ]; then SUDO=""; else SUDO="sudo"; fi

# --- Erlang/OTP -------------------------------------------------------------
if [ -x "$OTP_DIR/bin/erl" ] && \
   [ "$("$OTP_DIR/bin/erl" -noshell -eval 'io:format("~s",[erlang:system_info(otp_release)]),halt().' 2>/dev/null)" -ge 27 ] 2>/dev/null; then
  log "OTP already installed at $OTP_DIR, skipping."
else
  log "Installing Erlang/OTP $OTP_VERSION ..."
  tmp="$(mktemp -d)"; TMPDIRS+=("$tmp")
  fetch -o "$tmp/otp.tar.gz" \
    "https://builds.hex.pm/builds/otp/${OTP_BUILD}/OTP-${OTP_VERSION}.tar.gz"
  echo "${OTP_SHA256}  ${tmp}/otp.tar.gz" | sha256sum -c - >/dev/null || {
    log "ERROR: OTP checksum mismatch, aborting."; exit 1;
  }
  $SUDO rm -rf "$OTP_DIR"
  $SUDO mkdir -p "$OTP_DIR"
  $SUDO tar -xzf "$tmp/otp.tar.gz" -C "$OTP_DIR" --strip-components=1
  # Precompiled OTP is relocatable; its Install script fixes up ROOTDIR paths.
  ( cd "$OTP_DIR" && $SUDO ./Install -minimal "$OTP_DIR" >/dev/null )
  $SUDO ln -sf "$OTP_DIR/bin/erl"     /usr/local/bin/erl
  $SUDO ln -sf "$OTP_DIR/bin/erlc"    /usr/local/bin/erlc
  $SUDO ln -sf "$OTP_DIR/bin/escript" /usr/local/bin/escript
  log "OTP installed."
fi

export PATH="$OTP_DIR/bin:$PATH"

# --- rebar3 -----------------------------------------------------------------
# The versioned rebar3 URLs live on GitHub (blocked by egress policy), so we
# use the official unversioned S3 escript, which always serves latest stable.
# We don't pin a version; instead we verify the escript actually runs under our
# OTP before trusting it.
if [ -x /usr/local/bin/rebar3 ] && rebar3 version >/dev/null 2>&1; then
  log "rebar3 already installed ($(rebar3 version 2>/dev/null))."
else
  log "Installing rebar3 ..."
  tmp="$(mktemp -d)"; TMPDIRS+=("$tmp")
  fetch -o "$tmp/rebar3" "https://s3.amazonaws.com/rebar3/rebar3"
  chmod +x "$tmp/rebar3"
  # Sanity-check the escript before installing it into a system location.
  if ! "$tmp/rebar3" version >/dev/null 2>&1; then
    log "ERROR: downloaded rebar3 escript did not run, aborting."; exit 1
  fi
  $SUDO mv "$tmp/rebar3" /usr/local/bin/rebar3
  log "rebar3 installed ($(rebar3 version 2>/dev/null))."
fi

# --- Persist PATH for the session -------------------------------------------
# Put OTP first so its erl/erlc win over any apt-provided (older) Erlang.
if [ -n "${CLAUDE_ENV_FILE:-}" ]; then
  echo "export PATH=\"$OTP_DIR/bin:\$PATH\"" >> "$CLAUDE_ENV_FILE"
fi

otp_release="$(erl -noshell -eval 'io:format("~s",[erlang:system_info(otp_release)]),halt().' 2>/dev/null)"
rebar_version="$(rebar3 version 2>/dev/null)"
log "Toolchain ready: OTP ${otp_release} / ${rebar_version}"
