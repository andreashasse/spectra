#!/bin/bash
#
# SessionStart hook for spectra (Claude Code on the web).
#
# Installs the pinned BEAM toolchain (see .tool-versions) so that the Makefile
# targets (make hank / build-test / test / proper / dialyzer) work in remote
# sessions, which start from a bare container without Erlang/Elixir/rebar3.
#
# Toolchain acquisition strategy:
#   * Erlang/OTP + Elixir: prefer the PRECOMPILED builds from builds.hex.pm
#     (fast: download + extract). This requires `builds.hex.pm` to be on the
#     environment's egress allowlist. If that host is blocked, fall back to
#     building OTP from source via a git clone of github.com/erlang/otp
#     (slow the first time, but git-over-HTTPS to github.com is allowed).
#   * rebar3: bootstrapped from a git clone of github.com/erlang/rebar3 at the
#     pinned tag (robust; does not depend on release-asset downloads).
#
# The install is idempotent and cached: once a container has been provisioned,
# re-running the hook is a fast no-op. PATH is exported for the session via
# $CLAUDE_ENV_FILE.
set -euo pipefail

log() { echo "[session-start] $*" >&2; }

# --- pinned versions (kept in sync with .tool-versions) ----------------------
OTP_VERSION="28.2"
ELIXIR_VERSION="1.18.4"
REBAR3_VERSION="3.27.0"
# builds.hex.pm publishes Elixir builds keyed by the OTP MAJOR they were built
# against.
ELIXIR_OTP_MAJOR="28"
# Target used by builds.hex.pm precompiled OTP tarballs (matches this image).
HEX_OTP_TARGET="ubuntu-24.04"

# --- install locations (persist in the cached container image) ---------------
TOOLCHAIN_DIR="${TOOLCHAIN_DIR:-$HOME/.local/spectra-toolchain}"
OTP_DIR="$TOOLCHAIN_DIR/otp-$OTP_VERSION"
ELIXIR_DIR="$TOOLCHAIN_DIR/elixir-$ELIXIR_VERSION"
BIN_DIR="$TOOLCHAIN_DIR/bin"
SRC_DIR="$TOOLCHAIN_DIR/src"
mkdir -p "$TOOLCHAIN_DIR" "$BIN_DIR" "$SRC_DIR"

export PATH="$OTP_DIR/bin:$ELIXIR_DIR/bin:$BIN_DIR:$PATH"

# --- helpers -----------------------------------------------------------------
otp_ok() {
  [ -x "$OTP_DIR/bin/erl" ] && \
    [ "$("$OTP_DIR/bin/erl" -noshell -eval \
        'io:format("~s",[erlang:system_info(otp_release)]), halt().' 2>/dev/null)" = \
      "${OTP_VERSION%%.*}" ]
}

rebar3_ok() {
  [ -x "$BIN_DIR/rebar3" ] && \
    "$BIN_DIR/rebar3" version 2>/dev/null | grep -q "rebar $REBAR3_VERSION"
}

elixir_ok() {
  [ -x "$ELIXIR_DIR/bin/elixir" ] && \
    "$ELIXIR_DIR/bin/elixir" --version 2>/dev/null | grep -q "Elixir $ELIXIR_VERSION"
}

apt_build_deps() {
  log "installing OTP build dependencies via apt"
  local sudo=""
  command -v sudo >/dev/null 2>&1 && sudo="sudo"
  $sudo apt-get update -qq || true
  $sudo apt-get install -y --no-install-recommends \
    build-essential autoconf m4 libncurses-dev libssl-dev git ca-certificates
}

# --- Erlang/OTP --------------------------------------------------------------
install_otp_precompiled() {
  local url="https://builds.hex.pm/builds/otp/${HEX_OTP_TARGET}/OTP-${OTP_VERSION}.tar.gz"
  log "trying precompiled OTP: $url"
  local tmp
  tmp="$(mktemp -d)"
  if ! curl -fsSL --max-time 300 "$url" -o "$tmp/otp.tar.gz"; then
    log "precompiled OTP not reachable (host likely not allowlisted); will build from source"
    rm -rf "$tmp"
    return 1
  fi
  rm -rf "$OTP_DIR"
  mkdir -p "$OTP_DIR"
  tar -xzf "$tmp/otp.tar.gz" -C "$OTP_DIR"
  rm -rf "$tmp"
  # The tarball may or may not have a single top-level dir; locate the Install script.
  local install_script
  install_script="$(find "$OTP_DIR" -maxdepth 3 -name Install -type f | head -1)"
  if [ -z "$install_script" ]; then
    log "could not find OTP Install script in the precompiled tarball"
    return 1
  fi
  local root
  root="$(dirname "$install_script")"
  ( cd "$root" && ./Install -minimal "$root" >/dev/null )
  # If OTP landed in a nested dir, expose bin/ at $OTP_DIR/bin.
  if [ ! -x "$OTP_DIR/bin/erl" ] && [ -x "$root/bin/erl" ]; then
    ln -sfn "$root/bin" "$OTP_DIR/bin"
  fi
  otp_ok
}

install_otp_source() {
  apt_build_deps
  local src="$SRC_DIR/otp"
  if [ ! -d "$src/.git" ]; then
    log "cloning OTP $OTP_VERSION source (git)"
    rm -rf "$src"
    git clone --depth 1 --branch "OTP-$OTP_VERSION" \
      https://github.com/erlang/otp.git "$src"
  fi
  log "building OTP $OTP_VERSION from source (this can take several minutes)"
  (
    cd "$src"
    export ERL_TOP="$src"
    ./otp_build autoconf
    ./configure --prefix="$OTP_DIR" \
      --without-wx --without-observer --without-debugger --without-et \
      --without-megaco --without-jinterface --without-odbc
    make -j"$(nproc)"
    make install
  )
  otp_ok
}

ensure_otp() {
  if otp_ok; then
    log "OTP $OTP_VERSION already present"
    return 0
  fi
  install_otp_precompiled || install_otp_source
  otp_ok || { log "FATAL: failed to provision OTP $OTP_VERSION"; exit 1; }
  log "OTP ready: $("$OTP_DIR/bin/erl" -noshell -eval \
    'io:format("~s",[erlang:system_info(otp_release)]),halt().')"
}

# --- rebar3 ------------------------------------------------------------------
ensure_rebar3() {
  if rebar3_ok; then
    log "rebar3 $REBAR3_VERSION already present"
    return 0
  fi
  local src="$SRC_DIR/rebar3"
  if [ ! -d "$src/.git" ]; then
    log "cloning rebar3 $REBAR3_VERSION source (git)"
    rm -rf "$src"
    git clone --depth 1 --branch "$REBAR3_VERSION" \
      https://github.com/erlang/rebar3.git "$src"
  fi
  log "bootstrapping rebar3 $REBAR3_VERSION"
  ( cd "$src" && ./bootstrap )
  install -m 0755 "$src/rebar3" "$BIN_DIR/rebar3"
  rebar3_ok || { log "FATAL: failed to provision rebar3 $REBAR3_VERSION"; exit 1; }
  log "rebar3 ready"
}

# --- Elixir (best-effort: needed for `make test`'s Elixir helpers) -----------
install_elixir_precompiled() {
  local url="https://builds.hex.pm/builds/elixir/v${ELIXIR_VERSION}-otp-${ELIXIR_OTP_MAJOR}.zip"
  log "trying precompiled Elixir: $url"
  local tmp
  tmp="$(mktemp -d)"
  if ! curl -fsSL --max-time 300 "$url" -o "$tmp/elixir.zip"; then
    rm -rf "$tmp"
    return 1
  fi
  rm -rf "$ELIXIR_DIR"
  mkdir -p "$ELIXIR_DIR"
  ( cd "$ELIXIR_DIR" && unzip -q "$tmp/elixir.zip" )
  rm -rf "$tmp"
  elixir_ok
}

install_elixir_source() {
  local src="$SRC_DIR/elixir"
  if [ ! -d "$src/.git" ]; then
    log "cloning Elixir $ELIXIR_VERSION source (git)"
    rm -rf "$src"
    git clone --depth 1 --branch "v$ELIXIR_VERSION" \
      https://github.com/elixir-lang/elixir.git "$src"
  fi
  log "building Elixir $ELIXIR_VERSION from source"
  ( cd "$src" && make -j"$(nproc)" >/dev/null )
  rm -rf "$ELIXIR_DIR"
  mkdir -p "$ELIXIR_DIR"
  cp -a "$src/bin" "$src/lib" "$ELIXIR_DIR/"
  elixir_ok
}

ensure_elixir() {
  if elixir_ok; then
    log "Elixir $ELIXIR_VERSION already present"
    return 0
  fi
  if command -v unzip >/dev/null 2>&1 && install_elixir_precompiled; then
    log "Elixir ready (precompiled)"
    return 0
  fi
  if install_elixir_source; then
    log "Elixir ready (source)"
    return 0
  fi
  # Non-fatal: only `make test`'s optional Elixir compilation needs this.
  log "WARNING: could not provision Elixir $ELIXIR_VERSION; continuing without it"
}

# --- main --------------------------------------------------------------------
ensure_otp
ensure_rebar3
ensure_elixir

# Persist PATH (and Elixir's UTF-8 flag) for the rest of the session.
if [ -n "${CLAUDE_ENV_FILE:-}" ]; then
  {
    echo "export PATH=\"$OTP_DIR/bin:$ELIXIR_DIR/bin:$BIN_DIR:\$PATH\""
    # The base image runs with a latin1 name encoding; without this Elixir warns
    # it "may malfunction". +fnu forces UTF-8 filename handling for the Erlang VM.
    echo "export ELIXIR_ERL_OPTIONS=\"+fnu\""
  } >> "$CLAUDE_ENV_FILE"
  log "exported toolchain PATH via CLAUDE_ENV_FILE"
fi

log "toolchain ready (OTP $OTP_VERSION, rebar3 $REBAR3_VERSION, Elixir $ELIXIR_VERSION)"
