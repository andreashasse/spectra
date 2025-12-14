# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.2.0] - 2025-12-14

### Changed
- **Breaking**: Extra fields in JSON objects are now ignored during deserialization instead of causing a `not_matched_fields` error. This affects map, struct (elixir) and record deserialization.

### Notes
- The old strict validation behavior has been commented out with a TODO to potentially add it back as a configuration option in the future
