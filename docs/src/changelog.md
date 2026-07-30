# Changelog

All notable changes to this project are documented here. This page mirrors
[`CHANGELOG.md`](https://github.com/nerima-lisp/cl-regex-kit/blob/main/CHANGELOG.md)
at the repository root, which remains the source of truth. Releases are also
listed on the [GitHub releases page](https://github.com/nerima-lisp/cl-regex-kit/releases).

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Fixed

- Make non-overlapping iteration suppress an empty match immediately after a
  non-empty match, matching Rust `Regex` iteration, splitting, and replacement
  semantics.
- Reject a single string passed as a regex-set pattern container instead of
  treating its characters as individual malformed patterns.
- Validate replacement values and split inputs even when a replacement or split
  is skipped by an empty limit or a non-matching pattern.

### Added

- Add Rust `Regex::split_inclusive`-compatible delimiter-retaining splitting
  through `split-inclusive` for character and byte regexes.
- Add RE2/Rust-compatible one- through three-digit octal escapes, rejecting
  values outside the byte range.
- Add empty character classes: `[]` matches no input, while `[^]` matches any
  alphabet element, including for byte regexes and character-class set
  operations.
- Allow byte regex and byte regex set builders to use any octet as
  `:line-terminator`, including non-ASCII values.
- Complete the Thompson NFA and Pike VM implementation for RE2/Rust-compatible
  linear-time matching, including capture, byte-regex, replacement, and
  regex-set APIs.
