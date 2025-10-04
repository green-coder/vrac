# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),

## [Unreleased]

## [0.1.2] - 2025-10-04

### Fixed

- Cljdoc documentation is fixed.
- Fixed a bug which prevented reactive fragments (e.g. if, for) to be used directly inside each other.

### Changed

- Bumped the dependencies on the example projects, notably Shadow-CLJS.

## [0.1.1] - 2025-04-24

### Changed

- Uses a new format for naming the attributes, properties and event handlers (#5).
- `vrac.web/attributes-effect` renamed to `vrac.web/props-effect`.

### Added

- Documentation about the attributes and properties in Vcup.

## [0.1.0] - 2025-03-17

### Added

- the `vrac.web` namespace
- some examples in the `/examples` directory
