
[private]
default:
  @just --list
  
# Show current version number
@showvers:
  grep '^\s*version' build.sbt


