# Generate a list of Emacs versions from a possibly abstract version
# spec.
{ pkgs ? import <nixpkgs> {}
, elispFile ? null
, spec
}:
with builtins;
with pkgs;
let
  elinterLib = import ./lib.nix;

  emacs-ci = import (elinterLib.sourceWithFallback "nix-emacs-ci");

  attrNameToVersion = name:
    lib.replaceStrings [ "-" ] [ "." ]
      (lib.removePrefix "emacs-" name);

  # List of Emacs versions available from nix-emacs-ci
  knownVersions = map attrNameToVersion (attrNames emacs-ci);

  # Find a Package-Requires library header line
  #
  # This is used for determining the minimum Emacs Version of the
  # package.
  #
  # Since only the first line is matched, the Emacs dependency should
  # appear at the beginning.
  isPackageLine = s: isString s && match ";;+ *Package-Requires: .+" s != null;
  headers = filter isPackageLine (split "\n" (readFile elispFile));
  header = if length headers > 0 then head headers else null;

  # Retrieve the minimum version from the header.
  emacsVersionFromHeader =
    if header != null
    then builtins.match ".+\\(emacs \"([.[:digit:]]+)\"\\).+" header
    else null;
  minVersion =
    if length emacsVersionFromHeader > 0
    then head emacsVersionFromHeader
    else null;

  # Retrieve a sorted list of Emacs versions available, including
  # "snapshot".
  compareEmacsVersions = v1: v2:
    if v1 == "snapshot" then
      true
    else if v2 == "snapshot" then
      false
    else
      compareVersions v1 v2 >= 0;
  descendingVersions = sort compareEmacsVersions knownVersions;

  # The latest stable version is the first entry excluding snapshot.
  latestStable = head (filter (v: v != "snapshot") descendingVersions);
in

if spec == "min"
then
  [ minVersion ]
else if spec == "latest"
then
  [ latestStable ]
else if spec == "all"
then
  filter (v: compareEmacsVersions v minVersion) descendingVersions
else if isString spec
then
  [ spec ]
else
  throw "Version spec must be a string"
