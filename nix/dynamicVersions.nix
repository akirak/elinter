{ pkgs ? import <nixpkgs> {}
, elispFile ? null
, spec
}:
with builtins;
with pkgs;
let
  emacs-ci = import (import ./sources.nix).nix-emacs-ci;
  knownVersions = map (name: lib.replaceStrings [ "-" ] [ "." ] (lib.removePrefix "emacs-" name)) (attrNames emacs-ci);
  source = readFile elispFile;
  headers = filter (s: isString s && match ";;+ *Package-Requires: .+" s != null) (split "\n" source);
  header = if length headers > 0 then head headers else null;
  emacsVersionFromHeader =
    if header != null
    then builtins.match ".+\\(emacs \"([.[:digit:]]+)\"\\).+" header
    else null;
  minVersion =
    if length emacsVersionFromHeader > 0
    then head emacsVersionFromHeader
    else null;
  compareEmacsVersions = v1: v2:
    if v1 == "snapshot" then
      true
    else if v2 == "snapshot" then
      false
    else
      compareVersions v1 v2 >= 0;
  descendingVersions = sort compareEmacsVersions knownVersions;
  latestStable = head (filter (v: v != "snapshot") descendingVersions);
in
if spec == "min"
then [ minVersion ]
else if spec == "latest"
then [ latestStable ]
else if spec == "all"
then filter (v: compareEmacsVersions v minVersion) descendingVersions
else throw "unsupported spec"
