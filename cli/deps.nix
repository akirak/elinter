{ pkgs ? import <nixpkgs> { } }:
let
  easyPS = import (pkgs.fetchFromGitHub {
    owner = "justinwoo";
    repo = "easy-purescript-nix";
    rev = "aa3e608608232f4a009b5c132ae763fdabfb4aba";
    sha256 = "0y6jikncxs9l2zgngbd1775f1zy5s1hdc5rhkyzsyaalcl5cajk8";
    # date = 2020-03-16T09:31:55+01:00;
  }) { };

  spago2nix = import (pkgs.fetchFromGitHub {
    owner = "justinwoo";
    repo = "spago2nix";
    rev = "704fc193dd1066d3bee91e525ad5ea4876ad990e";
    sha256 = "1g82s3wz18lxif3pdd9nk6vb3c5cy1i1w5xpkl9gpvc44x8w7lrl";
    # date = 2020-04-09T19:01:13+02:00;
  }) { };

  inputs = { inherit (easyPS) spago purs spago2nix; };
  buildInputs = builtins.attrValues inputs;
  shell = pkgs.mkShell { buildInputs = buildInputs ++ [ pkgs.nodejs ]; };
in inputs // { inherit buildInputs shell; }
