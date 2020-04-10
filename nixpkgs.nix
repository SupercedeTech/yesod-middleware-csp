let
  owner = "NixOS";
  repo = "nixpkgs";
  # https://releases.nixos.org/nixos/19.09/nixos-19.09.1965.274e095f761
  rev = "274e095f761b2da76a376d105c41591739350b14";
  url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz";
in
  import (builtins.fetchTarball url)
